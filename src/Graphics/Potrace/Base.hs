{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Potrace.Base
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- A mid-level interface to potrace. Includes helpers for making
-- bitmap images. This is useful for other libraries that want to
-- implement their own tools on top of potrace (see
-- "potrace-diagrams").
--
-- In principle this could be in a separate module but there's already
-- three separate potrace modules and the dependencies aren't huge.


module Graphics.Potrace.Base
  (
    -- * Tracing
    Curve (..)
  , Segment (..)
  , P2 (..)
  , trace
  , trace'
  , traceForest
  , traceForest'

    -- * Bitmaps
  , Bitmap (..)
  , generate

  -- * Parameters
  , Parameters (..)
  , TurnPolicy (..)
  ) where

import           Control.Applicative
import           Data.Bits
import           Data.Default
import           Data.Monoid
import           Data.Tree
import qualified Data.Vector.Storable as V
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe (unsafePerformIO)

import           Bindings.Potrace

------------------------------------------------------------------------
-- Bitmap
------------------------------------------------------------------------

-- | Data type to represent a bit packed image. This can be passed to
--   potrace via 'trace'. The constructor is exported by
--   'Graphics.Potrace.Base' but be aware the underlying vector has a
--   host dependant form. You are advised to use 'generate' to create a
--   'Bitmap'.
data Bitmap = Bitmap
  { bitmapWidth  :: Int
  , bitmapHeight :: Int
  , bitmapDy     :: Int
  , bitmapData   :: V.Vector CULong
  }

-- | Given an image and a predicate for whether a pixel is on or off,
--   create a bit-packed image suitable for passing to potrace.
generate :: Int -> Int -> (Int -> Int -> Bool) -> Bitmap
generate w h f = Bitmap w h (n+1) v
  where
  -- The format of the vector potrace needs depends on the size and
  -- endianness of the machine.
  m = sizeOf (0 :: CULong) * 8

  -- Potrace works in blocks of the Word size m. We make each x
  -- (horizontal) line n words wide, the last word only uses r of its
  -- bits.
  (n,r) = w `divMod` m

  -- Number of potential pixels (bits) in each x line (we only use w of them).
  l = (n+1) * m

  -- Each x line is made up of n+1 words, so we need (n+1)*h words for
  -- the whole picture.
  v = V.generate ((n+1)*h) $ \i ->
    -- The starting point in the image for current block of pixels.
    let (y,x) = (i*m) `divMod` l
        -- Number of pixels we need to fill.
        c | x + m  > w = r
          | otherwise  = m

        -- Loop to fill word with bits. XXX Deal with endianness
        go !k !b
          | k < 0       = b
          | f (x + k) y = go (k - 1) $ setBit b (m - 1 - k)
          | otherwise   = go (k - 1) b

    in  go (c - 1) 0
{-# INLINE generate #-}

------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------

-- | Parameters to control the tracing operation of potrace. The default
--   parameters are
--
-- @
-- Parameters
--   { turdSize     = 2
--   , turnPolicy   = MinorityTP
--   , alphaMax     = 1.0
--   , optTolerance = 0.2
--   }
-- @
data Parameters = Parameters
  { _turdSize     :: Int            -- ^ See 'Graphics.Potrace.turdSize'.
  , _turnPolicy   :: TurnPolicy     -- ^ See 'Graphics.Potrace.turnPolicy'.
  , _alphaMax     :: Double         -- ^ See 'Graphics.Potrace.alphaMax'.
  , _optTolerance :: (Maybe Double) -- ^ See 'Graphics.Potrace.optTolerance'.
  }

instance Default Parameters where
  def = Parameters
    { _turdSize     = 2
    , _turnPolicy   = MinorityTP
    , _alphaMax     = 1.0
    , _optTolerance = Just 0.2
    }

-- | How to resolve ambiguities during decomposition of bitmaps into
--   paths.
data TurnPolicy
  = BlackTP    -- ^ Prefers to connect black (foreground) components
  | WhiteTP    -- ^ Prefers to connect white (background) components.
  | LeftTP     -- ^ Always take a left turn.
  | RightTP    -- ^ Always take a right turn.
  | MinorityTP -- ^ Prefers to connect the color (black or white) that
               --   occurs least frequently in a local neighborhood of the
               --   current position.
  | MajorityTP -- ^ Prefers to connect the color (black or white) that
               --   occurs most frequently in a local neighborhood of
               --   the current position. (default)
  | RandomTP   -- ^ Choose pseudo-randomly

-- Parameter internals -------------------------------------------------

-- | Allocate new parameters from potrace's default parameters. This
--   should protect against future changes to the options. When no
--   longer needed, these should be deallocated with
--   'p'potrace_param_free'
params :: Parameters -> IO (Ptr C'potrace_param_s)
params (Parameters ts tp am ot) = do
  c_param <- c'potrace_param_default
  poke (p'potrace_param_s'turdsize     c_param) $ fromIntegral ts
  poke (p'potrace_param_s'turnpolicy   c_param) $ turn tp
  poke (p'potrace_param_s'alphamax     c_param) $ CDouble am
  poke (p'potrace_param_s'opticurve    c_param) $ optiOn
  poke (p'potrace_param_s'opttolerance c_param) $ optiVal
  return c_param
  where (optiOn, optiVal) = case ot of
          Just t  -> (1, CDouble t)
          Nothing -> (0, 0)

turn :: TurnPolicy -> CInt
turn = \case
  BlackTP    -> c'POTRACE_TURNPOLICY_BLACK
  WhiteTP    -> c'POTRACE_TURNPOLICY_WHITE
  LeftTP     -> c'POTRACE_TURNPOLICY_LEFT
  RightTP    -> c'POTRACE_TURNPOLICY_RIGHT
  MinorityTP -> c'POTRACE_TURNPOLICY_MINORITY
  MajorityTP -> c'POTRACE_TURNPOLICY_MAJORITY
  RandomTP   -> c'POTRACE_TURNPOLICY_RANDOM

------------------------------------------------------------------------
-- Tracing
------------------------------------------------------------------------

-- | Data type representing a 2D point were the origin is at the bottom
--   left.
data P2 = P2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Read)

-- | potrace defines a segment as either 'Bezier' or a 'Corner' (in most
--   systems this is equivalent to linear segments).
data Segment
  = Bezier {-# UNPACK #-} !P2 {-# UNPACK #-} !P2 {-# UNPACK #-} !P2
  | Corner {-# UNPACK #-} !P2 {-# UNPACK #-} !P2
  deriving (Show, Read)

-- | A curve is a list of segments. The starting point is provided for
--   convenience but this is just the final point of the last segment.
data Curve = Curve {-# UNPACK #-} !P2 [Segment]

-- | Trace the bitmap image to a list of curves using potrace with 'def'
--   parameters.
trace :: Bitmap -> [Curve]
trace = trace' def

-- | Trace the bitmap image to a list of curves using potrace with given
--   parameters.
trace' :: Parameters -> Bitmap -> [Curve]
trace' p bm = unsafePerformIO $ unsafeWithImage pathList p bm

-- | Trace the bitmap image as a forest of curves using potrace with 'def'
--   parameters. Each child curve is completely contained in it's
--   parent.
traceForest :: Bitmap -> [Curve]
traceForest = trace' def

-- | Trace the bitmap image as a forest of curves using potrace with
--   given parameters parameters. Each child curve is completely
--   contained in it's parent.
traceForest' :: Parameters -> Bitmap -> Forest Curve
traceForest' p bm = unsafePerformIO $ unsafeWithImage pathForest p bm

-- Internals -----------------------------------------------------------

-- | Go though the curve and turn in into a list of segments.
curve :: C'potrace_curve_s -> IO Curve
curve (C'potrace_curve_s n ts ps_) = Curve <$> p0 <*> go 0 where
  ps = castPtr ps_
  -- The last point in the vector is also the starting point
  p0 = peekElemOff ps (3*fromIntegral n - 1)
  go i | i >= fromIntegral n = pure []
  go i = do
    let o = 3*i
    t <- peekElemOff ts i
    s <- if t == c'POTRACE_CORNER
         then Corner <$> peekElemOff ps (o + 1)
                     <*> peekElemOff ps (o + 2)
         else Bezier <$> peekElemOff ps (o + 0)
                     <*> peekElemOff ps (o + 1)
                     <*> peekElemOff ps (o + 2)
    ss <- go (i+1)
    return (s:ss)

-- | Helper for constructing lists from linked lists of pointers. If the
--   ptr is 'nullPtr' return mempty, otherwise pass the pointer value to
--   the function.
onPtr :: (Storable a, Monoid b) => (a -> IO b) -> Ptr a -> IO b
onPtr f ptr
  | ptr == nullPtr = pure mempty
  | otherwise      = peek ptr >>= f
{-# INLINE onPtr #-}

type State = C'potrace_state_s

-- | List of curves in order, suitable for passing to a renderer.
pathList :: State -> IO [Curve]
pathList (C'potrace_state_s _i ptr0 _) = go ptr0 where
  go = onPtr $ \p -> do
    s  <- curve $ c'potrace_path_s'curve p
    ss <- go (c'potrace_path_s'next p)
    pure (s:ss)

-- | Tree structure of paths where each child is enclosed within it's
--   parent.
pathForest :: State -> IO (Forest Curve)
pathForest (C'potrace_state_s _i ptr0 _) = go ptr0 where
  go = onPtr $ \p -> do
    s  <- curve $ c'potrace_path_s'curve p
    ch <- go $ c'potrace_path_s'childlist p
    sb <- go $ c'potrace_path_s'sibling p
    pure (Node s ch : sb)

unsafeWithImage :: (State -> IO r) -> Parameters -> Bitmap -> IO r
unsafeWithImage f ps (Bitmap w h dy v) = V.unsafeWith v $ \p -> do
  c_param <- params ps
  alloca $ \c_bitmap -> do
    poke c_bitmap $ C'potrace_bitmap_s (fromIntegral w) (fromIntegral h) (fromIntegral dy) p
    c_status <- c'potrace_trace c_param c_bitmap
    r        <- peek c_status >>= f

    c'potrace_state_free c_status
    c'potrace_param_free c_param
    return r
------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Storable P2 where
  sizeOf _ = 2 * sizeOf (undefined::Double)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined::Double)
  {-# INLINE alignment #-}
  poke ptr (P2 x y) = poke ptr' x >> pokeElemOff ptr' 1 y
    where ptr' = castPtr ptr
  {-# INLINE poke #-}
  peek ptr = P2 <$> peek ptr' <*> peekElemOff ptr' 1
    where ptr' = castPtr ptr
  {-# INLINE peek #-}

