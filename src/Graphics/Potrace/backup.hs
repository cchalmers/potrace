{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Graphics.Potrace.Base where
import Data.Bits
import Bindings.Potrace
import qualified Data.Vector.Storable as V
-- import Data.Vector.Storable ((!))
-- import Data.Word
-- import Control.Exception (bracket)
-- import Foreign.C.String (peekCString)
import Foreign.C.Types
import Foreign.Ptr
-- import Foreign.Storable (Storable, poke)
import System.IO.Unsafe (unsafePerformIO)
-- import Codec.Picture
-- import Codec.Picture.Types
-- import qualified Data.ByteString as B
import Foreign.Storable
import Control.Applicative
import Foreign.Marshal.Alloc
import Data.Monoid
import Data.Tree
import Data.Default

-- | A mid-level interface to potrace. Includes helpers for making
--   bitmap images. This is useful for other librarys that want to
--   impliment their own tools on top of potrace (see
--   "potrace-diagrams").
--
--   In principle this could be in a

------------------------------------------------------------------------
-- Bitmap
------------------------------------------------------------------------

-- | Data type to represent a bit packed image. This can be passed to
--   potrace via 'trace'. The constructor is exported by
--   'Graphics.Potrace.Base' but be aware the underlying vector has a
--   pecurier, host dependent from.
data Bitmap = Bitmap
  { width  :: Int
  , height :: Int
  , scan   :: Int
  , img    :: V.Vector CULong
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

-- | Parameters to control the tracing operation of potrace.
data Parameters = Parameters
  { _turdSize     :: Int
  , _turnPolicy   :: TurnPolicy
  , _alphaMax     :: Double
  , _optTolerance :: (Maybe Double)
  }

instance Default Parameters where
  def = Parameters
    { _turdSize     = 2
    , _turnPolicy   = MinorityTP
    , _alphaMax     = 1.0
    , _optTolerance = Just 0.2
    }

type Lens' s a = Functor f => (a -> f a) -> s -> f s

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

-- | The 'turdSize' parameter can be used to “despeckle” the bitmap to
--   be traced, by removing all curves whose enclosed area is below the
--   given threshold.
--
--   Default is @2@.
turdSize :: Lens' Parameters Int
turdSize f ps = f (_turdSize ps) <&> \p' -> ps { _turdSize = p' }

-- | The 'TurnPolicy' parameter determines how to resolve ambiguities
--   during decomposition of bitmaps into paths.
--
--   Default is 'MinorityTP'.
turnPolicy :: Lens' Parameters TurnPolicy
turnPolicy f ps = f (_turnPolicy ps) <&> \p' -> ps { _turnPolicy = p' }

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


-- | The `alphamax` parameter is a threshold for the detection of
--   corners.  It controls the smoothness of the traced curve.The useful
--   range of this parameter is from 0.0 (polygon) to 1.3334 (no
--   corners).
--
--   Default is @1.0@.
alphaMax :: Lens' Parameters Double
alphaMax f ps = f (_alphaMax ps) <&> \p' -> ps { _alphaMax = p' }

-- | The `optTolerance` parameter defines the amount of error allowed in
--   this simplification. Larger values tend to decrease the number of
--   segments, at the expense of less accuracy. The useful range is from
--   0 to infinity, although in practice one would hardly choose values
--   greater than 1 or so. For most purposes, the default value is a
--   good tradeoff between space and accuracy. 'Nothing' turns
--   simplification off.
--
--   Default is @'Just' 1.0@.
optTolerance :: Lens' Parameters (Maybe Double)
optTolerance f ps = f (_optTolerance ps) <&> \p' -> ps { _optTolerance = p' }

-- Parameter internals -------------------------------------------------

-- | Allocate new parameters from potrace's default parameters. This
--   should protect against future changes to the options. When finished
--   these should be deallocated with 'p'potrace_param_free'
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
-- Internals
------------------------------------------------------------------------

type State = C'potrace_state_s

-- | Data type representing a 2D point were the origin is at the bottom
--   left.
data P2 = P2 {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show, Read)

-- | potrace defines a segment as either 'Bezier' or a 'Corner' (in most
--   systems this is equivilent to linear segments).
data Segment
  = Bezier {-# UNPACK #-} !P2 {-# UNPACK #-} !P2 {-# UNPACK #-} !P2
  | Corner {-# UNPACK #-} !P2 {-# UNPACK #-} !P2
  deriving (Show, Read)

-- | A curve with it's starting point. (Starting point is actually the
--   last point of the
data Curve = Curve {-# UNPACK #-} !P2 [Segment]

-- | Go though the curve and turn in into a list of segments.
curve :: C'potrace_curve_s -> IO Curve
curve (C'potrace_curve_s n ts ps_) = Curve <$> p0 <*> go 0 where
  ps = castPtr ps_
  -- The last point is the starting point
  p0 = peekElemOff ps (3*fromIntegral n + 2)
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

-- -- pic :: Image Pixel1 -> [[Segment]]
-- trace :: Parameters -> Bitmap -> [Curve]
-- trace ps (Bitmap w h r v) = unsafePerformIO $ V.unsafeWith v $ \p -> do
--   c_param <- params ps
--   alloca $ \c_bitmap -> do
--     poke c_bitmap $ C'potrace_bitmap_s (fromIntegral w) (fromIntegral h) (fromIntegral r) p
--     c_status <- c'potrace_trace c_param c_bitmap
--     path     <- peek c_status >>= pathList

--     c'potrace_state_free c_status
--     c'potrace_param_free c_param
--     return path

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

trace :: Bitmap -> [Curve]
trace = trace' def

trace' :: Parameters -> Bitmap -> [Curve]
trace' p bm = unsafePerformIO $ unsafeWithImage pathList p bm

traceForest' :: Parameters -> Bitmap -> Forest Curve
traceForest' p bm = unsafePerformIO $ unsafeWithImage pathForest p bm

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- -- P2 instances --------------------------------------------------------

-- newtype instance U.Vector    P2 =  V_P2 (U.Vector    Double)
-- newtype instance U.MVector s P2 = MV_P2 (U.MVector s Double)
-- instance U.Unbox P2

-- instance U.Unbox a => M.MVector U.MVector (P2 a) where
--   basicLength (MV_P2 v)             = 2*M.basicLength v
--   basicUnsafeSlice m n (MV_P2 v)    = MV_P2 n (M.basicUnsafeSlice (2*m) (2*n) v)
--   basicOverlaps (MV_P2 v) (MV_P2 u) = M.basicOverlaps v u
--   basicUnsafeNew n                  = MV_P2 `liftM` M.basicUnsafeNew (2*n)
--   basicUnsafeRead (MV_P2 v) i =
--     P2 `liftM` M.basicUnsafeRead v o
--        `ap`    M.basicUnsafeRead v (o+1)
--     where o = 2*i
--   basicUnsafeWrite (MV_P2 v) i (P2 x y) = do
--     M.basicUnsafeWrite v o     x
--     M.basicUnsafeWrite v (o+1) y
--     where o = 2*i

-- instance U.Unbox a => G.Vector U.Vector (P2 a) where
--   basicUnsafeFreeze (MV_P2 v)   = V_P2 `liftM` G.basicUnsafeFreeze v
--   basicUnsafeThaw   ( V_P2 v)   = MV_P2 `liftM` G.basicUnsafeThaw   v
--   basicLength       ( V_P2 _)   = 2 * basicLength vn
--   basicUnsafeSlice m n (V_P2 v) = V_P2 n (G.basicUnsafeSlice (2*m) (2*n) v)
--   basicUnsafeIndexM (V_P2 v) i  =
--     P2 <$> G.basicUnsafeIndexM v o
--        <*> G.basicUnsafeIndexM v (o + 1)
--     where o = 2*i

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

-- -- Segment instances ---------------------------------------------------

-- -- This is a funny unboxed instance because it uses Storable vector's as
-- -- it's base type. There's a good reason for this. We can use potrace's
-- -- vectors without copying them if they're storable. But we don't get a
-- -- nice interface with storable vectors but Unboxed is general enough to
-- -- do so.
-- data instance MVector s Segment = MV_Seg {-# UNPACK #-} !(S.MVector s Word8)
--                                          {-# UNPACK #-} !(S.MVector s P2)
-- data instance Vector    Segment = V_Seg  {-# UNPACK #-} !(S.Vector    Word8)
--                                          {-# UNPACK #-} !(S.Vector s P2)

-- instance Unbox Segment

-- instance M.MVector MVector Bool where
--   {-# INLINE basicLength #-}
--   {-# INLINE basicUnsafeSlice #-}
--   {-# INLINE basicOverlaps #-}
--   {-# INLINE basicUnsafeNew #-}
--   {-# INLINE basicUnsafeReplicate #-}
--   {-# INLINE basicUnsafeRead #-}
--   {-# INLINE basicUnsafeWrite #-}
--   {-# INLINE basicClear #-}
--   {-# INLINE basicSet #-}
--   {-# INLINE basicUnsafeCopy #-}
--   {-# INLINE basicUnsafeGrow #-}
--   basicLength (MV_Seg x _) = M.basicLength x
--   basicUnsafeSlice i n (MV_Seg x v) =
--     MV_Seg (M.basicUnsafeSlice i n v)
--            (M.basicUnsafeSlice (3*i) (3*n) v)
--   basicOverlaps (MV_Seg v1) (MV_Seg v2) =
--     M.basicOverlaps x1 x2 &&
--     M.basicOverlaps v1 v2
--   basicUnsafeNew n =
--     MV_Seg `liftM` M.basicUnsafeNew n
--            `ap`    M.basicUnsafeNew (3*n)
--   basicUnsafeRead (MV_Seg x v) i =
--     M.basicUnsafeRead i x >>= \case
--       1 -> Bezier `liftM` S.basicUnsafeRead ps (o + 0)
--                   `ap`    S.basicUnsafeRead ps (o + 1)
--                   `ap`    S.basicUnsafeRead ps (o + 2)
--       _ -> Corner `liftM` M.basicUnsafeRead ps (o + 1)
--                   `ap`    M.basicUnsafeRead ps (o + 2)
--       where o = 3*i
--   basicUnsafeWrite (MV_Seg x v) i (Bezier p0 p1 p2) = do
--     M.basicUnsafeWrite x i 1
--     M.basicUnsafeWrite v (3*i) p0
--     M.basicUnsafeWrite v (3*i+1) p1
--     M.basicUnsafeWrite v (3*i+2) p2
--   basicUnsafeWrite (MV_Seg x v) i (Corner p1 p2) = do
--     M.basicUnsafeWrite x i 2
--     M.basicUnsafeWrite v (3*i+1) p1
--     M.basicUnsafeWrite v (3*i+2) p2
--   basicClear (MV_Seg x v) = do
--     M.basicClear x
--     M.basicClear v
--   basicUnsafeCopy (MV_Seg x1 v1) (MV_Seg x2 v2) = do
--     M.basicUnsafeCopy x1 x2
--     M.basicUnsafeCopy v1 v2
--   basicUnsafeMove (MV_Seg x1 v1) (MV_Seg x2 v2) = do
--     M.basicUnsafeMove x1 x2
--     M.basicUnsafeMove v1 v2
--   basicUnsafeGrow (MV_Seg v) n =
--     MV_Seg `liftM` M.basicUnsafeGrow x n
--            `ap`    M.basicUnsafeGrow v (3*n)

-- instance G.Vector Vector Segment where
--   {-# INLINE basicUnsafeFreeze #-}
--   {-# INLINE basicUnsafeThaw #-}
--   {-# INLINE basicLength #-}
--   {-# INLINE basicUnsafeSlice #-}
--   {-# INLINE basicUnsafeIndexM #-}
--   {-# INLINE elemseq #-}
--   basicUnsafeFreeze (MV_Seg x v) =
--     V_Seg `liftM` G.basicUnsafeFreeze x
--           `ap`    G.basicUnsafeFreeze v
--   basicUnsafeThaw (V_Seg x v) =
--     MV_Seg `liftM` G.basicUnsafeThaw x
--            `ap`    G.basicUnsafeThaw v
--   basicLength (V_Seg x _) = G.basicLength x
--   basicUnsafeSlice i n (V_Seg v)         =
--     V_Seg (G.basicUnsafeSlice i n v)
--           (G.basicUnsafeSlice (3*i) (3*n) v)
--   basicUnsafeIndexM (V_Seg x v) i =
--     G.basicUnsafeIndexM x i >>= \case
--       1 -> Bezier `liftM` M.basicUnsafeIndexM ps (3*i + 0)
--                   `ap`    M.basicUnsafeIndexM ps (3*i + 1)
--                   `ap`    M.basicUnsafeIndexM ps (3*i + 2)
--       _ -> Corner `liftM` M.basicUnsafeIndexM ps (3*i + 1)
--                   `ap`    M.basicUnsafeIndexM ps (3*i + 2)
--   basicUnsafeCopy (MV_Seg mx mv) (V_Seg x v) = do
--     G.basicUnsafeCopy mx x
--     G.basicUnsafeCopy mv v
--   elemseq _ = seq

