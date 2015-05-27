{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Potrace
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  c.chalmers@me.com
--
-- Trace bitmap images into vector paths using the potrace library. This
-- module also provides helpers for turning any "JuicyPixel"s image to a
-- bitmap.
--
module Graphics.Potrace
  (
    -- * Tracing
    trace
  , trace'
  , traceForest
  , traceForest'

    -- * Path type
  , Curve (..)
  , Segment (..)
  , P2 (..)

    -- * Bitmaps
  , Bitmap
  , generate
  , fromImage
  , toImage
  , lumaThreshold

  -- * Parameters
  , Parameters (..)
  , TurnPolicy (..)

  -- ** Lenses
  , Lens'
  , turdSize
  , turnPolicy
  , alphaMax
  , optTolerance
  )
  where

import Data.Bool
import Codec.Picture.Types

import Graphics.Potrace.Base

------------------------------------------------------------------------
-- Images
------------------------------------------------------------------------

-- | Generate a bitmap by apply the predicate function to each pixel of
--   "JuicyPixels" image. 'True' corresponds to a black pixel, 'False'
--   corresponds to white.
fromImage :: Pixel a => Image a -> (a -> Bool) -> Bitmap
fromImage img@(Image w h _) f =
  generate w h $ \i j -> f $ pixelAt img i (h - j - 1)
  -- potrace starts at bottom left, juicy starts at top left

-- | Convert a 'Bitmap' to an 'image' by using the given pixels for
--   'True' and 'False'. This is mainly here for debugging purposes.
toImage :: Pixel a => Bitmap -> a -> a -> Image a
toImage bm@(Bitmap w h _ _) on off = generateImage f w h
  where f i j = bool off on $ index bm i (h - j - 1)

-- | Generate a bitmap choosing pixels according to their luma plane for
--   a threshold given between 0 and 1. Anything below the threshold is
--   white. Anything above the threshold is black. Throws an error for
--   CMYK images.
lumaThreshold :: DynamicImage -> Double -> Bitmap
lumaThreshold dimg t = case dimg of
  ImageY8 i     -> word8 i
  ImageY16 i    -> word16 i
  ImageYF i     -> float i
  ImageYA8 i    -> word8 i
  ImageYA16 i   -> word16 (dropAlphaLayer i)
  ImageRGB8 i   -> word8 i
  ImageRGB16 i  -> word16 i
  ImageRGBF i   -> float i
  ImageRGBA8 i  -> word8 i
  ImageRGBA16 i -> word16 (dropAlphaLayer i)
  ImageYCbCr8 i -> word8 i
  _             -> error "unable to compute lumaTheshold for CMYK images"
  -- ImageCMYK8 i  -> word8 (promoteImage i :: Image PixelRGB8)
  -- ImageCMYK16 i -> word16 (promoteImage i :: Image PixelRGB16)
  where
  word8 :: (PixelBaseComponent a ~ Pixel8, LumaPlaneExtractable a)
        => Image a -> Bitmap
  word8 img = fromImage img ((<x) . computeLuma)
    where x = round $ t * 255

  word16 :: (PixelBaseComponent a ~ Pixel16, LumaPlaneExtractable a)
        => Image a -> Bitmap
  word16 img = fromImage img ((<x) . computeLuma)
    where x = round $ t * 65535

  float :: (PixelBaseComponent a ~ PixelF, LumaPlaneExtractable a)
        => Image a -> Bitmap
  float img = fromImage img ((<x) . computeLuma)
    where x = realToFrac t

-- Lenses --------------------------------------------------------------

-- | A van Laarhoven lens, compatible with various lens libraries.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

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

-- | The `alphamax` parameter is a threshold for the detection of
--   corners.  It controls the smoothness of the traced curve.The useful
--   range of this parameter is from 0.0 (polygon) to 1.3334 (no
--   corners).
--
--   Default is @1.0@.
--
-- <<diagrams/alphaMax.svg#diagram=alphaMaxExample&width=500>>
alphaMax :: Lens' Parameters Double
alphaMax f ps = f (_alphaMax ps) <&> \p' -> ps { _alphaMax = p' }

-- | The `optTolerance` parameter defines the amount of error allowed in
--   this simplification. Larger values tend to decrease the number of
--   segments, at the expense of less accuracy. The useful range is from
--   0 to infinity, although in practice one would hardly choose values
--   greater than 1 or so. For most purposes, the default value is a
--   good trade off between space and accuracy. 'Nothing' turns
--   simplification off.
--
--   Default is @'Just' 1.0@.
optTolerance :: Lens' Parameters (Maybe Double)
optTolerance f ps = f (_optTolerance ps) <&> \p' -> ps { _optTolerance = p' }

------------------------------------------------------------------------
-- SVG
------------------------------------------------------------------------

-- | Very basic svg conversion. If you want more control over the output
--   consider "diagrams-potrace".

-- path :: [P.Loop] -> [Builder] -> Builder
-- path ls attr = "<path " <> attrs <> d_ (loops ls)

-- d_ :: Builder -> Builder
-- d_ attrs = "d=\"" <> d <> "\""

-- -- | Multiple loops together in order, suitable to put inside a \"d\" attribute.
-- loops :: [P.Loop] -> Builder
-- loops = foldMap loop

-- -- | A single loop in an svg suitable to put inside a \"d\" attribute.
-- loop :: P.Loop -> Builder
-- loop (Loop p0 segs) = "M" <> pt p0 <> foldMap seg path <> " Z"
--   where
--     seg (Cubic p1 p2 p3) = "C" <> pt p1 <> " " <> pt p2 <> " " <> pt p3
--     seg (Corner p1 p2)   = "L" <> pt p1 <> "L" <> pt p2

-- mv :: Point -> Builder
-- mv p = "M " <> pt p
-- {-# INLINE mv #-}

-- pt :: Point -> Builder
-- pt (P2 x y) = n x <> " " <> n (-y)
-- {-# INLINE pt #-}

-- -- Not the fastest way but it'll do. 4 d.p. should be enough.
-- n :: Double -> Builder
-- n = string7 . showFFloat (Just 4)
-- {-# INLINE n #-}

