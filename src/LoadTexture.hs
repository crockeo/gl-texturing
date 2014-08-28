module LoadTexture (loadTexture) where

import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (PixelInternalFormat (..))
import Codec.Picture (DynamicImage (..))
import Control.Applicative ((<$>))
import Graphics.Rendering.OpenGL
import Data.ByteString.Unsafe
import Codec.Picture.Types
import Codec.Picture.Repa
import Data.ByteString
import Foreign.Ptr

{-|
  Getting the size from a @'DynamicImage'@.
-}
getInfo :: DynamicImage -> (Int, Int, PixelInternalFormat)
getInfo (ImageRGB8   (Image w h _)) = (w, h, RGB8)
getInfo (ImageRGB16  (Image w h _)) = (w, h, RGB16)
getInfo (ImageRGBA8  (Image w h _)) = (w, h, RGBA8)
getInfo (ImageRGBA16 (Image w h _)) = (w, h, RGBA16)

-- Loading a texture
loadTexture :: FilePath -> IO (TextureObject, Size)
loadTexture path = do
  img <- either error id <$> readImageRGBA path

  let dynimg         = imgToImage img
      (w, h, format) = getInfo dynimg
      glSize         = TextureSize2D (fromIntegral w) (fromIntegral h)
      bs             = toByteString img

  ptr <- unsafeUseAsCString bs $ \cstr ->
    return $ castPtr cstr

  [t] <- genObjectNames 1

  textureBinding Texture2D $= Just t
  texImage2D Texture2D NoProxy 0 format glSize 0 (PixelData BGRA UnsignedByte ptr)

  return (t, Size (fromIntegral w) (fromIntegral h))

