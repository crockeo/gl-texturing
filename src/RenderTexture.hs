module RenderTexture where

import Graphics.Rendering.OpenGL
import Control.Monad

import LoadTexture

data Sprite = Sprite { spriteTex :: TextureObject
                     , width     :: Float
                     , height    :: Float
                     }

toGl :: Float -> GLfloat
toGl = realToFrac

{-|
  Loading a @'Sprite'@ from a file. Builds a @'Sprite'@ from the
  @'loadTexture'@ function.
-}
loadSprite :: FilePath -> IO Sprite
loadSprite path = do
  (to, Size w h) <- loadTexture path
  return $ Sprite { spriteTex = to
                  , width     = (fromIntegral w) / 640
                  , height    = (fromIntegral h) / 480
                  }

{-|
  Rendering a @'Sprite'@.
-}
renderSprite :: Sprite -> Float -> Float -> IO ()
renderSprite (Sprite tex width height) x y = do
  textureWrapMode Texture2D S $= (Repeated, ClampToBorder)
  textureWrapMode Texture2D T $= (Repeated, ClampToBorder)
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')

  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just tex

  let vs  = generatePoints x y width height
      tvs = generatePoints 0 0 1 1

  renderPrimitive Quads $
    forM_ (zip vs tvs) $ \((x, y), (tx, ty)) -> do
      texCoord $ TexCoord2 (toGl tx) (toGl ty)
      vertex $ Vertex2 (toGl x) (toGl y)

  texture Texture2D $= Disabled
  where generatePoints :: Float -> Float -> Float -> Float -> [(Float, Float)]
        generatePoints x y w h =
          [ (x    , y    )
          , (x + w, y    )
          , (x + w, y + h)
          , (x    , y + h)
          ]
