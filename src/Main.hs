module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Control.Monad
import Data.IORef

import RenderTexture
import LoadTexture

loop :: Sprite -> IORef Bool -> IO ()
loop sprite shouldCloseRef = do
  shouldClose <- readIORef shouldCloseRef

  if shouldClose
    then return ()
    else do
      pollEvents
      close <- getKey ESC

      clear [ColorBuffer]
      renderSprite sprite 0 0

      swapBuffers
      loop sprite shouldCloseRef

main :: IO ()
main = do
  init <- initialize
  unless init $ error "Could not initialize."
  open <- openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "gl-texturing"

  shouldCloseRef <- newIORef False
  windowCloseCallback $= do
    writeIORef shouldCloseRef True
    return True

  sprite <- loadSprite "data/test.png"

  loop sprite shouldCloseRef

  closeWindow
