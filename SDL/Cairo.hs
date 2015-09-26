module SDL.Cairo (
  createCairoTexture, createCairoTexture', withCairoTexture
) where

import Foreign.Ptr (castPtr)
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL
import Graphics.Rendering.Cairo

-- |create new texture for Cairo with given size
createCairoTexture :: Renderer -> V2 CInt -> IO Texture
createCairoTexture r = createTexture r ARGB8888 TextureAccessStreaming

-- |create new texture for Cairo with the size of the given window
createCairoTexture' :: Renderer -> Window -> IO Texture
createCairoTexture' r w = do
  surf <- getWindowSurface w
  sz@(V2 w h) <- surfaceDimensions surf
  createCairoTexture r sz

-- |draw on SDL texture with Render monad from Cairo
withCairoTexture :: Texture -> Render () -> IO ()
withCairoTexture t m = do
  (TextureInfo f _ w h) <- queryTexture t
  case mapFormat f of
    Nothing -> error "ERROR: Invalid pixel format for cairo use!"
    Just f' -> do
      (pixels, pitch) <- lockTexture t Nothing
      withImageSurfaceForData (castPtr pixels) f'
            (fromIntegral w) (fromIntegral h) (fromIntegral pitch) $ \s -> renderWith s m
      unlockTexture t
  where mapFormat ARGB8888 = Just FormatARGB32
        mapFormat RGB888 = Just FormatRGB24
        mapFormat _ = Nothing
