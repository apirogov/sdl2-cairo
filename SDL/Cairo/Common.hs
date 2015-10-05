module SDL.Cairo.Common where

import Foreign.Ptr (castPtr)
import SDL hiding (Surface)
import Graphics.Rendering.Cairo

-- lock and unwrap SDL texture, get a Cairo surface, pass it to some function
withCairoTexture' :: Texture -> (Surface -> IO ()) -> IO ()
withCairoTexture' t m = do
  (TextureInfo f _ w h) <- queryTexture t
  case mapFormat f of
    Nothing -> error "ERROR: Invalid pixel format for cairo use!"
    Just f' -> do
      (pixels, pitch) <- lockTexture t Nothing
      withImageSurfaceForData (castPtr pixels) f'
            (fromIntegral w) (fromIntegral h) (fromIntegral pitch) m
      unlockTexture t
  where mapFormat ARGB8888 = Just FormatARGB32
        mapFormat RGB888 = Just FormatRGB24
        mapFormat _ = Nothing
