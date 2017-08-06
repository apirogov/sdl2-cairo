{-|
Module      : SDL.Cairo
Copyright   : Copyright (c) 2015 Anton Pirogov
License     : MIT
Maintainer  : anton.pirogov@gmail.com

This module exposes the functions to glue SDL2 'Texture's to the Cairo 'Render' monad.
-}
module SDL.Cairo (
  createCairoTexture, createCairoTexture', withCairoTexture,
  withCairoTexture'
) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)

import Linear.V2 (V2(..))
import SDL hiding (Surface)
import Graphics.Rendering.Cairo

-- |create new texture for Cairo with given size
createCairoTexture :: Renderer -> V2 CInt -> IO Texture
createCairoTexture r = createTexture r ARGB8888 TextureAccessStreaming

-- |create new texture for Cairo with the size of the given window
createCairoTexture' :: Renderer -> Window -> IO Texture
createCairoTexture' r w = do
  surf <- getWindowSurface w
  sz <- surfaceDimensions surf
  createCairoTexture r sz

-- |draw on SDL texture with Render monad from Cairo
withCairoTexture :: Texture -> Render () -> IO ()
withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)

----

-- | lock and unwrap SDL texture, get a Cairo surface, pass it to some function
withCairoTexture' :: Texture -> (Surface -> IO a) -> IO a
withCairoTexture' t m = do
  (TextureInfo f _ w h) <- queryTexture t
  case mapFormat f of
    Nothing -> error "ERROR: Invalid pixel format for cairo use!"
    Just f' -> do
      (pixels, pitch) <- lockTexture t Nothing
      ret <- withImageSurfaceForData (castPtr pixels) f'
               (fromIntegral w) (fromIntegral h) (fromIntegral pitch) m
      unlockTexture t
      return ret
  where mapFormat ARGB8888 = Just FormatARGB32
        mapFormat RGB888 = Just FormatRGB24
        mapFormat _ = Nothing
