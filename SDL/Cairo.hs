module SDL.Cairo (
  createCairoTexture, createCairoTexture', withCairoTexture
) where

import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import SDL hiding (Surface)

import SDL.Cairo.Common (withCairoTexture')
import Graphics.Rendering.Cairo(Render(..),renderWith)

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
withCairoTexture t m = withCairoTexture' t (\s -> renderWith s m)

