# sdl2-cairo [![Hackage version](https://img.shields.io/hackage/v/sdl2-cairo.svg?style=flat)](https://hackage.haskell.org/package/sdl2-cairo) [![Build Status](https://travis-ci.org/apirogov/sdl2-cairo.svg)](https://travis-ci.org/apirogov/sdl2-cairo)

Haskell library providing functions to use Cairo to draw on SDL textures.

**NOTE:**
The Processing-style drawing API has been moved to a separate package
[cairo-canvas](https://github.com/apirogov/cairo-canvas).

##### Install

This library depends on the new
[SDL2 bindings](https://github.com/haskell-game/sdl2), available on Hackage as
[sdl2 version 2.1.0 or greater](http://hackage.haskell.org/package/sdl2)
and [cairo bindings](https://hackage.haskell.org/package/cairo).

Just clone and install this repository:
```bash
git clone git@github.com:apirogov/sdl2-cairo.git
cd sdl2-cairo
stack install
```

It should work with recent GHC versions (>= 7.8.4) without problems under Linux und OS X.

##### Documentation

You can use Cairo with the Render monad on an SDL texture like this:

```haskell
import SDL.Cairo
import Graphics.Rendering.Cairo
...
  texture <- createCairoTexture renderer (V2 800 600)
  withCairoTexture texture $ do
    setSourceRGBA 1 0 0
    lineTo 800 600
    stroke

  copy renderer texture Nothing Nothing
  present renderer
```

If you are familiar with [Processing](https://processing.org/reference),
you can also use this together with the
[cairo-canvas](https://github.com/apirogov/cairo-canvas) package.

```haskell
import SDL.Cairo
import Graphics.Rendering.Cairo.Canvas
...
  texture <- createCairoTexture renderer (V2 800 600)
  withCairoTexture' texture $ runCanvas $ do
    background $ gray 100
    stroke $ red 255
    fill $ blue 255 !@ 128
    rect $ D 0 0 100 100
    rect $ toD (V2 50 50) (V2 150 150)

  copy renderer texture Nothing Nothing
  present renderer
```

Finally, you can of course use this as glue to use [diagrams](http://projects.haskell.org/diagrams/)
with SDL with the [Cairo backend](https://hackage.haskell.org/package/diagrams-cairo):
```haskell
import SDL.Cairo
import Diagrams.Backend.Cairo
...
  let (_,render) = renderDia Cairo (CairoOptions "" (mkSizeSpec $ V2 (Just 800) (Just 600))
                                                 RenderOnly False)
                                   (myDiagram :: QDiagram Cairo V2 Double Any)
  withCairoTexture texture render
...
```
