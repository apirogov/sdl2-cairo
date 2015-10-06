# sdl2-cairo

Haskell library providing functions to use Cairo to draw on SDL textures and containing a Processing-style convenience drawing API.

##### Install

This library depends on the new [SDL2 bindings](https://github.com/haskell-game/sdl2), available on
Hackage as [sdl2 version 2.0.0 or greater](http://hackage.haskell.org/package/sdl2)
and [cairo bindings](https://hackage.haskell.org/package/cairo).

Just clone and install this repository:
```bash
git clone git@github.com:apirogov/sdl2-cairo.git
cd sdl2-cairo
cabal install
```

It has been tested with GHC 7.10.1 on a Linux system without any problems.

##### Documentation

Generate the haddock documentation for reference.

You can use Cairo directly with the Render monad on an SDL texture like this:

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
you can also use the simpler Canvas-API:

```haskell
import SDL.Cairo
import SDL.Cairo.Canvas
...
  texture <- createCairoTexture renderer (V2 800 600)
  withCanvas texture $ do
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

See also the source of Main.hs for more examples. You start that demo with:
```bash
cabal run
```
