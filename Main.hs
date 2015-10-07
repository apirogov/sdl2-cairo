{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad (unless)

import SDL
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Linear.Affine (Point(..))

import SDL.Cairo
import SDL.Cairo.Canvas

main :: IO ()
main = do
  initializeAll
  window <- createWindow "SDL2 Cairo Canvas" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  -- create a texture suitable to use cairo on
  texture <- createCairoTexture' renderer window

  appLoop renderer texture 0 (V2 0 0)

appLoop :: Renderer -> Texture -> Int -> V2 Double -> IO ()
appLoop renderer texture framecount mousepos = do
  events <- pollEvents
  let quitEvent event = case eventPayload event of
                          QuitEvent -> True
                          _ -> False
      mouseMoveEvent e = case eventPayload e of
                          MouseMotionEvent _ -> True
                          _ -> False
      getMousePos (MouseMotionEvent (MouseMotionEventData _ _ _ (P p) _)) = fromIntegral <$> p
      getMousePos _ = V2 0 0
      mousePositions = map getMousePos $ map eventPayload $ filter mouseMoveEvent events
      quit = any quitEvent events
      mousepos' = if null mousePositions then mousepos else head mousePositions

  -- draw on the texture
  withCanvas texture $ do
    drawExample
    drawRndCircle
    drawStars (fromIntegral framecount)
    drawTriangleStrip mousepos'
    drawBezierTest

  -- apply texture and show on screen
  copy renderer texture Nothing Nothing
  present renderer

  delay $ 1000 `div` 60     -- slow down to approx. 60 FPS

  unless quit (appLoop renderer texture (framecount+1) mousepos')

-- draw a circle in a random color
drawRndCircle :: Canvas ()
drawRndCircle = do
    rnd <- random (0,255)
    fill $ gray rnd
    circle (V2 700 500) 100
    fill $ gray 255

-- some shapes, show some features
drawExample :: Canvas ()
drawExample = do
    background $ gray 102

    stroke $ blue 255
    point $ V2 500 10

    line (V2 0 0) (V2 100 100)
    line (V2 100 0) (V2 0 100)
    fill $ red 255 !@ 128
    noStroke
    rect $ D 200 200 100 100
    stroke $ green 255 !@ 128
    fill $ blue 255 !@ 128
    rect $ D 250 250 100 100
    triangle (V2 400 300) (V2 350 400) (V2 400 400)

    strokeWeight 5
    strokeJoin LineJoinRound
    stroke $ (blue 255 + red 255) !@ 128
    fill $ rgb 0 255 255 !@ 128
    polygon [V2 500 500,V2 510 505,V2 520 530, V2 500 530]

    circle (V2 200 500) 30

    strokeWeight 1
    ellipse $ D 300 500 30 50

    pushMatrix
    translate $ V2 600 500
    rotate $ pi/4
    ellipse $ D 0 0 100 50
    popMatrix
    ellipse $ D 0 0 100 50

-- ported star example from Processing home page
drawStars :: Double -> Canvas ()
drawStars frameCount = do
  sz@(V2 w h) <- getCanvasSize
  stroke $ gray 0

  pushMatrix
  translate $ V2 (0.2*w) (0.3*h)
  rotate $ frameCount / 200
  star 0 0 5 70 3
  popMatrix

  pushMatrix
  translate $ V2 (0.5*w) (0.3*h)
  rotate $ frameCount / 400
  star 0 0 80 100 40
  popMatrix

  pushMatrix
  translate $ V2 (0.8*w) (0.3*h)
  rotate $ frameCount / (-100)
  star 0 0 30 70 5
  popMatrix

star :: Double -> Double -> Double -> Double -> Double -> Canvas ()
star x y r1 r2 n = do
  let angle = 2*pi/n
      halfAngle = angle/2
      p1 a = V2 (x+cos a*r2) (y+sin a*r2)
      p2 a = V2 (x+cos (a+halfAngle)*r1) (y+sin (a+halfAngle)*r1)
      vertices = concatMap snd $ takeWhile (\(a, _) -> a<2*pi+angle)
               $ iterate (\(a, _) -> (a+angle,[p1 a, p2 a])) (0,[])
  polygon vertices

-- ported triangle strip example from Processing homepage
drawTriangleStrip :: V2 Double -> Canvas ()
drawTriangleStrip mousePos = do
  sz@(V2 w h) <- getCanvasSize
  let (V2 mouseX mouseY) = mousePos
      pos = V2 (w/2) (h/4*3)
      n = fromIntegral $ round $ mapRange mouseX (0,w) (6,60)
      astep = 180/n
      insideRadius = 100
      outsideRadius = 150
      cossin a = V2 (cos a) (sin a)
      makeVert a = [ pos+cossin (radians a) * outsideRadius
                   , pos+cossin (radians $ a+astep)*insideRadius
                   ]
      vertices = concatMap makeVert $ map (2*astep*) [0..n]
  shape ShapeTriangleStrip vertices
  return ()

-- draw bezier curve examples
drawBezierTest :: Canvas ()
drawBezierTest = do
  pushMatrix
  translate $ V2 600 0
  stroke $ rgb 255 102 0
  line (V2 85 20) (V2 10 10)
  line (V2 90 90) (V2 15 80)
  stroke $ gray 0
  bezier (V2 85 20) (V2 10 10) (V2 90 90) (V2 15 80)
  translate $ V2 100 0
  stroke $ rgb 255 102 0
  noFill
  line (V2 30 20) (V2 80 5)
  line (V2 80 75) (V2 30 75)
  stroke $ gray 0
  bezier (V2 30 20) (V2 80 5) (V2 80 75) (V2 30 75)
  popMatrix
