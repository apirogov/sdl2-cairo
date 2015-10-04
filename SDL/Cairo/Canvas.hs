{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : SDL.Cairo.Canvas
Copyright   : (c) Anton Pirogov 2015
License     : MIT
Maintainer  : anton.pirogov@gmail.com

This module defines the Canvas monad, which is a convenience wrapper around
the underlying Cairo rendering and can be used with the same textures
created by 'createCairoTexture'. You can also mix both, if the need arises.

The Canvas API imitates most of the drawing functions
of the Processing language. See <http://processing.org/reference> for comparison.
While having the Processing spirit, this module does not aim for a perfect mapping and
deviates where necessary or appropriate. Nevertheless most Processing examples should be
trivial to port to the Canvas API.

-}
module SDL.Cairo.Canvas (
  -- * Entry point
  Canvas, withCanvas, getCanvasSize,
  -- * Transformations
  resetMatrix, pushMatrix, popMatrix, translate, rotate, scale,
  -- * Color and Style
  Color, Byte, gray, red, green, blue, rgb, (!@),
  stroke, fill, noStroke, noFill, strokeWeight, strokeJoin, strokeCap,
  -- * Primitives
  background, point, line, triangle, rect, rect', polygon, shape, ShapeMode(..),
  -- * Arcs and Curves
  arc, arc', circle, circle', ellipse, ellipse', bezier,
  -- * Math
  mapRange, radians, degrees,
  -- * Misc
  randomSeed, random, getTime, Time(..),
  module Graphics.Rendering.Cairo
) where

import Data.Monoid
import Data.Word (Word8)
import Control.Monad.State

import Data.Time.Clock (UTCTime(..),getCurrentTime)
import Data.Time.LocalTime (timeToTimeOfDay,TimeOfDay(..))
import Data.Time.Calendar (toGregorian)

import System.Random (mkStdGen,setStdGen,randomRIO,Random)

import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Linear.Affine (Point(..))

import SDL (Texture,TextureInfo(..),queryTexture)
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render,LineJoin(..),LineCap(..))

import SDL.Cairo

type Byte = Word8
type Color = V4 Byte

data CanvasState = CanvasState{ csSize :: V2 Double, -- ^texture size
                                csFG :: Maybe Color, -- ^stroke color
                                csBG :: Maybe Color, -- ^fill color
                                csActions :: Endo [Render ()] -- ^list of actions to perform
                              }

-- |get size of the canvas (Processing: @width(), height()@)
getCanvasSize :: Canvas (V2 Double)
getCanvasSize = gets csSize

-- |wrapper around the Cairo 'Render' monad, providing a Processing-style API
newtype Canvas a = Canvas { unCanvas :: StateT CanvasState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CanvasState)

-- |draw on a SDL texture using the 'Canvas' monad
withCanvas :: Texture -> Canvas a -> IO ()
withCanvas t c = do
  (TextureInfo _ _ w h) <- queryTexture t
  (_, result) <- runStateT (unCanvas (defaults >> c))
                  CanvasState{ csSize = V2 (fromIntegral w) (fromIntegral h)
                             , csFG = Just $ gray 0
                             , csBG = Just $ gray 255
                             , csActions = mempty
                             }
  let render = appEndo (csActions result) []
  withCairoTexture t $ sequence_ render
  where defaults = do
          strokeWeight 1
          strokeCap C.LineCapRound

----

-- |set current stroke color
stroke :: Color -> Canvas ()
stroke clr = get >>= \cs -> put cs{csFG=Just clr}
-- |set current fill color
fill :: Color -> Canvas ()
fill clr = get >>= \cs -> put cs{csBG=Just clr}
-- |disable stroke (-> shapes without borders!), reenabled by using stroke
noStroke :: Canvas ()
noStroke = get >>= \cs -> put cs{csFG=Nothing}
-- |disable fill (-> shapes are not filled!), reenabled by using fill
noFill :: Canvas ()
noFill = get >>= \cs -> put cs{csBG=Nothing}

-- |create opaque gray color
gray :: Byte -> Color
gray c = V4 c c c 255
-- |create opaque red color
red :: Byte -> Color
red c = V4 c 0 0 255
-- |create opaque green color
green :: Byte -> Color
green c = V4 0 c 0 255
-- |create opaque blue color
blue :: Byte -> Color
blue c = V4 0 0 c 255
-- |create opaque mixed color
rgb :: Byte -> Byte -> Byte -> Color
rgb r g b = V4 r g b 255
-- |set transparency of color (half red would be: @red 255 <\@ 128@)
(!@) :: Color -> Byte -> Color
(V4 r g b _) !@ a = V4 r g b a

-- |set line width for shape borders etc.
strokeWeight :: Double -> Canvas ()
strokeWeight d = addAction $ C.setLineWidth d

-- |set the style of connections between lines of shapes
strokeJoin :: C.LineJoin -> Canvas ()
strokeJoin l = addAction $ C.setLineJoin l
-- |set the style of the line caps
strokeCap :: C.LineCap -> Canvas ()
strokeCap l = addAction $ C.setLineCap l

----

-- |replace current matrix with identity
resetMatrix :: Canvas ()
resetMatrix = addAction $ C.identityMatrix

-- |push current matrix onto the stack
pushMatrix :: Canvas ()
pushMatrix = addAction $ C.save

-- |pop a matrix
popMatrix :: Canvas ()
popMatrix = addAction $ C.restore

-- |translate coordinate system
translate :: V2 Double -> Canvas ()
translate (V2 x y) = addAction $ C.translate x y

-- |scale coordinate system
scale :: V2 Double -> Canvas ()
scale (V2 x y) = addAction $ C.scale x y

-- |rotate coordinate system
rotate :: Double -> Canvas ()
rotate a = addAction $ C.rotate a

----

-- |clear the canvas with given color
background :: Color -> Canvas ()
background c = do
  (V2 w h) <- gets csSize
  addAction $ setColor c >> C.rectangle 0 0 w h >> C.fill

-- |draw a point with stroke color (cairo emulates this with 1x1 rects!)
point :: V2 Double -> Canvas ()
point (V2 x y) = ifColor csFG $ \c -> do
      C.rectangle x y 1 1
      setColor c
      C.fill

-- |draw a line between two points with stroke color
line :: V2 Double -> V2 Double -> Canvas ()
line (V2 x1 y1) (V2 x2 y2) = ifColor csFG $ \c -> do
      C.moveTo x1 y1
      C.lineTo x2 y2
      setColor c
      C.stroke

-- |draw a triangle connecting three points
triangle :: V2 Double -> V2 Double -> V2 Double -> Canvas ()
triangle (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) = drawShape $ do
    C.moveTo x1 y1
    C.lineTo x2 y2
    C.lineTo x3 y3
    C.lineTo x1 y1

-- |draw a rectangle: @rect leftCorner dimensions@
rect :: V2 Double -> V2 Double -> Canvas ()
rect (V2 x y) (V2 w h) = drawShape $ C.rectangle x y w h

-- |draw a rectangle: @rect' centerPoint dimensions@
rect' (V2 x y) sz@(V2 w h) = rect (V2 (x-w/2) (y-h/2)) sz

-- |draw a polygon connecting given points (equivalent to @'shape' ('ShapeRegular' True)@)
polygon :: [V2 Double] -> Canvas ()
polygon = shape (ShapeRegular True)

-- | Shape mode to use
data ShapeMode = ShapeRegular Bool  -- ^regular path. flag decides whether the first and last point are connected
               | ShapePoints -- ^just draw the points, no lines
               | ShapeLines -- ^interpret points as pairs, draw lines
               | ShapeTriangles -- ^interpret points as triples, draw triangles
               | ShapeTriangleStrip -- ^draw triangle for every neighborhood of 3 points
               | ShapeTriangleFan -- ^fix first point, draw triangles with every neighboring pair and first point
               deriving (Eq,Show)

-- |draw shape along a given path using given @'ShapeMode'@.
-- (Processing: @beginShape(),vertex(),endShape()@)
shape :: ShapeMode -> [V2 Double] -> Canvas ()
shape (ShapeRegular closed) ((V2 x y):ps) = drawShape $ do
  C.moveTo x y
  forM_ ps $ \(V2 x' y') -> C.lineTo x' y'
  when closed $ C.closePath
shape ShapePoints ps = forM_ ps point
shape ShapeLines (p1:p2:ps) = do
  line p1 p2
  shape ShapeLines ps
shape ShapeLines _ = return ()
shape ShapeTriangles (p1:p2:p3:ps) = do
  triangle p1 p2 p3
  shape ShapeTriangles ps
shape ShapeTriangles _ = return ()
shape ShapeTriangleStrip (p1:p2:p3:ps) = do
  triangle p1 p2 p3
  shape ShapeTriangleStrip (p2:p3:ps)
shape ShapeTriangleStrip _ = return ()
shape ShapeTriangleFan (p1:p2:p3:ps) = do
  triangle p1 p2 p3
  shape ShapeTriangleFan (p1:p3:ps)
shape ShapeTriangleFan _ = return ()

----

-- |draw arc: @arc leftCorner dimensions startAngle endAngle@
arc :: V2 Double -> V2 Double -> Double -> Double -> Canvas ()
arc (V2 x y) sz@(V2 w h) = arc' (V2 (x+w/2) (y+h/2)) sz

-- |draw arc: @arc' centerPoint dimensions startAngle endAngle@
arc' :: V2 Double -> V2 Double -> Double -> Double -> Canvas ()
arc' (V2 x y) (V2 w h) sa ea = drawShape $ do
  C.save
  C.translate x y
  C.scale (w/2) (h/2)
  C.arc 0 0 1 sa ea
  C.restore

-- |draw ellipse: @ellipse leftCorner dimensions@
ellipse :: V2 Double -> V2 Double -> Canvas ()
ellipse p sz = arc p sz 0 (2*pi)

-- |draw ellipse: @ellipse' centerPoint dimensions@
ellipse' :: V2 Double -> V2 Double -> Canvas ()
ellipse' p sz = arc' p sz 0 (2*pi)

-- |draw circle: @circle leftCorner diameter@
circle :: V2 Double -> Double -> Canvas ()
circle p d = ellipse p (V2 d d)

-- |draw circle: @circle centerPoint diameter@
circle' :: V2 Double -> Double -> Canvas ()
circle' p d = ellipse' p (V2 d d)

-- |draw bezier spline: @bezier fstAnchor fstControl sndControl sndAnchor@
bezier :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Canvas ()
bezier (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x4 y4) = drawShape $ do
  C.moveTo x1 y1
  C.curveTo x2 y2 x3 y3 x4 y4

----

-- |map a value from one range onto another
mapRange :: Double -> (Double,Double) -> (Double,Double) -> Double
mapRange v (l1,r1) (l2,r2) = (v-l1)*fac + l2
  where fac = (r2-l2)/(r1-l1)

-- |convert degrees to radians
radians :: Double -> Double
radians d = d*pi/180
-- |convert radians to degrees
degrees :: Double -> Double
degrees r = r/pi*180
-- |force value v into given range
constrain :: Double -> (Double,Double) -> Double
constrain v (l,h) = max l $ min h v

-- |set new random seed
randomSeed :: Int -> Canvas ()
randomSeed s = liftIO $ setStdGen $ mkStdGen s

-- |get new random number
random :: (Random a) => (a,a) -> Canvas a
random = liftIO . randomRIO

-- |date and time as returned by getTime
data Time = Time { year :: Int, month :: Int, day :: Int
                 , hour :: Int, minute :: Int, second :: Int }

-- |get current system time. Use the 'Time' accessors for specific components.
-- (Processing: @year(),month(),day(),hour(),minute(),second()@)
getTime :: IO Time
getTime = do
  (UTCTime day time) <- getCurrentTime
  let (y,m,d) = toGregorian day
      (TimeOfDay h mins s) = timeToTimeOfDay time
  return $ Time (fromIntegral y::Int) m d h mins (round s :: Int)

-- TODO: fonts/text, image loading

-- helpers --

-- |take a render action and append to list
addAction :: Render () -> Canvas ()
addAction m = get >>= \cs -> put cs{csActions = csActions cs <> Endo ([m]++)}

-- |draw a shape - first fill with bg color, then draw border with stroke color
drawShape :: Render a -> Canvas ()
drawShape m = do
 ifColor csBG $ \c -> m >> setColor c >> C.fill
 ifColor csFG $ \c -> m >> setColor c >> C.stroke

-- |if color (csFG/csBG) is set, perform given render block
ifColor :: (CanvasState -> Maybe Color) -> (Color -> Render ()) -> Canvas ()
ifColor cf m = get >>= \cs -> forM_ (cf cs) $ \c -> addAction (m c)

-- |convert from 256-value RGBA to Double representation, set color
setColor :: Color -> Render ()
setColor c@(V4 r g b a) = C.setSourceRGBA (conv r) (conv g) (conv b) (conv a)
  where conv = ((1.0/256)*).fromIntegral

