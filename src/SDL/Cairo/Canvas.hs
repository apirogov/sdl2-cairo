{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : SDL.Cairo.Canvas
Copyright   : Copyright (c) 2015 Anton Pirogov
License     : MIT
Maintainer  : anton.pirogov@gmail.com

This module defines the 'Canvas' monad, which is a convenience wrapper around
the underlying Cairo rendering and can be used with the same textures
created by 'createCairoTexture'. You can also mix both, if the need arises.

The Canvas API imitates most of the drawing functions
of the Processing language. See <http://processing.org/reference> for comparison.
While having the Processing spirit, this module does not aim for a perfect mapping and
deviates where necessary or appropriate. Nevertheless most Processing examples should be
trivial to port to the Canvas API. Example:

>import SDL
>import Linear.V2 (V2(..))
>import SDL.Cairo
>import SDL.Cairo.Canvas
>
>main :: IO ()
>main = do
>  initialize [InitEverything]
>  window <- createWindow "SDL2 Cairo Canvas" defaultWindow
>  renderer <- createRenderer window (-1) defaultRenderer
>  texture <- createCairoTexture' renderer window
>
>  withCanvas texture $ do
>    background $ gray 102
>    fill $ red 255 !@ 128
>    noStroke
>    rect $ D 200 200 100 100
>    stroke $ green 255 !@ 128
>    fill $ blue 255 !@ 128
>    rect $ D 250 250 100 100
>    triangle (V2 400 300) (V2 350 400) (V2 400 400)
>
>  copy renderer texture Nothing Nothing
>  present renderer
>  delay 5000
-}
module SDL.Cairo.Canvas (
  -- * Entry point
  Canvas, withCanvas, getCanvasSize, renderCairo,
  -- * Color and Style
  Color, Byte, gray, red, green, blue, rgb, (!@),
  stroke, fill, noStroke, noFill, strokeWeight, strokeJoin, strokeCap,
  -- * Coordinates
  Dim(..), toD, centered, corners,
  -- * Primitives
  background, point, line, triangle, rect, polygon, shape, ShapeMode(..),
  -- * Arcs and Curves
  circle, circle', arc, ellipse, bezier, bezierQ,
  -- * Transformations
  resetMatrix, pushMatrix, popMatrix, translate, rotate, scale,
  -- * Images
  Image(imageSize), createImage, loadImagePNG, saveImagePNG, image, image', blend, grab,
  -- * Text
  Font(..), textFont, textSize, text, textC, textR,
  -- * Math
  mapRange, radians, degrees,
  -- * Misc
  randomSeed, random, getTime, Time(..),
  module Graphics.Rendering.Cairo
) where

import Data.Monoid
import Control.Monad.State
import Data.Word (Word8)

import Data.Time.Clock (UTCTime(..),getCurrentTime)
import Data.Time.LocalTime (timeToTimeOfDay,TimeOfDay(..))
import Data.Time.Calendar (toGregorian)

import System.Random (mkStdGen,setStdGen,randomRIO,Random)

import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Linear.Affine (Point(..))

import SDL (Texture,TextureInfo(..),queryTexture)
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render,LineJoin(..),LineCap(..),Format(..),Operator(..))

import SDL.Cairo (withCairoTexture')

type Byte = Word8

-- | RGBA Color is just a byte vector. Colors can be added, subtracted, etc.
type Color = V4 Byte

data CanvasState = CanvasState{ csSize :: V2 Double, -- ^texture size
                                csSurface :: C.Surface, -- ^Cairo surface
                                csFG :: Maybe Color, -- ^stroke color
                                csBG :: Maybe Color, -- ^fill color
                                csFont :: Font,      -- ^current font
                                csActions :: Endo [Render ()], -- ^list of actions to perform
                                csImages :: [Image] -- ^keeping track of images to free later
                              }

-- |get size of the canvas (Processing: @width(), height()@)
getCanvasSize :: Canvas (V2 Double)
getCanvasSize = gets csSize

-- |wrapper around the Cairo 'Render' monad, providing a Processing-style API
newtype Canvas a = Canvas { unCanvas :: StateT CanvasState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CanvasState)

-- |draw on a SDL texture using the 'Canvas' monad
withCanvas :: Texture -> Canvas a -> IO a
withCanvas t c = withCairoTexture' t $ \s -> do
  (TextureInfo _ _ w h) <- queryTexture t
  (ret, result) <- runStateT (unCanvas $ defaults >> c)
                  CanvasState{ csSize = V2 (fromIntegral w) (fromIntegral h)
                            , csSurface = s
                            , csFG = Just $ gray 0
                            , csBG = Just $ gray 255
                            , csFont = Font "" 10 False False
                            , csActions = mempty
                            , csImages = []
                            }
  let render = appEndo (csActions result) []
  C.renderWith s $ sequence_ render
  forM_ (csImages result) $ \(Image s _ _) -> C.surfaceFinish s
  return ret
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
-- |disable stroke (-> shapes without borders!), reenabled by using 'stroke'
noStroke :: Canvas ()
noStroke = get >>= \cs -> put cs{csFG=Nothing}
-- |disable fill (-> shapes are not filled!), reenabled by using 'fill'
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
-- |set transparency of color (half red would be: @red 255 !\@ 128@)
(!@) :: Color -> Byte -> Color
(V4 r g b _) !@ a = V4 r g b a

-- |set line width for shape borders etc.
strokeWeight :: Double -> Canvas ()
strokeWeight d = renderCairo $ C.setLineWidth d

-- |set the style of connections between lines of shapes
strokeJoin :: C.LineJoin -> Canvas ()
strokeJoin l = renderCairo $ C.setLineJoin l
-- |set the style of the line caps
strokeCap :: C.LineCap -> Canvas ()
strokeCap l = renderCairo $ C.setLineCap l

----

-- | position and size representation (X Y W H)
data Dim = D Double Double Double Double deriving (Show,Eq)

-- | create dimensions from position and size vector
toD (V2 a b) (V2 c d) = D a b c d

-- | takes dimensions with centered position, returns normalized (left corner)
centered (D cx cy w h) = D (cx-w/2) (cy-h/2) w h
-- | takes dimensions with bottom-right corner instead of size, returns normalized (with size)
corners (D xl yl xh yh) = D xl yl (xh-xl) (yh-yl)

----

-- |replace current matrix with identity
resetMatrix :: Canvas ()
resetMatrix = renderCairo $ C.identityMatrix

-- |push current matrix onto the stack
pushMatrix :: Canvas ()
pushMatrix = renderCairo $ C.save

-- |pop a matrix
popMatrix :: Canvas ()
popMatrix = renderCairo $ C.restore

-- |translate coordinate system
translate :: V2 Double -> Canvas ()
translate (V2 x y) = renderCairo $ C.translate x y

-- |scale coordinate system
scale :: V2 Double -> Canvas ()
scale (V2 x y) = renderCairo $ C.scale x y

-- |rotate coordinate system
rotate :: Double -> Canvas ()
rotate a = renderCairo $ C.rotate a

----

-- |clear the canvas with given color
background :: Color -> Canvas ()
background c = do
  (V2 w h) <- gets csSize
  renderCairo $ setColor c >> C.rectangle 0 0 w h >> C.fill

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

-- |draw a rectangle
rect :: Dim -> Canvas ()
rect (D x y w h) = drawShape $ C.rectangle x y w h

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
               deriving (Show,Eq)

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

-- |draw arc: @arc dimensions startAngle endAngle@
arc :: Dim -> Double -> Double -> Canvas ()
arc (D x y w h) sa ea = drawShape $ do
  C.save
  C.translate x y
  C.scale (w/2) (h/2)
  C.arc 0 0 1 sa ea
  C.restore

-- |draw ellipse
ellipse :: Dim -> Canvas ()
ellipse dim = arc dim 0 (2*pi)

-- |draw circle: @circle leftCorner diameter@
circle :: V2 Double -> Double -> Canvas ()
circle (V2 x y) d = ellipse (D x y d d)

-- |draw circle: @circle centerPoint diameter@
circle' :: V2 Double -> Double -> Canvas ()
circle' (V2 x y) d = ellipse $ centered (D x y d d)

-- |draw cubic bezier spline: @bezier fstAnchor fstControl sndControl sndAnchor@
bezier :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Canvas ()
bezier (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x4 y4) = drawShape $ do
  C.moveTo x1 y1
  C.curveTo x2 y2 x3 y3 x4 y4

-- |draw quadratic bezier spline: @bezier fstAnchor control sndAnchor@
bezierQ :: V2 Double -> V2 Double -> V2 Double -> Canvas ()
bezierQ p0 p12 p3 = bezier p0 p1 p2 p3
  where p1 = p0 + 2/3*(p12-p0)
        p2 = p3 + 2/3*(p12-p3)

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
                 , hour :: Int, minute :: Int, second :: Int } deriving (Show,Eq)

-- |get current system time. Use the 'Time' accessors for specific components.
-- (Processing: @year(),month(),day(),hour(),minute(),second()@)
getTime :: IO Time
getTime = do
  (UTCTime day time) <- getCurrentTime
  let (y,m,d) = toGregorian day
      (TimeOfDay h mins s) = timeToTimeOfDay time
  return $ Time (fromIntegral y::Int) m d h mins (round s :: Int)

----

data Image = Image {imageSurface::C.Surface, imageSize::(V2 Int), imageFormat::Format}

-- | create a new empty image of given size
createImage :: V2 Int -> Canvas Image
createImage (V2 w h) = do
  s <- liftIO $ C.createImageSurface FormatARGB32 w h
  let img = Image s (V2 w h) FormatARGB32
  track img
  return img

--TODO: add checks (file exists, correct format, etc.)
-- | load a PNG image from given path.
loadImagePNG :: FilePath -> Canvas Image
loadImagePNG path = do
  s <- liftIO $ C.imageSurfaceCreateFromPNG path
  w <- C.imageSurfaceGetWidth s
  h <- C.imageSurfaceGetHeight s
  f <- C.imageSurfaceGetFormat s
  let img = Image s (V2 w h) f
  track img
  return img

-- | Save an image as PNG to given file path
saveImagePNG :: Image -> FilePath -> Canvas ()
saveImagePNG (Image s _ _) fp = renderCairo $ liftIO (C.surfaceWriteToPNG s fp)

-- | Render complete image on given coordinates
image :: Image -> V2 Double -> Canvas ()
image img@(Image _ (V2 w h) _) (V2 x y) =
  image' img (D x y (fromIntegral w) (fromIntegral h))

-- | Render complete image inside given dimensions
image' :: Image -> Dim -> Canvas ()
image' img@(Image s (V2 ow oh) _) =
  blend OperatorSource img (D 0 0 (fromIntegral ow) (fromIntegral oh))

-- | Copy given part of image to given part of screen, using given blending
-- operator and resizing when necessary. Use 'OperatorSource' to copy without
-- blending effects. (Processing: @copy(),blend()@)
blend :: Operator -> Image -> Dim -> Dim -> Canvas ()
blend op (Image s (V2 ow oh) _) sdim ddim = do
  surf <- gets csSurface
  renderCairo $ copyFromToSurface op s sdim surf ddim

-- | get a copy of the image from current window (Processing: @get()@)
grab :: Dim -> Canvas Image
grab dim@(D x y w h) = do
  surf <- gets csSurface
  i@(Image s _ _) <- createImage (V2 (round w) (round h))
  renderCairo $ copyFromToSurface OperatorSource surf dim s (D 0 0 w h)
  return i

----

-- | Font definition
data Font = Font{fontFace::String
                ,fontSize::Double
                ,fontBold::Bool
                ,fontItalic::Bool} deriving (Show,Eq)

-- | set current font for text rendering
textFont :: Font -> Canvas ()
textFont f = do
  get >>= \cs -> put cs{csFont=f}
  renderCairo $ setFont f

-- | get the size of the text when rendered in current font
textSize :: String -> Canvas (V2 Double)
textSize s = gets csSurface >>= \cs -> do
  font <- gets csFont
  (C.TextExtents _ _ w h _ _) <- C.renderWith cs $ setFont font >> C.textExtents s
  return $ V2 w h

-- | render text left-aligned (coordinate is top-left corner)
text :: String -> V2 Double -> Canvas ()
text str (V2 x y) = ifColor csFG $ \c -> do
  (C.TextExtents _ yb _ h _ _) <- C.textExtents str
  setColor c
  C.moveTo x (y-yb)
  C.showText str

-- | render text right-aligned (coordinate is top-right corner)
textR :: String -> V2 Double -> Canvas ()
textR str (V2 x y) = do
  (V2 w h) <- textSize str
  text str $ V2 (x-w) y

-- | render text centered (coordinate is central)
textC :: String -> V2 Double -> Canvas ()
textC str (V2 x y) = do
  (V2 w h) <- textSize str
  text str $ V2 (x-(w/2)) (y-(h/2))


-- helpers --

-- | execute a raw Cairo Render action
renderCairo :: Render () -> Canvas ()
renderCairo m = get >>= \cs -> put cs{csActions = csActions cs <> Endo ([m]++)}

-- |draw a shape - first fill with bg color, then draw border with stroke color
drawShape :: Render a -> Canvas ()
drawShape m = do
 ifColor csBG $ \c -> m >> setColor c >> C.fill
 ifColor csFG $ \c -> m >> setColor c >> C.stroke

-- |if color (csFG/csBG) is set, perform given render block
ifColor :: (CanvasState -> Maybe Color) -> (Color -> Render ()) -> Canvas ()
ifColor cf m = get >>= \cs -> forM_ (cf cs) $ \c -> renderCairo (m c)

-- |convert from 256-value RGBA to Double representation, set color
setColor :: Color -> Render ()
setColor c@(V4 r g b a) = C.setSourceRGBA (conv r) (conv g) (conv b) (conv a)
  where conv = ((1.0/256)*).fromIntegral

-- | Add to garbage collection list
track :: Image -> Canvas ()
track img = get >>= \cs -> put cs{csImages=img:csImages cs}

-- cairo helpers --

-- | helper: returns new surface with scaled content. does NOT cleanup!
createScaledSurface :: C.Surface -> (V2 Double) -> Render C.Surface
createScaledSurface s (V2 w h) = do
  ow <- C.imageSurfaceGetWidth s
  oh <- C.imageSurfaceGetHeight s
  s' <- liftIO $ C.createSimilarSurface s C.ContentColorAlpha (round w) (round h)
  C.renderWith s' $ do
    C.scale (w/fromIntegral ow) (h/fromIntegral oh)
    C.setSourceSurface s 0 0
    pat <- C.getSource
    C.patternSetExtend pat C.ExtendPad
    C.setOperator C.OperatorSource
    C.paint
  return s'

-- | helper: returns new surface with only part of original content. does NOT cleanup!
createTrimmedSurface :: C.Surface -> Dim -> Render C.Surface
createTrimmedSurface s (D x y w h) = do
  ow <- C.imageSurfaceGetWidth s
  oh <- C.imageSurfaceGetHeight s
  s' <- liftIO $ C.createSimilarSurface s C.ContentColorAlpha (round w) (round h)
  C.renderWith s' $ do
    C.setSourceSurface s (-x) (-y)
    C.setOperator C.OperatorSource
    C.rectangle 0 0 w h
    C.fill
  return s'

copyFromToSurface :: Operator -> C.Surface -> Dim -> C.Surface -> Dim -> Render ()
copyFromToSurface op src sdim@(D sx sy sw sh) dest (D x y w h) = do
  ow <- C.imageSurfaceGetWidth src
  oh <- C.imageSurfaceGetHeight src
  let needsTrim = sx/=0 || sy/=0 || round sw/=oh || round sh/=oh
      needsRescale = round sw/=round w || round sh/=round h
  s' <- if needsTrim then createTrimmedSurface src sdim else return src
  s'' <- if needsRescale then createScaledSurface s' (V2 w h) else return s'
  C.renderWith dest $ do
    C.save
    C.setSourceSurface s'' x y
    C.setOperator op
    C.rectangle x y w h
    C.fill
    C.restore
  when needsTrim $ C.surfaceFinish s'
  when needsRescale $ C.surfaceFinish s''

-- | Set the current font
setFont :: Font -> Render ()
setFont (Font face sz bold italic) = do
  C.selectFontFace face
                   (if italic then C.FontSlantItalic else C.FontSlantNormal)
                   (if bold then C.FontWeightBold else C.FontWeightNormal)
  C.setFontSize sz

