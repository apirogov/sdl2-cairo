{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SDL.Cairo.Canvas (
  Canvas(..), withCanvas, getCanvasSize, mapRange, radians, degrees, randomSeed, random,
  resetMatrix, pushMatrix, popMatrix, translate, rotate, scale,
  stroke, fill, gray, noStroke, noFill, strokeWeight, strokeJoin, strokeCap,
  background, point, line, triangle, rect, rect', polygon, shape, ShapeMode(..),
  arc, arc', circle, circle', ellipse, ellipse'
) where

import Data.Monoid
import Data.Word (Word8)
import Control.Monad.State

import System.Random (mkStdGen,setStdGen,randomRIO,Random)

import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Linear.Affine (Point(..))
import SDL hiding (get)

import SDL.Cairo
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render)

type Color = V4 Word8

data CanvasState = CanvasState{ csSize :: V2 Double,    -- ^texture size
                                csFG :: Maybe Color, -- ^stroke color
                                csBG :: Maybe Color, -- ^fill color
                                csActions :: Endo [Render ()] -- ^list of actions to perform
                              }

-- |Get size of the canvas
getCanvasSize :: Canvas (V2 Double)
getCanvasSize = gets csSize

-- |Set new random seed
randomSeed :: Int -> Canvas ()
randomSeed s = liftIO $ setStdGen $ mkStdGen s

-- |Get new random number
random :: (Random a) => a -> a -> Canvas a
random l h = liftIO $ randomRIO (l,h)

-- |map a value from one range onto another
mapRange :: Double -> (Double,Double) -> (Double,Double) -> Double
mapRange v (l1,r1) (l2,r2) = (v-l1)*fac + l2
  where fac = (r2-l2)/(r1-l1)

radians d = d*pi/180
degrees r = r/pi*180

-- |Wrapper around the Cairo Render Monad, providing a Processing-style API
newtype Canvas a = Canvas { unCanvas :: StateT CanvasState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CanvasState)

-- |Draw on a SDL texture using the Canvas monad (Processing-style API)
withCanvas :: Texture -> Canvas a -> IO ()
withCanvas t c = do
  (TextureInfo _ _ w h) <- queryTexture t
  (_, result) <- runStateT (unCanvas c)
                  CanvasState{ csSize = V2 (fromIntegral w) (fromIntegral h)
                             , csFG = Just $ gray 0
                             , csBG = Just $ gray 255
                             , csActions = mempty
                             }
  let render = appEndo (csActions result) []
  withCairoTexture t $ sequence_ ((C.setLineWidth 1.0):render)

----

-- |set current stroke color
stroke :: Color -> Canvas ()
stroke clr = get >>= \cs -> put cs{csFG=Just clr}
-- |set current fill color
fill :: Color -> Canvas ()
fill clr = get >>= \cs -> put cs{csBG=Just clr}
-- |disable stroke (-> shapes without contours!), reenabled by using stroke
noStroke :: Canvas ()
noStroke = get >>= \cs -> put cs{csFG=Nothing}
-- |disable fill (-> shapes are not filled!), reenabled by using fill
noFill :: Canvas ()
noFill = get >>= \cs -> put cs{csBG=Nothing}
-- |create gray tone
gray :: Word8 -> Color
gray c = V4 c c c 255

strokeWeight :: Double -> Canvas ()
strokeWeight d = addAction $ C.setLineWidth d

strokeJoin :: C.LineJoin -> Canvas ()
strokeJoin l = addAction $ C.setLineJoin l

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

-- |draw a line with stroke color
line :: V2 Double -> V2 Double -> Canvas ()
line (V2 x1 y1) (V2 x2 y2) = ifColor csFG $ \c -> do
      C.moveTo x1 y1
      C.lineTo x2 y2
      setColor c
      C.stroke

-- |draw a triangle
triangle :: V2 Double -> V2 Double -> V2 Double -> Canvas ()
triangle (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) = drawShape $ do
    C.moveTo x1 y1
    C.lineTo x2 y2
    C.lineTo x3 y3
    C.lineTo x1 y1

-- |draw a rectangle
rect :: V2 Double -> V2 Double -> Canvas ()
rect (V2 x y) (V2 w h) = drawShape $ C.rectangle x y w h

-- |draw a rectangle, centered
rect' (V2 x y) sz@(V2 w h) = rect (V2 (x-w/2) (y-h/2)) sz

-- |draw a polygon
polygon :: [V2 Double] -> Canvas ()
polygon = shape (ShapeRegular True)

-- | Shape mode to use
data ShapeMode = ShapeRegular Bool | ShapePoints | ShapeLines
               | ShapeTriangles | ShapeTriangleStrip | ShapeTriangleFan deriving (Eq,Show)

-- |draw shapes along a given path in different modes
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

-- |draw arc
arc :: V2 Double -> V2 Double -> Double -> Double -> Canvas ()
arc (V2 x y) sz@(V2 w h) = arc' (V2 (x+w/2) (y+h/2)) sz

-- |draw arc, centered
arc' :: V2 Double -> V2 Double -> Double -> Double -> Canvas ()
arc' (V2 x y) (V2 w h) sa ea = drawShape $ do
  C.save
  C.translate x y
  C.scale (w/2) (h/2)
  C.arc 0 0 1 sa ea
  C.restore

-- |draw ellipse
ellipse :: V2 Double -> V2 Double -> Canvas ()
ellipse p sz = arc p sz 0 (2*pi)

-- |draw ellipse, centered
ellipse' :: V2 Double -> V2 Double -> Canvas ()
ellipse' p sz = arc' p sz 0 (2*pi)

-- |draw circle
circle :: V2 Double -> Double -> Canvas ()
circle p r = ellipse p (V2 r r)

-- |draw circle, centered
circle' :: V2 Double -> Double -> Canvas ()
circle' p r = ellipse' p (V2 r r)

-- TODO: curves, fonts/text, image loading

-- helpers --

-- |take a render action and append to list
addAction :: Render () -> Canvas ()
addAction m = get >>= \cs -> put cs{csActions = csActions cs <> Endo ([m]++)}

-- |draw a shape - first fill with bg color, then border with stroke color
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

