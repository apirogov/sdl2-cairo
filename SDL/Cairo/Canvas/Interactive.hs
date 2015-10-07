{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : SDL.Cairo
Copyright   : Copyright (c) 2015 Anton Pirogov
License     : MIT
Maintainer  : anton.pirogov@gmail.com

This module provides convenience functions for interactive development with ghci.
-}
module SDL.Cairo.Canvas.Interactive where

import Control.Monad (forever)
import Control.Concurrent (forkIO)

import SDL

import SDL.Cairo (createCairoTexture')
import SDL.Cairo.Canvas (Canvas, withCanvas)

-- |for testing and debugging usage with ghci. Starts up an SDL window,
-- forks a rendering loop and returns a function to draw in this window.
getInteractive :: IO (Canvas () -> IO ())
getInteractive = do
  initializeAll
  w <- createWindow "SDL2 Cairo Canvas Interactive" defaultWindow
  r <- createRenderer w (-1) defaultRenderer
  t <- createCairoTexture' r w
  forkIO $ forever $ do
    lockTexture t Nothing
    copy r t Nothing Nothing
    unlockTexture t
    present r
  return $ draw t
  where draw :: Texture -> Canvas () -> IO ()
        draw = withCanvas

