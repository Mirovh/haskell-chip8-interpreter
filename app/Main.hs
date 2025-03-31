{-# LANGUAGE OverloadedStrings #-}
module Main where
    
import System.Environment (getArgs)
import SDL hiding (Timer)
import qualified Data.Vector as V
import Data.Word
import Control.Monad (when)
import Control.Monad.IO.Class
import System.CPUTime (getCPUTime, cpuTimePrecision)

-- Internal Modules
import State
import DebugHelper
import EventHelper

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL app" defaultWindow
    renderer <-
        SDL.createRenderer
            window
            (-1)
            SDL.RendererConfig
                { SDL.rendererType = SDL.AcceleratedRenderer
                , SDL.rendererTargetTexture = False
                }

    let mem = V.replicate 4096 0 :: Memory

    cpuTimeNow <- liftIO getCPUTime
    let timer = createTimerTicks 600 cpuTimeNow
    
    appLoop renderer timer mem

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- squareSize is the size (in pixels) of each cell. TODO: make dynamic to window size
squareSize :: Int
squareSize = 10

redrawScreen :: Renderer -> DisplayBuffer -> IO ()
redrawScreen renderer image = do
    clear renderer
    V.imapM_ (\y row ->
        V.imapM_ (\x pixel -> do
            let color = if pixel
                            then V4 255 255 255 255  -- White for true
                            else V4 0   0   0   255  -- Black for false
                -- Create a rectangle at position (x * squareSize, y * squareSize)
                rect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * squareSize)
                                                    (fromIntegral $ y * squareSize)))
                                    (SDL.V2 (fromIntegral squareSize) (fromIntegral squareSize))
            -- Set the draw color
            SDL.rendererDrawColor renderer SDL.$= color
            -- Draw the filled rectangle
            SDL.fillRect renderer $ Just rect
            ) row
        ) image
    present renderer

appLoop :: Renderer -> Timer -> Memory -> IO()
appLoop renderer timer mem = do
    sdlEvents <- SDL.pollEvents
    let events = parseEvents sdlEvents

    redrawScreen renderer exampleDisplayBuffer

    cpuTimeNow <- liftIO getCPUTime
    let newTimer = updateTimer timer cpuTimeNow
    print $ querryTimerSecs newTimer

    let newMem = mem

    when (eKeyDown events KeycodeQ) $ print "yes"
    if (eQuit events) then (print "quit the application") else (appLoop renderer newTimer newMem)

