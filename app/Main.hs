{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)

import SDL hiding (Timer)
import Linear (V4(..))
import Control.Monad (unless)
import qualified Data.IntMap.Strict as IM
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Word
import Control.Monad (when, forM_)
import Control.Monad.ST (runST)
import Control.Monad.IO.Class

import State
import DebugHelper
import System.CPUTime (getCPUTime, cpuTimePrecision)


-- main :: IO ()
-- main = do
--   initializeAll
--   window <- createWindow "My SDL Application" defaultWindow
--   renderer <- createRenderer window (-1) defaultRenderer
--   appLoop renderer
--   destroyWindow window

-- appLoop :: Renderer -> IO ()
-- appLoop renderer = do
--   events <- pollEvents
--   let eventIsQPress event =
--         case eventPayload event of
--           KeyboardEvent keyboardEvent ->
--             keyboardEventKeyMotion keyboardEvent == Pressed &&
--             keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
--           _ -> False
--       qPressed = any eventIsQPress events
--   rendererDrawColor renderer $= V4 0 0 255 255
--   clear renderer
--   present renderer
--   unless qPressed (appLoop renderer)

-- how the ram works
-- ram is represented as a [word8]

main :: IO ()
main = do
    -- let size = 4096
    --     initialVec = V.replicate size 0  -- Initialize with all zeros
    --     modifications = [(0, 255), (1024, 128), (2048, 64), (4095, 32)]  -- Some example changes

    -- let modifiedVec = applyMemoryWrites initialVec modifications
    -- print $ V.toList $ V.slice 0 10 modifiedVec  -- Print the first 10 elements
    -- print $ V.toList $ V.slice 1020 10 modifiedVec  -- Print around index 1024
    -- print $ V.toList $ V.slice 2045 10 modifiedVec  -- Print around index 2048
    -- print $ V.toList $ V.slice 4095 1 modifiedVec  -- Print around index 4095
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



data MyEvents = MyEvents    { eQuit :: Bool
                            , eKeyDown :: Keycode -> Bool
                            , eKeyUp :: Keycode -> Bool
                            }

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

parseEvents :: [Event] -> MyEvents
parseEvents events = MyEvents {
    eQuit = any eventIsQuit events,
    eKeyDown = keycodeStatusLookupFromEvents events Pressed,
    eKeyUp = keycodeStatusLookupFromEvents events Released
}

keycodeStatusLookupFromEvents :: [Event] -> InputMotion -> Keycode -> Bool
keycodeStatusLookupFromEvents events motion keycode =
    any (\event -> case eventPayload event of
                    KeyboardEvent keyboardEvent ->
                        keyboardEventKeyMotion keyboardEvent == motion &&
                        keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
                    _ -> False
        ) events

eventIsQuit :: Event -> Bool
eventIsQuit event = case eventPayload event of
    QuitEvent -> True
    _ -> False

appLoop :: Renderer -> Timer -> Memory -> IO()
appLoop renderer timer mem = do
    sdlEvents <- SDL.pollEvents
    let events = parseEvents sdlEvents

    -- clear renderer
    -- SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
    -- SDL.fillRect renderer $ Just $ SDL.Rectangle (P $ V2 100 100) $ V2 100 50
    -- present renderer

    redrawScreen renderer exampleDisplayBuffer

    cpuTimeNow <- liftIO getCPUTime

    let newTimer = updateTimer timer cpuTimeNow

    let newMem = mem

    print $ querryTimerSecs newTimer

    when (eKeyDown events KeycodeQ) $ (print "yes")
    if (eQuit events) then (print "quit the application") else (appLoop renderer newTimer newMem)

