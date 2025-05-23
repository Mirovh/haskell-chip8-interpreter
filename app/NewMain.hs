{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import SDL hiding (Timer)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class
import System.CPUTime (getCPUTime, cpuTimePrecision)
import Data.Word
import Data.Array.IO

-- Internal Modules
import NewState
import DebugHelper
import EventHelper
-- import Data.Bits as B

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

    mem <- newArray (0, 2095) 0 :: IO (IOUArray Int Word8)
    regs <- initRegisterBank
    stk <- initStack
    dbuf <- initDPBuffer 64 32

    cpuTimeNow <- liftIO getCPUTime
    initTimers regs cpuTimeNow
    tickTimerUp regs (RTimer RDelayTimer) 600
    -- let timer = createTimerTicks 600 cpuTimeNow

    appLoop renderer MkCPUState{
        memory = mem,
        registers = regs,
        stack = stk,
        dpbuffer = dbuf
    }

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- squareSize is the size (in pixels) of each cell. TODO: make dynamic to window size
squareSize :: Int
squareSize = 10

redrawScreen :: Renderer -> DisplayBuffer -> IO ()
redrawScreen renderer image = do
    clear renderer

    forM_ [0..63] $ \x ->
        forM_ [0..31] $ \y -> do
            val <- readArray image (x,y)
            drawPixel renderer (x,y) val

    present renderer

drawPixel :: Renderer -> (Int,Int) -> Bool -> IO ()
drawPixel renderer (x,y) white = do
    let color = if white
                    then V4 255 255 255 255 -- White for true
                    else V4 0   0   0   255 -- Black for false
    -- create rectangle mesh
    let rect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral $ x * squareSize)
                                            (fromIntegral $ y * squareSize)))
                             (SDL.V2 (fromIntegral squareSize) (fromIntegral squareSize))
    -- set draw color
    SDL.rendererDrawColor renderer SDL.$= color
    -- draw rectangle
    SDL.fillRect renderer $ Just rect

appLoop :: Renderer -> CPUState -> IO ()
appLoop renderer chipState = do
    -- state
    let mem = memory chipState
    let regs = registers chipState
    let stk = stack chipState
    let dbuf = dpbuffer chipState

    -- IO/events
    sdlEvents <- SDL.pollEvents
    let events = parseEvents sdlEvents

    -- render
    dpb <- exampleDisplayBuffer
    redrawScreen renderer dpb

    -- timers
    cpuTimeNow <- liftIO getCPUTime
    updateTimers (registers chipState) cpuTimeNow
    readTimerTicks regs (RTimer RDelayTimer) >>= print    
    --let newTimer = updateTimer timer cpuTimeNow
    --print $ querryTimerSecs newTimer

    --exec

    when (eKeyDown events KeycodeQ) $ putStrLn "q down"
    when (eKeyUp events KeycodeQ) $ putStrLn "q up"
    if eQuit events then putStrLn "quit the application" else appLoop renderer chipState
