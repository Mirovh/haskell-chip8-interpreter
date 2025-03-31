{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)

import SDL
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

type Memory = V.Vector Word8
type MemoryWrite = (Int, Word8)

-- this is not the best way to manage memory state because it requires a full copy (O(n))
-- but I write this program to learn haskell and by extension functional programming
-- not to learn how to write imperative code in haskell, so I try to keep things pure
applyMemoryWrites :: Memory -> [MemoryWrite] -> Memory
applyMemoryWrites mem writes = runST $ do
    mutmem <- V.thaw mem
    forM_ writes $ \(idx, val) -> MV.write mutmem idx val
    V.freeze mutmem

main :: IO ()
main = do
    -- let size = 4096
    --     initialVec = V.replicate size 0  -- Initialize with all zeros
    --     modifications = [(0, 255), (1024, 128), (2048, 64), (4095, 32)]  -- Some example changes

    -- let modifiedVec = applyMemoryWrites initialVec modifications
    -- print $ V.toList $ V.slice 0 10 modifiedVec  -- Print the first 10 elements
    -- print $ V.toList $ V.slice 1020 10 modifiedVec  -- Print around index 1024
    -- print $ V.toList $ V.slice 2045 10 modifiedVec  -- Print around index 2048
    -- print $ V.toList $ V.slice 4086 10 modifiedVec  -- Print around index 4095
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
    let mem = V.replicate 4096 0
    appLoop renderer mem
    
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit



data MyEvents = MyEvents    { eQuit :: Bool
                            , eKeyDown :: Keycode -> Bool
                            , eKeyUp :: Keycode -> Bool
                            }

parseEvents :: [Event] -> MyEvents
parseEvents events = MyEvents {
    eQuit = any eventIsQuit events,
    eKeyDown = keycodeStatusLookupFromEvents events Released,
    eKeyUp = keycodeStatusLookupFromEvents events Pressed
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

appLoop :: Renderer -> Memory -> IO()
appLoop renderer mem = do
    sdlEvents <- SDL.pollEvents
    let events = parseEvents sdlEvents

    clear renderer
    SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
    SDL.fillRect renderer $ Just $ SDL.Rectangle (P $ V2 100 100) $ V2 100 50
    present renderer

    let newMem = mem
    when (eKeyDown events KeycodeQ) $ (print "yes")
    if (eQuit events) then (print "quit the application") else (appLoop renderer newMem) 
