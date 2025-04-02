{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import SDL hiding (Timer)
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.IO.Class
import System.CPUTime (getCPUTime, cpuTimePrecision)

-- Internal Modules
import State
import DebugHelper
import EventHelper
import Data.Bits as B

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

    -- appLoop renderer timer mem

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

appLoop :: Renderer -> CHIPState -> IO()
appLoop renderer chipState = do
    sdlEvents <- SDL.pollEvents
    let events = parseEvents sdlEvents

    redrawScreen renderer exampleDisplayBuffer

    --cpuTimeNow <- liftIO getCPUTime
    --let newTimer = updateTimer timer cpuTimeNow
    --print $ querryTimerSecs newTimer

    -- let newMem = mem



    --exec






    when (eKeyDown events KeycodeQ) $ print "q down"
    when (eKeyUp events KeycodeQ) $ print "q up"
    if (eQuit events) then (print "quit the application") else (appLoop renderer chipState)

fetch :: Memory -> ProgramCounter -> Instruction
fetch mem pc = (fromIntegral (mem V.! addr) `B.shiftL` 8) B..|. (fromIntegral (mem V.! (addr + 1)))
    where addr = fromIntegral pc :: Int

-- An instruction can be split into nibbles
-- Nibbles have a length multiple of 4, so 4 nibbles make up 1 instruction
-- C: first nibble (instruction type code)
-- X: second nibble (target register 1)
-- Y: third nibble (target register 2)
-- N: fourth nibble (4 bit number)
-- NN: third+fourth nibble (8 bit number)
-- NNN: second+third+fourth nibble (12 bit immediate mem address)

nibC :: (Integral a) => Instruction -> a
nibC i = fromIntegral $ i .&. 0xF000

nibX :: (Integral a) => Instruction -> a
nibX i = fromIntegral $ i .&. 0x0F00

nibY :: (Integral a) => Instruction -> a
nibY i = fromIntegral $ i .&. 0x00F0

nibN :: (Integral a) => Instruction -> a
nibN i = fromIntegral $ i .&. 0x000F

nibNN :: (Integral a) => Instruction -> a
nibNN i = fromIntegral $ i .&. 0x00FF

nibNNN :: (Integral a) => Instruction -> a
nibNNN i = fromIntegral $ i .&. 0x0FFF

--decExec :: Instruction -> CHIPState -> CHIPState
--decExec instr = iClearScreen instr

emptyDisplayBuffer :: DisplayBuffer
emptyDisplayBuffer = V.generate 32 $ \y ->
  V.generate 64 $ const False

iClearScreen :: CHIPState -> CHIPState
iClearScreen state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = stack state,
    ir = ir state,
    pc = pc state,
    registers = registers state,
    dpbuffer = emptyDisplayBuffer
}

iJump :: Instruction -> CHIPState -> CHIPState
iJump instr state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = stack state,
    ir = ir state,
    pc = nibNNN instr,
    registers = registers state,
    dpbuffer = dpbuffer state
}

iEnterSubroutine :: Instruction -> CHIPState -> CHIPState
iEnterSubroutine instr state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = pushStack (pc state) (stack state),
    ir = ir state,
    pc = nibNNN instr,
    registers = registers state,
    dpbuffer = dpbuffer state
}

iBreakSubroutine :: Instruction -> CHIPState -> CHIPState
iBreakSubroutine _ state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = snd $ popStack (stack state),
    ir = ir state,
    pc = fst $ popStack (stack state),
    registers = registers state,
    dpbuffer = dpbuffer state
}

iSet :: Instruction -> CHIPState -> CHIPState
iSet instr state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = stack state,
    ir = ir state,
    pc = pc state,
    registers = updateRegister (registers state) (nibX instr) (nibNN instr),
    dpbuffer = dpbuffer state
}


-- DXYN + IR
-- IR points to sprite
-- sprite is N pixels tall and 8 pixels wide
-- every byte is one row of pixels
-- VX and VY contain the coordinates
-- display FLIPS value, not replace it
-- if any pixels were turned off because of a flip, set VF to 1, otherwise to 0
iDisplay :: Instruction -> CHIPState -> CHIPState
iDisplay instr state = State.CHIPState {
    timers  = timers state,
    mem = mem state,
    stack = stack state,
    ir = ir state,
    pc = pc state,
    registers = updateRegister (registers state) 0xF (fromIntegral vf),
    dpbuffer = newDPBuffer
}
    where (newDPBuffer, vf) = flipDpBufferMaskCheck (dpbuffer state) $ 
            readSpriteFromMem (mem state) (fromIntegral ir state) (nibN instr) 
                (fromIntegral readRegisterValue (registers state) (nibX instr))
                (fromIntegral readRegisterValue (registers state) (nibY instr))
-- what needs to change: VF, dpbuffer