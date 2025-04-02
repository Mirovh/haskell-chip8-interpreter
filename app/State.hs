module State
  ( Memory
  , MemoryWrite
  , DisplayBuffer
  , Stack
  , Register8
  , Register16
  , Instruction
  , IndexRegister
  , ProgramCounter
  , Timer(..)
  , Timers(..)
  , GPRegisters
  , CHIPState(..)
  , applyMemoryWrites
  , readRegisterValue
  , updateRegister
  , updateTimers
  , updateTimer
  , ceilDiv
  , querryTimerSecs
  , tickTimerUp
  , createTimerTicks
  , popStack
  , pushStack
  , flipDpBufferMask
  , flipDpBufferMaskCheck
  , readSpriteFromMem
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad (forM_)
import qualified Data.Word as B
import SDL (Display)
import Data.Bits (Bits(xor, testBit))

type Memory = V.Vector B.Word8
type MemoryWrite = (Int, B.Word8)
type DisplayBuffer = V.Vector (V.Vector Bool) -- vector of (column) vectors of booleans (white = true, black = false)
type DisplayBufferMask = DisplayBuffer
type Stack = [B.Word16]
type Register8 = B.Word8
type Register16 = B.Word16
type Instruction = B.Word16
type IndexRegister = Register16 -- index and pc are 16 bit to fit spec, but only actually address 12 bits
type ProgramCounter = Register16
data Timer = Timer  { lastUpdate :: Integer
                    , time :: Integer
                    } -- time is kept in picoseconds (s*e-12)
data Timers = Timers    { delayTimer :: Timer
                        , soundTimer :: Timer
                        }
type GPRegisters = V.Vector Register8 -- 16 general purpose registers
data CHIPState = CHIPState  { timers :: Timers
                            , mem :: Memory
                            , stack :: Stack
                            , ir :: IndexRegister
                            , pc :: ProgramCounter
                            , registers :: GPRegisters
                            , dpbuffer :: DisplayBuffer
                            }

-- this is not the best way to manage memory state because it requires a full copy (O(n))
-- a better more performant way would be an STUArray
-- but I write this program to learn haskell and by extension functional programming
-- not to learn how to write imperative code in haskell, so I try to keep things pure even if it costs performance
applyMemoryWrites :: Memory -> [MemoryWrite] -> Memory
applyMemoryWrites mem writes = runST $ do
    mutmem <- V.thaw mem
    forM_ writes $ \(idx, val) -> MV.write mutmem idx val
    V.freeze mutmem

readRegisterValue :: GPRegisters -> Int -> Register8
readRegisterValue regs idx = regs V.! idx

updateRegister :: GPRegisters -> Int -> Register8 -> GPRegisters
updateRegister regs idx val = V.update regs $ V.zip (V.singleton idx) (V.singleton val)

-- System.CPUTime getCPUTime is in picoseconds (s*e-12)
-- This function also takes the new cputime in picoseconds
updateTimers :: Timers -> Integer -> Timers
updateTimers timers cputime = Timers {
    delayTimer = updateTimer (delayTimer timers) cputime,
    soundTimer = updateTimer (soundTimer timers) cputime
}

updateTimer :: Timer -> Integer -> Timer
updateTimer timer cputime = Timer {
    lastUpdate = cputime,
    time = max 0 $ time timer - (cputime - (lastUpdate timer))
}

ceilDiv :: Integral a => a -> a -> a
ceilDiv a b = (a + b - 1) `div` b

querryTimerSecs :: Timer -> Int
querryTimerSecs timer = fromIntegral $ ceilDiv (time timer) (1000000000000) -- weird bug 1e12 is considered fractional

-- 1 tick = 1/60th of a second
tickTimerUp :: Timer -> Integer -> Timer
tickTimerUp timer ticks = Timer {
    lastUpdate = lastUpdate timer, -- lastupdate is used to keep relative time, so doesnt need to change
    time = time timer + (ticks * 1000000000000 `div` 60)
}

-- requires cpu time to set initial lastUpdate value
createTimerTicks :: Integer -> Integer -> Timer
createTimerTicks ticks cpuTime = Timer {
    lastUpdate = cpuTime,
    time = ticks * 1000000000000 `div` 60
}

pushStack :: B.Word16 -> Stack -> Stack
pushStack item (x:xs) = item:x:xs
pushStack item [] = [item]

popStack :: Stack -> (B.Word16, Stack)
popStack (x:xs) = (x,xs)
popStack [] = (0,[])

flipDpBufferMaskCheck :: DisplayBuffer -> DisplayBufferMask -> (DisplayBuffer, Bool)
flipDpBufferMaskCheck buffer mask = (newDPBuffer, bitFlippedOff buffer newDPBuffer)
  where newDPBuffer = flipDpBufferMask buffer mask

bitFlippedOff :: DisplayBuffer -> DisplayBuffer -> Bool
bitFlippedOff dpbuffer1 dpbuffer2 = 
  V.any (\(row1, row2) ->
    V.any (\(pixel1, pixel2) -> pixel1 && not pixel2) (V.zip row1 row2)
  ) (V.zip dpbuffer1 dpbuffer2)

-- mask is a dpbuffer where pixels that need to be flipped are true
flipDpBufferMask :: DisplayBuffer -> DisplayBufferMask -> DisplayBuffer
flipDpBufferMask buffer mask = V.generate 32 $ \y ->
  V.generate 64 $ \x ->
    ((buffer V.! y) V.! x) `xor` ((mask V.! y) V.! x)

--
readSpriteFromMem :: Memory -> Int -> Int -> Int -> Int -> DisplayBufferMask
readSpriteFromMem memory addr height xSprite ySprite = V.generate 32 $ \y ->
  V.generate 64 $ \x ->
    inBounds x y xSprite ySprite height && ((spriteVec V.! (y - ySprite)) `testBit` (x-xSprite))
  where spriteVec = V.slice addr height memory -- vectorxheight(word8) -- vectorx32(vectorx64(bool))

-- boundwidth = 8
inBounds :: Int -> Int -> Int -> Int -> Int -> Bool
inBounds x y xBoundStart yBoundStart boundHeight =
    (x >= xBoundStart) && (x <= (xBoundStart + 8)) && (y >= yBoundStart) && (y <= (yBoundStart + boundHeight))