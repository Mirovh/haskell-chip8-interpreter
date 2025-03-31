module State
  ( Memory
  , MemoryWrite
  , DisplayBuffer
  , Stack
  , Register8
  , Register16
  , IndexRegister
  , ProgramCounter
  , Timer(..)
  , Timers(..)
  , GPRegisters
  , applyMemoryWrites
  , readRegisterValue
  , updateRegister
  , updateTimers
  , updateTimer
  , ceilDiv
  , querryTimerSecs
  , tickTimerUp
  , createTimerTicks
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Word
import Control.Monad (forM_)

type Memory = V.Vector Word8
type MemoryWrite = (Int, Word8)
type DisplayBuffer = V.Vector (V.Vector Bool) -- vector of (column) vectors of booleans (white = true, black = false)
type Stack = [Word16]
type Register8 = Word8
type Register16 = Word16
type IndexRegister = Register16 -- index and pc are 16 bit to fit spec, but only actually address 12 bits
type ProgramCounter = Register16
data Timer = Timer  { lastUpdate :: Integer
                    , time :: Integer
                    } -- time is kept in picoseconds (s*e-12)
data Timers = Timers    { delayTimer :: Timer
                        , soundTimer :: Timer
                        }
type GPRegisters = V.Vector Register8 -- 16 general purpose registers

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