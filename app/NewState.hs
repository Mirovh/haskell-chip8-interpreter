module NewState
    ( CPUState(..)
    , DisplayBuffer
    , RegisterId(..)
    , Timer(..)
    , initStack
    , pushStack
    , popStack
    , initRegisterBank
    , getRegister
    , readRegisterValue
    , writeRegister
    , initTimers
    , createTimerTicks
    , updateTimers
    , tickTimerUp
    , readTimerTicks
    , initDPBuffer
    , flipDpBufferMask
    ) where

import System.IO
import Data.Word (Word8, Word16) 
import Data.Array.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.IORef

data RegisterId = RNumbered Int
    | RPC
    | RIR
    | RTimer Timer
    deriving (Show, Eq, Ord)

data Timer = RDelayTimer | RSoundTimer deriving (Show, Eq, Ord)

data Register = Register8 Word8
    | Register16 Word16
    | RegisterTimer { lastUpdate :: Integer
                    , time :: Integer
                    } -- time is kept in picoseconds
    deriving (Show, Eq, Ord)

type RegisterBank = IORef (Map RegisterId Register)

type DisplayBuffer = IOUArray (Int,Int) Bool --(white = true, black = false)
type DisplayBufferMask = [(Int,Int)]

type Stack = IORef [Word16]

data CPUState = MkCPUState
    { memory :: IOUArray Int Word8
    , registers :: RegisterBank
    , stack :: Stack
    , dpbuffer :: DisplayBuffer
    }

initStack :: IO Stack
initStack = newIORef []

pushStack :: Stack -> Word16 -> IO ()
pushStack stack item = modifyIORef stack (item:) --prepend item

popStack :: Stack -> IO Word16
popStack stack = do
    s <- readIORef stack
    modifyIORef stack tail
    if (length s > 0)
        then return $ head s
        else return 0

initRegisterBank :: IO RegisterBank
initRegisterBank = newIORef Map.empty

getRegister :: RegisterBank -> RegisterId -> IO Register
getRegister regs id = do
    regMap <- readIORef regs
    return $ Map.findWithDefault (Register16 0) id regMap

regValue :: Register -> Integer
regValue (Register8 w) = fromIntegral w
regValue (Register16 w) = fromIntegral w
regValue (RegisterTimer { time = t }) = fromIntegral t

readRegisterValue :: RegisterBank -> RegisterId -> IO Integer
readRegisterValue regs id = do
    reg <- getRegister regs id
    return (regValue reg)

writeRegister :: RegisterBank -> RegisterId -> Register -> IO ()
writeRegister regs id val = modifyIORef regs (Map.insert id val)

updateTimer :: Register -> Integer -> Register
updateTimer reg@RegisterTimer{} cputime = RegisterTimer {
    lastUpdate = cputime,
    time = max 0 $ time reg - (cputime - (lastUpdate reg))
}
updateTimer other _ = other

updateTimers :: RegisterBank -> Integer -> IO ()
updateTimers regs cputime = do
    regMap <- readIORef regs
    let regMapUpdated = Map.map (\reg -> updateTimer reg cputime) regMap
    writeIORef regs regMapUpdated

initDPBuffer :: Int -> Int -> IO DisplayBuffer
initDPBuffer width height = newArray ((0,0),((width-1),(height-1))) False

-- mask contains all pixels that need to be flipped
-- returns true if any pixel was flipped from on to off
flipDpBufferMask :: DisplayBuffer -> DisplayBufferMask -> IO Bool
flipDpBufferMask b m = do
    res <- mapM (flipPixel b) m
    return (or res)

-- returns true if pixel was flipped from on to off
flipPixel :: DisplayBuffer -> (Int,Int) -> IO Bool
flipPixel b c = do
    p <- readArray b c
    writeArray b c (not p)
    return p

initTimers :: RegisterBank -> Integer -> IO ()
initTimers regs cputime = do
    let timer = RegisterTimer {
        lastUpdate = cputime,
        time = 0
    }
    writeRegister regs (RTimer RDelayTimer) timer
    writeRegister regs (RTimer RSoundTimer) timer

tickTimerUp :: RegisterBank -> RegisterId -> Integer -> IO ()
tickTimerUp regs id@(RTimer _) ticks = do
    timer <- getRegister regs id
    writeRegister regs id RegisterTimer {
        lastUpdate = lastUpdate timer,
        time = ticks * 1000000000000 `div` 60
    }

readTimerTicks :: RegisterBank -> RegisterId -> IO Integer
readTimerTicks regs id@(RTimer _) =
    (`div` 1000000000000) . (* 60) <$> readRegisterValue regs id

createTimerTicks :: Integer -> Integer -> Register
createTimerTicks ticks cpuTime = RegisterTimer {
    lastUpdate = cpuTime,
    time = ticks * 1000000000000 `div` 60
}

