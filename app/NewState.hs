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

import Data.Word (Word8, Word16) 
import Data.Array.IO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
pushStack s item = modifyIORef s (item:) --prepend item

popStack :: Stack -> IO Word16
popStack s = do
    l <- readIORef s
    modifyIORef s tail
    if not (null l)
        then return $ head l
        else return 0

initRegisterBank :: IO RegisterBank
initRegisterBank = newIORef Map.empty

getRegister :: RegisterBank -> RegisterId -> IO Register
getRegister regs regid = do
    regMap <- readIORef regs
    return $ Map.findWithDefault (Register16 0) regid regMap

regValue :: Register -> Integer
regValue (Register8 w) = fromIntegral w
regValue (Register16 w) = fromIntegral w
regValue (RegisterTimer { time = t }) = fromIntegral t

readRegisterValue :: RegisterBank -> RegisterId -> IO Integer
readRegisterValue regs regid = do
    reg <- getRegister regs regid
    return (regValue reg)

writeRegister :: RegisterBank -> RegisterId -> Register -> IO ()
writeRegister regs regid val = modifyIORef regs (Map.insert regid val)

updateTimer :: Integer -> Register -> Register
updateTimer cputime reg@RegisterTimer{} = RegisterTimer {
    lastUpdate = cputime,
    time = max 0 $ time reg - (cputime - lastUpdate reg)
}
updateTimer _ other = other

updateTimers :: RegisterBank -> Integer -> IO ()
updateTimers regs cputime = do
    regMap <- readIORef regs
    let regMapUpdated = Map.map (updateTimer cputime) regMap
    writeIORef regs regMapUpdated

initDPBuffer :: Int -> Int -> IO DisplayBuffer
initDPBuffer width height = newArray ((0,0),(width-1,height-1)) False

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
tickTimerUp regs regid@(RTimer _) ticks = do
    timer <- getRegister regs regid
    writeRegister regs regid RegisterTimer {
        lastUpdate = lastUpdate timer,
        time = ticks * 1000000000000 `div` 60
    }
tickTimerUp _ _ _ = error "RegisterId does not reference a timer"

readTimerTicks :: RegisterBank -> RegisterId -> IO Integer
readTimerTicks regs regid@(RTimer _) =
    (`div` 1000000000000) . (* 60) <$> readRegisterValue regs regid
readTimerTicks _ _ = error "RegisterId does not reference a timer"

createTimerTicks :: Integer -> Integer -> Register
createTimerTicks ticks cpuTime = RegisterTimer {
    lastUpdate = cpuTime,
    time = ticks * 1000000000000 `div` 60
}

