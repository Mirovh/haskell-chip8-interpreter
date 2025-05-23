module DebugHelper
(exampleDisplayBuffer) where

import Data.Array.IO (IOUArray, newListArray)
import NewState (DisplayBuffer)
import Data.Ix (range)

-- Example: A checkerboard pattern for a 64x32 grid.
exampleDisplayBuffer :: IO DisplayBuffer
exampleDisplayBuffer = newListArray bounds [even (x + y) | (x,y) <- range bounds]
    where bounds@((0,0),(wMax,hMax)) = ((0,0),(63,31))
