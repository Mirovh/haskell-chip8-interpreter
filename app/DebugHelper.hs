module DebugHelper
(exampleDisplayBuffer) where

import qualified Data.Vector as V
import State (DisplayBuffer)

-- Example: A checkerboard pattern for a 64x32 grid.
exampleDisplayBuffer :: DisplayBuffer
exampleDisplayBuffer = V.generate 32 $ \y ->
  V.generate 64 $ \x ->
    even (x + y)