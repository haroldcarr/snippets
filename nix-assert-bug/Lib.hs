module Lib where

import           Control.Exception.Assert

f :: Bool -> Int
f x = assert x 3
