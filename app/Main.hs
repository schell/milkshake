module Main where

import           Milkshake.Compile
import           System.Environment

main :: IO ()
main = getArgs >>= run
