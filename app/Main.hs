module Main (main) where

import Lib
import Data.Map

main :: IO ()
main =
    let " .,-~:;=!*#$@"

convertBoard :: [[Float]] -> [[Char]]
convertBoard floats = map convertLine float
  where
    convertLine line = map (Map.lookup  line


printBoard :: [[Char]] -> IO()
printBoard board = mapM_ (putStrLn) board
