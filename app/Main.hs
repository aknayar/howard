module Main (main) where

import Lib
import qualified Data.Map as Map

lum = Map.fromList [(0.0, ' '),  (1, '.'),  (2, ','),  (3, '-'),  (4, '~'),  (5, ':'),  (6, ';'),  (7, '='),  (8, '!'),  (9, '*'),  (10, '#'),  (11, '$'),  (12, '@')]


convertBoard :: [[Float]] -> [[Char]]
convertBoard floats = map convertLine floats
  where
    convertLine line = map (\x -> case Map.lookup x lum of
      Nothing -> ' '
      Just x -> x) line


printBoard :: [[Char]] -> IO()
printBoard board = mapM_ (putStrLn) board

main :: IO ()
main = do
  putStrLn "hello world"