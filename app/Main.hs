module Main (main) where

import Lib
import qualified Data.Map as Map

lum = Map.fromList [(0.0, ' '),  (1, '.'),  (2, ','),  (3, '-'),  (4, '~'),  (5, ':'),  (6, ';'),  (7, '='),  (8, '!'),  (9, '*'),  (10, '#'),  (11, '$'),  (12, '@')]


convertLine :: [Float] -> [Char]
convertLine line = map (\x -> case Map.lookup x lum of
  Nothing -> ' '
  Just x -> x) line


printBoard :: [[Float]] -> IO()
printBoard board = mapM_ (putStrLn . convertLine) board

main :: IO ()
main = do
  putStrLn "hello world"
  printBoard [[0 / 10, 10 / 10, 100 / 10]]