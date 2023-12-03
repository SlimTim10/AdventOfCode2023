module Main where

import qualified Day1
import qualified Day2

main :: IO ()
main = do
  putStrLn "Day 1"
  Day1.part1
  Day1.part2

  putStrLn "Day 2"
  Day2.part1
