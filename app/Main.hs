{-# LANGUAGE TupleSections #-}


module Main where

import           Analysis
import           MealyMoore

main :: IO ()
main = do
  let h = histogram $ replicate 10 (counter 0) :: Moore' (Int, a) [Int]

      h' = foldl (curry chomp) h $ (,()) <$> [1..5]

  print $ poop h'
