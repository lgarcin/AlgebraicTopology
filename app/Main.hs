module Main where

import Classification (Edge (Edge), congruent)

main :: IO ()
main =
  print
    $( congruent
         [Edge 3 False, Edge 2 True, Edge 1 False]
         [Edge 2 False, Edge 3 True, Edge 1 True]
     )
