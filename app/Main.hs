module Main where

import Lib



main :: IO ()
main = do

  (board, currentcell) <- setupGame
  
  gameLoop (board, currentcell)
  
  exitGame