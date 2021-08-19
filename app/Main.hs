module Main where

import Lib
import Control.Monad(when)
import System.IO
import Ansi




up :: IO ()
up = do
  putStr "\x1b[1A"
  hFlush stdout

loop :: IO ()

loop = do
  
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> moveCursor Up_d 1
      "\ESC[B" -> moveCursor Down_d 2
      "\ESC[C" -> moveCursor Left_d 3
      "\ESC[D" -> moveCursor Right_d 1
      "\ESC[10;10R" -> putStr "found you"
      "\n"     -> putStr "⎆"
      "\DEL"   -> putStr "⎋"
      "q"      -> exit
      "X"      -> printCell Cross
      "O"      -> printCell Naught
      "e"      -> printCell Empty
      "b"      -> printBoard (Board Cross Cross Empty Naught Empty Cross Cross Naught Empty)
      "c"      -> getCursorPosition
      ";"      -> putStr "\x1b[10;10H"
      _       -> return ()
  
  hFlush stdout
  --loop

getCursorPosition :: IO ()
getCursorPosition = do

  hSetEcho stdin True
  putStr $ "\x1b[6n"
  hFlush stdout
  hSetEcho stdin False




main :: IO ()
main = do
 
  --setup
  --loop

  (board, currentcell) <- setupGame
  gameLoop (board, currentcell)
  
  putStr "game Over"
  exit