module Ansi
    ( 
      Colour (..),
      Direction (..),
      printColour,
      clear,
      reset,
      exit,
      printUnderline,
      underlineString,
      moveCursor,
      saveCursorPosition,
      restoreCursorPosition,
      moveCursorAbs,
      showCursor,
      hideCursor
    ) where

import System.IO
import System.Exit

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

printColour :: Colour -> String -> IO ()

printColour Black s = putStr $ "\x1b[30m" ++ s ++ "\x1b[0m"
printColour Red   s = putStr $ "\x1b[31m" ++ s ++ "\x1b[0m"
printColour Green s = putStr $ "\x1b[32m" ++ s ++ "\x1b[0m"
printColour Yellow s = putStr $ "\x1b[33m" ++ s ++ "\x1b[0m"
printColour Blue s = putStr $ "\x1b[34m" ++ s ++ "\x1b[0m"
printColour Magenta s = putStr $ "\x1b[35m" ++ s ++ "\x1b[0m"
printColour Cyan s = putStr $ "\x1b[36m" ++ s ++ "\x1b[0m"
printColour White s = putStr $ "\x1b[37m" ++ s ++ "\x1b[0m"

printUnderline s = putStr $ "\x1b[4m" ++ s ++ "\x1b[0m"

underlineString s = "\x1b[4m" ++ s ++ "\x1b[0m"

reset :: IO ()
--ANSI Select graphic rendition normal mode, turns off all attributes ie colours
reset = putStr "\x1b[0m"

clear :: IO ()
-- ANSI Erase in display, clears display and sets cursor to top left position
clear = putStr $ "\x1b[2J"

saveCursorPosition :: IO ()
saveCursorPosition = putStr $ "\x1b[s"

restoreCursorPosition :: IO ()
restoreCursorPosition = putStr $ "\x1b[u"

data Direction = Up_d | Down_d | Left_d | Right_d

moveCursor :: Direction -> Integer -> IO ()
moveCursor Up_d n = putStr $ "\x1b[" ++ (show n) ++ "A"
moveCursor Down_d n = putStr $ "\x1b[" ++ (show n) ++ "B"
moveCursor Right_d n = putStr $ "\x1b[" ++ (show n) ++ "C"
moveCursor Left_d n = putStr $ "\x1b[" ++ (show n) ++ "D"

moveCursorAbs :: Integer -> Integer -> Integer -> Integer -> IO ()
moveCursorAbs u d r l = do
    putStr $ "\x1b[" ++ (show u) ++ "A"
    putStr $ "\x1b[" ++ (show d) ++ "B"
    putStr $ "\x1b[" ++ (show r) ++ "C"
    putStr $ "\x1b[" ++ (show l) ++ "D"

exit :: IO ()
exit = do
  clear
  putStr "\x1b[1;1H"
  exitSuccess

getCursorPosition :: IO ()
getCursorPosition = do

  hSetEcho stdin True
  putStr $ "\x1b[6n"
  hFlush stdout
  hSetEcho stdin False

hideCursor :: IO ()
hideCursor = putStr "\x1b[?25l"

showCursor :: IO ()
showCursor = putStr "\x1b[?25h"