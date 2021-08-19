module Ansi
    ( 
      Colour (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White),
      printColour,
      clear,
      reset,
      exit,
      printUnderline,
      underlineString,
      moveCursor,
      Direction (Up_d, Down_d, Left_d, Right_d)
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

data Direction = Up_d | Down_d | Left_d | Right_d

moveCursor :: Direction -> Integer -> IO ()
moveCursor Up_d n = putStr $ "\x1b[" ++ (show n) ++ "A"
moveCursor Down_d n = putStr $ "\x1b[" ++ (show n) ++ "B"
moveCursor Left_d n = putStr $ "\x1b[" ++ (show n) ++ "C"
moveCursor Right_d n = putStr $ "\x1b[" ++ (show n) ++ "D"

exit :: IO ()
exit = do
  clear
  putStr "\x1b[1;1H"
  exitSuccess