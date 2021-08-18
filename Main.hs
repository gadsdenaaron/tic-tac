module Main where


import Lib
import System.IO
import Control.Monad (when)
import System.Exit

getKey :: IO [Char]
getKey = reverse <$> getKey' ""

getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)




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


reset :: IO ()
reset = putStr "\x1b[0m"

clear = putStr $ "\x1b[2J"

printUnderline s = putStr $ "\x1b[4m" ++ s ++ "\x1b[0m"

underlineString s = "\x1b[4m" ++ s ++ "\x1b[0m"

data Cell = Empty | Naught | Cross deriving (Show)

pCell :: Cell -> String
pCell Empty = " "
pCell Naught = "\x1b[31mO\x1b[0m"
pCell Cross = "\x1b[32mX\x1b[0m"

printCell :: Cell -> IO ()
printCell c = do
  printUnderline "     "
  moveCursor Down_d 1
  moveCursor Right_d 5
  putStr $ "|   |"
  moveCursor Down_d 1
  moveCursor Right_d 5
  putStr $ "| " ++ (pCell c) ++ " |"
  -- moveCursor Down_d 1
  -- moveCursor Right_d 5
  -- putStr $ "|   |"
  moveCursor Down_d 1
  moveCursor Right_d 5
  printUnderline "|   |"
  moveCursor Up_d 3

data Board = Board {
  a1 :: Cell,
  a2 :: Cell,
  a3 :: Cell,
  b1 :: Cell,
  b2 :: Cell,
  b3 :: Cell,
  c1 :: Cell,
  c2 :: Cell,
  c3 :: Cell
  }deriving (Show)



boardString :: Board -> String

boardString (Board a1 a2 a3 b1 b2 b3 c1 c2 c3) = "   |   |   \n " ++ (pCell a1) ++ " | " ++ (pCell a2) ++ " | " ++ (pCell a3) ++ " \n" ++ (underlineString "   |   |   \n") ++ "   |   |   \n " ++ (pCell b1) ++ " | " ++ (pCell b2) ++ " | " ++ (pCell b3) ++ " \n" ++ (underlineString "   |   |   \n") ++ "   |   |   \n " ++ (pCell c1) ++ " | " ++ (pCell c2) ++ " | " ++ (pCell c3) ++ "\n   |   |   \n"

printBoard :: Board -> IO()
printBoard x = do
  putStr $ boardString x
  putStr $ "\n"

up :: IO ()
up = do
  putStr "\x1b[1A"
  hFlush stdout

data Direction = Up_d | Down_d | Left_d | Right_d

moveCursor :: Direction -> Integer -> IO ()
moveCursor Up_d n = putStr $ "\x1b[" ++ (show n) ++ "A"
moveCursor Down_d n = putStr $ "\x1b[" ++ (show n) ++ "B"
moveCursor Left_d n = putStr $ "\x1b[" ++ (show n) ++ "C"
moveCursor Right_d n = putStr $ "\x1b[" ++ (show n) ++ "D"

setup :: IO ()
setup = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  clear

  printBoard (Board Empty Empty Empty Empty Empty Empty Empty Empty Empty)

exit :: IO ()
exit = do
  clear
  putStr "\x1b[1;1H"
  exitSuccess


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
  loop

getCursorPosition :: IO ()
getCursorPosition = do

  hSetEcho stdin True
  putStr $ "\x1b[6n"
  hFlush stdout
  hSetEcho stdin False
  

main :: IO ()
main = do
 
  setup
  loop

  
  
  
  
  
  
  
  
  





