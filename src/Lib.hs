module Lib
    ( 

        getKey,
        Cell (Empty, Naught, Cross),
        Board (..),
        pCell,
        setup,
        printBoard,
        Action (..),
        CursorCell (..),
        gameLoop,
        setupGame,
        printCell

    ) where

import Ansi
import System.IO
import System.IO.Unsafe
import Control.Monad (when)


getKey :: IO [Char]
getKey = reverse <$> getKey' ""

getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)

setup :: IO ()
setup = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hSetBuffering stdout NoBuffering
  clear

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


data Action = Move_up | Move_down | Move_left | Move_right | X | O

printBoard :: Board -> IO ()
printBoard (Board a1 a2 a3 b1 b2 b3 c1 c2 c3) = do
    printCell a1
    printCell a2
    printCell a3
    putStr "\n"
    printCell a1
    printCell a2
    printCell a3
    putStr "\n"
    printCell a1
    printCell a2
    printCell a3

gameAction :: Board -> CursorCell -> Action -> (Board, CursorCell)
gameAction b cp a = (b, cp)

data CursorCell = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3

-- setCursorCell :: CursorCell -> IO ()
-- setCursorCell B2 = do




-- printState :: (Board, CursorCell) -> IO ()

-- printState (b, c) = do





setupGame :: IO (Board, CursorCell)
setupGame = do
    
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    clear
    putStr $ "\x1b[1;1H"

    let initialBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty
    let initialCursor = B2

    putStr "Second Game:\n\n"
    printBoard initialBoard

    return (initialBoard, initialCursor)


gameLoop :: (Board, CursorCell) -> IO (Board, CursorCell)
gameLoop (b, c) = do

    key <- getKey
   --when (key /= "\ESC") $ --do
    case key of

        "\ESC[A" -> gameLoop $ gameAction b c Move_up
        "\ESC[B" -> gameLoop $ gameAction b c Move_down
        "\ESC[C" -> gameLoop $ gameAction b c Move_left
        "\ESC[D" -> gameLoop $ gameAction b c Move_right


        "X"      -> gameLoop $ gameAction b c X
        "O"      -> gameLoop $ gameAction b c O

        --"\n"     -> **
        "q"      -> return (b, c) --exit
        _        -> gameLoop (b, c)

--exitGame :: (Board, )