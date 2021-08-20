module Lib
    ( 
        Action (..),
        CursorCell (..),
        Cell (Empty, Naught, Cross),
        Board (..),
        getKey,
        pCell,
        setup,
        printBoard,
        gameLoop,
        setupGame,
        printCell,
        exitGame
    ) where

import Ansi
import System.IO
import System.IO.Unsafe
import System.Exit


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

data Cell = Empty | Naught | Cross deriving (Show, Eq)

pCell :: Cell -> String
pCell Empty = " "
pCell Naught = "\x1b[31mO\x1b[0m"
pCell Cross = "\x1b[32mX\x1b[0m"

printCell :: Cell -> IO ()
printCell c = do
  printUnderline "     "
  moveCursor Down_d 1
  moveCursor Left_d 5
  putStr $ "|   |"
  moveCursor Down_d 1
  moveCursor Left_d 5
  putStr $ "| " ++ (pCell c) ++ " |"
  moveCursor Down_d 1
  moveCursor Left_d 5
  printUnderline "|   |"
  moveCursor Up_d 3
  hFlush stdout

printSaveCell :: Cell -> IO ()
printSaveCell c = do
  printUnderline "     "
  moveCursor Down_d 1
  moveCursor Left_d 5
  putStr $ "|   |"
  moveCursor Down_d 1
  moveCursor Left_d 5
  putStr $ "| "

  saveCursorPosition

  putStr $ (pCell c) ++ " |"
  moveCursor Down_d 1
  moveCursor Left_d 5
  printUnderline "|   |"
  moveCursor Up_d 3
  hFlush stdout

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
    putStr " "
    printCell a2
    putStr " "
    printCell a3
    
    moveCursor Down_d 4
    moveCursor Left_d 17

    printCell b1
    putStr " "
    printSaveCell b2
    putStr " "
    printCell b3
    
    moveCursor Down_d 4
    moveCursor Left_d 17

    printCell c1
    putStr " "
    printCell c2
    putStr " "
    printCell c3

    restoreCursorPosition

positionIsEmpty :: Board -> CursorCell -> Bool
positionIsEmpty (Board Empty _ _ _ _ _ _ _ _) A1 = True
positionIsEmpty (Board _ Empty _ _ _ _ _ _ _) A2 = True
positionIsEmpty (Board _ _ Empty _ _ _ _ _ _) A3 = True
positionIsEmpty (Board _ _ _ Empty _ _ _ _ _) B1 = True
positionIsEmpty (Board _ _ _ _ Empty _ _ _ _) B2 = True
positionIsEmpty (Board _ _ _ _ _ Empty _ _ _) B3 = True
positionIsEmpty (Board _ _ _ _ _ _ Empty _ _) C1 = True
positionIsEmpty (Board _ _ _ _ _ _ _ Empty _) C2 = True
positionIsEmpty (Board _ _ _ _ _ _ _ _ Empty) C3 = True
positionIsEmpty _ _ = False

placeInCell :: Board -> CursorCell -> Cell -> Board
placeInCell b A1 c = b{a1=c}
placeInCell b A2 c = b{a2=c}
placeInCell b A3 c = b{a3=c}
placeInCell b B1 c = b{b1=c}
placeInCell b B2 c = b{b2=c}
placeInCell b B3 c = b{b3=c}
placeInCell b C1 c = b{c1=c}
placeInCell b C2 c = b{c2=c}
placeInCell b C3 c = b{c3=c}

gameAction :: Board -> CursorCell -> Action -> (Board, CursorCell)

gameAction b cp X
    | positionIsEmpty b cp = ((placeInCell b cp Cross), cp)
    | otherwise = (b, cp)

gameAction b cp O
    | positionIsEmpty b cp = ((placeInCell b cp Naught), cp)
    | otherwise = (b, cp)

gameAction b cp action = (b, moveCell cp action)


moveCell :: CursorCell -> Action -> CursorCell
moveCell A1 Move_up = C1
moveCell A1 Move_left = A3
moveCell A2 Move_up = C2
moveCell A2 Move_left = A1
moveCell A3 Move_up = C3
moveCell A3 Move_left = A2
moveCell B1 Move_up = A1
moveCell B1 Move_left = B3
moveCell B2 Move_up = A2
moveCell B2 Move_left = B1
moveCell B3 Move_up = A3
moveCell B3 Move_left = B2
moveCell C1 Move_up = B1
moveCell C1 Move_left = C3
moveCell C2 Move_up = B2
moveCell C2 Move_left = C1
moveCell C3 Move_up = B3
moveCell C3 Move_left = C2

moveCell cp Move_right = moveCell (moveCell cp Move_left) Move_left
moveCell cp Move_down = moveCell (moveCell cp Move_up) Move_up



data CursorCell = A1 | A2 | A3 | B1 | B2 | B3 | C1 | C2 | C3



printState :: (Board, CursorCell) -> IO ()

printState (b, c) = do
    printBoard b
    case c of

        A1 -> moveCursorAbs 5 0 0 7 
        A2 -> moveCursorAbs 5 0 0 0
        A3 -> moveCursorAbs 5 0 7 0
        B1 -> moveCursorAbs 0 0 0 7
        B2 -> return ()
        B3 -> moveCursorAbs 0 0 7 0
        C1 -> moveCursorAbs 0 5 0 7
        C2 -> moveCursorAbs 0 5 0 0
        C3 -> moveCursorAbs 0 5 7 0


setupGame :: IO (Board, CursorCell)
setupGame = do
    
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering
    clear
    putStr $ "\x1b[1;1H"

    hFlush stdout

    let initialBoard = Board Empty Empty Empty Empty Empty Empty Empty Empty Empty
    let initialCursor = B2

    putStr "Second Game:\n\n"

    printBoard initialBoard

    return (initialBoard, initialCursor)


gameLoop :: (Board, CursorCell) -> IO (Board, CursorCell)
gameLoop (b, c) = do
    
    hideCursor
    clear
    putStr $ "\x1b[1;1H"
    printState (b, c)
    showCursor

    let endstate = checkEndState b

    case endstate of

        (True, Cross)  -> clear >> putStr "\x1b[20;1HCross Wins" >> return (b, c)
        (True, Naught) -> clear >> putStr "\x1b[20;1HNaught Wins" >> return (b, c)
        (True, Empty)  -> clear >> putStr "\x1b[20;1HDraw" >> return (b, c)
        
        -- otherwise go into the gameloop
        (False, _)     -> getKey >>= (\key -> case key of
        
            --key <- getKey
            --case key of

                "\ESC[A" -> gameLoop $ gameAction b c Move_up
                "\ESC[B" -> gameLoop $ gameAction b c Move_down
                "\ESC[C" -> gameLoop $ gameAction b c Move_right
                "\ESC[D" -> gameLoop $ gameAction b c Move_left


                "X"      -> gameLoop $ gameAction b c X
                "O"      -> gameLoop $ gameAction b c O

                --"\n"     -> **
                "q"      -> return (b, c) --exit
                _        -> gameLoop (b, c)
                )


checkEndState :: Board -> (Bool, Cell)
--checks if the board is in a winning configuration or the board is full
checkEndState board@(Board a b c d e f g h i)
    
    --end state but no one won
    | allCellsFull board = (True, Empty)

    --3 in a row horizontal
    | (a == b) && (b == c) && (a /= Empty) = (True, a)
    | (d == e) && (e == f) && (d /= Empty) = (True, d)
    | (g == h) && (h == i) && (g /= Empty) = (True, g)

    --3 in a row vertical
    | (a == d) && (d == g) && (a /= Empty) = (True, a)
    | (b == e) && (e == h) && (b /= Empty) = (True, b)
    | (c == f) && (f == i) && (c /= Empty) = (True, c)

    --3 in a row diagonal
    | (a == e) && (e == i) && (a /= Empty) = (True, a)
    | (c == e) && (e == g) && (c /= Empty) = (True, c)

    -- it is not an endstate
    | otherwise = (False, Empty)

allCellsFull :: Board -> Bool
allCellsFull b
    
    | (a1 b /= Empty) && (a2 b /= Empty) && (a3 b /= Empty) && (b1 b /= Empty) && (b2 b /= Empty) && (b3 b /= Empty) && (c1 b /= Empty) && (c2 b /= Empty) && (c3 b /= Empty) = True
    | otherwise = False

exitGame :: IO ()
exitGame = do
    putStr "\n\nGame Over\n\nPress any Key to exit\n\n"
    c <- getChar
    exitSuccess