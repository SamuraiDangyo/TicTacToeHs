{-
  Name:    TicTacToeHs
  Version: 0.1
  Author:  Toni Helminen
  License: GPLv3
-}

import Control.Exception
import System.IO.Error  

data Color = White | Black deriving (Eq)
data Piece = X_piece | O_piece | Empty deriving (Eq)
data Result = White_win | Black_win | Tie deriving (Show, Eq)

first :: (a, b, c) -> a
first (a, _, _)  = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c)  = c

scan4Win :: [Piece] -> Piece -> Int -> Int -> Int -> Bool
scan4Win board piece start dx dy = (board !! start) == piece && (board !! (start + dx + 3 * dy)) == piece && (board !! (start + 2 * dx + 3 * (2 * dy))) == piece

isWhiteWin :: [Piece] -> Bool
isWhiteWin board = not $ null [x | x <- [(0,1,0),(0,0,1),(0,1,1),(2,(-1),1),(3,1,0),(6,1,0),(1,0,1),(2,0,1)], scan4Win board X_piece (first x) (second x) (third x) == True]

isTie :: [Piece] -> Bool
isTie board = length [x | x <- board, x /= Empty] == 9

isBlackWin :: [Piece] -> Bool
isBlackWin board = not $ null [x | x <- [(0,1,0),(0,0,1),(0,1,1),(2,(-1),1),(3,1,0),(6,1,0),(1,0,1),(2,0,1)], scan4Win board O_piece (first x) (second x) (third x) == True]

getResult :: [Piece] -> Maybe Result
getResult board
  | isWhiteWin board = Just White_win
  | isBlackWin board = Just Black_win
  | isTie board      = Just Tie
  | otherwise        = Nothing
  
result2String :: Maybe Result -> String
result2String res
  | res == Just White_win = "White wins!"
  | res == Just Black_win = "Black wins!"
  | otherwise = "Draw!"
  
piece2String :: Piece -> Char
piece2String c = case c of
  X_piece -> 'X'
  O_piece -> 'O'
  Empty    -> '.'
  
flipColor :: Color -> Color  
flipColor piece = case piece of
  White -> Black
  Black -> White
  
color2Piece :: Color -> Piece
color2Piece color = case color of
  White -> X_piece
  Black -> O_piece

board2String :: [Piece] -> [Char]  
board2String board = do
  let s = map piece2String board
  "+-+\n" ++ (take 3 (drop 6 s)) ++ " 3\n" ++ (take 3 (drop 3 s)) ++ " 2\n" ++ (take 3 s) ++ " 1\n123\n+-+\n"

makeMove :: Int -> Color -> [Piece] -> [Piece]
makeMove to color board
  | to == 0   = piece : (drop 1 board)
  | to == 8   = (take 8 board) ++ piece : []
  | otherwise = (take (to) board) ++ piece : (drop (to + 1) board)
  where piece = color2Piece color

legalMove :: [Piece] -> Int -> Bool
legalMove board to = to >= 0 && to < 9 && board !! to == Empty

--hashBoard :: [Piece] -> [Char]
--hashBoard []     = ' ' : []
--hashBoard (a:xs) = (piece2String a) : hashBoard xs

pow3 :: Int -> Int
pow3 x = x * x * x

playouts :: Int -> Int -> Color -> [Piece] -> Int
playouts depth to color board2 = do
  let board = makeMove to color board2
  let res = getResult board
  if res == Just White_win then 1 * (pow3(10 - depth)) else do
    if res == Just Black_win then -1 * (pow3(10 - depth)) else do
      if res == Just Tie || depth > 6 then 0 else sum [playouts (depth + 1) x (flipColor color) board | x <- [0..8], board !! x == Empty]

quicksort :: Ord a => [(a, b)] -> [(a, b)]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser  = filter ((<(fst p)).fst) xs
    greater = filter ((>=(fst p)).fst) xs

aiMove :: Color -> [Piece] -> Int
aiMove color board = do
  let moves = quicksort [(playouts 0 x color board, x) | x <- [0..8], board !! x == Empty]
  if color == White then snd $ last moves else snd $ head moves

-- impure as f...

getInt :: IO Int
getInt = readLn

aiPlay :: Color -> [Piece] -> IO()
aiPlay color board = do
  putStrLn $ board2String board
  let res = getResult board
  if res /= Nothing then print $ result2String res else do
    let i = aiMove color board
    putStrLn $ "Ai Thinking..."
    print $ i -- <- lazy right here...
    let board2 = makeMove i color board
    humanPlay (flipColor color) board2
    
humanPlay :: Color -> [Piece] -> IO()
humanPlay color board = do
  putStrLn $ board2String board
  let res = getResult board
  if res /= Nothing then print $ result2String res else do
    putStrLn $ "Your move?"
    i <- getInt
    let board2 = makeMove i color board
    if legalMove board i then aiPlay (flipColor color) board2 else humanPlay color board

play :: [Piece] -> IO()
play board = do
  putStrLn $ "+-+ TicTacToeHs +-+"
  putStrLn $ "Play White [Y/n]?"
  s <- getLine
  if s == "n" then aiPlay White board else humanPlay White board

handler :: IOError -> IO ()  
handler e
  | isUserError e = putStrLn "User error: I quit!"
  | otherwise     = putStrLn "Other problems: I'm done!"

go :: IO()
go = play $ take 9 $ repeat Empty

main :: IO()
main = go `catch` handler
