module A3Winning where

import Data.Array

-- You can import other modules/functions here.

data XO = X | O deriving (Eq, Show)

-- A game board is represented by a 2D array.  Indexes are from (0,0)
-- to (2,2).  Each cell is Nothing, Just X, or Just O, meaning blank,
-- has X, or has O.
--
-- So the type is Array (Int, Int) (Maybe XO).
-- The (Int, Int) there means an index is like (0, 0).

-- Blank initial game board.
initBoard :: Array (Int, Int) (Maybe XO)
initBoard = listArray ((0,0), (2,2)) (repeat Nothing)

-- Example board that has a winning move.
albertBoard :: Array (Int, Int) (Maybe XO)
albertBoard = initBoard // [ ((0,1), Just X)
                           , ((1,0), Just X)
                           , ((1,1), Just O)
                           , ((1,2), Just X)
                           ]

-- Print a game board.
printBoard :: Array (Int, Int) (Maybe XO) -> IO ()
printBoard board = do
    putStrLn "---"
    mapM_ (\i -> putStrLn [toChar (board!(i,j)) | j <- [0,1,2]])
          [0,1,2]
    putStrLn "---"
  where
    toChar Nothing = ' '
    toChar (Just X) = 'X'
    toChar (Just O) = 'O'

-- Board bounds in seperate variables
start, end :: Array (Int, Int) (Maybe XO) -> (Int, Int)
leftCol, topRow, rightCol, botRow :: Array (Int, Int) (Maybe XO) -> Int
start = fst . bounds
end = snd . bounds
topRow = fst . start 
leftCol = snd . start
botRow = fst . end
rightCol = fst . end
-- List of possible col/row values for a board
cols, rows :: Array (Int, Int) (Maybe XO) -> [Int]
cols board = [(leftCol board) .. (rightCol board)]
rows board = [(topRow board) .. (botRow board)]
-- Fetch a specific row/column out of the board
col, row :: Array (Int, Int) (Maybe XO) -> Int -> [Maybe XO]
col board colNum = [board ! (i, colNum) | i <- cols board]
row board rowNum = [board ! (rowNum, j) | j <- rows board]
-- Fetch the two diagonals out of the board
-- ([Top left going down and right 1], [Top right going left and down one])
leastSize :: Array (Int, Int) (Maybe XO) -> Int
leastSize board = minimum ((botRow board) - (topRow board),
                           (rightCol board) - (leftCol board))
diag :: Array (Int, Int) (Maybe XO) -> [[Maybe XO]]
diag board = [[board ! (tR + i, lC + i) | i <- [0..(leastSize board)]],
              [board ! (tR + i, rC - i) | i <- [0..(leastSize board)]]]
              where tR = topRow board
                    lC = leftCol board
                    rC = rightCol board
-- Test if a line (row, col, diag) wins the game
winningLine :: [Maybe XO] -> Bool
winningLine line = or $ [\x -> and $ map ((==) (Just x)) line] <*> [X, O]
-- Test if a board is in an end state
drawingBoard, winningBoard :: Array (Int, Int) (Maybe XO) -> Bool
winningBoard board = or $ map winningLine $ allRows ++ allCols ++ allDiags
    where allRows = map (row board) $ rows board
          allCols = map (col board) $ cols board
          allDiags = diag board
drawingBoard board = not $ or [winningBoard board, or $ fmap ((==) Nothing) board]

-- Return new board state given a move
applyMove :: Array (Int, Int) (Maybe XO) -> (Int, Int, XO) -> Array (Int, Int) (Maybe XO)
applyMove board (i, j, xo) = board // [((i, j), Just xo)]

-- Test move for end state conditions
blowingMove, drawingMove, winningMove :: Array (Int, Int) (Maybe XO) -> (Int, Int, XO) -> Bool
winningMove board move = winningBoard $ applyMove board move
drawingMove board move = drawingBoard $ applyMove board move
-- Test for blowing move
blowingMove board move = or [winningMove board move, blowingCondition]
    where blowingCondition = and [not (null possibleEnemyMoves), enemyCantEnd, restIsBlowingMove ]
          enemyCantEnd = not $ or ([winningMove board', drawingMove board'] <*> (moves board'))
          restIsBlowingMove = and $ map boardHasBlowingCondition possibleEnemyMoves
          boardHasBlowingCondition x = or $ map (blowingMove x) (moves x)
          possibleEnemyMoves = map (applyMove board') (moves board')
          board' = applyMove board move
-- Compute all legal moves for the given board.
-- In the answers, (i, j, X) means "put an X at cell (i,j)" for example.
moves :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)]
moves board = [\x -> (i, j, x) | i <- cols board,
                                 j <- rows board, 
                                 board ! (i, j) == Nothing] <*> [X, O]

-- Compute all blowing moves for the given game board.
-- In the answers, (i, j, X) means "put an X at cell (i,j)" for example.
howToWin :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)]
howToWin board = if (or (fmap (\x -> x board) [winningBoard, drawingBoard])) then []
                    else filter (blowingMove board) (moves board)


-- Example board 
keeganBoard :: Array (Int, Int) (Maybe XO)
keeganBoard = initBoard // [ ((0,0), Just X)
                           , ((0,2), Just O)
                           , ((1,0), Just O)
                           , ((1,1), Just O)
                           , ((1,2), Just X)
                           , ((2,0), Just X)
                           ]
keeganBoard2 :: Array (Int, Int) (Maybe XO)
keeganBoard2 = initBoard // [ ((0,0), Just X)
                           , ((0,1), Just X)
                           , ((0,2), Just O)
                           , ((1,0), Just O)
                           , ((1,1), Just O)
                           , ((1,2), Just X)
                           , ((2,0), Just X)
                           , ((2,1), Just O)
                           ]

