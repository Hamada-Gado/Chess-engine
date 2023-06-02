import Prelude hiding (traverse) -- to avoid name clashes with our functions

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece]) -- (player, whitePieces, blackPieces)

-- getters
getLocation :: Piece -> Location
getLocation (P location) = location
getLocation (N location) = location
getLocation (B location) = location
getLocation (R location) = location
getLocation (Q location) = location
getLocation (K location) = location

getConstr :: Piece -> Location -> Piece
getConstr (P _) = P
getConstr (N _) = N
getConstr (B _) = B
getConstr (R _) = R
getConstr (Q _) = Q
getConstr (K _) = K

-- set board
setBoard :: (Player, [Piece], [Piece])
setBoard = (White,
    [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
    Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
    P ('h',2),P ('g',2),P ('f',2),P ('e',2),
    P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
    [R ('h',8),N ('g',8),B ('f',8),K ('e',8),
    Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
    P ('h',7),P ('g',7),P ('f',7),P ('e',7),
    P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

-- Visualize the board
visualizeBoard:: Board -> String
visualizeBoard (player, whitePieces, blackPieces) = "    a    b    c    d    e    f    g    h\n8 " ++ visualizeBoardHelper (player, whitePieces, blackPieces) ('a', 8) ++ "\n\n" ++ "Turn: " ++ show player

visualizeBoardHelper :: Board -> Location -> String
visualizeBoardHelper (player, whitePieces, blackPieces) ('h', 1) = getCell ('h', 1) whitePieces blackPieces ++ "|"
visualizeBoardHelper (player, whitePieces, blackPieces) (c, i) | c == 'h' = getCell (c, i) whitePieces blackPieces ++ "|" ++ "\n\n" ++ show (i-1) ++ " " ++ visualizeBoardHelper (player, whitePieces, blackPieces) ('a', i-1)
                                                               | otherwise = getCell (c, i) whitePieces blackPieces ++ visualizeBoardHelper (player, whitePieces, blackPieces) (succ c, i)

-- check if move is legal
isLegal :: Piece -> Board -> Location -> Bool
isLegal piece board nextLoc = nextLoc `elem` allLegalMoves piece board

-- suggest a legal move
suggestMove :: Piece -> Board -> [Location]
suggestMove = allLegalMoves

-- args is a function that takes a traverse function and apply it on depth, color, location, and board to return a list of locations
allLegalMoves :: Piece -> Board -> [Location]
allLegalMoves (P location) board = concatMap args [traversePawnVertical1, traversePawnVertical2, traversePawnDiagonal]
                              where color = getLocColor location board
                                    args f = f color location board

allLegalMoves (N location) board = concatMap args [traverse pred (+2) 1, traverse succ (+2) 1, traverse pred (subtract 2) 1, traverse succ (subtract 2) 1, traverse (pred . pred) (+1) 1, traverse (succ . succ) (+1) 1, traverse (pred . pred) (subtract 1) 1, traverse (succ . succ) (subtract 1) 1]
                                 where color = getLocColor location board
                                       args f = f color location board


allLegalMoves (B location) board = concatMap args [traverseDiagonalLeftDown, traverseDiagonalLeftUp, traverseDiagonalRightDown, traverseDiagonalRightUp]
                              where color = getLocColor location board
                                    args f = f (-1) color location board

allLegalMoves (R location) board = concatMap args [traverseLeft, traverseUp, traverseDown, traverseRight]
                              where color = getLocColor location board
                                    args f = f (-1) color location board

allLegalMoves (Q location) board = concatMap args [traverseLeft, traverseUp, traverseDown, traverseRight, traverseDiagonalLeftDown, traverseDiagonalLeftUp, traverseDiagonalRightDown, traverseDiagonalRightUp]
                              where color = getLocColor location board
                                    args f = f (-1) color location board

allLegalMoves (K location) board = concatMap args [traverseLeft, traverseUp, traverseDown, traverseRight, traverseDiagonalLeftDown, traverseDiagonalLeftUp, traverseDiagonalRightDown, traverseDiagonalRightUp]
                              where color = getLocColor location board
                                    args f = f 1 color location board

-- move a piece
move :: Piece -> Location -> Board -> Board
move piece location board
                        | not (checkPiece piece board) = error "Program error: Invalid piece"
                        | color /= player = error ("Program error: This is " ++ show player ++ " player's turn, " ++ show color ++ " can't move.")
                        | not (isLegal piece board location) = error ("Program error: Illegal move for piece " ++ show piece)
                        | otherwise = movePiece piece location board
                        where (player, _, _) = board
                              color = getLocColor (getLocation piece) board
                              nextPlayer = changeTurn player


movePiece :: Piece -> Location -> Board -> Board
movePiece piece location board
                              | player == White = (nextPlayer, getConstr piece location : nextWhitePieces, nextBlackPieces)
                              | player == Black = (nextPlayer, nextWhitePieces, getConstr piece location : nextBlackPieces)
                              where (player, whitePieces, blackPieces) = board
                                    nextPlayer = changeTurn player
                                    myFilter = filter (\p -> getLocation p /= getLocation piece && getLocation p /= location)
                                    nextWhitePieces = myFilter whitePieces
                                    nextBlackPieces = myFilter blackPieces





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- traverse functions
traversePawnVertical1 :: Player -> Location -> Board -> [Location]
traversePawnVertical1 color (c, i) (player, whitePieces, blackPieces)
                              | i > 7 || i < 2 = []
                              | color == Black && not (isPiece (c, i - 1) (whitePieces ++ blackPieces)) = [(c, i - 1)]
                              | color == White && not (isPiece (c, i + 1) (whitePieces ++ blackPieces)) = [(c, i + 1)]
                              | otherwise = []

traversePawnVertical2 :: Player -> Location -> Board -> [Location]
traversePawnVertical2 color (c, i) (player, whitePieces, blackPieces)
                              | color == Black && i == 7 && not (isPiece (c, i - 1) (whitePieces ++ blackPieces)) && not (isPiece (c, i - 2) (whitePieces ++ blackPieces)) = [(c, i - 2)]
                              | color == White && i == 2 && not (isPiece (c, i + 1) (whitePieces ++ blackPieces)) && not (isPiece (c, i + 2) (whitePieces ++ blackPieces)) = [(c, i + 2)]
                              | otherwise = []

traversePawnDiagonal :: Player -> Location -> Board -> [Location]
traversePawnDiagonal color (c, i) (player, whitePieces, blackPieces)
                              | i > 7 || i < 2 = []
                              | color == Black && succ c <= 'h' && isPiece (succ c, i - 1) whitePieces = [(succ c, i - 1)]
                              | color == Black && pred c >= 'a' && isPiece (pred c, i - 1) whitePieces = [(pred c, i - 1)]
                              | color == White && succ c <= 'h' && isPiece (succ c, i + 1) blackPieces = [(succ c, i + 1)]
                              | color == White && pred c >= 'a' && isPiece (pred c, i + 1) blackPieces = [(pred c, i + 1)]
                              | otherwise = []


-- fc: file change function, fi: row change function, depth: max depth, -- color: color of the piece, (c, i): current location, (player, whitePieces, blackPieces): board
traverse :: (Char -> Char) -> (Int -> Int) -> Int -> Player -> Location -> Board -> [Location]
traverse fc fi depth color (c, i) (player, whitePieces, blackPieces)
                | depth == 0 = [] -- reached max depth
                | dc < 'a' || dc > 'h' || di < 1 || di > 8 = [] -- out of bounds
                | color == White && isPiece nextLoc blackPieces = [nextLoc] -- capture
                | color == Black && isPiece nextLoc whitePieces = [nextLoc] -- capture
                | not (isPiece nextLoc (whitePieces ++ blackPieces)) = nextLoc : traverse fc fi (depth - 1) color nextLoc (player, whitePieces, blackPieces) -- empty cell
                | otherwise = [] -- blocked by own piece
                where dc = fc c -- next file
                      di = fi i -- next row
                      nextLoc = (dc, di) -- next location

traverseUp :: Int -> Player -> Location -> Board -> [Location]
traverseUp = traverse buffer (+1)

traverseDown :: Int -> Player -> Location -> Board -> [Location]
traverseDown = traverse buffer (subtract 1)

traverseRight :: Int -> Player -> Location -> Board -> [Location]
traverseRight = traverse succ buffer

traverseLeft :: Int -> Player -> Location -> Board -> [Location]
traverseLeft = traverse pred buffer

traverseDiagonalRightUp :: Int -> Player -> Location -> Board -> [Location]
traverseDiagonalRightUp = traverse succ (+1)

traverseDiagonalLeftUp :: Int -> Player -> Location -> Board -> [Location]
traverseDiagonalLeftUp = traverse pred (+1)

traverseDiagonalRightDown :: Int -> Player -> Location -> Board -> [Location]
traverseDiagonalRightDown = traverse succ (subtract 1)

traverseDiagonalLeftDown :: Int -> Player -> Location -> Board -> [Location]
traverseDiagonalLeftDown = traverse pred (subtract 1)





--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Utility functions
buffer :: x -> x
buffer x = x

representPiece :: Player -> Piece -> String
representPiece White piece = "| " ++ [head (show piece)] ++ "W "
representPiece Black piece = "| " ++ [head (show piece)] ++ "B "

getCell :: Location -> [Piece] -> [Piece] -> String
getCell location whitePieces blackPieces | isPiece location whitePieces = representPiece White (findPiece location whitePieces)
                                    | isPiece location blackPieces = representPiece Black (findPiece location blackPieces)
                                    | otherwise = "|    "

findPiece :: Location -> [Piece] -> Piece
findPiece location (p:px) = if location == getLocation p then p else findPiece location px

isPiece :: Location -> [Piece] -> Bool
isPiece location = foldr (\p -> (||) (location == getLocation p)) False

getLocColor :: Location -> Board -> Player
getLocColor location (_, whitePieces, blackPieces) 
                                                | isPiece location whitePieces = White
                                                | isPiece location blackPieces = Black
                                                | otherwise = error "Program error: Piece not found"

checkPiece :: Piece -> Board -> Bool
checkPiece piece (player, whitePieces, blackPieces) = isPiece location (whitePieces ++ blackPieces) && findPiece location (whitePieces ++ blackPieces) == piece
                                                      where location = getLocation piece

changeTurn :: Player -> Player
changeTurn White = Black
changeTurn Black = White