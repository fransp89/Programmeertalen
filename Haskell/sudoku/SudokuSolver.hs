{- Name: Francesco Pavlovic
Study: B.Sc Informatica
UvAnetID: 13782118
-}

import System.Environment
import Data.List
import qualified Data.Set as Set

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Extends a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Read a file-sudoku with a Grid like format into a Sudoku.
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{- Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

-- Makes a list containing all values of the given row in the given sudoku.
listOfRow :: Sudoku -> Row -> [Value]
listOfRow sudoku row = [sudoku(row, column) | column <- positions]

{- Makes a list of all values that are still available in a given row in
    the sudoku. -}
freeInRow :: Sudoku -> Row -> [Value]
freeInRow sudoku row =
    filter (`notElem` listRow) values
        where listRow = listOfRow sudoku row

-- Makes a list containing all values of the given column in the given sudoku.
listOfColumn :: Sudoku -> Column -> [Value]
listOfColumn sudoku column = [sudoku(row, column) | row <- positions]

{- Makes a list of all values that are still available in a given column in
    the sudoku. -}
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sudoku column =
    filter (`notElem` listColumn) values
        where listColumn = listOfColumn sudoku column

-- can be faster, more logical, non-partial
-- Finds the blocks arrays that the given (row, column) is a part of.
findBeginSubgrid :: Sudoku -> (Row, Column) -> ([Int], [Int])
findBeginSubgrid sudoku (row, column) =
    let x = [head list | list <- blocks, row `elem` list]
        y = [head list | list <- blocks, column `elem` list]
        in (x, y)

-- Makes a list of the subgrid that (row, column) is a part of.
listOfSubgrid :: Sudoku -> (Row, Column) -> [Value]
listOfSubgrid sudoku (row, column) =
    let x = fst (findBeginSubgrid sudoku (row, column))
        y = snd (findBeginSubgrid sudoku (row, column))
        in [sudoku(a, b) | a <- x, b <- y]

{- Makes a list of all values that are still available in a given subgrid in
    the sudoku. -}
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sudoku (row, column) =
    let subgrid = listOfSubgrid sudoku (row, column)
        in filter (`notElem` subgrid) values

{- Makes a list of all legal values at position (row, column) in the given
    sudoku. -}
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos sudoku (row, column) =
    let freeValuesRow = freeInRow sudoku row
        freeValuesColumn = freeInColumn sudoku column
        freeValuesSubgrid = freeInSubgrid sudoku (row, column)
        in filter (`notElem` freeValuesSubgrid) (filter (`notElem`
                   freeValuesColumn) (filter (`notElem` freeValuesRow) values))

-- Makes a list of coordinates that have 0 (empty) as value.
openPositions :: Sudoku -> [(Row,Column)]
openPositions sudoku = [(row, column) | row <- positions, column <- positions,
                                        sudoku(row, column) == 0]

-- True if no illegal values exist in row in sudoku, otherwise False.
rowValid :: Sudoku -> Row -> Bool
rowValid sudoku row =
    let list = listOfRow sudoku row
        nonZeroList = filter (`elem` values) list
        lengthList = length nonZeroList
        rowSet = Set.fromList nonZeroList
        lengthSet = Set.size rowSet
        in lengthList == lengthSet

{- True if no illegal values are present in all rows in sudoku, otherwise
    False. -}
allRowsValid :: Sudoku -> Bool
allRowsValid sudoku =
    let listOfBool = [rowValid sudoku row | row <- positions]
        in all (== True) listOfBool

-- True if no illegal values exist in column in sudoku, otherwise False.
colValid :: Sudoku -> Column -> Bool
colValid sudoku column =
    let list = listOfRow sudoku column
        nonZeroList = filter (`elem` values) list
        lengthList = length nonZeroList
        columnSet = Set.fromList nonZeroList
        lengthSet = Set.size columnSet
        in lengthList == lengthSet

{- True if no illegal values are present in all columns in sudoku, otherwise
    False. -}
allColumnsValid :: Sudoku -> Bool
allColumnsValid sudoku =
    let listOfBool = [colValid sudoku column | column <- positions]
        in all (== True) listOfBool

{- True if no illegal values exist in the subgrid where (row, column) is in in
    sudoku, otherwise False. -}
subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid sudoku (row, column) =
    let subgrid = listOfSubgrid sudoku (row, column)
        nonZeroSubgrid = filter (`elem` values) subgrid
        lengthSubgrid = length nonZeroSubgrid
        subgridSet = Set.fromList nonZeroSubgrid
        lengthSet = Set.size subgridSet
        in lengthSubgrid == lengthSet

{- True if no illegal values are present in all subgrid in sudoku, otherwise
    False. -}
allSubgridsValid :: Sudoku -> Bool
allSubgridsValid sudoku =
    let listOfBool = [subgridValid sudoku (row, column)
                      | row <- centerOfBlocks, column <- centerOfBlocks]
        in all (== True) listOfBool

{- True if all rows, columns and subgrids contain no illegal values, otherwise
    False. -}
consistent :: Sudoku -> Bool
consistent sudoku =
    let row = allRowsValid sudoku
        column = allColumnsValid sudoku
        subgrid = allSubgridsValid sudoku
        list = [row, column, subgrid]
        in and list

-- Prints node
printNode :: Node -> IO()
printNode = printSudoku . fst

-- Gives a list of all constraints in sudoku.
listConstraints :: Sudoku -> [Constraint]
listConstraints sudoku = [(row, column, freeAtPos sudoku (row, column))
                          | row <- positions,
                            column <- positions]

{--}
constraints :: Sudoku -> [Constraint]
constraints sudoku =
    let list = [(row, column, freeAtPos sudoku (row, column)) | row <- positions, column <- positions]
        in sortBy (\(_,_,a) (_,_,b) -> compare  (length a) (length b)) list

{--}
main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       -- TODO: Call your solver.
       printSudoku sud
