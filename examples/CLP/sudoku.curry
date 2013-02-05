-----------------------------------------------------------------------------
--- Solving Su Doku puzzles in Curry with FD constraints
---
--- @author Michael Hanus
--- @version February 2013
-----------------------------------------------------------------------------

import CLPFD
import List(transpose)
import Constraint(allC)

-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [[Int]] -> Success
sudoku m =
 domain (concat m) 1 9 &             -- define domain of all digits
 allC allDifferent m  &              -- all rows contain different digits
 allC allDifferent (transpose m)  &  -- all columns have different digits
 allC allDifferent (squares m) &     -- all 3x3 squares are different
 labeling [FirstFail] (concat m)
 where
  -- translate a matrix into a list of small 3x3 squares
  squares :: [[a]] -> [[a]]
  squares [] = []
  squares (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squares ls
  
  group3Rows l123 = if head l123 == [] then [] else
   concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- read a Su Doku specification written as a list of strings containing digits
-- and spaces
readSudoku :: [String] -> [[Int]]
readSudoku = map (map (\c -> if c==' ' then _ else ord c - ord '0'))

-- show a solved Su Doku matrix
showSudoku :: [[Int]] -> String
showSudoku = unlines . map (concatMap (\i->[chr (i + ord '0'),' ']))

-- the main function, e.g., evaluate (main s1):
main s | sudoku m = putStrLn (showSudoku m)
 where m = readSudoku s

s1 = ["9  2  5  ",
      " 4  6  3 ",
      "  3     6",
      "   9  2  ",
      "    5  8 ",
      "  7  4  3",
      "7     1  ",
      " 5  2  4 ",
      "  1  6  9"]

s2 = ["819  5   ",
      "  2   75 ",
      " 371 4 6 ",
      "4  59 1  ",
      "7  3 8  2",
      "  3 62  7",
      " 5 7 921 ",
      " 64   9  ",
      "   2  438"]

s3 = ["    63 8 ",
      "   1     ",
      "327   1  ",
      "9  2   3 ",
      "  6   4  ",
      " 3   4  9",
      "  8   627",
      "     6   ",
      " 4 51    "]
