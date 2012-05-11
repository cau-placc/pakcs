module Imports (imports) where

import CurryStringClassifier
import List
import System

main = do args <- getArgs 
          case args of 
           [name] -> readFile name >>= putStr . maybeSpace . imports 
           _ -> putStrLn "usage: imports filename"

isImport x = isPrefixOf "import " x

isImported x = case x of
                "qualified" -> False
                _ -> True

imports = unwords . map (takeWhile (/='(') . head . filter isImported . words . drop 6)
                  . filter isImport
                  . lines
                  . unscan 
                  . takeWhile isModuleHead 
                  . filter (not . isComment) 
                  . scan

maybeSpace "" = ""
maybeSpace (x:xs) = x:xs ++ " "