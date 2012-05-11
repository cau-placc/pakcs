------------------------------------------------------------------------------
--- Library for reading/writing files in CSV format.
--- Files in CSV (comma separated values) format can be imported and exported
--- by most spreadsheed and database applications.
---
--- @author Michael Hanus
--- @version September 2004
------------------------------------------------------------------------------

module CSV(showCSV,readCSV,readCSVWithDelims,
           writeCSVFile,readCSVFile,readCSVFileWithDelims) where

import List(intersperse)

--- Writes a list of records (where each record is a list of strings)
--- into a file in CSV format.
--- @param fname - the name of the result file (with standard suffix ".csv")
--- @param rows - the list of rows
writeCSVFile :: String -> [[String]] -> IO ()
writeCSVFile fname rows = writeFile fname (showCSV rows)

--- Shows a list of records (where each record is a list of strings)
--- as a string in CSV format.
showCSV :: [[String]] -> String
showCSV rows = concatMap showCSVLine rows

--- Shows a list of strings as a line in CSV format.
showCSVLine :: [String] -> String
showCSVLine row = concat (intersperse "," (map convert row)) ++ "\n"
 where
   -- enclose in quotation marks if necessary:
   convert s =
      if any (\c->c `elem` ['"',',',';',':','\n']) s
      then '"' : concatMap (\c->if c=='"' then [c,c] else [c]) s ++ "\""
      else s


--- Reads a file in CSV format and returns the list of records
--- (where each record is a list of strings).
--- @param fname - the name of the result file (with standard suffix ".csv")
readCSVFile :: String -> IO [[String]]
readCSVFile = readCSVFileWithDelims [',']

--- Reads a file in CSV format and returns the list of records
--- (where each record is a list of strings).
--- @param delims - the list of characters considered as delimiters
--- @param fname - the name of the result file (with standard suffix ".csv")
readCSVFileWithDelims :: [Char] -> String -> IO [[String]]
readCSVFileWithDelims delims fname = do
  contents <- readFile fname
  return (readCSVWithDelims delims contents)

--- Reads a string in CSV format and returns the list of records
--- (where each record is a list of strings).
--- @param str - the string in CSV format
readCSV :: String -> [[String]]
readCSV = readCSVWithDelims [',']

--- Reads a string in CSV format and returns the list of records
--- (where each record is a list of strings).
--- @param delims - the list of characters considered as delimiters
--- @param str - the string in CSV format
readCSVWithDelims :: [Char] -> String -> [[String]]
readCSVWithDelims delims str = map (components delims) (lines str)

--- Breaks a string in CSV record format into a list of components.
components :: [Char] -> String -> [String]
components _ [] = [[]]
components delims (c:cs) =
  if c=='"' then breakString cs
            else let (e,s) = break (`elem` delims) (c:cs)
                  in e : (if null s then [] else components delims (tail s))
 where
   breakString [] = delimError
   breakString [x] = if x=='"' then [[]]
                               else delimError
   breakString (x:y:zs) | x=='"' && y=='"' = let (b:bs) = breakString zs
                                              in (x:b):bs
                        | x=='"' && y `elem` delims = []:components delims zs
                        | otherwise = let (b:bs) = breakString (y:zs)
                                       in (x:b):bs

   delimError  = error "Missing closing delimiter in CSV record!"

-- Examples:
-- writeCSVFile "tmp.csv" [["Name","Value"],["aa\"bb,cc","1"]]
-- readCSVFile "tmp.csv"
