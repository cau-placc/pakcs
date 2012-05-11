------------------------------------------------------------------------------
--- A collection of useful operations when dealing with files.
---
--- @author Michael Hanus, Bernd Brassel
--- @version June 2009
------------------------------------------------------------------------------

module FileGoodies(separatorChar,pathSeparatorChar,suffixSeparatorChar,
                   isAbsolute,dirName,baseName,splitDirectoryBaseName,
                   stripSuffix,fileSuffix,splitBaseName,splitPath,
                   findFileInPath,lookupFileInPath,getFileInPath) where

import Directory
import List(intersperse)

--- The character for separating hierarchies in file names.
--- On UNIX systems the value is '/'.
separatorChar :: Char
separatorChar = '/'

--- The character for separating names in path expressions.
--- On UNIX systems the value is ':'.
pathSeparatorChar :: Char
pathSeparatorChar = ':'

--- The character for separating suffixes in file names.
--- On UNIX systems the value is '.'.
suffixSeparatorChar :: Char
suffixSeparatorChar = '.'

--- Is the argument an absolute name?
isAbsolute :: String -> Bool
isAbsolute (c:_) = c == separatorChar
isAbsolute [] = False

--- Extracts the directoy prefix of a given (Unix) file name.
--- Returns "." if there is no prefix.
dirName :: String -> String
dirName name = fst (splitDirectoryBaseName name)

--- Extracts the base name without directoy prefix of a given (Unix) file name.
baseName :: String -> String
baseName name = snd (splitDirectoryBaseName name)

--- Splits a (Unix) file name into the directory prefix and the base name.
--- The directory prefix is "." if there is no real prefix in the name.
splitDirectoryBaseName :: String -> (String,String)
splitDirectoryBaseName name =
  let (rbase,rdir) = break (==separatorChar) (reverse name) in
  if null rdir then (".",reverse rbase)
               else (reverse (tail rdir), reverse rbase)

--- Strips a suffix (the last suffix starting with a dot) from a file name.
stripSuffix :: String -> String
stripSuffix = fst . splitBaseName

--- Yields the suffix (the last suffix starting with a dot) from given file name.
fileSuffix :: String -> String
fileSuffix = snd . splitBaseName

--- Splits a file name into prefix and suffix (the last suffix starting with a dot 
--- and the rest).
splitBaseName :: String -> (String,String)
splitBaseName name = let (rsuffix,rbase) = break (==suffixSeparatorChar) (reverse name) in
  if null rbase || elem separatorChar rsuffix
  then (name,"")
  else (reverse (tail rbase),reverse rsuffix)

--- Splits a path string into list of directory names.
splitPath :: String -> [String]
splitPath [] = []
splitPath (x:xs) = let (ys,zs) = break (==pathSeparatorChar) (x:xs)
                    in if null zs then [ys]
                                  else ys : splitPath (tail zs)

--- Included for backward compatibility.
--- Use <code>lookupFileInPath</code> instead!
findFileInPath :: String -> [String] -> [String] -> IO (Maybe String)
findFileInPath = lookupFileInPath

--- Looks up the first file with a possible suffix in a list of directories.
--- Returns Nothing if such a file does not exist.
lookupFileInPath :: String -> [String] -> [String] -> IO (Maybe String)
lookupFileInPath file suffixes path =
  if isAbsolute file
  then lookupFirstFileWithSuffix file suffixes
  else lookupFirstFile path
 where
   lookupFirstFile [] = return Nothing
   lookupFirstFile (dir:dirs) = do
     mbfile <- lookupFirstFileWithSuffix (dir++separatorChar:file) suffixes
     maybe (lookupFirstFile dirs) (return . Just) mbfile

   lookupFirstFileWithSuffix _ [] = return Nothing
   lookupFirstFileWithSuffix f (suf:sufs) = do
     let fsuf = f++suf
     exfile <- doesFileExist fsuf
     if exfile then return (Just fsuf)
               else lookupFirstFileWithSuffix f sufs

--- Gets the first file with a possible suffix in a list of directories.
--- An error message is delivered if there is no such file.
getFileInPath :: String -> [String] -> [String] -> IO String
getFileInPath file suffixes path = do
  mbfile <- lookupFileInPath file suffixes path
  maybe (error $ "File "++file++" not found in path "++
                 concat (intersperse [pathSeparatorChar] path))
        return
        mbfile
