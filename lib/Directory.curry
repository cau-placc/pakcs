--- Library for accessing the directory structure of the
--- underlying operating system.
---
--- @author Michael Hanus
--- @version March 2008

module Directory(doesFileExist,doesDirectoryExist,fileSize,
                 getModificationTime,
                 getCurrentDirectory,setCurrentDirectory,
                 getDirectoryContents,createDirectory,
                 removeFile,removeDirectory,renameFile,renameDirectory) where

import Time(ClockTime)


--- Returns true if the argument is the name of an existing file.
doesFileExist :: String -> IO Bool
doesFileExist fname = prim_doesFileExist $## fname

prim_doesFileExist :: String -> IO Bool
prim_doesFileExist external

--- Returns true if the argument is the name of an existing directory.
doesDirectoryExist :: String -> IO Bool
doesDirectoryExist dir = prim_doesDirectoryExist $## dir

prim_doesDirectoryExist :: String -> IO Bool
prim_doesDirectoryExist external

--- Returns the size of the file.
fileSize :: String -> IO Int
fileSize fname = prim_fileSize $## fname

prim_fileSize :: String -> IO Int
prim_fileSize external

--- Returns the modification time of the file.
getModificationTime :: String -> IO ClockTime
getModificationTime fname = prim_getModificationTime $## fname

prim_getModificationTime :: String -> IO ClockTime
prim_getModificationTime external

--- Returns the current working directory.
getCurrentDirectory :: IO String
getCurrentDirectory external

--- Sets the current working directory.
setCurrentDirectory :: String -> IO ()
setCurrentDirectory dir = prim_setCurrentDirectory $## dir

prim_setCurrentDirectory :: String -> IO ()
prim_setCurrentDirectory external

--- Returns the list of all entries in a directory.
getDirectoryContents :: String -> IO [String]
getDirectoryContents dir = prim_getDirectoryContents $## dir

prim_getDirectoryContents :: String -> IO [String]
prim_getDirectoryContents external

--- Creates a new directory with the given name.
createDirectory :: String -> IO ()
createDirectory dir = prim_createDirectory $## dir

prim_createDirectory :: String -> IO ()
prim_createDirectory external

--- Deletes a file from the file system.
removeFile :: String -> IO ()
removeFile file = prim_removeFile $## file

prim_removeFile :: String -> IO ()
prim_removeFile external

--- Deletes a directory from the file system.
removeDirectory :: String -> IO ()
removeDirectory dir = prim_removeDirectory $## dir

prim_removeDirectory :: String -> IO ()
prim_removeDirectory external

--- Renames a file.
renameFile :: String -> String -> IO ()
renameFile file1 file2 = (prim_renameFile $## file1) $## file2

prim_renameFile :: String -> String -> IO ()
prim_renameFile external

--- Renames a directory.
renameDirectory :: String -> String -> IO ()
renameDirectory dir1 dir2 = (prim_renameDirectory $## dir1) $## dir2

prim_renameDirectory :: String -> String -> IO ()
prim_renameDirectory external
