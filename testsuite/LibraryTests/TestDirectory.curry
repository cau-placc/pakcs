------------------------------------------------------------------------------
--- Some tests for library Directory
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "curry check TestDirectory"
--- 
--- @author Michael Hanus
--- @version February 2016
------------------------------------------------------------------------------

import Directory
import Sort
import System
import Test.Prop

testCreateRenameDeleteFile = fileOps `returns` (True,False,True,False)
 where
  fileOps = do
    let fname = "xxx1234"
        fnamebak = fname++".bak"
    writeFile fname "test\n"
    ex1 <- doesFileExist fname
    renameFile fname fnamebak
    ex2 <- doesFileExist fname
    ex3 <- doesFileExist fnamebak
    removeFile fnamebak
    ex4 <- doesFileExist fnamebak
    return (ex1,ex2,ex3,ex4)

testCreateRenameDeleteDirectory = dirOps `returns` (True,False,True,False)
 where
  dirOps = do
    let dname = "xxx1111"
        dnamebak = dname++".bak"
    createDirectory dname
    ex1 <- doesDirectoryExist dname
    renameDirectory dname dnamebak
    ex2 <- doesDirectoryExist dname
    ex3 <- doesDirectoryExist dnamebak
    removeDirectory dnamebak
    ex4 <- doesDirectoryExist dnamebak
    return (ex1,ex2,ex3,ex4)

testGetSetDirectory = dirOps `returns` (True,True,"abcdef",False)
 where
  dirOps = do
    cdir <- getCurrentDirectory
    let dname = cdir++"/xxx2222"
    createDirectory dname
    ex1 <- doesDirectoryExist dname
    writeFile (dname++"/xxx") "abcdef"
    setCurrentDirectory dname
    ex2 <- doesFileExist "xxx"
    cnt <- readFile "xxx"
    cnt==cnt `seq` removeFile "xxx"
    setCurrentDirectory cdir
    removeDirectory dname
    ex3 <- doesDirectoryExist dname
    return (ex1,ex2,cnt,ex3)

testGetDirectoryContents = dirOps `returns` [".","..","xxx"]
 where
  dirOps = do
    cdir <- getCurrentDirectory
    let dname = cdir++"/xxx3333"
    createDirectory dname
    setCurrentDirectory dname
    d <- getCurrentDirectory
    writeFile "xxx" "Hello\n"
    fs <- getDirectoryContents d
    fs==fs `seq` removeFile "xxx"
    setCurrentDirectory cdir
    removeDirectory dname
    return (mergeSortBy leqString fs)
