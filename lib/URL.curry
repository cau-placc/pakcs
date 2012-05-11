------------------------------------------------------------------------------
--- Library for dealing with URLs (Uniform Resource Locators).
---
--- @author Michael Hanus
--- @version November 2003
------------------------------------------------------------------------------

module URL(getContentsOfUrl) where

import System
import IOExts(readCompleteFile)

--- Reads the contents of a document located by a URL.
--- This action requires that the program "wget" is in your path,
--- otherwise the implementation must be adapted to the local
--- installation.

getContentsOfUrl :: String -> IO String
getContentsOfUrl url = do
  pid <- getPID
  let tmpfile = "/tmp/wgeturl."++show pid
  system ("wget -O "++tmpfile++" \""++url++"\"")
  cont <- readCompleteFile tmpfile
  system ("rm -f "++tmpfile)
  return cont
