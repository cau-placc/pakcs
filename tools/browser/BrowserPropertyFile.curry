-----------------------------------------------------------------
-- Get and set properties in the browser property file ~/.currybrowser

module BrowserPropertyFile(getBrowserConfig,setBrowserConfig) where

import PropertyFile
import System
import Directory

--- Reads a browser property defined in the file "$HOME/.currybrowser".
getBrowserConfig :: String -> IO String
getBrowserConfig pname = do
  home <- getEnviron "HOME"
  let pfname = home++"/.currybrowser"
  pfexists <- doesFileExist pfname
  if pfexists then done else writeFile pfname initialBrowserProperties
  cbconfs <- readPropertyFile pfname
  return (maybe "" id (lookup pname cbconfs))

--- Sets a browser property defined in the file "$HOME/.currybrowser".
setBrowserConfig :: String -> String -> IO ()
setBrowserConfig pname pval = do
  home <- getEnviron "HOME"
  updatePropertyFile (home++"/.currybrowser") pname pval

-- Initial contents of browser property file:
initialBrowserProperties =
 "# Current configurations of CurryBrowser:\n"++
 "# viewer for dot graph specifications (that are written on stdin):\n"++
 "viewdot=dot -Tps | kghostview -\n"
