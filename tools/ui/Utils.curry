--- Useful functions
---
--- @author Sebastian Fischer
module Utils where

import IO
import IOExts
import Char
import System

import Unsafe ( unsafePerformIO ) -- for unsafeCatchFail

invert :: (a -> b) -> b -> a
invert f = g
 where
  f' x = f x
  g (f' x) = x

unsafeCatchFail :: a -> a -> a
unsafeCatchFail x y =
  unsafePerformIO (catch (return x) (\e -> putStrLn (showError e) >> return y))

doWhile :: (a -> Bool) -> [IO a] -> IO [a]
doWhile _ [] = return []
doWhile p (action:actions) = do
  x <- action
  xs <- if p x then doWhile p actions else return []
  return (x:xs)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs@(_:_) 
  = case break p xs of
      (_,[]) -> [xs]
      (ys,_:zs) -> ys : splitBy p zs

base64 :: String -> IO String
base64 s = do
  (hIn,hOut,hErr) <- execCmd $ "echo -n " ++ s ++ " | uuencode -m /dev/stdout"
  result <- hGetContents hOut
  mapIO_ hClose [hIn,hErr]
  return $ lines result !! 1

hashMD5 :: String -> IO String
hashMD5 s = do
  (hIn,hOut,hErr) <- execCmd $ "echo -n " ++ s ++ " | md5sum"
  result <- hGetContents hOut
  mapIO_ hClose [hIn,hErr]
  return $ takeWhile (not . isSpace) result

nonce :: IO String
nonce = getCPUTime >>= hashMD5 . show
-- it may be a security risk to use the hashed CPU time as a nonce!


