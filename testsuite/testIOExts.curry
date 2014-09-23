-- Testing operations from library IOExts:

import IO
import IOExts
import List
import Assertion

-- Execute shell command show the first output line of its execution:
getExec cmd = do
  hdl <- connectToCommand cmd
  s <- hGetLine hdl
  hClose hdl
  return s

testConnectCmd = assertIO "connectToCommand" (getExec "echo abcde") "abcde"

ioref1 = do
  r <- newIORef (1,True)
  (n,_) <- readIORef r
  writeIORef r (n,False)
  readIORef r

testIORef1 = assertIO "IORef" ioref1 (1,False)

ioref2 = do
  r <- newIORef [1,2..]
  l1 <- readIORef r
  writeIORef r (replace 42 2 l1)
  l2 <- readIORef r
  return (take 5 l2)

testIORef2 = assertIO "IORef infinite" ioref2 [1,2,42,4,5]


ioref3 = do
  r <- let x free in newIORef (x,x)
  (_,y) <- readIORef r
  doSolve (y=:=1)
  (z,_) <- readIORef r
  return z

testIORef3 = assertIO "IORef logic" ioref3 1

