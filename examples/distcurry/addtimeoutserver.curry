------------------------------------------------------------------------------
--- A simple "addition" server to test the Socket library with time limits
--- on socket connections.
---
--- @author Michael Hanus
--- @version March 2006
------------------------------------------------------------------------------

import Socket
import IO
import Read(readInt)

-- Choose a free port number:
portnr = 32145

sendTo host msg = do
  h <- connectToSocket host portnr
  hPutStr h msg
  hClose h

stopServer host = sendTo host "TERMINATE\n"


-- An "addition" server:
addServer = do
  socket <- listenOn portnr
  putStrLn $ "Serving port: " ++ show portnr
  addServeSocket socket

addServeSocket socket = do
  conn <- waitForSocketAccept socket 1000
  addServeSocketTest socket conn

addServeSocketTest socket Nothing = do
  putStrLn "Timeout"
  addServeSocket socket

addServeSocketTest socket (Just (chost,stream)) = do
  putStrLn $ "Connection from "++chost
  serverLoop stream
 where
   serverLoop h = do
     l1 <- hGetLine h
     if l1=="TERMINATE"
      then do hClose h
              sClose socket
      else do l2 <- hGetLine h
              hPutStrLn h (show (readInt l1 + readInt l2))
              hClose h
              addServeSocket socket

addClient host x y = do
  h <- connectToSocket host portnr
  hPutStr h (unlines (map show [x,y]))
  hFlush h
  answer <- hGetLine h
  putStrLn $ "Answer: "++answer
  hClose h

{-
Test with PAKCS:

:fork addServer
addClient "localhost" 3 4
stopServer "localhost"

-}
