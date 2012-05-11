-- A simple example showing the direct connection to Unix sockets.
-- This example does not use ports but more low level stream operations.
-- On the positive side, there is no requirement on the implementation
-- language of the server.

import Socket(connectToSocket)
import IO

-- An I/O action that shows the answer of a web server to the
-- request of a document:
httpGet host doc = do
 str <- connectToSocket host 80
 hPutStr str ("GET " ++ doc ++ " HTTP/1.0\n\n")
 hFlush str
 showStreamContents str

-- Show the complete contents of an output stream:
showStreamContents str = do
 b <- hIsEOF str
 if b then done
      else do l <- hGetLine str
              putStrLn l
              showStreamContents str

-- A test:
main = httpGet "www-ps.informatik.uni-kiel.de" "/index.html"

