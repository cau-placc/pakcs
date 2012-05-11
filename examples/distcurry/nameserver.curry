-- Programming with ports: a name server

import Ports

-- a name server which understands the following messages:
-- "PutName <name> <nr>": assign value <nr> (an int) to <name>
-- "GetName <name> <answerport>": return on <answerport> value of <name>
-- "Close"

-- the hostname of the machine running the name server:
--serverhost = "petrus.informatik.uni-kiel.de"
serverhost = "localhost"

-- the messages:
data Message = GetName String Int | PutName String Int | Close

--------------------------------------------------------------------------
-- the server side functions:
serve = openNamedPort "nameserver" >>= serverloop (const 0)

serverloop :: (String->Int) -> [Message] -> IO ()
serverloop _   (Close : _) = done
serverloop n2i (GetName n i : s) | i =:= (n2i n) = serverloop n2i s
serverloop n2i (PutName n i : s) = serverloop new_n2i s
   where new_n2i m = if m==n then i else n2i m


--------------------------------------------------------------------------
-- client side functions:
pn1 = ns_client (PutName "talk" 42)
pn2 = ns_client (PutName "email" 43)

-- ask for value of <name> at name server host <host>
nameNr name = ns_client (GetName name answer) >>
              putStrLn ("Answer: " ++ show answer)
 where answer free

-- Example use:
gn1 = nameNr "talk"
gn2 = nameNr "email"

-- close the name server by client:
closeServer = ns_client Close

-- send nameserver message to server
ns_client msg =
   do p <- connectPort ("nameserver@"++serverhost)
      doSend msg p
