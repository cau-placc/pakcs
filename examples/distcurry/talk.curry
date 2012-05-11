-- A simple program implementing the kernel of the Unix "talk"
-- to enable a conversation between two partners on different machines

import Ports
import IO

-- Message to inform the partner to show a string:
data TalkMsg = Talk String

-- start the talk between yourport and myport
talk :: String -> String -> IO ()
talk myport yourport = do
  incoming   <- openNamedPort myport
  putStrLn $ "Waiting for connection to '"++yourport++"'"
  Just targetPort <- connectPortRepeat 1000 (putChar '.') (-1) yourport
  putStrLn $ "Connected to '"++yourport++"' (terminate with \".\")"
  talkloop targetPort stdin incoming

-- the main loop to implement the conversation:
talkloop :: Port TalkMsg -> Handle -> [TalkMsg] -> IO ()
talkloop yourport tty yourmsgs = do
  input <- hWaitForInputOrMsg tty yourmsgs
  processInput input
 where
  processInput (Left handle) = do
    ttyline <- hGetLine handle
    doSend (Talk ttyline) yourport
    if ttyline=="." then done
                    else talkloop yourport tty yourmsgs
  processInput (Right (Talk msg:msgs)) =
    if msg=="." then done
                else putStrLn ('*':msg) >> talkloop yourport tty msgs


-- start "talk1" on one machine:
talk1 = talk "me" "you" --"you@sauron.informatik.uni-kiel.de"

-- start "talk2" on another machine:
talk2 = talk "you" "me" --"me@petrus.informatik.uni-kiel.de"
