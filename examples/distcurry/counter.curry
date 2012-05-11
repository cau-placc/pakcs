-- Distributed programming in Curry:
-- a counter server reacting on messages of type (Set v), Inc, or (Get x):

import Ports

-- the counter implemented as a concurrent object:
data CounterMessage = Set Int | Inc | Get Int

counter :: Int -> [CounterMessage] -> Success
counter _ (Set v : ms) = counter v ms
counter n (Inc   : ms) = counter (n+1) ms
counter n (Get v : ms) = v=:=n & counter n ms
counter _ []           = success

-- creating the counter object as a server:
counter_server = newNamedObject counter 0 "counter"

-- a counter client:
cc msg = do
  port <- connectPort "counter@localhost"
  doSend msg port

cinc = cc Inc

cset n = cc (Set n)

cget x = cc (Get x)

