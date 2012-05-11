-- an implementation of semaphores in Curry:

import Ports
import Unsafe(trace)

-- semaphores are considered as object which can receive the following
-- messages:
data SemMessage = Wait () | Signal | CondWait Bool | Stop

-- the local state of a semaphore consists of the current value
-- and a list of waiting clients (identified by their logical
-- variables in their Wait message)
semaphore :: (Int,[()]) -> [SemMessage] -> Success
semaphore (n,wl) (Wait w : ms) =
  if n>0 then w=:=() & semaphore (n-1,wl) ms
         else semaphore (n,wl++[w]) ms
semaphore (n,wl) (CondWait b : ms) =
  if n>0 then b=:=True & semaphore (n-1,wl) ms
         else b=:=False & semaphore (n,wl) ms
semaphore (n,[])   (Signal : ms) = semaphore (n+1,[]) ms
semaphore (n,w:wl) (Signal : ms) | w=:=() = semaphore (n,wl) ms
semaphore _ (Stop : _) = success


-------------------------------------------------------------------------
-- programmer interface for semaphores:

-- create new semaphore:
newsem :: Int -> Port SemMessage -> Success
newsem n semport = newObject semaphore (n,[]) semport

-- wait for sempahore to be available:
wait :: Port SemMessage -> Success
wait semport = send (Wait var) semport &> (var==var)=:=True
               where var free

-- wait for sempahore to be available:
condwait :: Port SemMessage -> Bool
condwait semport | send (CondWait b) semport = b
                 where b free

-- signal semaphore
signal :: Port SemMessage -> Success
signal semport = send Signal semport

-- delete semaphore:
delsem :: Port SemMessage -> Success
delsem semport = send Stop semport

-------------------------------------------------------------------------


-- example:
critical str sem = wait sem &> trace str success &> signal sem

critical2 str sem1 sem2 = wait sem1 &> wait sem2 &>
                            trace str success &>
                          signal sem1 &> signal sem2

critgoal1 n = newsem n sem
              & critical "region 1" sem
              & critical "region 2" sem
              where sem free

critgoal2 n = newsem n sem1 & newsem n sem2
               & wait sem1
               & critical2 "region 1" sem1 sem2
               & critical "region 2" sem2
               & signal sem1
              where sem1,sem2 free

-- end of semaphores
