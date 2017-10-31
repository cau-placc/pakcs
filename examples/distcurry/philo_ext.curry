-- The "dining philosophers" example in Curry
-- using the IO monad and external semaphores:
-- each philosopher is represented by a Curry program and
-- the semaphores are external ports

import Ports
import sema

-- create external semaphore:
newextsem n name = openNamedPort name >>= extsem (n,[])
extsem state stream | semaphore state stream = done

signalIO semport | signal semport = done

waitIO   semport | wait   semport = done


-- each philosopher is a process with the following loop:
-- think / enter room / take forks / eat / release forks / leave room
philosopherIO :: Int -> Port SemMessage -> [Port SemMessage] -> IO ()
philosopherIO i room forks =
  do thinkIO i
     waitIO room
     waitIO (forks!!i)
     waitIO (forks!!((i+1) `mod` 5))
     eatIO i
     signalIO (forks!!i)
     signalIO (forks!!((i+1) `mod` 5))
     signalIO room
     philosopherIO i room forks

thinkIO i = putStr ("Philosopher "++show i++" thinks.\n") >> sleep 40000
eatIO i   = putStr ("Philosopher "++show i++" eats.\n") >> sleep 30000


sleep n = if n==0 then done else sleep (n-1)

-- initialize the room and the philosophers:
-- at most four philosophers can enter the room (guarded by semaphore "room")
-- create for the room and each fork an external semaphore
start_room =  newextsem 4 "room"
start_fork0 = newextsem 1 "fork1"
start_fork1 = newextsem 1 "fork2"
start_fork2 = newextsem 1 "fork3"
start_fork3 = newextsem 1 "fork4"
start_fork4 = newextsem 1 "fork5"

serverhost = "localhost"

start_phil i =
  do room <- connectPort ("room@"++serverhost)
     fork0 <- connectPort ("fork1@"++serverhost)
     fork1 <- connectPort ("fork2@"++serverhost)
     fork2 <- connectPort ("fork3@"++serverhost)
     fork3 <- connectPort ("fork4@"++serverhost)
     fork4 <- connectPort ("fork5@"++serverhost)
     philosopherIO i room [fork0,fork1,fork2,fork3,fork4]

{- 
Shell commands to start all semaphores:

echo ':l philo_ext
      start_room' | pakcs &
echo ':l philo_ext
      start_fork0' | pakcs &
echo ':l philo_ext
      start_fork1' | pakcs &
echo ':l philo_ext
      start_fork2' | pakcs &
echo ':l philo_ext
      start_fork3' | pakcs &
echo ':l philo_ext
      start_fork4' | pakcs &

Shell commands to start all philosophers (after starting the semaphores!):

echo ':l philo_ext
      start_phil 0' | pakcs &
echo ':l philo_ext
      start_phil 1' | pakcs &
echo ':l philo_ext
      start_phil 2' | pakcs &
echo ':l philo_ext
      start_phil 3' | pakcs &
echo ':l philo_ext
      start_phil 4' | pakcs &

-}

