-- The "dining philosophers" example in Curry
-- (using semaphores implemented via ports)

import Ports
import sema
import Unsafe(trace)

-- initialize the room and the philosophers:
-- at most four philosophers can enter the room (guarded by semaphore "room")
dining =
  newsem 4 room  &
  newsem 1 fork0 &
  newsem 1 fork1 &
  newsem 1 fork2 &
  newsem 1 fork3 &
  newsem 1 fork4 &
  philosopher 0 room [fork0,fork1,fork2,fork3,fork4] &
  philosopher 1 room [fork0,fork1,fork2,fork3,fork4] &
  philosopher 2 room [fork0,fork1,fork2,fork3,fork4] &
  philosopher 3 room [fork0,fork1,fork2,fork3,fork4] &
  philosopher 4 room [fork0,fork1,fork2,fork3,fork4]
 where room,fork0,fork1,fork2,fork3,fork4 free


-- each philosopher is a process with the following loop:
-- think / enter room / take forks / eat / release forks / leave room
philosopher :: Int -> Port SemMessage -> [Port SemMessage] -> Success
philosopher i room forks =
  think i &>
  wait room &>
  wait (forks!!i) &>
  wait (forks!!((i+1) `mod` 5)) &>
  eat i &>
  signal (forks!!i) &>
  signal (forks!!((i+1) `mod` 5)) &>
  signal room &>
  philosopher i room forks

think i = trace ("Philosopher "++show i++" thinks.") success
eat i   = trace ("Philosopher "++show i++" eats.") success

