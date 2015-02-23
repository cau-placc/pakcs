----------------------------------------------------------------------
--- CHR(Curry): using CHR to compute Fibonacci numbers with tabling
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Fibonacci constraints (from Duck et al, ICLP 2004)
-- Note that we use an explicit addition constraint for adding
-- the result when smaller Fibonacci numbers are computed.
data Fib = Fib Int Int
         | Add Int Int Int -- for adding results when available

fib = toGoal2 Fib
add = toGoal3 Add

fibo1 [n,f]       = fib n f <=> n .<=. 1 |> f .=. 1
fibo2 [n,f0,f]    = fib n f0 \\ fib n f <=> n .>=. 2 |> f .=. f0
fibo3 [n,f,f1,f2] = fib n f ==> n .>=. 2 |>
                               fib (n-2)  f1 /\ fib (n-1) f2 /\ add f1 f2 f
addrule [x,y,z]   = add x y z <=> ground x /\ ground y |> z .=. x+y

runFib1 = runCHR [fibo1,fibo2,fibo3,addrule]

main40 x = runFib1 $ fib 7 x  --> x=21

dup [n,f1,f2]    = fib n f1 \\ fib n f2 <=> f1 .=. f2
fib1 [n,f]       = fib n f <=> n .<=. 1 |> f .=. 1
fibn [n,f,f1,f2] = fib n f ==> n .>=. 2 |>
                               fib (n-2)  f1 /\ fib (n-1) f2 /\ add f1 f2 f

runFib2 = runCHR [dup,fib1,fibn,addrule]

main41 x = runFib2 $ fib 7 x  --> x=21

compileFib = compileCHR "FIBCHR" [fibo1,fibo2,fibo3,addrule]
-- solveCHR $ fib 20 x  where x free
