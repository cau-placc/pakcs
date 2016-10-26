----------------------------------------------------------------------
--- CHR(Curry): use CHR to compute prime numbers
---
--- Advantage compared to CHR(Prolog): natural functional notation
---
--- @author Michael Hanus
--- @version October 2016
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Prime numbers
-- http://chr.informatik.uni-ulm.de/~webchr/cgi-bin/program.cgi?load=functions/primes1.pl

data Prime = Prime Int

prime = toGoal1 Prime

primeFail [n]   = prime n <=> n .<=. 1 |> false
primeGen  [n]   = prime n ==> n .>=. 3 |> prime (n-1)
primeSift [x,y] = prime x \\ prime y <=> y `mod` x .=. 0 |> true

runPrime = runCHR [primeFail,primeGen,primeSift]

main70 = runPrime $ prime 20

