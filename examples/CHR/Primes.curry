----------------------------------------------------------------------
--- Various examples for CHR rules and goals.
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- Prime numbers
-- http://chr.informatik.uni-ulm.de/~webchr/cgi-bin/program.cgi?load=functions/primes1.pl

data Prime = Prime Int

prime = toGoal1 Prime

primeFail [n]   = prime n <=> n .<=. 1 |> fail
primeGen  [n]   = prime n ==> n .>=. 3 |> prime (n-1)
primeSift [x,y] = prime x \\ prime y <=> y `mod` x .=. 0 |> true

runPrime = runCHR [primeFail,primeGen,primeSift]

main70 = runPrime $ prime 20

