----------------------------------------------------------------------
--- CHR(Curry): use CHR to compute the greatest common divisor
---
--- @author Michael Hanus
--- @version February 2015
----------------------------------------------------------------------

{-# OPTIONS_CYMAKE -Wno-incomplete-patterns -Wno-missing-signatures #-}

import CHR

----------------------------------------------------------------------
-- gcd constraints (from Duck et al, ICLP 2004)
-- see also http://chr.informatik.uni-ulm.de/~webchr/
data GCD = GCD Int | GCDanswer Int

gcd = toGoal1 GCD
gcdanswer = toGoal1 GCDanswer

gcda [n,x] = gcd 0 /\ gcd n /\ gcdanswer x <=> x .=. n
gcd1 []    = gcd 0 <=> true
gcd2 [m,n] = gcd n \\ gcd m <=> m .>=. n |> gcd (m-n)
-- Note that we can use functional syntax here!

runGCD = runCHR [gcd1,gcd2]

main30 = runGCD $ gcd 16 /\ gcd 28  --> gcd 4
main31 = runGCD $ gcd 206 /\ gcd 40 --> gcd 2

----------------------------------------------------------------------
compileGCD = compileCHR "GCDCHR" [gcda,gcd2]
-- solveCHR $ gcdanswer x /\ gcd 206 /\ gcd 40  where x free
