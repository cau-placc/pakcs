------------------------------------------------------------------------------
--- A collection of common operations on integer numbers.
--- Most operations make no assumption on the precision of integers.
--- Operation <EM>bitNot</EM> is necessarily an exception.
---
--- @author Sergio Antoy
--- @version March 20, 2006
------------------------------------------------------------------------------

module Integer(pow, ilog, isqrt, factorial, binomial,
               abs, max3, min3, maxlist, minlist,
               bitTrunc, bitAnd, bitOr, bitNot, bitXor,
               even, odd) where

------------------------------------------------------------------
--                       Public Operations
------------------------------------------------------------------

--- The value of <EM>pow a b</EM> is <EM>a</EM>
--- raised to the power of <EM>b</EM>.
--- Fails if <EM>b &lt; 0</EM>.
--- Executes in <EM>O(log b)</EM> steps.
---
--- @param a - The base.
--- @param b - The exponent.
--- @return <EM>a</EM> raised to the power of <EM>b</EM>.

pow a b | b>= 0 = powaux 1 a b
  where
    powaux n x y = if y == 0 then n
                   else powaux (n * if (y `mod` 2 == 1) then x else 1)
                               (x * x)
                               (y `div` 2)

--- The value of <EM>ilog n</EM> is the floor of the logarithm
--- in the base 10 of <EM>n</EM>.
--- Fails if <EM>n &lt;= 0</EM>.
--- For positive integers, the returned value is
--- 1 less the number of digits in the decimal representation of <EM>n</EM>.
---
--- @param n - The argument.
--- @return the floor of the logarithm in the base 10 of <EM>n</EM>.

ilog n | n>0 = if n<10 then 0 else 1 + ilog (n `div` 10)

--- The value of <EM>isqrt n</EM> is the floor
--- of the square root of <EM>n</EM>.
--- Fails if <EM>n &lt; 0</EM>.
--- Executes in <EM>O(log n)</EM> steps, but there must be a better way.
---
--- @param n - The argument.
--- @return the floor of the square root of <EM>n</EM>.

isqrt n | n >= 0 =
  if n == 0 then 0 else
  if n <  4 then 1 else
  aux 2 n
  where aux low past = -- invariant low <= result < past
          if past == low+1 then low
          else let cand = (past + low) `div` 2
                in if cand*cand > n then aux low cand else aux cand past

--- The value of <EM>factorial n</EM> is the factorial of <EM>n</EM>.
--- Fails if <EM>n &lt; 0</EM>.
---
--- @param n - The argument.
--- @return the factorial of <EM>n</EM>.

factorial n | n >= 0 = if n == 0 then 1 else n * factorial (n-1)

--- The value of <EM>binomial n m</EM> is 
--- n*(n-1)*...*(n-m+1)/m*(m-1)*...1
--- Fails if <EM>m &lt;= 0</EM> or <EM>n &lt; m</EM>.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the binomial coefficient of <EM>n</EM> over <EM>m</EM>.

binomial n m | m > 0 && n >= m = aux m n `div` factorial m
  where aux x y = if x == 0 then 1 else y * aux (x-1) (y-1)

--- The value of <EM>abs n</EM> is the absolute value of <EM>n</EM>.
---
--- @param n - The argument.
--- @return the absolute value of <EM>n</EM>.

abs n = if n<0 then -n else n

--- Returns the maximum of the three arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @param p - Argument.
--- @return the maximum among <EM>n</EM>, <EM>m</EM> and <EM>p</EM>.

max3 n m p = max n (max m p)

--- Returns the minimum of the three arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @param p - Argument.
--- @return the minimum among <EM>n</EM>, <EM>m</EM> and <EM>p</EM>.

min3 n m p = min n (min m p)

--- Returns the maximum of a list of integer values.
--- Fails if the list is empty.
---
--- @param l - The list of values.
--- @return the maximum element of <EM>l</EM>.

maxlist [n] = n
maxlist (n:m:ns) = max n (maxlist (m:ns))

--- Returns the minimum of a list of integer values.
--- Fails if the list is empty.
---
--- @param l - The list of values.
--- @return the minimum element of <EM>l</EM>.

minlist [n] = n
minlist (n:m:ns) = min n  (minlist (m:ns))

--- The value of <EM>bitTrunc n m</EM> is the value of the <EM>n</EM>
--- least significant bits of <EM>m</EM>.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return <EM>m</EM> truncated to the <EM>n</EM> least significant bits.

bitTrunc n m = bitAnd (pow 2 n - 1) m

--- Returns the bitwise AND of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise and of <EM>n</EM> and <EM>m</EM>.

bitAnd n m = if m == 0 then 0
             else let p = 2 * bitAnd (n `div` 2) (m `div` 2)
                      q = if m `mod` 2 == 0 then 0 else n `mod` 2
                   in p + q

--- Returns the bitwise inclusive OR of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise inclusive or of <EM>n</EM> and <EM>m</EM>.

bitOr n m = if m == 0 then n
            else let p = 2 * bitOr (n `div` 2) (m `div` 2)
                     q = if m `mod` 2 == 1 then 1 else n `mod` 2
                  in p + q

--- Returns the bitwise NOT of the argument.
--- Since integers have unlimited precision,
--- only the 32 least significant bits are computed.
---
--- @param n - Argument.
--- @return the bitwise negation of <EM>n</EM> truncated to 32 bits.

bitNot n = aux 32 n
  where aux c m = if c==0 then 0
                  else let p = 2 * aux (c-1) (m `div` 2)
                           q = 1 - m `mod` 2
                        in p + q

--- Returns the bitwise exclusive OR of the two arguments.
---
--- @param n - Argument.
--- @param m - Argument.
--- @return the bitwise exclusive of <EM>n</EM> and <EM>m</EM>.

bitXor n m = if m == 0 then n
             else let p = 2 * bitXor (n `div` 2) (m `div` 2)
                      q = if m `mod` 2 == n `mod` 2 then 0 else 1
                   in p + q

--- Returns whether an integer is even
---
--- @param n - Argument.
--- @return whether <EM>n</EM> is even.

even n = n `mod` 2 == 0

--- Returns whether an integer is odd
---
--- @param n - Argument.
--- @return whether <EM>n</EM> is odd.

odd n = n `mod` 2 /= 0
