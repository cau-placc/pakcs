-- Curry's solution to the "exchange" problem of the
-- Prolog programming contest at JICSLP'98 in Manchester

-- Prolog-like solution:
exchange l = maxList (findall \x -> value (swap l) =:= x)

-- solution using best solution search:
exchangebest l | sol s = s
 where s free
       sol = head (best (\x-> value (swap l) =:= x) (>))

-- basic value of a list of integers (alternating sum and difference):
value [] = 0
value [x] = x
value (x:y:xs) = x - y + (value xs)

-- possible swappings of the integer list:
swap [] = []
swap (x:xs) = x : swap xs
swap (x:y:xs) = (swapValue y) : (swapValue x) : swap xs

swapValue x = string2PosInt (rev (posInt2String x))

maxList [x] = x
maxList (x:y:xs) = max x (maxList (y:xs))

max x y = if x<y then y else x

rev []     = []
rev (x:xs) = (rev xs) ++ [x]

posInt2String n =
   if n<=9 then [chr(ord '0' + n)]
           else posInt2String (n `div` 10) ++ [chr(ord '0' + n `mod` 10)]

string2PosInt s = string2PosIntPrefix s 0
string2PosIntPrefix []     n = n
string2PosIntPrefix (c:cs) n =
   if oc>=ord '0' && oc<=ord '9'
      then string2PosIntPrefix cs (n*10+(ord c)-(ord '0'))
      else 0
   where oc = ord c

goal1 = exchange [1,2]
goal1b = exchangebest [1,2]
---> 1

goal2 = exchange [12,56,34]
goal2b = exchangebest [12,56,34]
--> 78
