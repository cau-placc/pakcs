-- right alignment of strings so that all strings have the same length
-- (= length of maximal string)


max x y = if x>y then x else y

data Letter = A|B|C|D|E|F|G|H|O


maxlength sl = foldr max 0 (map length sl)


rjustify n s = replicate (n-length(s)) O ++ s


-- right alignment in two passes:
-- 1. pass: compute maximum length
-- 2. pass: right alignment

ralign2 sl = map (rjustify (maxlength sl)) sl


-- right alignment in one pass:

ralign sl | (rsl,mx) =:= pass_ralign sl mx  = rsl  where rsl,mx free

pass_ralign []     _      = ([],0)
pass_ralign (s:sl) maxall = (rjustify maxall s:ralignsl, max (length s) maxsl)
    where (ralignsl,maxsl) = pass_ralign sl maxall


-- goals:
-- two-pass right alignment:
goal1 = ralign2 [[A,B],[A,B,C,D],[A,B,C],[A,B,C,D,E]]
-- one-pass right alignment:
goal2 = ralign [[A,B],[A,B,C,D],[A,B,C],[A,B,C,D,E]]

-- ...and two larger goals to see a significant time difference:
-- ralign2([[a,b],[a,b,c,d],[a,b,c],[a,b,c,d,e],[a,b],[a,b,c,d],[a,b,c],[a,b,c,d,e],[a,b]])
-- ralign([[a,b],[a,b,c,d],[a,b,c],[a,b,c,d,e],[a,b],[a,b,c,d],[a,b,c],[a,b,c,d,e],[a,b]])
