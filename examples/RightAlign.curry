-- Right alignment of strings so that all strings have the same length
-- (= length of maximal string)


-- Some letters occurring in the string (O represents a blank).
data Letter = A | B | C | D | E | F | G | H | O

maxlength :: [[a]] -> Int
maxlength sl = foldr max 0 (map length sl)

rjustify :: Int -> a -> [a] -> [a]
rjustify n b s = replicate (n-length(s)) b ++ s

-- Right alignment in two passes:
-- 1. pass: compute maximum length
-- 2. pass: right alignment
ralign2 :: a -> [[a]] -> [[a]]
ralign2 b sl = map (rjustify (maxlength sl) b) sl


-- Right alignment in one pass: use a free variable `mx` for
-- the maximum length of all strings and return its value together
-- with the right-aligned strings.
ralign :: Data a => a ->  [[a]] -> [[a]]
ralign b sl | (mx,rsl) =:= ralignMax b sl mx  = rsl  where rsl,mx free

ralignMax :: a -> [[a]] -> Int -> (Int, [[a]])
ralignMax _ []     _      = (0, [])
ralignMax b (s:sl) maxall = (max (length s) maxsl,
                             rjustify maxall b s : ralignsl)
 where
  (maxsl,ralignsl) = ralignMax b sl maxall

-- Right-align and print a file:
alignFile :: String -> IO ()
alignFile f = readFile f >>= putStrLn . unlines . ralign ' ' . lines

-- Example:

-- two-pass right alignment:
main1 :: [[Letter]]
main1 = ralign2 O [[A,B],[A,B,C,D],[A,B,C],[A,B,C,D,E]]

-- one-pass right alignment:
main2 :: [[Letter]]
main2 = ralign O [[A,B],[A,B,C,D],[A,B,C],[A,B,C,D,E]]

