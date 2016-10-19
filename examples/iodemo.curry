-- Some demos of monadic I/O:

import Findall

-- a simple echo:

echo = getChar >>= \c -> if ord c == (-1) then done else putChar c >> echo


-- a simple dialog:
dialog = putStrLn "Ihr Name?" >> getLine >>= processName
  where processName str = putStr "Hallo " >>
                          putStr str >>
                          putStr ", rueckwaerts lautet Ihr Name: " >>
                          putStrLn (rev str)


rev []     = []
rev (x:xs) = rev xs++[x]


-- a call to "nondet" is not allowed since it would duplicate the world:
nondet :: IO ()
nondet | generate x  = putChar x  where x free

generate 'a' = True
generate 'b' = True

-- ...but is allowed if the non-determinism is encapsulated:
det | findfirst (\x -> generate x) =:= y  = putChar y  where y free


-- copy a file:
copyFile :: String -> String -> IO ()
copyFile from to = readFile from >>= writeFile to



-- reading and writing integers:

-- write an integer:
putInt n = if n<0 then putChar '-' >> putInt (-n) else 
           if n<=9 then putChar(chr(ord '0' + n))
                   else putInt (n `div` 10) >>
                        putChar(chr(ord '0' + n `mod` 10))

-- parse an integer:
parseInt l = parseIntPrefix (dropWhile (==' ') l) 0
 where
   parseIntPrefix []     n = n
   parseIntPrefix (c:cs) n =
    let oc = ord c
     in if c==' ' then n else
        if c=='-' then - parseIntPrefix cs n else
           if oc>=ord '0' && oc<=ord '9'
                 then parseIntPrefix cs (n*10+(oc)-(ord '0'))
                 else 0

-- read an integer:
getInt = getLine >>= \s -> return (parseInt s)

-- factorial function:
fac n | n==0      = 1
      | otherwise = fac(n-1)*n

-- an interactive factorial computation:
facint =
  putStr "Factorial computation: Type in a small natural number: " >>
  getInt >>= \n ->
  putStr "The factorial of  " >> putInt n >>
  putStr " is " >> putInt (fac n) >> putChar '\n'

