digit :: Int -> Success

digit 0 = success
digit 1 = success
digit 2 = success
digit 3 = success
digit 4 = success
digit 5 = success
digit 6 = success
digit 7 = success
digit 8 = success
digit 9 = success

-- goals: arithmetic functions as passive constraints:
goal x y =  x+x=:=y & x*x=:=y & digit x
