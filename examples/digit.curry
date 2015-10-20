digit :: Int -> Bool
digit 0 = True
digit 1 = True
digit 2 = True
digit 3 = True
digit 4 = True
digit 5 = True
digit 6 = True
digit 7 = True
digit 8 = True
digit 9 = True

-- goals: arithmetic functions as passive constraints:
goal x y =  x+x=:=y & x*x=:=y & digit x
