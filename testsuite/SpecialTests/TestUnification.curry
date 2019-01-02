-- Testing unification of free variables:

import Test.Prop

-- simple types
test0 = (True =:= True) <~> True
test1 = failing (True =:= False)
test2 = (let x free in x =:= True) <~> True
test3 = (let x free in x =:= True &> x) <~> True
test4 = (let x free in x =:= True &> [x,x]) <~> [True,True]
test5 = (let x free in [x =:= True &> x,x]) <~> [True,True]
test6 = (let x free in [x,x =:= True &> x]) <~> [True,True]

uni :: Bool -> Bool -> Bool
uni x y = x =:= y

test7 = (let x,y free in uni x y) <~> True
test8 = (let x,y free in
         x =:= y    
      &> y=:= True
      &> [x,y]) <~> [True,True]

test9 = (let x,y free in
         x =:= y  
      &> x=:= True
      &> [x,y]) <~> [True,True]

test10 = (let x,y,z free in
          x =:= y
       &> x =:= z
       &> y=:=False &> [x,y]) <~> [False,False]

test11 = (let x,y,z free in
          x =:= y     
       &> x =:= z
       &> x=:=False
       &> [z,x,y]) <~> [False,False,False]

test12 = (let x,y,z free in
          x =:= y     
       &> x =:= z
       &> z=:=False &> [x,z,y]) <~> [False,False,False]

test13 = (let x,y,z free in
          x =:= y      
       &> x =:= z
       &> z=:=False
       &> y=:=False
       &> [x,y,z]) <~> [False,False,False]

test14 = (let x,y free in x=:=y &> y=:=False &> x) <~> False
test15 = (let x,y free in x=:=(y?True) &> y=:=False &> x) <~> (False ? True)

-- complex types

test16 =  (let x free in x=:=[] &> True:x) <~> [True]
test17 =  (let x free in x=:=[True] &> x) <~> [True]

test18 =  (let x,y free in x=:=y &> (y=:= [True] &> x)) <~> [True]

f [False] = True
test19 =  failing (let x,y free in x=:=y &> y=:= [True] &> f x &> x)

g [True] = True
test20 =  (let x,y free in x=:=y &> y=:= [True] &> g x &> x) <~> [True]


test21 =  (let x,y free in x=:=(y?[False]) &> y=:=[True] &> x) <~> ([True] ? [False])

uni2 :: [Bool] -> [Bool] -> Bool
uni2 x y = x =:= y

test22 =  (let x,y free in uni2 x [y]) <~> True
--test23 =  (let x,y free in uni2 x [y] &> x)  <~> y -- TODO: how to test this?
test24 =  (let x,y free in x =:= [y] &> y=:=True &> x) <~> [True]
--test25 =  (let x,y,z,z1,z2 free in x =:= (y:z) &> x=:=(False:z1:z2) &> z2 =:=[] &> x) <~> [False,z1]
--  TODO: how to test this?


test26 = (let x,y free in x =:= [y?True] &> y=:=False &> x)
         <~> ([False] ? [True])

test27 = (let x,y,z free in [x,True,z]=:=[False,y,y] &> [x,y,z])
         <~> [False,True,True]

test28 = (let x,y free in x =:= (y =:= [True] &> y) &> x) <~> [True]

test29 = (let x,y free in x =:= (True:(y =:= [] &> y)) &> x) <~> [True]

test30 = failing (let x,y free in x =:= [True] &> y =:= [False] &> x =:= y)

test31 = failing (let x,y free in x =:= [] &> y ++ [False] =:= x)
test32 = failing (let x,y1,y2 free in x =:= [] &> y1:(y2 ++ [False]) =:= x)
test33 = failing (let x,y2 free in x =:= [] &> (y2 ++ [False]) =:= x)
test34 = failing (let x,y1 free in x =:= [] &> y1:[False] =:= x)
-- This call should fail due to occure check
-- An occure check is not yet implemented
-- test35 = failing (let ones free in ones =:= 1:ones &> head ones)