import RandomTest
import Array
import Integer
import Maybe

array = listToErrorArray 

allEntries n a = map (a!) [0 .. n]

maxEntry = lenRnds-1

rndEntries = allEntries maxEntry

test1 = eq "initialize and retrieve from array" id (rndEntries . array) 

test2 = test "update already initialized positions" upTest

upTest nums = 
  let startArray = array (map Just nums)
      rndChanges = map (abs . flip mod maxEntry) (take (div lenRnds 3) nums)
      newArray   =  startArray // zip rndChanges (repeat Nothing)
   in compare rndChanges 0 nums (rndEntries newArray)

compare _ _ [] [] = True
compare nos n (_:xs) (Nothing:ys) = elem n nos && compare nos (n+1) xs ys
compare nos n (x:xs) (Just y:ys) = x==y && compare nos (n+1) xs ys
