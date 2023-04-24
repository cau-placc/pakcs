-- Concatenating two lists:
-- (predefined as `++' in the standard prelude)
append :: [a] -> [a] -> [a]
append []     x  = x
append (x:xs) ys = x : append xs ys


-- Reverse the order of elements in a list:
rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

-- Some main expressions for testing:

main1 :: [Int]
main1 = append [1,2] [3,4]

main2 :: [Int]
main2 = rev [1,2,3,4]

main3 :: [Int]
main3 = rev [1,2,3,4,5,6,7,8,9,10]

-- end of program
