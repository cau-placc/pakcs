
goal1 xs = sort intMerge [3,1,2] xs
goal2 xs = sort intMerge [3,1,2,5,4,8] xs
goal3 xs = sort intMerge [3,1,2,5,4,8,6,7,2,9,1,4,3] xs


sort :: Data a => ([a] -> [a] -> [a] -> Bool) -> [a] -> [a] -> Bool
sort merge xs ys =
   if length xs < 2 then ys =:= xs
   else sort merge (firsthalf xs) us
        & sort merge (secondhalf xs) vs
        & merge us vs ys
   where us,vs free


intMerge :: [Int] -> [Int] -> [Int] -> Bool

intMerge [] ys zs =  zs =:= ys
intMerge xs [] zs =  zs =:= xs
intMerge (x:xs) (y:ys) zs =
   if (x > y) then intMerge (x:xs) ys us & zs =:= y:us
              else intMerge xs (y:ys) vs & zs =:= x:vs
   where us,vs free

  
firsthalf  xs = take (length xs `div` 2) xs
secondhalf xs = drop (length xs `div` 2) xs

