-- best solution search:
 
shorter l1 l2 = length l1 <= length l2
 
first g = head (findall g)


g1 s = (head (best (\x -> let y free in x ++ y =:= [1,2,3]) shorter)) s


g2 s = (head (best (\x -> let y free in x ++ y =:= [1,2,3])
                   (\l1 l2 -> length l1 > length l2)       )) s

