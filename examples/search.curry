-- a simple example for encapsulated search:
import Findall(allSolutions)

append []     ys = ys
append (x:xs) ys = x : append xs ys

-- compute all solutions to equation  append [True] l == [True,False]:
g1 = allSolutions (\l -> append [True] l == [True,False])

-- compute all solutions for l2 to equation  append l1 l2 == [True,False]
-- where l1 is arbitrary:
g2 = allSolutions (\l2 -> let l1 free in append l1 l2 == [True,False])

-- compute the list of all splittings of the list [True,False]:
g3 = allSolutions (\(l1,l2) -> append l1 l2 == [True,False])

