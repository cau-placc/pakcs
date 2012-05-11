-- Here are a few examples for list comprehensions with multiple generators:

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [ (x,y) | x<-xs, y<-ys ]

goal1 = pairs [1,2,3] [4,5]
-- Result: [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]


triangle :: Int -> [(Int,Int)]
triangle n = [ (x,y) | x <- [1..n], y <- [1..x] ]

goal2 = triangle 3
-- Result: [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3)] 


-- Pythogorean triples:
pyTriple n = [ (x,y,z) | x <- [2 .. n], y <- [x+1 .. n],
                         z <- [y+1 .. n], x*x + y*y == z*z  ]

goal3 = pyTriple 50
-- Result: [(3,4,5),(5,12,13),(6,8,10),(7,24,25),(8,15,17),(9,12,15),
--          (9,40,41),(10,24,26),(12,16,20),(12,35,37),(14,48,50),
--          (15,20,25),(15,36,39),(16,30,34),(18,24,30),(20,21,29),
--          (21,28,35),(24,32,40),(27,36,45),(30,40,50)]
