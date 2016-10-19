-- Simulation of the Prolog shell with different search strategies:

-- print a list of solved goals:
printloop []     = putStrLn "no"
printloop (a:as) = browse a >>
                   putStr " ? " >>
                   getLine >>=
                   evalAnswer as
   where
     evalAnswer as ln =
        if ln==";"
        then putChar '\n' >> printloop as
        else if ln==""
             then putChar '\n' >> putStrLn "yes"
             else putStrLn "(\";\" for more choices, otherwise <return>): " >>
                  getLine >>= evalAnswer as


-- Standard Prolog with depth-first search:
prolog g = printloop (solveAll g)

-- Prolog with breadth-first search:
prolog_bfs g = printloop (bfs g)

-- Prolog with depth-bounded search:
prolog_depth n g = printloop (allbounded n g)

-- Prolog with iterative deepening search:
prolog_id n g = printloop (all_id n g)


append []     ys = ys
append (x:xs) ys = x : append xs ys

goal = prolog \(l1,l2) -> append l1 l2 =:= [0,1]



-- breadth-first search:
bfs g = trygoals [g]
  where
    trygoals [] = [] 
    trygoals (g:gs) = splitgoals (map try (g:gs)) []

    splitgoals []               ugs = trygoals ugs
    splitgoals ([]:gs)          ugs = splitgoals gs ugs
    splitgoals ([g]:gs)         ugs = g:(splitgoals gs ugs)
    splitgoals ((g1:g2:g3s):gs) ugs = splitgoals gs (ugs++g1:g2:g3s)


-- depth-first search with depth bound:
allbounded n g = if n>0 then evalall (try g) else []
      where
        evalall []         = []
        evalall [g]        = [g]
        evalall (g1:g2:gs) = concat (map (allbounded (n-1)) (g1:g2:gs))


-- iterative deepening search:
all_id n g  = depthLoop n n (toDepthN n n g) False
  where
    depthLoop n m [] True          = depthLoop (n+m) m
                                               (toDepthN (n+m) m g) False
    depthLoop _ _ [] False         = []
    depthLoop n m ([]:gs) recomp   = depthLoop n m gs recomp
    depthLoop n m ([g1]:gs) recomp = g1:(depthLoop n m gs recomp)
    depthLoop n m ((_:_:_):gs) _   = depthLoop n m gs True

    toDepthN n m g  = collect (try g)
      where
        collect []   = [[]]
        collect [g]  = if n>m then [[]]
                              else [[g]]
        collect(g1:g2:gs) = if n==1
                            then [g1:g2:gs]
                            else concat (map (toDepthN (n-1) m) (g1:g2:gs))


-- Examples:

p (_:xs) = p xs
p []     = True

g1 = prolog          \x -> p x

g2 = prolog_bfs      \x -> p x

g3 = prolog_depth 10 \x -> p x

g4 = prolog_id 5     \x -> p x


