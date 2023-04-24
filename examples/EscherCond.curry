-- Module "conditional" from the Escher report:

data Alpha = A | B | C | D
 deriving Eq

membercheck :: (Alpha , [Alpha]) -> Bool
membercheck(_,[])  = False
membercheck(x,y:z) = if x==y then True else membercheck(x,z)


-- goals:
goal1 = membercheck(B,[A,B])               -->  True
goal2 = membercheck(C,[A,B])               -->  False
goal3 = membercheck(x,[A,B]) where x free  -->  suspend
