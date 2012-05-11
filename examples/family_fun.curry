-- Database programming in Curry: family relationships
-- (functional logic style with explicit functional dependencies)

{- Structure of the family:

                    Christine --- Antony  Maria --- Bill
                      /    \              |
                     /      \             |
       Monica --- John       Alice --- Frank
        /  \                   |
       /    \                  |
    Susan  Peter             Andrew
-}

data Person = Christine | Maria | Monica | Alice | Susan |
              Antony | Bill | John | Frank | Peter | Andrew

husband Christine  = Antony
husband Maria      = Bill
husband Monica     = John
husband Alice      = Frank


mother John    = Christine
mother Alice   = Christine
mother Frank   = Maria
mother Susan   = Monica
mother Peter   = Monica
mother Andrew  = Alice

father c  = husband (mother c)

grandfather g c = g =:= father (father c)
grandfather g c = g =:= father (mother c)

-- A is ancestor of P:
ancestor a p = a =:= father p
ancestor a p = a =:= mother p
ancestor a p = let p1 free in a =:= father p1 & ancestor p1 p
ancestor a p = let p1 free in a =:= mother p1 & ancestor p1 p


-- Example goals:
goal1       = father Peter
goal2 child = father child =:= John
goal3 g c   = grandfather g c
goal4 a     = ancestor a Andrew
goal5 a p   = ancestor a p

