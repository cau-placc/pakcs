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

grandfather g c | g == father (father c) = True
grandfather g c | g == father (mother c) = True

-- A is ancestor of P:
ancestor a p | a == father p = True
ancestor a p | a == mother p = True
ancestor a p | a == father p1 && ancestor p1 p = True where p1 free
ancestor a p | a == mother p1 && ancestor p1 p = True where p1 free


-- Example goals:
goal1       = father Peter
goal2 child = solve $ father child == John
goal3 g c   = grandfather g c
goal4 a     = ancestor a Andrew
goal5 a p   = ancestor a p

