-- Database programming in Curry: family relationships
-- (non-deterministic functional style)

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

female = Christine
female = Maria
female = Monica
female = Alice
female = Susan


male = Antony
male = Bill
male = John
male = Frank
male = Peter
male = Andrew


husband :: Person -> Person
husband Christine  = Antony
husband Maria      = Bill
husband Monica     = John
husband Alice      = Frank

mother :: Person -> Person
mother John   = Christine
mother Alice  = Christine
mother Frank  = Maria
mother Susan  = Monica
mother Peter  = Monica
mother Andrew = Alice


father c = husband (mother c)

grandfather c = father (father c)
grandfather c = father (mother c)

-- ancestors of a person p:
ancestor p = father p
ancestor p = mother p
ancestor p = father (ancestor p)
ancestor p = mother (ancestor p)

-- example goals: 
goal1   = father John
goal2 c = grandfather c
goal3   = ancestor Andrew
