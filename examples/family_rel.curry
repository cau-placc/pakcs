-- Database programming in Curry: family relationships (relational style)

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

female Christine  = True
female Maria      = True
female Monica     = True
female Alice      = True
female Susan      = True


male Antony  = True
male Bill    = True
male John    = True
male Frank   = True
male Peter   = True
male Andrew  = True


married Christine Antony  = True
married Maria Bill        = True
married Monica John       = True
married Alice Frank       = True


mother Christine John   = True
mother Christine Alice  = True
mother Maria Frank      = True
mother Monica Susan     = True
mother Monica Peter     = True
mother Alice Andrew     = True


father f c | let m free in (married m f && mother m c) =:= True  = True


grandfather g c | let f free in (father g f && father f c) =:= True  = True
grandfather g c | let m free in (father g m && mother m c) =:= True  = True


-- goals: 
goal1 child = father John child
goal2 g c   = grandfather g c

