-- Database programming in Curry: family relationships (constraint style)

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

female Christine  = success
female Maria      = success
female Monica     = success
female Alice      = success
female Susan      = success


male Antony  = success
male Bill    = success
male John    = success
male Frank   = success
male Peter   = success
male Andrew  = success


married Christine Antony  = success
married Maria Bill        = success
married Monica John       = success
married Alice Frank       = success


mother Christine John   = success
mother Christine Alice  = success
mother Maria Frank      = success
mother Monica Susan     = success
mother Monica Peter     = success
mother Alice Andrew     = success


father f c = let m free in married m f & mother m c


grandfather g c = let f free in father g f & father f c
grandfather g c = let m free in father g m & mother m c


-- goals: 
goal1 child = father John child
goal2 g c   = grandfather g c

