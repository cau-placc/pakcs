-- Curry with arithmetic constraints:

import CLP.R

-- Circuit analysis:

data Circuit = Resistor CFloat
             | Series Circuit Circuit
             | Parallel Circuit Circuit

cvi (Resistor r) v i =  v =:= i * r

cvi (Series   c1 c2) v i =
    let v1,v2 free in v=:=v1+v2 & cvi c1 v1 i & cvi c2 v2 i

cvi (Parallel c1 c2) v i =
    let i1,i2 free in i=:=i1+i2 & cvi c1 v i1 & cvi c2 v i2


-- cvi (Series (Resistor 180.0) (Resistor 470.0)) 5.0 i where i free
-- i = 0.007692307692307693

-- cvi (Series (Series (Resistor R) (Resistor R)) (Resistor R)) V 5.0
-- {V=15.0*R}

