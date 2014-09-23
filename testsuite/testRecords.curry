-- Testing records:

import Assertion

type Person = {name :: String, age :: Int}
type Address = {person :: Person, street :: String, city :: String}

getName :: Person -> String
getName p = p :> name

john1 = {age := 30, name := "John"}
john2 = { name := "John", age:=30 }

test1 = assertTrue "eqName" (getName john1 == getName john2)

test2 = assertTrue "eqRecords" (john1 == john2)


john3 = { name := "John", age:=50 }

test3 = assertTrue "neqRecords" (john1 /= john3)

test4 = assertTrue "eqUpdRecords" ({age:=50 | john1 } == john3)


-- Pattern matching:
isJohn :: Person -> Bool
isJohn {name = n | _} = n=="John"

test5 = assertTrue "isJohn1" (isJohn john1)

test6 = assertTrue "isJohn2" (not (isJohn {name:="Bill" | john1 }))
