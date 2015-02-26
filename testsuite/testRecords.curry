{-# OPTIONS_CYMAKE -Wno-missing-signatures #-}

-- Testing records:

import Assertion

data Person = Person {name :: String, age :: Int}
data Address = Address {person :: Person, street :: String, city :: String}

john1 = Person {age = 30, name = "John"}

john2 = Person { name = "John", age=30 }

test1 = assertTrue "eqName" (name john1 == name john2)

test2 = assertTrue "eqRecords" (john1 == john2)


john3 = Person { name = "John", age=50 }

test3 = assertTrue "neqRecords" (john1 /= john3)

test4 = assertTrue "eqUpdRecords" (john1 {age=50} == john3)


-- Pattern matching:
isJohn :: Person -> Bool
isJohn (Person {name = n}) = n=="John"

test5 = assertTrue "isJohn1" (isJohn john1)

test6 = assertTrue "isJohn2" (not (isJohn (john1 {name="Bill"})))
