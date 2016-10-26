{-# OPTIONS_CYMAKE -Wno-missing-signatures #-}

-- Testing records:

import Test.EasyCheck

data Person = Person {name :: String, age :: Int}
 deriving Eq

data Address = Address {person :: Person, street :: String, city :: String}

john1 = Person {age = 30, name = "John"}

john2 = Person { name = "John", age=30 }

testEqName = always (name john1 == name john2)

testEqRecords = always (john1 == john2)


john3 = Person { name = "John", age=50 }

testNeqRecords = always (john1 /= john3)

testEqUpdRecords = always (john1 {age=50} == john3)


-- Pattern matching:
isJohn :: Person -> Bool
isJohn (Person {name = n}) = n=="John"

testIsJohn1 = always (isJohn john1)

testIsJohn2 = always (not (isJohn (john1 {name="Bill"})))
