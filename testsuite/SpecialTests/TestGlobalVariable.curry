-- Examples for testing library GlobalVariable

import GlobalVariable
import Test.EasyCheck

g :: GVar Int
g = gvar 42

m1 = do
  writeGVar g 42
  v1 <- readGVar g
  writeGVar g 99
  v2 <- readGVar g
  return (v1,v2)

h :: GVar (Maybe Int)
h = gvar Nothing

m2 = do
  writeGVar g 42
  let x free
  writeGVar h (Just x)
  v1 <- readGVar g
  v2 <- readGVar h
  doSolve (x=:=99)
  v3 <- readGVar h
  return (v1,v2,v3)

testGlobalVariableTest1 = m1 `returns` (42,99)

testGlobalVariableTest2 = m2 `returns` (42,Just 99, Just 99)
