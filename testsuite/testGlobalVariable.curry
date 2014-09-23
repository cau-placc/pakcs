-- Examples for testing library GlobalVariable

import Assertion
import GlobalVariable

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

test1 = assertIO "GVar test1" m1 (42,99)

test2 = assertIO "GVar test2" m2 (42,Just 99, Just 99)
