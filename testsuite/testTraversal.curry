--- Some tests for the Traversal library.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testTraversal"
--- 
--- @author Sebastian Fischer
--- @version February 2008
---

import Traversal
import Assertion


--- A datatype for lambda expressions.
---
data Exp = Var Name | Lam Name Exp | App Exp Exp
  deriving (Show,Eq)

type Name = String


--- Values of type Exp have children of the same type.
---
tExp :: Traversable Exp Exp
tExp exp@(Var _) = noChildren exp
tExp (Lam name exp) = ([exp], \ [exp] -> Lam name exp)
tExp (App fun arg) = ([fun,arg], \ [fun,arg] -> App fun arg)


--- Retrieve names of used variables.
---
usedVars :: Exp -> [Name]
usedVars exp = [ name | Var name <- family tExp exp ]

testUsedVars = assertEqual "usedVars" (usedVars exp) ["b","c"]
 where
  exp = Lam "a" (App (Var "b") (Var "c"))


--- Append a prefix to all variable names.
---
prefixVars :: String -> Exp -> Exp
prefixVars prefix = mapFamily tExp inc
 where inc exp = case exp of
                   Var name -> Var (prefix++name)
                   Lam name exp -> Lam (prefix++name) exp
                   _ -> exp

testPrefixVars = assertEqual "prefixVars" (prefixVars "_" exp) pexp
 where
  exp = Lam "a" (App (Var "b") (Var "c"))
  pexp = Lam "_a" (App (Var "_b") (Var "_c"))


--- Retrieve names of free variables.
---
freeVars :: Exp -> [Name]
freeVars = outOfScopeVars []

type Scope = [Name]

outOfScopeVars :: Scope -> Exp -> [Name]
outOfScopeVars scope exp =
  case exp of
    Var name -> if name `elem` scope then [] else [name]
    Lam name exp -> outOfScopeVars (name:scope) exp
    _ -> concatMap (outOfScopeVars scope) (children tExp exp)

testFreeVars = assertEqual "freeVars" (freeVars exp) ["y"]
 where
  exp = App (Lam "x" (Var "x")) (Var "y")


type Env = [(Name,Exp)]

--- Replace free variables according to environment.
---
replaceFreeVars :: Env -> Exp -> Exp
replaceFreeVars env exp = fold tExp replace exp env
 where
  replace exp cs env =
    case (exp,cs) of
      (Var name, _) -> maybe exp id (lookup name env)
      (Lam name _, [body]) -> Lam name (body (filter ((name/=).fst) env))
      _ -> replaceChildren tExp exp (map ($env) cs)

testReplaceFreeVars =
  assertEqual "replaceFreeVars" (replaceFreeVars env exp) rexp
 where
  env = [("x", Var "z"), ("y", Var "z")]
  exp = App (Lam "x" (Var "x")) (Var "y")
  rexp = App (Lam "x" (Var "x")) (Var "z")


