--- Some tests for library FlatCurryUtils.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testFlatCurryUtils"
--- 
--- @author Sebastian Fischer
--- @version December 2005

import FlatCurry
import FlatCurryGoodies
import Assertion

test = assertIO "identity transformation" identity True

identity = do
  prog <- readFlatCurry "testFlatCurryGoodies"
  return (prog == idProg prog)

idProg = trProg prog
 where
  prog name imps types funcs ops
    = Prog name imps (map idType types) (map idFunc funcs) (map idOp ops)

idType = trType typ typesyn
 where
  typ name vis params cs = Type name vis params (map idCons cs)
  typesyn name vis params syn = TypeSyn name vis params (idTypeExpr syn)

idCons = trCons cons
 where
  cons name arity vis args = Cons name arity vis (map idTypeExpr args)

idTypeExpr = trTypeExpr TVar TCons FuncType

idFunc = trFunc func
 where
  func name arity vis t rule = Func name arity vis (idTypeExpr t) (idRule rule)

idRule = trRule rule External
 where
  rule args exp = Rule args (idExpr exp)

idExpr = trExpr Var Lit Comb Let Free Or Case Branch

idOp = trOp Op


