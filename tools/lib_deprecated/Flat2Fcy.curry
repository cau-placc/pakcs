------------------------------------------------------------------------------
--- Translation of old FlatCurry format into new one.

--- @author Michael Hanus
--- @version August 2005
------------------------------------------------------------------------------

module Flat2Fcy(writeFCY,flc2fcy) where

import Flat
import qualified FlatCurry as FCY
import ReadShowTerm

--- Writes a FlatCurry program (old format) into a file in .fcy format.
writeFCY :: String -> Prog -> IO ()
writeFCY file prog = writeFile file (showTerm (flc2fcy prog))

--- Converts old FlatCurry format into new one.
flc2fcy :: Prog -> FCY.Prog
flc2fcy (Prog modname imports types funcs ops transtable) =
  FCY.Prog modname imports
           (map (flc2fcyType pubnames) types)
           (map (flc2fcyFunc pubnames) funcs)
           (map flc2fcyOp ops)
  where pubnames = map (\(Trans _ n)->n) transtable

flc2fcyVis pubnames name =
  if name `elem` pubnames then FCY.Public else FCY.Private

flc2fcyOp (Op name fix prec) =
  FCY.Op (splitFlatModName name) (flc2fcyFixity fix) prec

flc2fcyFixity InfixOp   = FCY.InfixOp
flc2fcyFixity InfixlOp  = FCY.InfixlOp
flc2fcyFixity InfixrOp  = FCY.InfixrOp

flc2fcyType pubnames (Type name tpars consdecls) =
  FCY.Type (splitFlatModName name) (flc2fcyVis pubnames name) tpars
           (map flc2fcyCons consdecls)
 where
   flc2fcyCons (Cons cname arity types) =
      FCY.Cons (splitFlatModName cname) arity (flc2fcyVis pubnames cname)
               (map flc2fcyTExp types)

flc2fcyTExp (FuncType t1 t2) =
  FCY.FuncType (flc2fcyTExp t1) (flc2fcyTExp t2)
flc2fcyTExp (TCons tc ts) =
  FCY.TCons (splitFlatModName tc) (map flc2fcyTExp ts)
flc2fcyTExp (TVar n) = FCY.TVar n


flc2fcyFunc pubnames (Func name arity ftype rl) =
  FCY.Func (splitFlatModName name) arity
           (flc2fcyVis pubnames name)
           (flc2fcyTExp ftype)
           (flc2fcyRule rl)

flc2fcyRule (Rule params expr) = FCY.Rule params (flc2fcyExpr expr)
flc2fcyRule (External name) = FCY.External name

flc2fcyCombType FuncCall = FCY.FuncCall
flc2fcyCombType ConsCall = FCY.ConsCall
flc2fcyCombType PartCall = FCY.FuncPartCall 999 -- to improve

flc2fcyExpr (Var n) = FCY.Var n
flc2fcyExpr (Lit l) = FCY.Lit (flc2fcyLit l)
flc2fcyExpr (Comb ctype cf es) =
  FCY.Comb (flc2fcyCombType ctype) (splitFlatModName cf) (map flc2fcyExpr es)
flc2fcyExpr (Apply e1 e2) =
  FCY.Comb FCY.FuncCall ("Prelude","apply") [flc2fcyExpr e1, flc2fcyExpr e2]
flc2fcyExpr (Constr xs e) = FCY.Free xs (flc2fcyExpr e)
flc2fcyExpr (Or e1 e2) = FCY.Or (flc2fcyExpr e1) (flc2fcyExpr e2)
flc2fcyExpr (Case Rigid e bs) =
 FCY.Case FCY.Rigid (flc2fcyExpr e) (map flc2fcyBranch bs)
flc2fcyExpr (Case Flex e bs) =
 FCY.Case FCY.Flex (flc2fcyExpr e) (map flc2fcyBranch bs)
flc2fcyExpr (GuardedExpr xs e1 e2) =
  FCY.Free xs
    (FCY.Comb FCY.FuncCall ("Prelude","cond") [flc2fcyExpr e1, flc2fcyExpr e2])
flc2fcyExpr (Let bindings exp) =
  FCY.Let (map flc2fcyBinding bindings) (flc2fcyExpr exp)
flc2fcyExpr (Choice e) =
  FCY.Comb FCY.FuncCall ("Prelude","choice") [flc2fcyExpr e]

flc2fcyBinding (n,exp) = (n,flc2fcyExpr exp)

flc2fcyLit (Intc   i) = FCY.Intc i
flc2fcyLit (Floatc f) = FCY.Floatc f
flc2fcyLit (Charc  c) = FCY.Charc c

flc2fcyBranch (Branch (Pattern s xs) e) =
  FCY.Branch (FCY.Pattern (splitFlatModName s) xs) (flc2fcyExpr e)
flc2fcyBranch (Branch (LPattern l) e) =
  FCY.Branch (FCY.LPattern (flc2fcyLit l)) (flc2fcyExpr e)

