------------------------------------------------------------------------------
--- Translation of old FlatCurry format into new one.

--- @author Michael Hanus
--- @version October 2008
------------------------------------------------------------------------------

module Flat2Fcy(writeFCY,flc2fcy) where

import Flat
import qualified FlatCurry.Types as FCY
import qualified FlatCurry.Files
import ReadShowTerm
import Unsafe(trace)

--- Writes a FlatCurry program (old format) into a file in .fcy format.
writeFCY :: String -> Prog -> IO ()
writeFCY file prog = do
  fcyprog <- flc2fcy prog
  writeFile file (showTerm fcyprog)

--- Converts old FlatCurry format into new one.
flc2fcy :: Prog -> IO FCY.Prog
flc2fcy (Prog modname imports types funcs ops transtable) = do
  importarities <- mapIO readModArities imports
  return $
    FCY.Prog modname imports
             (map (flc2fcyType pubnames) types)
             (map (flc2fcyFunc pubnames (farities ++ concat importarities))
                  funcs)
             (map flc2fcyOp ops)
 where pubnames = map (\(Trans _ n)->n) transtable
       farities = map (\(Func name ar _ _) -> (splitFlatModName name,ar))
                      funcs

--- Read function arities of a module:
readModArities modname = do
  (FCY.Prog _ _ _ funcs _) <- FlatCurry.Files.readFlatCurryInt modname
  return $ map (\(FCY.Func name ar _ _ _) -> (name,ar)) funcs
  
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


flc2fcyFunc pubnames fars (Func name arity ftype rl) =
  FCY.Func (splitFlatModName name) arity
           (flc2fcyVis pubnames name)
           (flc2fcyTExp ftype)
           (flc2fcyRule fars rl)

flc2fcyRule fars (Rule params expr) = FCY.Rule params (flc2fcyExpr fars expr)
flc2fcyRule _    (External name) = FCY.External name

flc2fcyCombType _ _ _ FuncCall = FCY.FuncCall
flc2fcyCombType _ _ _ ConsCall = FCY.ConsCall
flc2fcyCombType fars fname numargs PartCall =
  let arity = maybe (trace ("Warning: cannot determine arity of " ++ fst fname
                            ++ '.' : snd fname ++ "\n") 999)
                    id (lookup fname fars) -- to improve 999 case!
   in FCY.FuncPartCall (arity - numargs)

flc2fcyExpr _ (Var n) = FCY.Var n
flc2fcyExpr _ (Lit l) = FCY.Lit (flc2fcyLit l)
flc2fcyExpr fars (Comb ctype cf es) = let qname = splitFlatModName cf in
  FCY.Comb (flc2fcyCombType fars qname (length es) ctype)
           qname
           (map (flc2fcyExpr fars) es)
flc2fcyExpr fars (Apply e1 e2) =
  FCY.Comb FCY.FuncCall ("Prelude","apply") [flc2fcyExpr fars e1, flc2fcyExpr fars e2]
flc2fcyExpr fars (Constr xs e) = FCY.Free xs (flc2fcyExpr fars e)
flc2fcyExpr fars (Or e1 e2) = FCY.Or (flc2fcyExpr fars e1) (flc2fcyExpr fars e2)
flc2fcyExpr fars (Case Rigid e bs) =
 FCY.Case FCY.Rigid (flc2fcyExpr fars e) (map (flc2fcyBranch fars) bs)
flc2fcyExpr fars (Case Flex e bs) =
 FCY.Case FCY.Flex (flc2fcyExpr fars e) (map (flc2fcyBranch fars) bs)
flc2fcyExpr fars (GuardedExpr xs e1 e2) =
  FCY.Free xs
    (FCY.Comb FCY.FuncCall ("Prelude","cond") [flc2fcyExpr fars e1, flc2fcyExpr fars e2])
flc2fcyExpr fars (Choice e) =
  FCY.Comb FCY.FuncCall ("Prelude","choice") [flc2fcyExpr fars e]

flc2fcyLit (Intc   i) = FCY.Intc i
flc2fcyLit (Floatc f) = FCY.Floatc f
flc2fcyLit (Charc  c) = FCY.Charc c

flc2fcyBranch fars (Branch (Pattern s xs) e) =
  FCY.Branch (FCY.Pattern (splitFlatModName s) xs) (flc2fcyExpr fars e)
flc2fcyBranch fars (Branch (LPattern l) e) =
  FCY.Branch (FCY.LPattern (flc2fcyLit l)) (flc2fcyExpr fars e)

