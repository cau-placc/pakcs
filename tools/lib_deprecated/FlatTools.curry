------------------------------------------------------------------------------
--- Some tools to support meta-programming in Curry based on FlatCurry.
---
--- This library contains
--- <UL>
--- <LI> a show function for a string representation of FlatCurry programs
---   (function "showFlatProg")
---
--- <LI> a function for showing FlatCurry expressions in (almost) Curry syntax
---   (function "showCurryExpr")
--- </UL>
---
--- Note that the previously contained function "writeFLC"
--- is no longer supported. Use Flat2Fcy.writeFCY instead
--- and change file suffix into ".fcy"!
---
--- @author Michael Hanus
--- @version February 2002
------------------------------------------------------------------------------

module FlatTools(showFlatProg,showCurryType,showCurryExpr,showCurryVar) where

import Flat
import List
import Char

--- Shows a FlatCurry program term as a string (with some pretty printing).
showFlatProg :: Prog -> String
showFlatProg (Prog modname imports types funcs ops transtable) =
     " (Prog " ++ flatShowString modname
     ++ (if imports==[] then "\n  []" else
         "\n  [" ++ flatShowListElems flatShowString imports ++ "]")
     ++ (if types==[] then "\n  []" else
         "\n  [" ++ flatShowListElems flatShowType types ++ "\n ]")
     ++ "\n  [" ++ flatShowListElems flatShowFunc funcs ++ "\n  ]"
     ++ "\n " ++ flatShowList flatShowOp ops
     ++ "\n  [" ++ flatShowListElems flatShowTrans transtable ++ "\n  ]\n )\n"

flatShowFixity InfixOp = " InfixOp "
flatShowFixity InfixlOp = " InfixlOp "
flatShowFixity InfixrOp = " InfixrOp "

flatShowOp (Op name fix prec) =
 "(Op " ++ flatShowString name ++ flatShowFixity fix ++ show prec ++ ")"

flatShowType (Type name tpars consdecls) =
  "\n  (Type " ++ flatShowString name
               ++ flatShowList show tpars
               ++ flatShowList flatShowCons consdecls ++ ")"

flatShowCons (Cons cname arity types) =
  "(Cons " ++ flatShowString cname ++ " " ++ show arity
           ++ flatShowList flatShowTypeExpr types ++ ")"

flatShowFunc (Func name arity ftype rl) =
  "\n  (Func " ++ flatShowString name ++ " " ++ show arity ++ " "
               ++ flatShowTypeExpr ftype
               ++ flatShowRule rl ++ ")"

flatShowRule (Rule params expr) =
  " (Rule " ++ flatShowList show params
            ++ flatShowExpr expr ++ ")"
flatShowRule (External name) =
  " (External " ++ flatShowString name ++ ")"

flatShowTypeExpr (FuncType t1 t2) =
  "(FuncType " ++ flatShowTypeExpr t1 ++ " " ++ flatShowTypeExpr t2 ++ ")"
flatShowTypeExpr (TCons tc ts) =
  "(TCons " ++ flatShowString tc ++ flatShowList flatShowTypeExpr ts ++ ")"
flatShowTypeExpr (TVar n) = "(TVar " ++ show n ++ ")"


flatShowExpr (Var n) = "(Var " ++ show n ++ ")"
flatShowExpr (Lit l) = "(Lit " ++ flatShowLit l ++ ")"
flatShowExpr (Comb ctype cf es) =
  "(Comb " ++ show ctype ++ " "++
   flatShowString cf ++ flatShowList flatShowExpr es ++ ")"
flatShowExpr (Apply e1 e2) =
  "(Apply " ++ flatShowExpr e1 ++ " " ++ flatShowExpr e2 ++ ")"
flatShowExpr (Constr xs e) =
  "(Constr " ++ flatShowList show xs ++ flatShowExpr e ++ ")"
flatShowExpr (Or e1 e2) =
  "(Or " ++ flatShowExpr e1 ++ " " ++ flatShowExpr e2 ++ ")"
flatShowExpr (Case Rigid e bs) =
  "(Case Rigid " ++ flatShowExpr e ++ flatShowList flatShowBranch bs ++ ")"
flatShowExpr (Case Flex e bs) =
  "(Case Flex " ++ flatShowExpr e ++ flatShowList flatShowBranch bs ++ ")"
flatShowExpr (GuardedExpr xs e1 e2) =
  "(GuardedExpr " ++ flatShowList show xs
                  ++ flatShowExpr e1 ++ " " ++ flatShowExpr e2 ++ ")"
flatShowExpr (Let bindings exp) =
  "(Let " ++ flatShowList flatShowBinding bindings ++ flatShowExpr exp ++ ")"
 where flatShowBinding (x,e) = "("++show x++","++flatShowExpr e++")"
flatShowExpr (Choice e) = "(Choice " ++ flatShowExpr e ++ ")"

flatShowLit (Intc   i) = "(Intc " ++ show i ++ ")"
flatShowLit (Floatc f) = "(Floatc " ++ show f ++ ")"
flatShowLit (Charc  c) =
 if ord c >= 32  &&  ord c < 127
 then "(Charc '" ++ [c] ++ "')"
 else "(Charc (chr " ++ show (ord c) ++ "))"

flatShowBranch (Branch p e) = "(Branch " ++ flatShowPattern p
                                         ++ flatShowExpr e ++ ")"

flatShowPattern (Pattern s xs) = "(Pattern " ++ flatShowString s
                                             ++ flatShowList show xs ++ ")"
flatShowPattern (LPattern lit) = "(LPattern " ++ flatShowLit lit ++ ")"

flatShowTrans (Trans n intn) = "\n  (Trans "++ flatShowString n ++ " "
                                            ++ flatShowString intn ++")"

-- format a finite list of elements:
flatShowList :: (a->String) -> [a] -> String
flatShowList format elems = " [" ++ flatShowListElems format elems ++ "] "

flatShowListElems :: (a->String) -> [a] -> String
flatShowListElems format elems = concat (intersperse "," (map format elems))

-- format a string:
flatShowString s = "\""++s++"\""


------------------------------------------------------------------------------
--- Shows a FlatCurry type in Curry syntax.
---
--- @param trans - a translation function from internal to external names
--- @param nested - True iff brackets must be written around complex types
--- @param texpr - the FlatCurry type expression to be formatted
--- @return the String representation of the formatted type expression

showCurryType :: (String -> String) -> Bool -> TypeExpr -> String

showCurryType _ _ (TVar i) = [chr (97+i)]
showCurryType tf nested (FuncType t1 t2) =
   (if nested then "(" else "") ++
     showCurryType tf (isFuncType t1) t1 ++ " -> " ++
     showCurryType tf False t2 ++
   (if nested then ")" else "")
showCurryType tf nested (TCons tc ts)
 | ts==[]  = tf tc
 | tc=="[]" && (head ts == TCons "Char" []) = "String"
 | tc=="[]" = "[" ++ showCurryType tf False (head ts) ++ "]" -- list type
 | take 2 tc=="(,"                                           -- tuple type
   = "(" ++ concat (intersperse "," (map (showCurryType tf False) ts)) ++ ")"
 | otherwise
   = (if nested then "(" else "") ++
       tf tc ++ " " ++
         concat (intersperse " " (map (showCurryType tf True) ts)) ++
     (if nested then ")" else "")

isFuncType (TVar _)       = False
isFuncType (FuncType _ _) = True
isFuncType (TCons _ _)    = False


------------------------------------------------------------------------------
--- Shows a FlatCurry expressions in (almost) Curry syntax.
---
--- @param trans - a translation function from internal to external names
--- @param nested - True iff brackets must be written around complex terms
--- @param indent - the indentation used in  case expressions and if-then-else
--- @param expr - the FlatCurry expression to be formatted
--- @return the String representation of the formatted expression

showCurryExpr :: (String -> String) -> Bool -> Int -> Expr -> String

showCurryExpr _ _ _ (Var n) = showCurryVar n

showCurryExpr _ _ _ (Lit l) = showCurryLit l

showCurryExpr tf _ _ (Comb _ cf []) = tf cf
showCurryExpr tf nested b (Comb _ cf [e]) =
  maybeShowBrackets nested (tf cf ++ " " ++ showCurryExpr tf True b e)
showCurryExpr tf nested b (Comb ct cf [e1,e2]) =
  if isAlpha (head (tf cf))
  then maybeShowBrackets nested
            (tf cf ++ " " ++ showCurryElems (showCurryExpr tf True b) [e1,e2])
  else
   if isFiniteList (Comb ct cf [e1,e2])
   then "[" ++
         concat
            (intersperse "," (showCurryFiniteList tf b (Comb ct cf [e1,e2])))
        ++ "]"
   else
    if cf=="(,)" -- pair constructor?
    then "(" ++ showCurryExpr tf False b e1 ++ "," ++
                showCurryExpr tf False b e2 ++ ")"
    else maybeShowBrackets nested
              (showCurryExpr tf True b e1 ++ " " ++ tf cf ++ " " ++
               showCurryExpr tf True b e2 )
showCurryExpr tf nested b (Comb _ cf (e1:e2:e3:es)) =
  if cf=="if_then_else" && es==[]
  then maybeShowBrackets nested
        ("\n" ++
         sceBlanks b ++ " if "   ++ showCurryExpr tf False (b+2) e1 ++ "\n" ++
         sceBlanks b ++ " then " ++ showCurryExpr tf False (b+2) e2 ++ "\n" ++
         sceBlanks b ++ " else " ++ showCurryExpr tf False (b+2) e3)
  else
   if take 2 cf == "(,"  -- tuple constructor?
   then "(" ++
         concat
            (intersperse "," (map (showCurryExpr tf False b) (e1:e2:e3:es)))
        ++ ")"
   else maybeShowBrackets nested
       (tf cf ++ " " ++ showCurryElems (showCurryExpr tf True b) (e1:e2:e3:es))

showCurryExpr tf nested b (Apply e1 e2) =
  maybeShowBrackets nested
       (showCurryExpr tf True b e1 ++ " " ++ showCurryExpr tf True b e2)

showCurryExpr tf nested b (Constr [] e) = showCurryExpr tf nested b e

showCurryExpr tf nested b (Constr (x:xs) e) =
  maybeShowBrackets nested
    ("let " ++ concat (intersperse "," (map showCurryVar (x:xs))) ++
     " free in " ++ showCurryExpr tf False b e)

showCurryExpr tf nested b (Or e1 e2) =
  maybeShowBrackets nested
    (showCurryExpr tf True b e1 ++ " ! " ++ showCurryExpr tf True b e2)

showCurryExpr tf nested b (Case ctype e cs) =
  maybeShowBrackets nested
    ((if ctype==Rigid then "case " else "fcase ") ++
     showCurryExpr tf True b e ++ " of\n " ++
     showCurryElems (showCurryCase tf (b+2)) cs ++ sceBlanks b)

showCurryExpr tf nested b (GuardedExpr [] e1 e2) =
  maybeShowBrackets nested
    (showCurryExpr tf False b e1 ++ " => " ++ showCurryExpr tf False b e2)
showCurryExpr tf nested b (GuardedExpr (x:xs) e1 e2) =
  maybeShowBrackets nested
    (showCurryExpr tf False b e1 ++ " => " ++ showCurryExpr tf False b e2 ++ 
     "  where " ++ concat (intersperse "," (map showCurryVar (x:xs))) ++
     " free")
 
showCurryExpr tf nested b (Choice e) =
  maybeShowBrackets nested ("Choice " ++ showCurryExpr tf True b e)


showCurryVar i = "v" ++ show i

showCurryLit (Intc   i) = show i
showCurryLit (Floatc f) = show f
showCurryLit (Charc  c) = "'" ++ [c] ++ "'"

showCurryCase tf b (Branch (Pattern l vs) e) =
  sceBlanks b ++ showPattern (tf l) vs
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"
 where
   showPattern c [] = c
   showPattern c [x] = c ++ " " ++ showCurryVar x
   showPattern c [x1,x2] =
     if isAlpha (head c)
     then c ++ " " ++ showCurryVar x1 ++ " " ++ showCurryVar x2
     else if c=="(,)" -- pair constructor?
          then "(" ++ showCurryVar x1 ++ "," ++ showCurryVar x2 ++ ")"
          else showCurryVar x1 ++ " " ++ c ++ " " ++ showCurryVar x2
   showPattern c (x1:x2:x3:xs) =
     if take 2 c == "(,"  -- tuple constructor?
     then "("++ concat (intersperse "," (map showCurryVar (x1:x2:x3:xs))) ++")"
     else c ++ " " ++ showCurryElems showCurryVar (x1:x2:x3:xs)

showCurryCase tf b (Branch (LPattern l) e) =
  sceBlanks b ++ showCurryLit l ++ " "
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"

showCurryFiniteList _ _ (Comb _ "[]" []) = []
showCurryFiniteList tf b (Comb _ ":" [e1,e2]) =
  showCurryExpr tf False b e1 : showCurryFiniteList tf b e2

-- format a finite list of elements (with a format function for list elems):
showCurryElems :: (a->String) -> [a] -> String
showCurryElems format elems =
   concat (intersperse " " (map format elems))

maybeShowBrackets nested s =
   (if nested then "(" else "") ++ s ++ (if nested then ")" else "")

sceBlanks b = take b (repeat ' ')

-- Is the expression a finite list (with an empty list at the end)?
isFiniteList :: Expr -> Bool
isFiniteList (Var _) = False
isFiniteList (Lit _) = False
isFiniteList (Comb _ name args)
  | name=="[]" && args==[] = True
  | name==":"  && length args == 2 = isFiniteList (args!!1)
  | otherwise = False
isFiniteList (Apply _ _) = False
isFiniteList (Constr _ _) = False
isFiniteList (Or _ _) = False
isFiniteList (Case _ _ _) = False
isFiniteList (Choice _) = False
isFiniteList (GuardedExpr _ _ _) = False

------------------------------------------------------------------------------
