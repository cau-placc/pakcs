------------------------------------------------------------------------------
--- A pretty printer for AbstractCurry programs.
---
--- This library defines a function "showProg" that shows
--- an AbstractCurry program in standard Curry syntax.
---
--- @author Martin Engelke, Bernd Brassel, Michael Hanus, Marion Mueller,
---         Parissa Sadeghi
--- @version May 2010
------------------------------------------------------------------------------

module AbstractCurryPrinter(showProg,
                            showTypeDecls,
                            showTypeDecl,
                            showTypeExpr,
                            showFuncDecl,
                            showExpr, showPattern) where

import AbstractCurry
import List
import Read(readNat)
import Char(isDigit)
import FiniteMap
import Sort (cmpString)
import Maybe (isJust)

-------------------------------------------------------------------------------
-- Functions to print an AbstractCurry program in standard Curry syntax
-------------------------------------------------------------------------------

--- Shows an AbstractCurry program in standard Curry syntax.
--- The export list contains the public functions and the
--- types with their data constructors (if all data constructors are public),
--- otherwise only the type constructors.
--- The potential comments in function declarations are formatted as
--- documentation comments.
showProg :: CurryProg -> String
showProg (CurryProg m imports typedecls funcdecls opdecls) =
  let exports = showExports typedecls funcdecls in
  "module "++m 
  ++ (if exports=="" then "" else " ("++exports++")")
  ++ " where\n\n"
  ++ showImports imports
  ++ showOpDecls opdecls
  ++ showTypeDecls typedecls
  ++ prefixInter (showFuncDeclOpt (nameFM funcdecls,m)) funcdecls "\n\n"
  ++ "\n"

type NameFM = FM String ()
type Options = (NameFM,String)


defaultOptions :: Options
defaultOptions = (emptyFM lessString,"")


showExports :: [CTypeDecl] -> [CFuncDecl] -> String
showExports types funcs = 
  let publicTypes = filter isPublicType types
      (withCons, withoutCons) = partition allPublicCons publicTypes
  in
  concat 
    (intersperse ", " 
      (map ((++"(..)") . getTypeName) withCons
       ++ map getTypeName withoutCons
       ++ map getFuncName (filter isPublicFunc funcs)))
  where
    isPublicType :: CTypeDecl -> Bool
    isPublicType (CType _ visibility _ _) = visibility==Public 
    isPublicType (CTypeSyn _ visibility _ _) = visibility==Public 

    isPublicFunc :: CFuncDecl -> Bool
    isPublicFunc (CFunc _ _ visibility _ _) = visibility==Public 
    isPublicFunc (CmtFunc _ _ _ visibility _ _) = visibility==Public 

    getTypeName :: CTypeDecl -> String
    getTypeName (CType (_,name) _ _ _) = name
    getTypeName (CTypeSyn (_,name) _ _ _) = name

    allPublicCons :: CTypeDecl -> Bool
    allPublicCons (CType _ _ _ c) = length (filter isPublicCons c) == length c 
      where isPublicCons (CCons _ _ visibility _) = visibility==Public
    allPublicCons (CTypeSyn _ _ _ _) = False

    getFuncName :: CFuncDecl -> String
    getFuncName (CFunc (_,name) _ _ _ _) =
        if isInfixOpName name then "("++name++")" else name
    getFuncName (CmtFunc _ (_,name) _ _ _ _) =
        if isInfixOpName name then "("++name++")" else name


showImports :: [String] -> String
showImports imports =
  prefixInter showImport (filter (/=prelude) imports) "\n" ++
  (if imports==[prelude] then "" else "\n\n")

showImport :: String -> String
showImport imp = if imp /= prelude then "import " ++ imp else ""

showOpDecls :: [COpDecl] -> String
showOpDecls opdecls =
  prefixInter showOpDecl opdecls "\n" ++
  (if opdecls == [] then "" else "\n\n")

showOpDecl :: COpDecl -> String
showOpDecl (COp (_,name) fixity precedence) =
  showFixity fixity ++ " " ++ show precedence ++ " " ++
  if isInfixOpName name then name else '`':name++"`"

showFixity :: CFixity -> String
showFixity CInfixOp  = "infix"
showFixity CInfixlOp = "infixl"
showFixity CInfixrOp = "infixr"

--- Shows a list of AbstractCurry type declarations in standard Curry syntax.
showTypeDecls :: [CTypeDecl] -> String
showTypeDecls typedecls =
  prefixInter showTypeDecl typedecls "\n\n" ++
  (if typedecls == [] then "" else "\n\n")

--- Shows an AbstractCurry type declaration in standard Curry syntax.
showTypeDecl :: CTypeDecl -> String
showTypeDecl (CTypeSyn (_,name) _ indexlist typeexpr)
   = "type " ++ name
             ++ (prefixMap (showTypeExpr False) (map CTVar indexlist) " ")
     ++ " = " ++ showTypeExpr False typeexpr
showTypeDecl (CType (_,name) _ indexlist consdecls)
   = "data " ++ name
             ++ (prefixMap (showTypeExpr False) (map CTVar indexlist) " ")
     ++ "\n"++showBlock ("= "++(combineMap showConsDecl consdecls "\n| "))

showConsDecl :: CConsDecl -> String
showConsDecl (CCons (_,name) _ _ typelist)
   = name ++ (prefixMap (showTypeExpr True) typelist " ")

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, the type expression is enclosed
--- in brackets.
showTypeExpr :: Bool -> CTypeExpr -> String
showTypeExpr _ (CTVar (_,name)) = showTypeVar (showIdentifier name)
showTypeExpr nested (CFuncType domain range) =
   maybeShowBrackets nested (showTypeExpr (isCFuncType domain) domain ++
                             " -> " ++ showTypeExpr False range)
       
showTypeExpr nested (CTCons (mod,name) typelist)
   | mod==prelude && name == "untyped" = "-"
   | otherwise  = maybeShowBrackets (nested && not (null typelist))
                                    (showTypeCons mod name typelist)

-- Show a1,a2,a3 as a_1,a_2,a_3 (due to bug in PAKCS front-end):
showTypeVar (c:cs) =
  if c=='a' && not (null cs) && all isDigit cs
  then c:'_':cs
  else c:cs

-- Remove characters '<' and '>' from identifiers sind these characters
-- are sometimes introduced in new identifiers generated by the front end (for sections)
showIdentifier :: String -> String
showIdentifier = filter (not . flip elem "<>")

isCFuncType t = case t of
                  CFuncType _ _ -> True
                  _ -> False

--- Shows an AbstractCurry function declaration in standard Curry syntax.
showFuncDecl = showFuncDeclOpt defaultOptions

showFuncDeclOpt :: Options -> CFuncDecl -> String
showFuncDeclOpt opts (CmtFunc cmt qname ar vis typeexpr rules) =
  showCmtFunc opts cmt (CFunc qname ar vis typeexpr rules)
showFuncDeclOpt opts cfunc@(CFunc _ _ _ _ _) = showCmtFunc opts "" cfunc

showCmtFunc :: Options -> String -> CFuncDecl -> String
showCmtFunc opts cmt (CFunc (_,name) arity _ typeexpr (CRules evalannot rules))=
  funcComment cmt ++
  (if evalannot == CFlex then ""
      else bolName ++ " eval " ++ (showEvalAnnot evalannot)++"\n") ++
  (if isUntyped typeexpr then "\n" 
      else bolName ++ " :: " ++ (showTypeExpr False typeexpr)++"\n") ++
  (if funcIsInfixOp then  rulePrints arity
      else name ++ (prefixInter (showRule opts) rules ("\n"++name)))
   where
     funcIsInfixOp = isInfixOpName name
     bolName = if funcIsInfixOp then "("++name++")" else name
     rulePrints arity' = concat $ intersperse "\n" 
                    $ map (insertName arity' . (span (/=' ')) . tail . (showRule opts)) rules
     insertName arity' (fstArg,rest) = 
         if arity'/=0
           then fstArg++" "++name++rest
           else bolName++" "++fstArg++rest
showCmtFunc _ cmt (CFunc (_,name) _ _ typeexpr (CExternal _)) =
  funcComment cmt ++
  bolName ++ " :: " ++ (showTypeExpr False typeexpr) ++"\n"++
  bolName ++ " external"
 where
  bolName = if isInfixOpName name then "("++name++")" else name

-- format function comment as documentation comment
funcComment :: String -> String
funcComment = unlines . map ("--- "++) . lines

showLocalFuncDecl :: Options -> CFuncDecl -> String
showLocalFuncDecl opts = showFuncDeclOpt opts

showRule :: Options -> CRule -> String
showRule opts (CRule pattlist crhslist localdecls) =
  prefixMap showPattern pattlist " " ++
  showCrhsList opts crhslist ++
  (if null localdecls
   then ""
   else  "\n   where\n" ++
           showBlock (prefixMap (showLocalDecl opts) localdecls "\n")
  )

showEvalAnnot :: CEvalAnnot -> String
showEvalAnnot CFlex = "flex"
showEvalAnnot CRigid = "rigid"
showEvalAnnot CChoice = "choice"

showCrhsList :: Options -> [(CExpr,CExpr)] -> String
showCrhsList _ [] = ""
showCrhsList opts ((g,r):cs)
   | cs == [] && g == CSymbol (prelude,"success") 
   =  " = " ++ showExprOpt opts r
   | otherwise 
   = "\n" ++ showBlock (combineMap (showCrhs opts) ((g,r):cs) "\n")

showCrhs :: Options -> (CExpr,CExpr) -> String
showCrhs opts (cond,expr) =
  "| " ++ showExprOpt opts cond ++ "\n= " ++ showExprOpt opts expr

showLocalDecl :: Options -> CLocalDecl -> String
showLocalDecl opts (CLocalFunc funcdecl) = showLocalFuncDecl opts funcdecl
showLocalDecl opts (CLocalPat pattern expr localdecls) =
  showPattern pattern ++ " = " ++ showExprOpt opts expr ++
  (if null localdecls
   then ""
   else "\n   where\n" ++
        showBlock (prefixMap (showLocalDecl opts) localdecls "\n")
  )
showLocalDecl _ (CLocalVar index) = showPattern (CPVar index) ++ " free"

--- Shows an AbstractCurry expression in standard Curry syntax.
showExpr = showExprOpt defaultOptions

showExprOpt :: Options -> CExpr -> String
showExprOpt _ (CVar (_,name)) = showIdentifier name
showExprOpt _ (CLit lit) = showLiteral lit
showExprOpt opts (CSymbol name) 
  = if isInfixOpName (snd name) then "("++showSymbol opts name++")" 
                                else showSymbol opts name
showExprOpt opts (CApply func arg) = showApplication opts (CApply func arg)
showExprOpt opts (CLambda patts expr) = showLambdaOrSection opts patts expr
showExprOpt opts (CLetDecl localdecls expr)
   = "let\n" ++ showBlock ((combineMap (showLocalDecl opts) localdecls "\n") 
     ++ "\n in " ++ (showBoxedExpr opts expr))
showExprOpt opts (CDoExpr stmts)
   = "\n    do\n" ++ showBlock (combineMap (showStatement opts) stmts "\n")
showExprOpt opts (CListComp expr stmts)
   =    "[ " ++ (showBoxedExpr opts expr) ++ " | "
     ++ (combineMap (showStatement opts) stmts ", ") ++ "]"
showExprOpt opts (CCase expr branches)
   =    "case " ++ (showBoxedExpr opts expr) ++ " of\n"
     ++ showBlock (combineMap (showBranchExpr opts) branches "\n")


showSymbol :: Options -> QName -> String
showSymbol (fm,thisModule) (thatModule,symName)
  | thisModule == thatModule = symName
  | isJust (lookupFM fm symName) = thatModule++"."++symName
  | otherwise = symName

-- show a lambda expression as a left/right section, if 
-- it is a literal, var other than the pattern var or non-infix symbol.
-- A better test for sections would need the test for sub expressions
-- which is too complex for this simple purpose.
showLambdaOrSection opts patts expr = case patts of
  [CPVar pvar] -> case expr of
     (CApply (CApply (CSymbol (_,name)) lexpr) (CVar var))
      -> if isInfixOpName name && isAtom lexpr && (CVar var/=lexpr)
         then if pvar==var
              then "(" ++ showBoxedExpr opts lexpr ++ " " ++ name ++ ")"
              else if lexpr == (CVar pvar)
                   then "(" ++ name ++ " " ++ showExprOpt opts (CVar var) ++ ")"
                   else showLambda opts patts expr
         else showLambda opts patts expr
     (CApply (CApply (CSymbol (_,name)) (CVar var)) rexpr)
      -> if isInfixOpName name && pvar==var && isAtom rexpr && (CVar var/=rexpr)
         then "(" ++ name ++ " " ++ showBoxedExpr opts rexpr ++ ")"
         else showLambda opts patts expr
     _ -> showLambda opts patts expr 
  _ -> showLambda opts patts expr

showLambda opts patts expr = "\\" ++ (combineMap showPattern patts " ")
                        ++ " -> " ++ (showExprOpt opts expr)


showStatement :: Options -> CStatement -> String
showStatement opts (CSExpr expr) = showExprOpt opts expr
showStatement opts (CSPat pattern expr)
   = (showPattern pattern) ++ " <- " ++ (showExprOpt opts expr)
showStatement opts (CSLet localdecls)
   = case localdecls of
       (decl:[]) -> "let " ++ showLocalDecl opts decl
       _         -> "let\n" ++ showBlock (combineMap (showLocalDecl opts) localdecls "\n")

showPattern :: CPattern -> String
showPattern (CPVar (_,name)) = showIdentifier name
showPattern (CPLit lit) = showLiteral lit
showPattern (CPComb (_,name) []) = name 
showPattern (CPComb (mod,name) (p:ps))
   | mod == prelude = showPreludeCons (CPComb (mod,name) (p:ps))
   | isAsPattern p = showAsPatternList p   
   | otherwise        = "(" ++ name ++ (prefixMap showPattern (p:ps) " ") ++ ")"
showPattern (CPAs (_,name) pat) = showIdentifier name ++ "@" ++ showPattern pat
showPattern (CPFuncComb qname pats) = showPattern (CPComb qname pats)
   

showPreludeCons :: CPattern -> String
showPreludeCons p
   | name == ":"  = showPatternList p
   | isTuple name = "(" ++ (combineMap showPattern pattlist ",") ++ ")"
   | otherwise    = "(" ++ name ++ (prefixMap showPattern pattlist " ") ++ ")"
   where
     CPComb (_,name) pattlist = p

showPatternList :: CPattern -> String
showPatternList p
  | isClosedStringPattern p 
  = '\"':filter (/='\'') (concat (showPatListElems p))++"\""
  | isClosedPatternList p
  = "["++concat (intersperse "," (showPatListElems p))++"]"
  | isAsPattern p
  = showAsPatternList p
  | otherwise = "(" ++ concat (intersperse ":" (showPatListElems p))++")"

showPatListElems (CPComb (_,":") [x,xs]) 
  = showPattern x : showPatListElems xs
showPatListElems (CPComb (_,"[]") []) = []
showPatListElems (CPVar v) = [showPattern (CPVar v)]
showPatListElems (CPAs name p) = [showPattern (CPAs name p)]

isClosedPatternList (CPComb (m,":") [_,xs]) =
  m==prelude && isClosedPatternList xs
isClosedPatternList (CPComb (m,"[]") []) = m==prelude
isClosedPatternList (CPVar _) = False
isClosedPatternList (CPAs _ p) = isClosedPatternList p

isClosedStringPattern (CPComb (m,":") [x,xs]) 
  = m==prelude && isCharPattern x && isClosedStringPattern xs
isClosedStringPattern (CPComb (m,"[]") []) = m==prelude
isClosedStringPattern (CPVar _) = False

isCharPattern p = case p of 
                    CPLit (CCharc _) -> True
                    _                -> False

isAsPattern p = case p of
                  CPAs _ _ -> True
                  _        -> False

showAsPatternList (CPAs (_,name) p) = 
     name++"@"++"(" ++ concat (intersperse ":" (showPatListElems p))++")"

showBranchExpr :: Options -> CBranchExpr -> String
showBranchExpr opts (CBranch pattern expr)
   = (showPattern pattern) ++ " -> " ++ (showExprOpt opts expr)

showLiteral :: CLiteral -> String
showLiteral (CIntc i) = show i
showLiteral (CFloatc f) = show f
showLiteral (CCharc c) = "'"++showCCharc (CCharc c)++"'"

showCCharc :: CLiteral -> String
showCCharc (CCharc c) | c=='\n' = "\\n"
                      | c=='\r' = "\\r"
                      | c=='\\' = "\\\\"
                      | c=='\"' = "\\\""
                      | otherwise = [c]

showBlock :: String -> String
showBlock text
   = combineMap id (map ((++) "     ") (filter ((/=) "") (lines text))) "\n"


showTypeCons :: String -> String -> [CTypeExpr] -> String
showTypeCons _ name [] = name
showTypeCons mod name (t:ts)
   | mod == prelude = showPreludeTypeCons name (t:ts)
   | otherwise        = name ++ (prefixMap (showTypeExpr True) (t:ts) " ")

showPreludeTypeCons :: String -> [CTypeExpr] -> String
showPreludeTypeCons name typelist
  | name == "[]" && head typelist == CTCons (prelude,"Char") [] = "String"
  | name == "[]" = "[" ++ (showTypeExpr False (head typelist)) ++ "]"
  | isTuple name = "(" ++ (combineMap (showTypeExpr False) typelist ",") ++ ")"
  | otherwise    = name ++ (prefixMap (showTypeExpr True) typelist " ")



showApplication :: Options -> CExpr -> String
showApplication opts appl
   = case (applicationHead appl) of
       (CSymbol name) -> showSymbolApplication opts name appl
       _              -> showSimpleApplication opts appl

applicationHead :: CExpr -> CExpr
applicationHead expr
   = case expr of
       (CApply func _) -> applicationHead func
       _               -> expr

showSymbolApplication :: Options -> (String,String) -> CExpr -> String
showSymbolApplication opts (mod,name) appl
   | mod == prelude && name == ":" 
   = showListApplication opts appl
   | isInfixOpName name 
   = showInfixApplication opts (mod,name) appl
   | mod == prelude && name == "if_then_else" 
   = showITEApplication opts appl
   | isTuple name
   = showTupleApplication opts appl
   | otherwise        
   = showSimpleApplication opts appl

showListApplication :: Options -> CExpr -> String
showListApplication opts appl
   | isStringList appl
     = "\"" ++ (showCharListApplication opts appl) ++ "\""
   | isClosedList appl
     = "[" ++ (showConsListApplication opts appl) ++ "]"
   | otherwise
     = "(" ++ (showSimpleListApplication opts appl) ++ ")"

showCharListApplication :: Options -> CExpr -> String
showCharListApplication opts (CApply (CApply _ (CLit c)) tail)
   = case tail of
       (CSymbol _) -> showCCharc c
       _           -> showCCharc c ++ showCharListApplication opts tail

showConsListApplication :: Options -> CExpr -> String
showConsListApplication opts (CApply (CApply _ head) tail)
   = case tail of
       (CSymbol _) -> showBoxedExpr opts head
       _           -> (showBoxedExpr opts head) ++ "," 
                        ++ (showConsListApplication opts tail)

showSimpleListApplication :: Options -> CExpr -> String
showSimpleListApplication opts (CApply (CApply _ head) tail)
   = case tail of
       (CSymbol _) -> showBoxedExpr opts head ++ ":[]"
       _           -> showBoxedExpr opts head ++ ":" ++ showBoxedExpr opts tail
showSimpleListApplication opts (CApply (CSymbol (_,str)) tail)
   = showBoxedExpr opts tail ++ str

showInfixApplication :: Options -> QName -> CExpr -> String
showInfixApplication opts infixop (CApply func arg2) 
   = case func of 
       (CApply f arg1) -> case f of
                             (CApply _ arg0) -> 
                                  "(" ++ showBoxedExpr opts arg0 ++ " " ++
                                   showSymbol opts infixop ++ " " ++
                                   showBoxedExpr opts arg1 ++ ") " ++
                                   showBoxedExpr opts arg2
                             _ -> showBoxedExpr opts arg1 ++ " "
                                  ++ showSymbol opts infixop
                                  ++ " " ++ showBoxedExpr opts arg2
       _ -> "(" ++ showSymbol opts infixop ++ ") " ++ (showBoxedExpr opts arg2)

showITEApplication :: Options -> CExpr -> String
showITEApplication opts (CApply (CApply (CApply (CSymbol _) condExpr) thenExpr) elseExpr)
   =    "if " ++ (showExprOpt opts condExpr) ++ " then "
     ++ (showExprOpt opts thenExpr) ++ " else "
     ++ (showExprOpt opts elseExpr) 
showITEApplication opts (CApply e@(CApply (CApply (CApply _ _) _) _) e')
   = "("++showITEApplication opts e ++ ") "++showBoxedExpr opts e'

showTupleApplication :: Options -> CExpr -> String
showTupleApplication opts appl
   = "(" ++ (p_showTuple appl) ++ ")"
   where
   p_showTuple (CApply (CSymbol _) arg)
      = showExprOpt opts arg
   p_showTuple (CApply (CApply e1 e2) arg)
      = (p_showTuple (CApply e1 e2)) ++ "," ++ (showExprOpt opts arg)

showSimpleApplication :: Options -> CExpr -> String
showSimpleApplication opts appl =
  case appl of
     CApply func arg -> showSimpleApplication opts func ++ " "
                        ++ showBoxedExpr opts arg
     _               -> showBoxedExpr opts appl

showBoxedExpr :: Options -> CExpr -> String
showBoxedExpr opts expr
   | isSimpleExpr expr = showExprOpt opts expr
   | otherwise         = "(" ++ showExprOpt opts expr ++ ")"

-------------------------------------------------------------------------------
--- composition functions for AbstractCurryPrinter
-------------------------------------------------------------------------------

prefixMap :: (a -> String) -> [a] ->  String -> String
prefixMap f xs s
   = concatMap ((++)s) (map f xs)

prefixInter :: (a -> String) -> [a] ->  String -> String
prefixInter f xs s
   = concat $ intersperse s (map f xs)

combineMap :: (a -> String) -> [a] ->  String -> String
combineMap _ [] _ = ""
combineMap f (x:xs) s
   = (f x) ++ (prefixMap f xs s)


dropTags :: String -> String
dropTags (x:xs) = case x of
                    '\"' -> dropTags $ tail $ dropWhile (/='\"') xs
                    '>'  -> xs
                    _    -> dropTags xs

-------------------------------------------------------------------------------
--- tests for various properties of AbstractCurry constructs
-------------------------------------------------------------------------------

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

isStringList :: CExpr -> Bool
isStringList (CSymbol (mod,name))
   = mod == prelude && name == "[]"
isStringList (CVar _) = False
isStringList (CApply head tail)
   = case head of 
       (CApply _ (CLit (CCharc _))) -> isStringList tail
       _                            -> False

isClosedList :: CExpr -> Bool
isClosedList expr
   = case expr of
       (CApply (CApply (CSymbol (mod,name)) _) tail)
          -> mod==prelude && name==":" && isClosedList tail
       (CSymbol (mod,name))
          -> mod == prelude && name == "[]"
       _  -> False

isSimpleExpr :: CExpr -> Bool
isSimpleExpr expr
   = case expr of
       (CVar _)      -> True
       (CLit _)      -> True
       (CSymbol (_, name)) -> not $ isInfixOpName name
       (CApply f _)  -> case (applicationHead f) of
                          (CSymbol (m,name)) ->  m==prelude &&
                                                 (name == ":"
                                                  || name == "[]"
                                                  || name == "()"
                                                  || isTuple name)
                          _                  -> False
       _             -> False


isAtom :: CExpr -> Bool
isAtom expr
   = case expr of
       (CVar _)      -> True
       (CLit _)      -> True
       (CSymbol (_, name)) -> not $ isInfixOpName name
       _ -> False

isUntyped :: CTypeExpr -> Bool
isUntyped typeexpr
   = case typeexpr of
       (CTCons (mod,name) []) -> mod == prelude && name == "untyped"
       _                    -> False

isTuple :: String -> Bool
isTuple [] = False
isTuple (x:xs) = (x == '(') && (p1_isTuple xs)
   where
   p1_isTuple [] = False
   p1_isTuple (z:[]) = z == ')'
   p1_isTuple (z1:z2:zs) = (z1 == ',') && (p1_isTuple (z2:zs))

------------------------------------------------------------------------------
--- constants used by AbstractCurryPrinter
------------------------------------------------------------------------------

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

prelude = "Prelude"

-- enclose string with brackets, if required by first argument:
maybeShowBrackets nested s =
   (if nested then "(" else "") ++ s ++ (if nested then ")" else "")

------------------------------------------------
-- building the map of defined function names
------------------------------------------------

nameFM :: [CFuncDecl] -> NameFM
nameFM = foldr addName (emptyFM lessString) 

addName :: CFuncDecl -> NameFM -> NameFM
addName (CFunc (_,n) _ _ _ _) fm = addToFM fm n () 
addName (CmtFunc _ (_,n) _ _ _ _) fm = addToFM fm n () 

lessString s1 s2 = LT==cmpString s1 s2
