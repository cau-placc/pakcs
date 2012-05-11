-------------------------------------------------------------------------------
--- Library for pretty printing AbstractCurry programs.
--- In contrast to the library AbstractCurryPrinter,
--- this library implements a better human-readable pretty printing
--- of AbstractCurry programs.
---
--- @author Stefan Junge
--- @version June 2011
-------------------------------------------------------------------------------
module PrettyAbstract (showCProg, printCProg, cprogDoc,
                       prettyCProg, cprogDocWithPrecedences,
                       printUCProg, preludePrecs, precs, prettyCTypeExpr,
                       prettyCTypes, prettyCOps) where

import Pretty
import AbstractCurry
import Char
import System
import Maybe
import List (groupBy)

--- Should names from imported modules be shown with module prefixes?
qualifiedNames = True --False

debug = False

showPrecs name prec
    | debug = text ("{-" ++ show name ++ "@" ++ show prec ++ "-}")
    | otherwise = empty

prelude = "Prelude"

arrow = text "->"
bar = char '|'
dcolon = text "::"

type Precs = [(QName,(CFixity,Int))]

--- the precedences of the operators in the <code>Prelude</code> module
--- @return a list of precedences
preludePrecs :: Precs
preludePrecs = 
    [(("Prelude","!!"),(CInfixlOp,9)),(("Prelude","."),(CInfixrOp,9)),
     (("Prelude","mod"),(CInfixlOp,7)),(("Prelude","div"),(CInfixlOp,7)),
     (("Prelude","*"),(CInfixlOp,7)),(("Prelude","-"),(CInfixlOp,6)),
     (("Prelude","+"),(CInfixlOp,6)),(("Prelude","++"),(CInfixrOp,5)),
     (("Prelude","=:<<="),(CInfixOp,4)),(("Prelude","=:<="),(CInfixOp,4)),
     (("Prelude",">="),(CInfixOp,4)),(("Prelude","<="),(CInfixOp,4)),
     (("Prelude",">"),(CInfixOp,4)),(("Prelude","<"),(CInfixOp,4)),
     (("Prelude","/="),(CInfixOp,4)),(("Prelude","=="),(CInfixOp,4)),
     (("Prelude","=:="),(CInfixOp,4)),(("Prelude","notElem"),(CInfixOp,4)),
     (("Prelude","elem"),(CInfixOp,4)),(("Prelude","&&"),(CInfixrOp,3)),
     (("Prelude","||"),(CInfixrOp,2)),(("Prelude",">>="),(CInfixlOp,1)),
     (("Prelude",">>"),(CInfixlOp,1)),(("Prelude","?"),(CInfixrOp,0)),
     (("Prelude","&>"),(CInfixrOp,0)),(("Prelude","&"),(CInfixrOp,0)),
     (("Prelude","seq"),(CInfixrOp,0)),(("Prelude","$##"),(CInfixrOp,0)),
     (("Prelude","$#"),(CInfixrOp,0)),(("Prelude","$!!"),(CInfixrOp,0)),
     (("Prelude","$!"),(CInfixrOp,0)),(("Prelude","$"),(CInfixrOp,0)),
     (("Prelude",":"),(CInfixrOp,5))]


-- ----------------------------------------------------------------------------+

--- (prettyCProg w prog) pretty prints the curry prog <code>prog</code> and
--- fits it to a page width of <code>w</code> characters. 
--- @param w - width of page
--- @param prog - a curry prog
--- @return a string, which represents the <code>prog</code>
prettyCProg :: Int -> CurryProg -> String
prettyCProg w = pretty w . cprogDoc

--- (prettyCTypeExpr mod typeExpr) pretty prints the type expression 
--- <code>typeExpr</code> of the module <code>mod</code> and fits it to a page
--- width of 78 characters. 
--- @param mod - module name of the current module
--- @param typeExpr - a type expression
--- @return a string, which represents the <code>typeExpr</code>
prettyCTypeExpr :: String -> CTypeExpr -> String
prettyCTypeExpr mod = pretty 78 . typeExprDoc mod 0

--- (prettyCTypes mod typeDecls) pretty prints the type declarations
--- <code>typeDecls</code> of the module <code>mod</code> and fits it to a page
--- width of 78 characters. 
--- @param mod - module name of the current module
--- @param typeDecls - a list of type declarations
--- @return a string, which represents the <code>typeDecls</code>
prettyCTypes :: String -> [CTypeDecl] -> String
prettyCTypes mod = pretty 78 . typesDoc mod

--- (prettyCOps opDecls) pretty prints the operators
--- <code>opDecls</code> and fits it to a page width of 78 characters.
--- @param opDecls - a list of operators
--- @return a string, which represents the <code>opDecls</code>
prettyCOps :: [COpDecl] -> String
prettyCOps = pretty 78 . opsDoc

--- (showCProg prog) pretty prints the curry prog
--- <code>prog</code> and fits it to a page width of 78 characters.
--- @param prog - a curry prog
--- @return a string, which represents the <code>prog</code>
showCProg :: CurryProg -> String
showCProg = prettyCProg 78

--- main uses <code>printCProg</code> to pretty print a
--- typed Abstract Curry program and <code>printUCProg</code> to pretty print an
--- untyped Abstract Curry program.
--- main is suitable for an initial expression (:save main).<br><br>
--- <code>usage: prettyabstract [--uacy] &lt;modulname&gt;</code><br>
--- <code>--uacy : use untyped Abstract Curry program instead 
---                of typed Abstract Curry program</code>
main :: IO()
main = do
    args <- getArgs
    case args of
        --["--acy",m] -> if checkMod m
        --                   then printCProg m
        --                   else usage
        ["--uacy",m] -> if checkMod m
                            then printUCProg m
                            else usage
        [m] -> if checkMod m
                   then printCProg m
                   else usage
        _ -> usage
  where
    usage = do
        putStrLn "usage: prettyabstract [--uacy] <modulname>"
        putStrLn "--uacy : use untyped Abstract Curry program instead of typed Abstract Curry program"
        
    checkMod m
        | "-" == take 1 m = False
        | otherwise = checkMod' $ reverse m
        
    checkMod' "" = True
    checkMod' (c:cs)
        | c == '/' = True
        | otherwise = c /= '.' && checkMod' cs
         

--- (printCProg modulname) pretty prints the typed Abstract Curry program of
--- <code>modulname</code> produced by <code>AbstractCurry.readCurry</code> and 
--- fits it to a page width of 78 characters.
--- The output is standard io.
--- @param modulname - the file name without suffix ".curry" or ".acy"
printCProg :: String -> IO ()
printCProg modulname = readCurry modulname >>= putStrLn . showCProg

--- (printUCProg modulname) pretty prints the untyped Abstract Curry program of
--- <code>modulname</code> produced by <code>AbstractCurry.readUntypedCurry</code> 
--- and fits it to a page width of 78 characters.
--- The output ist standard io.
--- @param modulname - the file name without suffix ".curry" or ".uacy"
printUCProg :: String -> IO ()
printUCProg modulname = readUntypedCurry modulname >>= putStrLn . showCProg

--- (cprogDoc prog) creates a document of the Curry program
--- <code>prog</code> and fits it to a page width of 78 characters.
--- @param prog - a curry prog
--- @return the document, which represents the <code>prog</code>
cprogDoc :: CurryProg -> Doc
cprogDoc = cprogDocWithPrecedences preludePrecs

--- (cprogDocWithPrecedences precs prog) creates a document of the curry prog
--- <code>prog</code> and fits it to a page width of 78 characters, 
--- the precedences <code>precs</code> ensure a correct bracketing 
--- of infix operators
--- @param precs - a list of precedences
--- @param prog - a curry prog
--- @return the document, which represents the <code>prog</code>
cprogDocWithPrecedences :: Precs -> CurryProg -> Doc
cprogDocWithPrecedences ps cprog@(CurryProg name imps types funcs ops)
  = moduleHeaderDoc name cprog (exportedNames name cprog) <$>>
    impsDoc imps <$>> opsDoc ops <$>>
    typesDoc name types <$>>
    funcsDoc (precs ops ++ ps) name funcs <$> empty

--- generates a list of precedences
--- @param opDecls - a list of operators
--- @return a list of precedences
precs :: [COpDecl] -> Precs
precs = map (\(COp name fix i) -> (name,(fix,i)))
    
-- -------------------------------layout--------------------------------------

d1 <$>> d2 | isEmpty d1 = d2
           | isEmpty d2 = d1
           | otherwise = d1 <$> line <> d2

def :: Doc -> [CTVarIName] -> Doc -> Doc
def name params body = block (name <> paramDoc <$> body)
 where
  paramDoc = if null params then empty 
              else space <> align (fillSep (map varDoc params))

block :: Doc -> Doc
block = group . hang 1

app :: Doc -> [Doc] -> Doc
app d ds = if null ds then d
            else block (fillEncloseSep empty empty space (d:ds))
            
par mPrec = if isJust mPrec then parens else id

precFillEncloseSep :: Bool -> (CFixity,Int) -> Maybe (CFixity,Int) -> Doc -> Doc -> Doc -> [Doc] -> Doc
precFillEncloseSep amILeft p1 mp2 l r s ds
  | isNothing mp2 = fillEncloseSep empty empty s ds
  | otherwise = fillEncloseSep (pre p1 p2 l) (pre p1 p2 r) s ds
 where
  p2 = fromJust mp2
  
  pre (fO,pO) (fI,pI) br
   | pO>pI = empty
   | pO<pI = br
   | fO == CInfixOp = br
   | fO /= fI = br
   | amILeft && fI == CInfixrOp = br
   | not amILeft && fI == CInfixlOp = br
   | True  = empty

layout :: [Doc] -> Doc
layout = align . compose (combine (linesep "; "))

-- -------------------------------qname----------------------------------------

qname :: String -> QName -> Doc
qname prog mn@(mod,name)
  | mn == (prelude,"[]") || isTupleName mn = text name
  | isInfixName mn = 
      if mod == prog || mod == prelude || not qualifiedNames
       then parens (text name)
       else parens (text mod <> dot <> (text name))
  | otherwise
    = if mod == prog || mod == prelude || not qualifiedNames
       then text (correctName name) 
       else text mod <> dot <> (text name)

isTupleName :: QName -> Bool
isTupleName (mod,name) = mod == prelude && elem (take 2 name) ["()","(,"]

isInfixName :: QName -> Bool
isInfixName (_,n) = all (`elem` infixIDs) n

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

correctName :: String -> String
correctName name = 
  let name' = filter (not . flip elem ['#','.']) name
  in
  case name' of 
       ('_':xs) -> xs
       _        -> name'

varDoc :: CTVarIName  -> Doc
varDoc = text . snd

tvarDoc :: CTVarIName -> Doc
tvarDoc (i,v)
  | v /= "" && '_' /= head v || v == "_" = text v
  | i>25      = text ("x" ++ show i)
  | otherwise = text [chr (97+i)]
  
litDoc :: CLiteral -> Doc
litDoc (CIntc n) = int n
litDoc (CFloatc x) = float x
litDoc (CCharc c) = squotes (text (quoteChar c))

quoteChar c = maybe [c] id (lookup c specialChars)

specialChars = [('\\',"\\\\"),('\n',"\\n"),('\r',"\\r"),('\t',"\\t"),('"',"\\\"")]

-- -------------------------------module header and exports-------------------

moduleHeaderDoc :: String -> CurryProg -> [Doc] -> Doc
moduleHeaderDoc name cprog exports
    | hasPrivate cprog = text "module" <+> text name <+> exportsDoc exports <+>
                         text "where"
    | otherwise = text "module" <+> text name <+> text "where"

exportsDoc :: [Doc] -> Doc
exportsDoc xs
  = group (nest 1 (lparen <$> align (fillSep (punctuate comma xs)) <$> rparen))

hasPrivate :: CurryProg -> Bool
hasPrivate (CurryProg _ _ types funcs _) =
    any (Private==) (map typeCVisibility types ++ map funcCVisibility funcs)

exportedNames :: String -> CurryProg -> [Doc]
exportedNames mod (CurryProg _ _ types funcs _)
  = map typeExpDoc (filter ((Public==) . typeCVisibility) types)
 ++ map (qname mod . funcName) (filter ((Public==) . funcCVisibility) funcs)
 where
  typeExpDoc tdecl =
    let ecs = filter ((Public==) . consCVisibility)
                     (trCType (\_ _ _ cs -> cs) (\_ _ _ _ -> []) tdecl)
     in qname mod (typeName tdecl) <> if null ecs then empty else text "(..)"

-- -------------------------------imports and ops------------------------------

impsDoc :: [String] -> Doc
impsDoc imps = vcat (map ((text "import" <+>) . text)
                         (filter (/=prelude) imps))

opsDoc :: [COpDecl] -> Doc
opsDoc ops = vcat (map opLineDoc (groupBy eqCOpDecl ops))

opLineDoc :: [COpDecl] -> Doc
opLineDoc [] = empty
opLineDoc ops@(COp _ fix prec:_) =
    text "infix" <> fixDoc fix <+> int prec <+> align (hsep (punctuate comma (map opDoc ops)))
  where     
    fixDoc CInfixOp = empty
    fixDoc CInfixlOp = text "l"
    fixDoc CInfixrOp = text "r"
    
eqCOpDecl :: COpDecl -> COpDecl -> Bool
eqCOpDecl (COp _ fix1 prec1) (COp _ fix2 prec2) =
    fix1 == fix2 && prec1 == prec2

opDoc :: COpDecl -> Doc
opDoc (COp n@(_,name) _ _)
  = text infname
 where
  infname = if isInfixName n then name else '`':name++"`"

-- -------------------------------types----------------------------------------

typesDoc :: String -> [CTypeDecl] -> Doc
typesDoc mod = vcat . map (typeDoc mod)

typeDoc :: String -> CTypeDecl -> Doc
typeDoc mod (CType name _ params cs)
  = def (text "data" <+> qname mod name) params (consDeclsDoc mod cs)

typeDoc mod (CTypeSyn name _ params syn)
  = def (text "type" <+> qname mod name) params
        (equals <+> typeExprDoc mod 0 syn)

consDeclsDoc :: String -> [CConsDecl] -> Doc
consDeclsDoc mod consDecls
    | null consDecls = empty
    | otherwise = fillEncloseSep (equals<>space) empty (bar<>space)
                  $ map ((<>space) . consDeclDoc mod) consDecls

consDeclDoc :: String -> CConsDecl -> Doc
consDeclDoc mod (CCons name _ _ args)
  = app (qname mod name) (map (typeExprDoc mod 2) args)
  
-- p == 0 -> parent is list / tuple / this is function result
-- p == 1 -> parent is CFuncType 
-- p == 2 -> parent is CTCons
typeExprDoc :: String -> Int -> CTypeExpr -> Doc
typeExprDoc _ _ (CTVar n) = tvarDoc n

typeExprDoc mod p (CTCons name args)
  | null args = qname mod name
  | name == (prelude,"[]") = brackets (typeExprDoc mod 0 (head args))
  | isTupleName name = tupled (map (typeExprDoc mod 0) args)
  | otherwise 
    = (if p == 2 then parens else id) $ app (qname mod name) (map (typeExprDoc mod 2) args)

typeExprDoc mod p typ@(CFuncType _ _)
  = (if p > 0 then parens else id) $ fillEncloseSep empty empty (space<>arrow<>space)
     (map (typeExprDoc mod 1) (argTypes typ) ++ 
      [typeExprDoc mod 0 (resultType typ)])

--typeExprDoc mod _ (CRecordType fields mName) =
--    fillEncloseSep lbrace 
--                   (maybe rbrace nDoc mName)
--                   comma 
--                   (map (fieldDoc dcolon (typeExprDoc mod 0)) fields)
--  where
--      nDoc n = space <> bar <+> tvarDoc n <> rbrace  
      
isUntyped :: CTypeExpr -> Bool
isUntyped typ = 
    case typ of
        (CTCons ("Prelude","untyped") []) -> True
        _ -> False
        
        
-- -------------------------------function and localDecls----------------------

funcsDoc :: Precs -> String -> [CFuncDecl] -> Doc
funcsDoc pr mod funcs = vcat (punctuate line (map (funcDoc pr mod) funcs))

funcDoc :: Precs -> String -> CFuncDecl -> Doc
funcDoc pr mod (CFunc name _ _ typ rules) =
  (if hasRec typ then text "--" else empty) <>
  (if isUntyped typ then empty else funcTypeDeclDoc mod name typ <$> empty) <>
  rulesDoc pr mod name rules
funcDoc pr mod (CmtFunc cmt name ar vis typ rules) =
  vsep (map (\l->text ("--- "++l)) (lines cmt)) <$>
  funcDoc pr mod (CFunc name ar vis typ rules)
    
hasRec :: CTypeExpr -> Bool
hasRec (CTVar _) = False
hasRec (CFuncType t1 t2) = hasRec t1 || hasRec t2
hasRec (CTCons _ ts) = any hasRec ts
--hasRec (CRecordType _ _) = True

localDeclsDoc :: Precs -> String -> [CLocalDecl] -> Doc
localDeclsDoc pr mod lds = align $ vsep $ punctuate line $ map (localDeclDoc pr mod) lds

localDeclDoc :: Precs -> String -> CLocalDecl -> Doc
localDeclDoc pr mod (CLocalFunc f) = funcDoc pr mod f
localDeclDoc pr mod (CLocalPat  pn e lds) =
    hang 1 $ patternDoc mod pn <+> equals <+> expDoc unknown pr Nothing mod e <>
             if null lds 
                then empty 
                else line <> text "where" <+> localDeclsDoc pr mod lds
localDeclDoc _ _ (CLocalVar vn) = hang 1 $ tvarDoc vn <+> text "free"

-- -------------------------------function type--------------------------------

funcTypeDeclDoc :: String -> QName -> CTypeExpr -> Doc
funcTypeDeclDoc mod name typ
  = def (qname mod name) [] (funcTypeDoc mod (argTypes typ) (resultType typ))

funcTypeDoc :: String -> [CTypeExpr] -> CTypeExpr -> Doc
funcTypeDoc mod args res
  = fillEncloseSep dcolon empty (space<>arrow)
     ((map ((space<>) . typeExprDoc mod 1) args) ++
      (map ((space<>) . typeExprDoc mod 1) [res]))

-- -------------------------------rules----------------------------------------

rulesDoc :: Precs -> String -> QName -> CRules -> Doc
rulesDoc pr mod name (CRules _ rules) =
    vsep (map (ruleDoc pr mod name) rules)
rulesDoc _ mod name (CExternal _) =
    qname mod name <+> text "external" 


ruleDoc :: Precs -> String -> QName -> CRule -> Doc
ruleDoc pr mod name (CRule patterns choices localDecls) -- (CRule args body)
    | fst (head choices) ==  CSymbol ("Prelude","success")
          =  hang 2 $ (group $ hang 2 ((nameAndParam <+> 
                   equals <$> align (expDoc unknown pr Nothing mod (snd (head choices))))))
            <> whereDoc
    | otherwise = hang 2 (hang 2 (nameAndParam <$> align (vsep (map choiceDoc choices))) <> whereDoc)
  where
    nameAndParam =
        if isInfixName name && length patterns == 2
            then patternDoc mod (patterns!!0) <+> text (snd name) <+> patternDoc mod (patterns!!1)
            else qname mod name <> paramDoc
  
    paramDoc = if null patterns 
                     then empty 
                 else space <> patternsDoc mod patterns
    choiceDoc (e1,e2) = text "|" <+> align (expDoc unknown pr Nothing mod e1) <+> 
                        equals <+> (expDoc unknown pr Nothing mod e2)
    whereDoc = if null localDecls
                  then empty
                  else line <>  text "where" <+> localDeclsDoc pr mod localDecls


-- -------------------------------expressions----------------------------------

expDoc :: Bool -> Precs -> Maybe (CFixity,Int) -> String -> CExpr -> Doc
expDoc amILeft pr mPrec mod exp = 
  maybe (maybe (expDoc2 amILeft pr mPrec mod exp)
          (\l -> list (map (expDoc unknown pr Nothing mod) l))
          (toList exp))
    (\s -> if null s then text "[]" else dquotes (text s))
      (toString exp)

          
toList :: CExpr -> Maybe [CExpr]
toList exp
  = case exp of
      CSymbol ("Prelude","[]") -> Just []
      CApply (CApply (CSymbol ("Prelude",":")) x) xs -> toList xs >>- Just . (x:)
      _ -> Nothing

toString :: CExpr -> Maybe String
toString exp
  = case exp of
      CSymbol ("Prelude","[]") -> Just ""
      CApply (CApply (CSymbol ("Prelude",":")) (CLit (CCharc c))) cs ->
        toString cs >>- Just . (quoteChar c++)
      _ -> Nothing

expDoc2 :: Bool -> Precs -> Maybe (CFixity,Int) -> String -> CExpr -> Doc
expDoc2 _ _ mPrec _ (CVar n) = showPrecs n mPrec <> varDoc n
expDoc2 _ _ mPrec _ (CLit l) = showPrecs l mPrec <> litDoc l
expDoc2 _ _ mPrec mod (CSymbol qn) = showPrecs qn mPrec <> qname mod qn

expDoc2 _ pr mPrec mod (CLetDecl bs e)
  = par mPrec $ hang 1 $ 
     text "let" <+> localDeclsDoc pr mod bs <$> 
     text "in" <+> expDoc unknown pr Nothing mod e

expDoc2 _ pr mPrec mod (CCase e bs)
  = par mPrec $ hang 1 $ 
     text "case" <+> align (expDoc unknown pr Nothing mod e) 
       <+> text "of" <$> layout (map (branchDoc pr mod) bs)

expDoc2 _ pr mPrec mod (CLambda ps e)
  = par mPrec $ hang 1 $ 
     backslash  <+> patternsDoc mod ps
       <+> arrow <+> expDoc unknown pr Nothing mod e

expDoc2 amILeft pr mPrec mod (CApply e1 e2)
    | maybe False isTupleName mfname = tupled (map (expDoc unknown pr Nothing mod) fargs)
    | maybe False isInfixName mfname && length fargs == 2
       = showPrecs (snd fname) (amILeft,pOp, mPrec) <> 
         (align $ precFillEncloseSep amILeft pOp mPrec lbr rbr empty
                 [expDoc True pr (Just pOp) mod (fargs!!0) 
                 ,space <> text (snd fname)
                 ,space <> expDoc False pr (Just pOp) mod (fargs!!1)])
    | maybe False isInfixName mfname && length fargs > 2
       = appPar True mPrec $
         app (fillEncloseSep lparen rparen empty
                 [expDoc  True pr (Just pOp) mod (fargs!!0) 
                 ,space <> text (snd fname)
                 ,space <> expDoc False pr (Just pOp) mod (fargs!!1)])
             (map (expDoc False pr (Just (unknown,11)) mod) (drop 2 fargs))
    | maybe False (== ("Prelude","if_then_else")) mfname && length fargs == 3 =
        par mPrec $ hang 1 $ ifThenElse
    | maybe False (== ("Prelude","if_then_else")) mfname && length fargs > 3 =
        appPar True mPrec $ hang 1 $
        app (parens ifThenElse)
            (map (expDoc False pr (Just (unknown,11)) mod) (drop 3 fargs))
    | otherwise = showPrecs (name (CApply e1 e2)) mPrec <> (appPar (not (null fargs)) mPrec $ 
                  app (expDoc False pr (Just (unknown,11)) mod (name (CApply e1 e2))) 
                      (map (expDoc False pr (Just (unknown,11)) mod) fargs))
   
 where    
 
    appPar _ Nothing = id
    appPar br (Just fNp)
        | snd fNp == 11 && br = parens
        | otherwise = id
    
    ifThenElse = 
        text "if" <+> align (expDoc unknown pr Nothing mod (fargs!!0)) <$>
        text "then" <+> align (expDoc unknown pr Nothing mod (fargs!!1)) <$>
        text "else" <+> align (expDoc unknown pr Nothing mod (fargs!!2))
 
    fname = maybe ("","") id mfname
 
    mfname = case name (CApply e1 e2) of
                CSymbol qn -> Just qn
                _          -> Nothing
    fargs = args (CApply e1 e2)
 
    name e = case e of
        (CApply e' _) -> name e'
        _ -> e
    args e = case e of
        (CApply e1' e2') -> args e1' ++ [e2']
        _ -> []
        
    (lbr,rbr) = if isJust mPrec then (lparen,rparen) else (empty,empty)
    pOp = case lookup fname pr of
               Just p' -> p'
               Nothing -> (CInfixlOp,9) 
        

expDoc2 _ pr mPrec mod (CDoExpr stms)
  = par mPrec $
      text "do" <+> layout (map (statementDoc pr mod) stms)

expDoc2 _ pr _ mod (CListComp e stms)
  = brackets $ expDoc unknown pr Nothing mod e <+> text "|" <+> 
               encloseSep empty empty comma (map (statementDoc pr mod) stms)
               
--expDoc2 _ pr _ mod (CRecConstr fields)
--  = fillEncloseSep lbrace 
--                   rbrace
--                   comma 
--                   (map (fieldDoc equals (expDoc unknown pr Nothing mod)) fields)
--                   
--expDoc2 _ pr mPrec mod (CRecSelect exp label)
--  = par mPrec $ expDoc False pr (Just (unknown,11)) mod exp <+> arrow <+> text label
--  
--expDoc2 _ pr _ mod (CRecUpdate fields exp)
--  = fillEncloseSep lbrace 
--                   (space <> bar <+> expDoc unknown pr Nothing mod exp <> rbrace)
--                   comma 
--                   (map (fieldDoc (colon<>equals) (expDoc unknown pr Nothing mod)) fields)
   

--fieldDoc :: Doc -> (a -> Doc) -> (CField a) -> Doc
--fieldDoc sep toDoc (label,x) = text label <+> sep <+> toDoc x

statementDoc :: Precs -> String -> CStatement -> Doc
statementDoc pr mod (CSExpr e) = hang 1 $ expDoc False pr Nothing mod e 
statementDoc pr mod (CSPat p e) = 
    hang 1 $ patternDoc mod p <+> text "<-" <+> expDoc False pr Nothing mod e
statementDoc pr mod (CSLet localDecls) =
    text "let" <+> localDeclsDoc pr mod localDecls
    

branchDoc :: Precs -> String -> CBranchExpr -> Doc
branchDoc pr mod (CBranch pat e)
  = def (patternDoc mod pat <+> arrow) [] (align (expDoc False pr Nothing mod e))

-- -------------------------------pattern--------------------------------------

patternsDoc :: String -> [CPattern] -> Doc
patternsDoc mod = align .  fillSep .  map (patternDoc mod)

patternDoc :: String -> CPattern -> Doc
patternDoc _ (CPVar vname) = tvarDoc vname
patternDoc _ (CPLit l) = litDoc l
patternDoc mod cpc@(CPComb qn pns)
   | isJust mStringPatt = if null stringPatt 
                                 then text "[]" 
                                 else dquotes (text stringPatt)
   | isJust mListPatt = listDoc (map (patternDoc mod) listPatt)
   | null pns = qname mod qn
   | isTupleName qn = tupleDoc (map (patternDoc mod) pns)
   | isInfixName qn && length pns == 2 = 
        parens $ patternDoc mod (pns!!0)  <+> 
                 text (snd qn) <+>
                 patternDoc mod (pns!!1)
   | otherwise = parens (qname mod qn <+> group (hang 0 (vsep (map (patternDoc mod) pns))))
  where
      listDoc = fillEncloseSep lbracket rbracket (space<>comma)
      tupleDoc = fillEncloseSep lparen rparen (space<>comma)
  
      mListPatt = toListPattern cpc
      mStringPatt = toStringPattern cpc
      
      listPatt = fromJust mListPatt
      stringPatt = fromJust mStringPatt
      
patternDoc mod (CPAs vn p) = tvarDoc vn <> text "@" <> patternDoc mod p
patternDoc mod (CPFuncComb qn pns)
   | null pns = qname mod qn
   | isInfixName qn && length pns == 2
       = parens $ patternDoc mod (pns!!0) <> text (snd qn) <> patternDoc mod (pns!!1)
   | otherwise = parens (qname mod qn <+> hsep (map (patternDoc mod) pns))
--patternDoc mod (CPLazy pn) = text "~" <> patternDoc mod pn
--patternDoc mod (CPRecord fields mPatt) =
--    fillEncloseSep lbrace 
--                   (maybe rbrace pattDoc mPatt)
--                   comma 
--                   (map (fieldDoc equals (patternDoc mod)) fields)
--  where
--      pattDoc p = space <> bar <+> patternDoc mod p <> rbrace

toListPattern :: CPattern -> Maybe [CPattern]
toListPattern patt
  = case patt of
      CPComb ("Prelude","[]") [] -> Just []
      CPComb ("Prelude",":") [x,xs] -> toListPattern xs >>- Just . (x:)
      _ -> Nothing

toStringPattern :: CPattern -> Maybe String
toStringPattern patt
  = case patt of
      CPComb ("Prelude","[]") [] -> Just ""
      CPComb ("Prelude",":") [CPLit (CCharc c),cs] ->
        toStringPattern cs >>- Just . (quoteChar c++)
      _ -> Nothing



-- AbstractCurryGoodies -------------------------------------------------------

-- Prog -------------------------------

--- transform program
trCurryProg :: (String -> [String] -> [CTypeDecl] -> [CFuncDecl] -> [COpDecl] -> a)
          -> CurryProg -> a
trCurryProg prog (CurryProg name imps types funcs ops) = prog name imps types funcs ops

-- TypeDecl ---------------------------

-- Selectors

--- transform type declaration
trCType  :: (QName -> CVisibility -> [CTVarIName] -> [CConsDecl] -> a) ->
          (QName -> CVisibility -> [CTVarIName] -> CTypeExpr -> a) -> CTypeDecl -> a
trCType  typ _ (CType name vis params cs) = typ name vis params cs
trCType  _ typesyn (CTypeSyn name vis params syn) = typesyn name vis params syn

--- get visibility of type declaration
typeCVisibility :: CTypeDecl -> CVisibility
typeCVisibility = trCType  (\_ vis _ _ -> vis) (\_ vis _ _ -> vis)

--- get name of type declaration
typeName :: CTypeDecl -> QName
typeName = trCType (\name _ _ _ -> name) (\name _ _ _ -> name)

-- FuncDecl ---------------------------

--- transform function
trCFunc :: (QName -> Int -> CVisibility -> CTypeExpr -> CRules -> a)
        -> CFuncDecl -> a
trCFunc func (CFunc name arity vis t rules) = func name arity vis t rules
trCFunc func (CmtFunc _ name arity vis t rules) = func name arity vis t rules

-- Selectors

--- get name of function
funcName :: CFuncDecl -> QName
funcName = trCFunc (\name _ _ _ _ -> name)

--- get visibility of function
funcCVisibility :: CFuncDecl -> CVisibility
funcCVisibility = trCFunc (\_ _ vis _ _ -> vis)

-- ConsDecl ---------------------------

-- Selectors

--- transform constructor declaration
trCCons :: (QName -> Int -> CVisibility -> [CTypeExpr] -> a) -> CConsDecl -> a
trCCons cons (CCons name arity vis args) = cons name arity vis args

--- get visibility of constructor declaration
consCVisibility :: CConsDecl -> CVisibility
consCVisibility = trCCons (\_ _ vis _ -> vis)


-- Auxiliary Functions ----------------

--- get argument types from functional type
argTypes :: CTypeExpr -> [CTypeExpr]
argTypes (CTVar _) = []
argTypes (CTCons _ _) = []
argTypes (CFuncType dom ran) = dom : argTypes ran
--argTypes (CRecordType _ _) = []

--- get result type from (nested) functional type
resultType :: CTypeExpr -> CTypeExpr
resultType (CTVar n) = CTVar n
resultType (CTCons name args) = CTCons name args
resultType (CFuncType _ ran) = resultType ran
--resultType rec@(CRecordType _ _) = rec

-- end of PrettyAbstract
