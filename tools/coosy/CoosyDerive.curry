------------------------------------------------------------------------------
-- Derivor for COOSy observation types
------------------------------------------------------------------------------

module CoosyDerive(derive,deriveFile) where

import AbstractCurry.Types
import AbstractCurry.Files
import System(getProgName)
import Char(isSpace)

derive :: IO ()
derive = do
  progname <- getProgName
  putStr $ "Program where type observers should be added (default: "++progname++"): "
  answer <- getLine 
  let fileName = if all isSpace answer
                 then progname
                 else answer
  msg <- deriveFile fileName
  putStrLn msg

-- Derives observers to a given program file:
deriveFile :: String -> IO String
deriveFile progfile = do
  let progName = takeWhile (/='.') progfile
  addOTypes progName
  return  $ "Observer functions have been added to '"++progName++"'.\n\n"++
            "A backup of the original file has been written to:\n"++
            progName++".curry.bak\n\n"++
            "Don't forget to recompile the program and to reload it"++
            " into your editor!"

addOTypes :: String -> IO ()
addOTypes fileName = do
  progLines <- readFile (fileName++".curry") 
  writeFile (fileName++".curry.bak") progLines
  writeFile (fileName++".curry") 
            (unlines (takeWhile (/=coosyComment) $ lines progLines))
  prog <- readCurry fileName
  appendFile (fileName++".curry")
    ("\n\n"++coosyComment++"\n\n"++deriveProg prog)
 where
  coosyComment = "--oTypes added by Coosy"

deriveProg :: CurryProg -> String
deriveProg (CurryProg _ _ typeDecls _ _) = concatMap deriveTypeDecl typeDecls

deriveTypeDecl :: CTypeDecl -> String
deriveTypeDecl (CType (_,name) _ vs cs) =
  'o':name ++ " ::" ++ concatMap (\i->" Observer x"++show i++" ->") [1..arity]
             ++ " Observer "
             ++ brackets (arity>0) (name ++ derivePatArgs arity) ++"\n"++
    concatMap (deriveCCons ('o':name) vs) cs ++"\n"
  where arity = length vs
deriveTypeDecl (CTypeSyn (_,name) _ vs t) 
  = ('o':name) ++concatMap deriveTypeVar vs ++ "= "++deriveTypeExpr t++"\n"

deriveCCons :: String -> [CTVarIName] -> CConsDecl -> String
deriveCCons tname vs (CCons (_,cname) _ texps) =
  tname ++deriveTypeVarPattern vs (usedVars texps) ++  
  ' ':brackets (arity>0) (cname ++ derivePatArgs arity) ++
  " = o" ++ show arity ++ concatMap deriveTypeExpr texps ++
  ' ':show cname ++ ' ':cname++derivePatArgs arity++"\n"
 where arity = length texps

deriveTypeExpr :: CTypeExpr -> String
deriveTypeExpr (CTVar index) = deriveTypeVar index
deriveTypeExpr (CTCons (_,name) ts) 
  | name=="[]" = " (oList"++concatMap deriveTypeExpr ts++")"
  | ti>0       = " ("++tupleOName ti++concatMap deriveTypeExpr ts++")"
  | otherwise  = ' ':brackets (not (null ts))
                              ('o':name++concatMap deriveTypeExpr ts)
  where ti = tupleIndex name
deriveTypeExpr (CFuncType t1 t2) =
  ' ':'(':dropWhile (==' ') (deriveTypeExpr t1)++" ~>"++ deriveTypeExpr t2++")"

deriveTypeVar :: CTVarIName -> String
deriveTypeVar (_,tvarname) = ' ':tvarname
--deriveTypeVar (index,_) | index < 26 = [' ',chr $ 97+index]
--                        | otherwise = ' ':"t" ++ (show index)

derivePatArgs :: Int -> String
derivePatArgs n = concatMap (\ i->' ':'x':show i) [1..n]

deriveTypeVarPattern :: [CTVarIName] -> [CTVarIName] -> String
deriveTypeVarPattern [] _ = ""
deriveTypeVarPattern (v:vs) used 
  = (if elem v used then (deriveTypeVar v) else " _") ++
    deriveTypeVarPattern vs used

usedVars :: [CTypeExpr] -> [CTVarIName]
usedVars [] = []
usedVars (CTVar index:ts) = index:usedVars ts
usedVars (CTCons _ texps:ts) = usedVars (texps++ts)
usedVars (CFuncType t1 t2:ts) = usedVars (t1:t2:ts)

tupleIndex :: String -> Int
tupleIndex s = case s of
                ('(':s1) -> countComma 1 s1
                _        -> 0

tupleOName :: Int -> String
tupleOName arity | arity==2  = "oPair"
                 | arity==3  = "oTriple"
                 | otherwise = 'o' : (show arity ++ "Tuple")

countComma :: Int -> String -> Int
countComma _ [] = 0
countComma n [c] = if c==')' then n else 0
countComma n (',':s1:s) = countComma (n+1) (s1:s)

brackets :: Bool -> String -> String
brackets b s = if b then '(':s++")" else s
