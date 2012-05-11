------------------------------------------------------------------
-- A tool to add all those type signatures, you didn't bother to 
-- write while developing the program. 
--
-- @author Bernd Brassel, with changes by Michael Hanus
-- @version June 2005
-- 
-- Possible extensions: Use type synonyms to reduce annotations
------------------------------------------------------------------

module AddTypes(main,addTypeSignatures) where

import AbstractCurry
import AbstractCurryPrinter
import System (system,getArgs)
import CurryStringClassifier
import List
import FileGoodies

-- The tool is rather simple, it uses PAKCS' facilities for 
-- metaprogramming to read the program in the form defined 
-- in the AbstractCurry module. 
-- PAKCS provides commands to read AbstractCurry programs typed and untyped.
-- By comparing the results of these two operations, we are able to
-- distinguish the inferred types from those given by the programmer.
-- 
-- addtypes makes use of the CurryStringClassifier, cf. function addTypes.


--- addtypes is supposed to get its argument, the file to add type signatures
--- to from the shell. 

main :: IO ()
main = do args <- getArgs
          if length args /= 1 then putStrLn "Usage: addtypes <Curry program>"
            else do let fileName = stripSuffix (head args)
                    writeWithTypeSignatures fileName
                    putStrLn $ "Signatures added.\nA backup of the original "
                         ++"file has been written to "++fileName++".TS.curry"

--- the given file is read three times: a) typed, to get all the necessary 
--- type information b) untyped to find out, which of the types were 
--- specified by the user and c) as a simple string to which the signatures
--- are added. Before adding anything, addtypes will write a backup
--- to <given filename>.TS.curry

writeWithTypeSignatures :: String -> IO ()
writeWithTypeSignatures fileName = do
   system ("cp -p "++fileName++".curry "++fileName++".TS.curry")
   newprog <- addTypeSignatures fileName
   writeFile (fileName++".curry") newprog

addTypeSignatures :: String -> IO String
addTypeSignatures fileName = do
   typedProg <- readCurry fileName
   untypedProg <- readUntypedCurry fileName
   progLines <- readFile (fileName++".curry")
   doSolve (newprog =:= -- strict equality to enfore the reading of all files here
                        unscan (addTypes (scan progLines) 
                                         (getTypes typedProg untypedProg)))
   system $ "rm -f "++fileName++".acy "++fileName++".uacy"
   return newprog
 where
   newprog free

--- retrieve the functions whithout type signature and their type

getTypes :: CurryProg -> CurryProg -> [(String,CTypeExpr)]
getTypes (CurryProg _ _ _ funcDecls1 _) (CurryProg _ _ _ funcDecls2 _) 
         = getTypesFuncDecls funcDecls1 funcDecls2
  where
    getTypesFuncDecls [] [] = []
    getTypesFuncDecls (CFunc name _ _ t1 _:fs1) (CFunc _ _ _ t2 _:fs2) 
      | isUntyped t2 = (snd name,t1):getTypesFuncDecls fs1 fs2
      | otherwise = getTypesFuncDecls fs1 fs2

--- addtypes implements a simple algorithm to decide where to add type 
--- information. Find the first line wich contains the function name 
--- on the left hand side and insert the type annotation before that line.
--- The problem with this algorithm is that it might get confused by 
--- comments. This is where the Curry string classifier comes in.
--- After using CussryStringClassifier.scan the function addTypes only 
--- has to process "Code" tokens and can be sure that there will be no
--- confusion with Comments, Strings or Chars within the program.

addTypes :: Tokens -> [(String,CTypeExpr)] -> Tokens
addTypes [] _ = []
addTypes (ModuleHead s:ts) fts = ModuleHead s : (addTypes ts fts)
addTypes (SmallComment s:ts) fts = SmallComment s : (addTypes ts fts)
addTypes (BigComment s:ts) fts = BigComment s : (addTypes ts fts)
addTypes (Text s:ts) fts = Text s : (addTypes ts fts)
addTypes (Letter s:ts) fts = Letter s : (addTypes ts fts)
addTypes (Code s:ts) fts = Code newS : newTs
  where
    newS = let (lastline,newline)=break (=='\n') s
            in lastline++addTypesCode newline newFts fts
    newTs = if null newFts then ts else addTypes ts newFts
    newFts = x where x free

--- Within a given  code segment insert all annotations for the contained
--- function and return the new code + the list of functions not yet 
--- inserted (via the logical variable newFts).

addTypesCode :: [Char] -> [([Char],CTypeExpr)] -> [([Char],CTypeExpr)] -> [Char]
addTypesCode code [] [] = code
addTypesCode code newFts ((f,t):fts)
  | null code = (newFts=:=((f,t):fts)) &> []
  | otherwise 
  = case lhs of 
      [] -> head remainder 
          : addTypesCode (tail remainder) newFts ((f,t):fts)
      ' ':_ -> line ++ addTypesCode remainder newFts ((f,t):fts)
      _ -> if defines f lhs
             then let typed = (showTypeExpr False $ normalize t)++"\n"
                       in (printf++" :: "++typed) ++ line 
                          ++ addTypesCode remainder newFts fts
             else line ++ addTypesCode remainder newFts ((f,t):fts)

  where
    (line,remainder) = break (=='\n') code
    (lhs,_) = break (=='=') line
    printf = if all (flip elem infixIDs) f then '(':f++")" else f



--- name type variables with a,b,c ... z, t0, t1, ...

toTVar :: Int -> CTypeExpr
toTVar n | n<26 = CTVar (n,[chr (97+n)])
         | otherwise = CTVar (n,"t"++show (n-26))

--- test for functions not typed by the programmer

isUntyped :: CTypeExpr -> Bool
isUntyped typeexpr
   = case typeexpr of
       (CTCons (mod,name) []) -> name == "untyped" && mod == "Prelude"
       _                      -> False

--- normalizing is to rename Variables left-right beginning with 0
--- and replace singletons with an "_"
normalize :: CTypeExpr -> CTypeExpr
normalize t | varNames 0 (tvars t newT) = newT where newT free

--- retrieve all vars contained in a ttype expression and simultaniously
--- build a new type expression with logical variables for type vars

tvars :: CTypeExpr -> CTypeExpr -> [(Int,CTypeExpr)]
tvars (CTVar (i,_)) m = [(i,m)]
tvars (CTCons n args) (CTCons n' args') 
  | n=:=n' = concat (dualMap tvars args args')
tvars (CFuncType t1 t2) (CFuncType t1' t2')
  = tvars t1 t1' ++ tvars t2 t2'

--- give a list of variables names depending on whether they are singletons
--- or not

varNames :: Int -> [(_,CTypeExpr)] -> Success
varNames _ [] = success
varNames n ((i,v):ivs) 
  | null is =   (v=:=(CTVar (0,"_"))) &> (varNames n others)
  | otherwise = (giveName (toTVar n) (v:map snd is)) &> (varNames (n+1) others)
  where
    (is,others) = partition (\ (i',_) -> i==i') ivs
    giveName _ [] = success
    giveName name (x:xs) = name=:=x & giveName name xs

--- map on two lists simultaniously. Can't use zip, because the second
--- argument here is a logical variable.

dualMap :: (a -> b -> c) -> [a] -> [b] -> [c]
dualMap _ [] [] = []
dualMap f (x:xs) (y:ys) = f x y:dualMap f xs ys

--- a left hand side defines a function named f, if it starts leftmost,
--- and contains f 
defines :: [Char] -> [Char] -> Bool
defines f lhs 
  | null ts = False
  | head lhs == ' ' = False
  | otherwise = elem f ts 
  where
    ts = symbols lhs

--- delimiters between terms on left hand sides
delimiters :: String
delimiters = " ([{,}])"

--- these characters form infix operator names
infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"

--- divide a left hand side to a list of symbols contained
--- e.g. symbols "f x [y,z]" = ["f","x","y","z"]
symbols :: [Char] -> [[Char]]
symbols lhs = syms [] lhs
  where
    maybeSym t = if null t then [] else [t]
    syms s [] = maybeSym s
    syms s (x:xs) 
      | elem x delimiters 
      = maybeSym s ++ syms [] (dropWhile (flip elem delimiters) xs)
      | otherwise 
      = syms (s++[x]) xs


