------------------------------------------------------------------------------
--- A library to represent JavaScript programs.
---
--- @author Michael Hanus
--- @version January 22, 2007
------------------------------------------------------------------------------

module JavaScript(JSExp(..),JSStat(..),JSBranch(..),JSFDecl(..),
                  showJSExp,showJSStat,showJSFDecl,
                  jsConsTerm) where

import List(intersperse)

------------------------------------------------------------------------------
--- Type of JavaScript expressions.
--- @cons JSString    - string constant
--- @cons JSInt       - integer constant
--- @cons JSBool      - Boolean constant
--- @cons JSIVar      - indexed variable
--- @cons JSIArrayIdx - array access to index array variable
--- @cons JSOp        - infix operator expression
--- @cons JSFCall     - function call
--- @cons JSApply     - function call where the function is an expression
--- @cons JSLambda    - (anonymous) function with indexed variables as arguments
data JSExp = JSString String
           | JSInt    Int
           | JSBool   Bool
           | JSIVar   Int
           | JSIArrayIdx Int Int
           | JSOp     String JSExp JSExp
           | JSFCall  String [JSExp]
           | JSApply  JSExp JSExp
           | JSLambda [Int] [JSStat]

--- Type of JavaScript statements.
--- @cons JSAssign  - assignment
--- @cons JSIf      - conditional
--- @cons JSSwitch  - switch statement
--- @cons JSPCall   - procedure call
--- @cons JSReturn  - return statement
--- @cons JSVarDecl - local variable declaration
data JSStat = JSAssign  JSExp JSExp
            | JSIf      JSExp [JSStat] [JSStat]
            | JSSwitch  JSExp [JSBranch]
            | JSPCall   String [JSExp]
            | JSReturn  JSExp
            | JSVarDecl Int

-- Type of branches in a switch statement.
--- @cons JSCase    - case branch
--- @cons JSDefault - default branch
data JSBranch = JSCase String [JSStat]
              | JSDefault [JSStat]

-- Type of JavaScript function declarations.
data JSFDecl = JSFDecl String [Int] [JSStat]


------------------------------------------------------------------------------
--- Shows a JavaScript expression as a string in JavaScript syntax.
showJSExp :: JSExp -> String
showJSExp (JSString s) = "\""++s++"\""
showJSExp (JSInt i) = show i
showJSExp (JSBool b) = if b then "true" else "false"
showJSExp (JSIVar i) = "x"++show i
showJSExp (JSIArrayIdx ai i) = "x"++show ai++"["++show i++"]"
showJSExp (JSOp op e1 e2) =
  "(" ++ showJSExp e1 ++ " " ++ op ++ " " ++ showJSExp e2 ++ ")"
showJSExp (JSFCall f args) =
  f ++ "(" ++ concat (intersperse "," (map showJSExp args)) ++ ")"
showJSExp (JSApply f e) = showJSExp f ++ "(" ++ showJSExp e ++ ")"
showJSExp (JSLambda params body) =
  "function(" ++ concat (intersperse "," (map (showJSExp . JSIVar) params)) ++
  ") {" ++ concatMap (showJSStat 1) body ++ "} "

--- Shows a JavaScript statement as a string in JavaScript syntax
--- with indenting.
--- @param i - number of spaces to indent this statement
--- @param jstat - the JavaScript statement to print
showJSStat :: Int -> JSStat -> String
showJSStat i (JSAssign e1 e2) =
  blanks i ++ showJSExp e1 ++ " = " ++ showJSExp e2 ++";"
showJSStat i (JSIf e s1 s2) =
  blanks i ++ "if ("++showJSExp e++") {\n"++
  concatMap ((++"\n") . (showJSStat (i+2))) s1 ++
  if null s2
  then blanks i ++ "}"
  else blanks i ++ "} else {\n" ++
       concatMap ((++"\n") . (showJSStat (i+2))) s2 ++
       blanks i ++ "}"
showJSStat i (JSSwitch e bs) =
  blanks i ++ "switch ("++showJSExp e++") {\n"++
  concatMap showJSBranch bs ++
  blanks i ++ "}"
 where
  showJSBranch (JSCase cs bstats) =
     blanks (i+2) ++ "case \"" ++ cs ++ "\" :\n" ++
     concatMap ((++"\n") . (showJSStat (i+4))) bstats ++
     blanks (i+4) ++ "break;\n"
  showJSBranch (JSDefault bstats) =
     blanks (i+2) ++ "default :\n" ++
     concatMap ((++"\n") . (showJSStat (i+4))) bstats

showJSStat i (JSPCall p args) =
  blanks i ++ p ++ "(" ++ concat (intersperse "," (map showJSExp args)) ++ ")"
showJSStat i (JSReturn e) = blanks i ++ "return " ++ showJSExp e ++";"
showJSStat i (JSVarDecl vi) = blanks i ++ "var x" ++ show vi ++";"

blanks n = replicate n ' '

--- Shows a JavaScript function declaration as a string in JavaScript syntax.
showJSFDecl (JSFDecl f args body) =
  "function " ++ f ++ "(" ++
      concat (intersperse "," (map showJSExp (map JSIVar args))) ++ ") {\n" ++
  concatMap ((++"\n") . (showJSStat 2)) body ++"}\n\n"

------------------------------------------------------------------------------
--- Representation of constructor terms in JavaScript.
--- @param cons - the name of the data constructor
--- @param args - the arguments of the constructor term
jsConsTerm :: String -> [JSExp] -> JSExp
jsConsTerm cons args = JSFCall "new Array" (JSString cons : args)

------------------------------------------------------------------------------
