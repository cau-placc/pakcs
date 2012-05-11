------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing FlatCurry programs
--- in Curry (type "Prog") and an I/O action to read Curry programs and
--- transform them into this representation (function "readFlatCurry").
---
--- @author Michael Hanus
--- @version June 2009
------------------------------------------------------------------------------

module FlatCurry where

import Directory(doesFileExist)
import ReadShowTerm
import Distribution
import FileGoodies(stripSuffix)

------------------------------------------------------------------------------
-- Definition of data types for representing FlatCurry programs:
-- =============================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- 
---     (Prog modname imports typedecls functions opdecls translation_table)
--- 
--- where
--- `modname` is the name of this module,
--- `imports` is the list of modules names that are imported,
--- `typedecls`, `opdecls`, `functions`, translation of type names
--- and constructor/function names are explained see below

data Prog = Prog String [String] [TypeDecl] [FuncDecl] [OpDecl]


--- The data type for representing qualified names.
--- In FlatCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
type QName = (String,String)

--- Data type to specify the visibility of various entities.

data Visibility = Public    -- public (exported) entity
                | Private   -- private entity

--- The data type for representing type variables.
--- They are represented by `(TVar i)` where `i` is a type variable index.
type TVarIndex = Int

--- Data type for representing definitions of algebraic data types.
---
--- A data type definition of the form
---
---     data t x1...xn = ...| c t1....tkc |...
---
--- is represented by the FlatCurry term
---
---     (Type t [i1,...,in] [...(Cons c kc [t1,...,tkc])...])
---
--- where each `ij` is the index of the type variable `xj`.
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.

data TypeDecl = Type    QName Visibility [TVarIndex] [ConsDecl]
              | TypeSyn QName Visibility [TVarIndex] TypeExpr

--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.

data ConsDecl = Cons QName Int Visibility [TypeExpr]


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
--- "Int", "Float", "Bool", "Char", "IO", "Success",
--- "()" (unit type), "(,...,)" (tuple types), "[]" (list type)

data TypeExpr =
     TVar TVarIndex                 -- type variable
   | FuncType TypeExpr TypeExpr     -- function type t1->t2
   | TCons QName [TypeExpr]         -- type constructor application
                                    -- TCons module name typeargs


--- Data type for operator declarations.
--- An operator declaration `fix p n` in Curry corresponds to the
--- FlatCurry term `(Op n fix p)`.

data OpDecl = Op QName Fixity Int

--- Data types for the different choices for the fixity of an operator.
data Fixity = InfixOp | InfixlOp | InfixrOp


--- Data type for representing object variables.
--- Object variables occurring in expressions are represented by `(Var i)`
--- where `i` is a variable index.
type VarIndex = Int


--- Data type for representing function declarations.
---
--- A function declaration in FlatCurry is a term of the form
---
---     (Func name k type (Rule [i1,...,ik] e))
---
--- and represents the function `name` with definition
---
---     name :: type
---     name x1...xk = e
---
--- where each `ij` is the index of the variable `xj`.
---
--- Note: the variable indices are unique inside each function declaration
---       and are usually numbered from 0
---
--- External functions are represented as
---
---     (Func name arity type (External s))
---
--- where s is the external name associated to this function.
---
--- Thus, a function declaration consists of the name, arity, type, and rule.

data FuncDecl = Func QName Int Visibility TypeExpr Rule


--- A rule is either a list of formal parameters together with an expression
--- or an "External" tag.

data Rule = Rule [VarIndex] Expr
          | External String

--- Data type for classifying case expressions.
--- Case expressions can be either flexible or rigid in Curry.

data CaseType = Rigid | Flex       -- type of a case expression

--- Data type for classifying combinations
--- (i.e., a function/constructor applied to some arguments).
--- @cons FuncCall     - a call to a function where all arguments are provided
--- @cons ConsCall     - a call with a constructor at the top, all arguments are provided
--- @cons FuncPartCall - a partial call to a function (i.e., not all arguments
---                      are provided) where the parameter is the number of
---                      missing arguments
--- @cons ConsPartCall - a partial call to a constructor (i.e., not all arguments
---                      are provided) where the parameter is the number of
---                      missing arguments

data CombType = FuncCall | ConsCall | FuncPartCall Int | ConsPartCall Int

--- Data type for representing expressions.
---
--- Remarks:
---
--- if-then-else expressions are represented as function calls:
---
---     (if e1 then e2 else e3)
---
--- is represented as
---
---     (Comb FuncCall ("Prelude","if_then_else") [e1,e2,e3])
--- 
--- Higher-order applications are represented as calls to the (external)
--- function `apply`. For instance, the rule
---
---     app f x = f x
---
--- is represented as
---
---     (Rule  [0,1] (Comb FuncCall ("Prelude","apply") [Var 0, Var 1]))
--- 
--- A conditional rule is represented as a call to an external function
--- `cond` where the first argument is the condition (a constraint).
--- For instance, the rule
---
---     equal2 x | x=:=2 = success
---
--- is represented as
---
---     (Rule [0]
---           (Comb FuncCall ("Prelude","cond")
---                 [Comb FuncCall ("Prelude","=:=") [Var 0, Lit (Intc 2)],
---                  Comb FuncCall ("Prelude","success") []]))
---
--- @cons Var - variable (represented by unique index)
--- @cons Lit - literal (Int/Float/Char constant)
--- @cons Comb - application `(f e1 ... en)` of function/constructor `f`
---              with `n`&lt;=arity(`f`)
--- @cons Free - introduction of free local variables
--- @cons Or - disjunction of two expressions (used to translate rules
---            with overlapping left-hand sides)
--- @cons Case - case distinction (rigid or flex)

data Expr = Var VarIndex 
          | Lit Literal
          | Comb CombType QName [Expr]
          | Let [(VarIndex,Expr)] Expr
          | Free [VarIndex] Expr
          | Or Expr Expr
          | Case CaseType Expr [BranchExpr]


--- Data type for representing branches in a case expression.
---
--- Branches "(m.c x1...xn) -> e" in case expressions are represented as
---
---     (Branch (Pattern (m,c) [i1,...,in]) e)
---
--- where each `ij` is the index of the pattern variable `xj`, or as
---
---     (Branch (LPattern (Intc i)) e)
---
--- for integers as branch patterns (similarly for other literals
--- like float or character constants).

data BranchExpr = Branch Pattern Expr

--- Data type for representing patterns in case expressions.

data Pattern = Pattern QName [VarIndex]
             | LPattern Literal

--- Data type for representing literals occurring in an expression
--- or case branch. It is either an integer, a float, or a character constant.

data Literal = Intc   Int
             | Floatc Float
             | Charc  Char


------------------------------------------------------------------------------
--- I/O action which parses a Curry program and returns the corresponding
--- FlatCurry program.
--- Thus, the argument is the file name without suffix ".curry"
--- (or ".lcurry") and the result is a FlatCurry term representing this
--- program.

readFlatCurry :: String -> IO Prog
readFlatCurry progfile =
   readFlatCurryWithParseOptions progfile (setQuiet True defaultParams)

--- I/O action which reads a FlatCurry program from a file
--- with respect to some parser options.
--- This I/O action is used by the standard action `readFlatCurry`.
--- It is currently predefined only in Curry2Prolog.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end

readFlatCurryWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryWithParseOptions progname options = do
  mbCurryFile  <- lookupFileInLoadPath (progname++".curry")
  mbLCurryFile <- lookupFileInLoadPath (progname++".lcurry")
  if mbCurryFile==Nothing && mbLCurryFile==Nothing
   then done
   else callFrontendWithParams FCY options progname
  filename <- findFileInLoadPath (progname++".fcy")
  readFlatCurryFile filename


--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryFileName :: String -> String
flatCurryFileName prog = inCurrySubdir (stripSuffix prog ++ ".fcy")

--- Transforms a name of a Curry program (with or without suffix ".curry"
--- or ".lcurry") into the name of the file containing the
--- corresponding FlatCurry program.
flatCurryIntName :: String -> String
flatCurryIntName prog = inCurrySubdir (stripSuffix prog ++ ".fint")

--- I/O action which reads a FlatCurry program from a file in ".fcy" format.
--- In contrast to `readFlatCurry`, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".fcy") containing a FlatCurry program in ".fcy"
--- format and the result is a FlatCurry term representing this program.

readFlatCurryFile :: String -> IO Prog
readFlatCurryFile filename = do
  exfcy <- doesFileExist filename
  if exfcy
   then readExistingFCY filename
   else do let subdirfilename = inCurrySubdir filename
           exdirfcy <- doesFileExist subdirfilename
           if exdirfcy
            then readExistingFCY subdirfilename
            else error ("EXISTENCE ERROR: FlatCurry file '"++filename++
                        "' does not exist")
 where
   readExistingFCY fname = do
     filecontents <- readFile fname
     return (readUnqualifiedTerm ["FlatCurry","Prelude"] filecontents)

--- I/O action which returns the interface of a Curry program, i.e.,
--- a FlatCurry program containing only "Public" entities and function
--- definitions without rules (i.e., external functions).
--- The argument is the file name without suffix ".curry"
--- (or ".lcurry") and the result is a FlatCurry term representing the
--- interface of this program.

readFlatCurryInt :: String -> IO Prog
readFlatCurryInt progname = do
  existsCurry <- doesFileExist (progname++".curry")
  existsLCurry <- doesFileExist (progname++".lcurry")
  if existsCurry || existsLCurry
   then callFrontend FINT progname
   else done
  filename <- findFileInLoadPath (progname++".fint")
  readFlatCurryFile filename


--- Writes a FlatCurry program into a file in ".fcy" format.
--- The first argument must be the name of the target file
--- (with suffix ".fcy").
writeFCY :: String -> Prog -> IO ()
writeFCY file prog = writeFile file (showTerm prog)


-----------------------------------------------------------------------
--- Translates a given qualified type name into external name relative to
--- a module. Thus, names not defined in this module (except for names
--- defined in the prelude) are prefixed with their module name.
showQNameInModule :: String -> (String,String) -> String
showQNameInModule mod (qmod,name) =
  if qmod==mod || qmod=="Prelude"
  then name
  else qmod++"."++name

