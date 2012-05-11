------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing FlatCurry programs
--- in Curry (type "Prog") and an I/O action to read Curry programs and
--- transform them into this representation (function "readFlatCurry").
---
--- @author Michael Hanus
--- @version March 2001
------------------------------------------------------------------------------

module Flat where

import Directory
import Char
import Distribution

------------------------------------------------------------------------------
-- Definition of data types for representing FlatCurry programs:
-- =============================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (Prog modname imports typedecls functions opdecls translation_table)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions, translation_table: see below

data Prog = Prog String [String] [TypeDecl] [FuncDecl] [OpDecl] [Translation]


--- The data type for representing type variables.
--- They are represented by (TVar i) where i is a type variable index.
type TVarIndex = Int

--- Data type for representing definitions of algebraic data types.
--- <PRE>
--- A data type definition of the form
---
--- data t x1...xn = ...| c t1....tkc |...
---
--- is represented by the FlatCurry term
---
--- (Type t [i1,...,in] [...(Cons c kc [t1,...,tkc])...])
---
--- where each ij is the index of the type variable xj
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
--- </PRE>

data TypeDecl = Type String [TVarIndex] [ConsDecl]


--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.

data ConsDecl = Cons String Int [TypeExpr]


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)

data TypeExpr = TVar TVarIndex              -- type variable
              | FuncType TypeExpr TypeExpr  -- function type t1->t2
              | TCons String [TypeExpr]     -- type constructor application


--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- FlatCurry term (Op n fix p).

data OpDecl = Op String Fixity Int

--- Data types for the different choices for the fixity of an operator.
data Fixity = InfixOp | InfixlOp | InfixrOp


--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.

type VarIndex = Int


--- Data type for representing function declarations.
--- <PRE>
--- A function declaration in FlatCurry is a term of the form
---
---  (Func name arity type (Rule [i_1,...,i_arity] e))
---
--- and represents the function "name" with definition
---
---   name :: type
---   name x_1...x_arity = e
---
--- where each i_j is the index of the variable x_j
---
--- Note: the variable indices are unique inside each function declaration
---       and are usually numbered from 0
---
--- External functions are represented as (Func name arity type (External s))
--- where s is the external name associated to this function.
---
--- Thus, a function declaration consists of the name, arity, type, and rule.
--- </PRE>

data FuncDecl = Func String Int TypeExpr Rule


--- A rule is either a list of formal parameters together with an expression
--- or an "External" tag.

data Rule = Rule [VarIndex] Expr
          | External String

--- Data type for classifying case expressions.
--- Case expressions can be either flexible or rigid in Curry.

data CaseType = Rigid | Flex       -- type of a case expression

--- Data type for classifying combinations
--- (i.e., a function/constructor applied to some arguments).

data CombType = FuncCall     -- a call to a function
              | ConsCall     -- a call with a constructor at the top
              | PartCall     -- a partial application (i.e., FuncCall or
                             -- ConsCall with some arguments missing)

--- Data types for representing expressions.

data Expr =
   Var VarIndex                     -- variable (represented by unique index)
 | Lit Literal                      -- literal (Integer/Float/Char constant)
 | Comb CombType String [Expr]      -- application (f e1 ... en) of function/
                                    --         constructor f with n<=arity(f)
 | Apply Expr Expr                  -- application (e1 e2)
 | Constr [VarIndex] Expr           -- constraint: let x1,...,xn free in e
 | Or Expr Expr                     -- disjunction of e1 e2
 | Case CaseType Expr [BranchExpr]  -- case (rigid or flex)
 | Let [(VarIndex,Expr)] Expr       -- (recursive) let binding
 | Choice Expr                      -- committed choice
 | GuardedExpr [VarIndex] Expr Expr -- guarded expression

{-
The latter guarded expression represents conditional right-hand sides,
possibly containing extra variables, i.e.,

 (GuardedExpr [i1,...,in] c e)

represents

 "| c = e where x1,...,xn free"

i.e., c is always a constraint and each ij is the index of the variable xj.

Remarks:

1. if-then-else expressions are represented as function calls:
     (if e1 then e2 else e3)
   is represented as
     (Comb FuncCall "if_then_else" [e1,e2,e3])

2. Functions with evaluation annotation "choice" are represented
   by a rule whose right-hand side is enclosed in a "Choice".
   Furthermore, all rules of the original definition must be
   represented by GuardedExpr after pattern matching.
   Example:

      m eval choice
      m [] y = y
      m x [] = x

   is translated into:

      Rule [0,1]
           (Choice
             (Or (Case Rigid (Var 0)
                    [(Pattern (Ident "[]") []
                         (GuardedExpr [] (Comb FuncCall "success" [])
                                         (Var 1)))] )
                 (Case Rigid (Var 1)
                    [(Pattern (Ident "[]") []
                         (GuardedExpr [] (Comb FuncCall "success" [])
                                         (Var 0)))] ))

   Operational meaning of (Choice e):
   evaluate e with local search spaces and commit to the first
   (GuardedExpr [...] c ge) in e whose constraint c is satisfied
-}

--- Data types for representing branches in a case expressions.
--- <PRE>
--- Branches "(c x1...xn) -> e" in case expressions are represented as
---
---   (Branch (Pattern c [i1,...,in]) e)
---
--- where each ij is the index of the pattern variable xj, or as
---
---   (Branch (LPattern (Intc i)) e)
---
--- for integers as branch patterns (similarly for other literals
--- like float or character constants).
--- </PRE>

data BranchExpr = Branch Pattern Expr

--- Data type for representing patterns in case expressions.

data Pattern = Pattern String [VarIndex]
             | LPattern Literal

--- Data type for representing literals occurring in an expression
--- or case branch. It is either an integer, a float, or a character constant.

data Literal = Intc   Int
             | Floatc Float
             | Charc  Char


--- Data type for translating external into internal names.
--- Each module contains a translation table to translate the
--- external names (visible to the user) into internal names (used in
--- the implementation). Usually, the internal names are prefixed by
--- the name of the module (except for the prelude). Thus, the translation
--- table is a list of elements of the form
--- <CODE>(Trans name internal_name)</CODE>.

data Translation = Trans String String


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
--- This I/O action is used by the standard action <CODE>readFlatCurry</CODE>.
--- It is currently predefined only in Curry2Prolog.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end

readFlatCurryWithParseOptions :: String -> FrontendParams -> IO Prog
readFlatCurryWithParseOptions progname options = do
  existsCurry <- doesFileExist (progname++".curry")
  existsLCurry <- doesFileExist (progname++".lcurry")
  if existsCurry || existsLCurry
   then callFrontendWithParams FCY options progname
   else done
  filename <- findFileInLoadPath (progname++".fcy")
  readFlatCurryFile filename

--- I/O action which reads a FlatCurry program from a file in ".fcy" format.
--- In contrast to <CODE>readFlatCurry</CODE>, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".fcy") containing a FlatCurry program in ".fcy"
--- format and the result is a FlatCurry term representing this program.
--- It is currently predefined only in Curry2Prolog.

readFlatCurryFile :: String -> IO Prog
readFlatCurryFile fname = prim_readFlatCurryFile $## fname

prim_readFlatCurryFile :: String -> IO Prog
prim_readFlatCurryFile external


------------------------------------------------------------------------------
--- Splits an internal name into a pair of module name and name in local
--- module.
--- @param intname - the internal name of a (type) constructor or function
---                  occurring in the FlatCurry program
--- @return (mod,lname) where mod is the module name and
---         lname the local name of the parameter in this module
splitFlatModName :: String -> (String,String)
splitFlatModName name
 | isAlpha (head name)
  = let (modname,rname) = break (=='.') name in
     if rname=="" then ("Prelude",name)
                  else (modname,tail rname)
 | otherwise = ("Prelude",name)


------------------------------------------------------------------------------
