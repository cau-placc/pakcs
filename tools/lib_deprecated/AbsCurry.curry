------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Curry programs
--- in Curry (type "CProg").
---
--- NOTE: The I/O actions to read Curry programs and transform them
---       into this abstract representation can be found in the module
---       AbsCurryIO.
---
--- @author Michael Hanus
--- @version May 2004
------------------------------------------------------------------------------

module AbsCurry where

------------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ==================================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (CProg modname imports typedecls functions opdecls)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions: see below

data CProg = CProg String [String] [CTypeDecl] [CFuncDecl] [COpDecl]


-- Data type to specify the visibility of various entities.

data CVisibility = CExported  -- exported entity
                 | CPrivate   -- private entity


--- The data type for representing type variables.
--- They are represented by (CTVar i) where i is a type variable index.
type CTVarIndex = Int

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
--- <PRE>
--- A data type definition of the form
---
--- data t x1...xn = ...| c t1....tkc |...
---
--- is represented by the Curry term
---
--- (CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])
---
--- where each ij is the index of the type variable xj
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
--- </PRE>

data CTypeDecl = CType    String CVisibility [CTVarIndex] [CConsDecl]
               | CTypeSyn String CVisibility [CTVarIndex] CTypeExpr


--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.

data CConsDecl = CCons String Int CVisibility [CTypeExpr]


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)

data CTypeExpr =
    CTVar CTVarIndex                 -- type variable
  | CFuncType CTypeExpr CTypeExpr    -- function type t1->t2
  | CTCons String String [CTypeExpr] -- type constructor application
                                     -- (CTCons module name arguments)


--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).

data COpDecl = COp String CFixity Int

data CFixity = CInfixOp   -- non-associative infix operator
             | CInfixlOp  -- left-associative infix operator
             | CInfixrOp  -- right-associative infix operator


--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.

type CVarIndex = Int


--- Data type for representing function declarations.
--- <PRE>
--- A function declaration in FlatCurry is a term of the form
---
---  (CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))
---
--- and represents the function "name" with definition
---
---   name :: type
---   rule1
---   ...
---   rulek
---
--- Note: the variable indices are unique inside each rule
---
--- External functions are represented as (CFunc name arity type (CExternal s))
--- where s is the external name associated to this function.
---
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules.
--- </PRE>

data CFuncDecl = CFunc String Int CVisibility CTypeExpr CRules


--- A rule is either a list of formal parameters together with an expression
--- (i.e., a rule in flat form), a list of general program rules with
--- an evaluation annotation, or it is externally defined

data CRules = CRules CEvalAnnot [CRule]
            | CExternal String

--- Data type for classifying evaluation annotations for functions.
--- They can be either flexible (default), rigid, or choice.

data CEvalAnnot = CFlex | CRigid | CChoice

--- The most general form of a rule. It consists of a list of patterns
--- (left-hand side), a list of guards ("success" if not present in the
--- source text) with their corresponding right-hand sides, and
--- a list of local declarations.
data CRule = CRule [CPattern] [(CExpr,CExpr)] [CLocalDecl]

--- Data type for representing local (let/where) declarations
data CLocalDecl =
     CLocalFunc CFuncDecl                   -- local function declaration
   | CLocalPat  CTypeExpr CPattern CExpr    -- local pattern declaration
                [CLocalDecl]                -- (with type of the pattern)
   | CLocalVar  CTypeExpr CVarIndex         -- local free variable declaration
                                            -- (with type of the pattern)

--- Data type for representing Curry expressions.

data CExpr =
   CVar CVarIndex                   -- variable (represented by unique index)
 | CLit CLiteral                    -- literal (Integer/Float/Char constant)
 | CSymbol String String            -- a defined symbol with module and name
 | CApply CExpr CExpr               -- application (e1 e2)
 | CLambda [CPattern] CExpr         -- lambda abstraction
 | CLetDecl [CLocalDecl] CExpr      -- local let declarations
 | CDoExpr [CStatement]             -- do expression
 | CListComp CExpr [CStatement]     -- list comprehension
 | CCase CExpr [CBranchExpr]        -- case expression

--- Data type for representing statements in do expressions and
--- list comprehensions.

data CStatement = CSExpr CExpr          -- an expression (I/O action or boolean)
                | CSPat CPattern CExpr  -- a pattern definition
                | CSLet [CLocalDecl]    -- a local let declaration

--- Data type for representing pattern expressions.

data CPattern =
   CPVar CVarIndex          -- variable (represented by unique index)
 | CPLit CLiteral           -- literal (Integer/Float/Char constant)
 | CPComb String String [CPattern] -- application (m.c e1 ... en) of n-ary
                                   -- constructor m.c (CPComb m c [e1,...,en])

--- Data type for representing branches in case expressions.

data CBranchExpr = CBranch CPattern CExpr

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.

data CLiteral = CIntc   Int
              | CFloatc Float
              | CCharc  Char

------------------------------------------------------------------------------
