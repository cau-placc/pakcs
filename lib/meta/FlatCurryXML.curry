------------------------------------------------------------------------------
--- This library contains functions to convert FlatCurry programs
--- into corresponding XML expressions and vice versa.
--- This can be used to store Curry programs in a way independent
--- from PAKCS or to use the PAKCS back end by other systems.
---
--- @author Sebastian Fischer
--- @version March 2006
------------------------------------------------------------------------------
module FlatCurryXML (

  flatCurry2XmlFile, flatCurry2Xml, xmlFile2FlatCurry, xml2FlatCurry

  ) where

import XML
import XmlConv
import FlatCurry

-- URL for the FlatCurry DTD:
flatCurryDtd = "http://www.informatik.uni-kiel.de/~curry/flatcurrynew.dtd"

--- Transforms a FlatCurry program term into a corresponding XML file.
flatCurry2XmlFile :: Prog -> String -> IO ()
flatCurry2XmlFile flatprog filename = writeFile filename $
  showXmlDocWithParams [DtdUrl flatCurryDtd] (flatCurry2Xml flatprog)

--- Transforms a FlatCurry program term into a corresponding XML expression.
flatCurry2Xml :: Prog -> XmlExp
flatCurry2Xml = xmlShow cProg

--- Reads an XML file with a FlatCurry program and returns
--- the FlatCurry program.
xmlFile2FlatCurry :: String -> IO Prog
xmlFile2FlatCurry filename = readXmlFile filename >>= return . xml2FlatCurry

--- Transforms an XML term into a FlatCurry program.
xml2FlatCurry :: XmlExp -> Prog
xml2FlatCurry = xmlRead cProg

-- FlatCurry XML converter specification:
cProg 	      = eSeq5 	"prog" Prog cModname cImports cTypes cFuncs cOps
cModname      = eString "module"
cImports      = eRep 	"import" (eString "module")
cTypes	      = eRep	"types"	cType
cType	      = eSeq4	"type" Type cQName cVis cTParams (rep cConsDecl)
	      ! eSeq4	"typesyn" TypeSyn cQName cVis cTParams cTypeExpr
cQName	      = seq2 	(\a b -> (a,b)) (aString "module") (aString "name")
cVis	      = adapt	(b2v,v2b) (aBool "visibility" "public" "private")
b2v b	      = if b then Public else Private
v2b v	      = v==Public
cTParams      = eRep	"params" (eInt "tvar")
cConsDecl     = eSeq4	"cons" Cons cQName cArity cVis (rep cTypeExpr)
cArity	      = aInt	"arity"
cTypeExpr     = eSeq2	"functype" FuncType cTypeExpr cTypeExpr
	      ! eSeq2	"tcons" TCons cQName (rep cTypeExpr)
	      ! eSeq1	"tvar" TVar int
cFuncs	      = eRep	"functions" cFunc
cFunc	      = eSeq5	"func" Func cQName cArity cVis cTypeExpr cRule
cRule	      = eSeq2	"rule" Rule cLHS cRHS
	      ! eSeq1	"external" External string
cLHS	      = element	"lhs" cVars
cRHS	      = element	"rhs" cExpr
cVars	      = rep	cVar
cVar	      = eInt	"var"
cExpr	      = eSeq1	"var" Var int
	      ! eSeq1	"lit" Lit cLit
	      ! eSeq2	"funccall" fc cQName cExps
	      ! eSeq2	"conscall" cc cQName cExps
	      ! eSeq3	"funcpartcall" pfc cQName cMissing cExps
	      ! eSeq3	"conspartcall" pcc cQName cMissing cExps
	      ! eSeq2	"free" Free (element "freevars" cVars) cExpr
	      ! eSeq2	"or" Or cExpr cExpr
              ! eSeq2	"case" cr cExpr (rep cBranch)
	      ! eSeq2	"fcase" cf cExpr (rep cBranch)
	      ! eSeq2	"letrec" Let (rep cBind) cExpr
cLit	      = eSeq1	"intc" Intc int
	      ! eSeq1	"floatc" Floatc float
	      ! eSeq1	"charc" Charc (adapt (chr,ord) int)
fc	      = Comb	FuncCall
cc	      = Comb	ConsCall
pfc n m	      = Comb	(FuncPartCall m) n
pcc n m	      = Comb	(ConsPartCall m) n
cExps	      = rep	cExpr
cMissing      = aInt	"missing"
cr	      = Case	Rigid
cf	      = Case	Flex
cBranch	      = eSeq2	"branch" Branch cPat cExpr
cPat	      = eSeq2	"pattern" Pattern cQName cVars
	      ! eSeq1	"lpattern" LPattern cLit
cBind	      = eSeq2	"binding" (\a b -> (a,b)) cVar cExpr
cOps	      = eRep	"operators" cOp
cOp	      = eSeq3	"op" Op cQName cFixity (aInt "prec")
cFixity	      = adapt	(rf,show) (aString "fixity")
rf "InfixOp"  = InfixOp
rf "InfixlOp" = InfixlOp
rf "InfixrOp" = InfixrOp
