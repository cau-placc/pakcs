------------------------------------------------------------------------------
--- This library contains functions to convert FlatCurry programs
--- into corresponding XML expressions and vice versa.
--- This can be used to store Curry programs in a way independent
--- from PAKCS or to use the PAKCS back end by other systems.
---
--- The library provides the following functions:
--- <PRE>
--- "flatCurry2XmlFile": transform a FlatCurry program into XML file
--- "flatCurry2Xml":     transform a FlatCurry program into XML expression
--- "xmlFile2FlatCurry": read an XML file and return the FlatCurry program
--- "xml2FlatCurry":     transform an XML expression into a FlatCurry program
--- </PRE>
---
--- @author Michael Hanus
--- @version March 2001
------------------------------------------------------------------------------

module FlatXML(flatCurry2XmlFile,flatCurry2Xml,
               xmlFile2FlatCurry,xml2FlatCurry)  where

import Flat
import FlatTools
import XML
import Read

-- URL for the FlatCurry DTD:
flatCurryDtd = "http://www.informatik.uni-kiel.de/~curry/flatcurry.dtd"

------------------------------------------------------------------------------
-- Transforming FlatCurry programs into corresponding XML terms:
------------------------------------------------------------------------------

--- Transforms a FlatCurry program term into a corresponding XML file.
flatCurry2XmlFile :: Prog -> String -> IO ()
flatCurry2XmlFile flatprog filename = writeFile filename
  (showXmlDocWithParams [DtdUrl flatCurryDtd] (flatCurry2Xml flatprog))

--- Transforms a FlatCurry program term into a corresponding XML expression.
flatCurry2Xml :: Prog -> XmlExp
flatCurry2Xml (Prog modname imports types funcs ops table) =
  xml "prog"
   [xml "module" [xtxt modname],
    xml "import" (map (\s->xml "module" [xtxt s]) imports),
    xml "types" (map xmlShowType types),
    xml "functions" (map xmlShowFunc funcs),
    xml "operators" (map xmlShowOp ops),
    xml "translation" (map xmlShowTrans table)]

xmlShowType (Type name tpars consdecls) =
  XElem "type" [("name",name)]
        ([xml "params" (map xmlShowTVar tpars)] ++ map xmlShowCons consdecls)

xmlShowCons (Cons cname arity types) =
  XElem "cons" [("name",cname),("arity",show arity)]
        (map xmlShowTypeExpr types)

xmlShowTypeExpr (FuncType t1 t2) =
  xml "functype" [xmlShowTypeExpr t1,xmlShowTypeExpr t2]
xmlShowTypeExpr (TCons tc ts) =
  XElem "tcons" [("name",tc)] (map xmlShowTypeExpr ts)
xmlShowTypeExpr (TVar n) = xmlShowTVar n

xmlShowTVar i = xml "tvar" [xtxt (show i)]

xmlShowFunc (Func name arity ftype rl) =
  XElem "func" [("name",name),("arity",show arity)]
        [xmlShowTypeExpr ftype,xmlShowRule rl]

xmlShowRule (Rule params expr) =
  xml "rule" [xml "lhs" (map xmlShowVar params),
              xml "rhs" [xmlShowExpr expr]]
xmlShowRule (External name) = xml "external" [xtxt name]

xmlShowVar i = xml "var" [xtxt (show i)]

xmlShowExpr (Var n) = xmlShowVar n
xmlShowExpr (Lit l) = xml "lit" [xmlShowLit l]
xmlShowExpr (Comb ctype cf es) =
  XElem "comb" [("type",show ctype),("name",cf)] (map xmlShowExpr es)
xmlShowExpr (Apply e1 e2) =
  xml "apply" [xmlShowExpr e1,xmlShowExpr e2]
xmlShowExpr (Constr xs e) =
  xml "constr" [xml "freevars" (map xmlShowVar xs), xmlShowExpr e]
xmlShowExpr (Or e1 e2) =
  xml "or" [xmlShowExpr e1,xmlShowExpr e2]
xmlShowExpr (Case ctype e cs) =
  XElem "case" [("type",show ctype)] ([xmlShowExpr e] ++ map xmlShowBranch cs)
xmlShowExpr (GuardedExpr xs e1 e2) =
  xml "guardedexpr" [xml "freevars" (map xmlShowVar xs),
                     xmlShowExpr e1, xmlShowExpr e2]
xmlShowExpr (Choice e) =
  xml "choice" [xmlShowExpr e]

xmlShowLit (Intc   i) = xml "intc"   [xtxt (show i)]
xmlShowLit (Floatc f) = xml "floatc" [xtxt (show f)]
xmlShowLit (Charc  c) = xml "charc"  [xtxt (show (ord c))]

xmlShowBranch (Branch (Pattern cons xs) e) =
  xml "branch" [XElem "pattern" [("name",cons)] (map xmlShowVar xs),
                xmlShowExpr e]
xmlShowBranch (Branch (LPattern lit) e) =
  xml "branch" [xml "lpattern" [xmlShowLit lit], xmlShowExpr e]

xmlShowOp (Op name fix prec) =
  XElem "op" [("fixity",show fix),("prec",show prec)] [xtxt name]

xmlShowTrans (Trans n intn) =
 xml "trans" [xml "name" [xtxt n], xml "intname" [xtxt intn]]


------------------------------------------------------------------------------
-- Transforming XML terms into corresponding FlatCurry programs:
------------------------------------------------------------------------------

--- Reads an XML file with a FlatCurry program and returns
--- the FlatCurry program.
xmlFile2FlatCurry :: String -> IO Prog
xmlFile2FlatCurry filename =
  do xexp <- readXmlFile filename
     return (xml2FlatCurry xexp)

--- Transforms an XML term into a FlatCurry program.
xml2FlatCurry :: XmlExp -> Prog
xml2FlatCurry
 (XElem "prog" []
   [XElem "module"      [] xmodname,
    XElem "import"      [] ximports,
    XElem "types"       [] xtypes,
    XElem "functions"   [] xfunctions,
    XElem "operators"   [] xoperators,
    XElem "translation" [] xtable    ]) =
  Prog (flatx2String xmodname)
       (map (\(XElem "module" [] xim) -> flatx2String xim) ximports)
       (map (\(XElem "type" [("name",tname)]
                (XElem "params" [] xtvars : xconstructors))
             -> Type tname
                     (map (\(XElem "tvar" [] xtvar)
                            -> readNat (flatx2String xtvar)) xtvars)
                     (map (\(XElem "cons" [("name",xcn),("arity",xar)] xtexps)
                            -> Cons xcn (readNat xar)
                                (map flatx2texp xtexps))
                          xconstructors))
            xtypes)
       (map (\(XElem "func" [("name",fname),("arity",farity)] [xftype,xfbody])
             -> Func fname (readNat farity) (flatx2texp xftype)
                     (flatx2FunBody xfbody))
            xfunctions)
       (map (\(XElem "op" [("fixity",xfix),("prec",xprec)] xop)
             -> Op (flatx2String xop) (flatx2Fixity xfix) (readNat xprec))
            xoperators)
       (map (\(XElem "trans" []
                [XElem "name" [] xtn, XElem "intname" [] xtin])
             -> Trans (flatx2String xtn) (flatx2String xtin)) xtable)

flatx2FunBody (XElem "external" [] xename) = External (flatx2String xename)
flatx2FunBody (XElem "rule" [] [XElem "lhs" [] xvars,
                                XElem "rhs" [] [xrhs]]) =
   Rule (map flatx2var xvars) (flatx2exp xrhs)

flatx2var :: XmlExp -> VarIndex
flatx2var (XElem "var" [] xvar) = readNat (flatx2String xvar)

flatx2exp :: XmlExp -> Expr
flatx2exp (XElem "var" [] xvar) = Var (readNat (flatx2String xvar))
flatx2exp (XElem "lit" [] [xlit]) = Lit (flatx2lit xlit)
flatx2exp (XElem "comb" [("type",ctype),("name",cname)] xexps) =
  Comb (flatx2CombType ctype) cname (map flatx2exp xexps)
flatx2exp (XElem "apply" [] [xexp1,xexp2]) =
  Apply (flatx2exp xexp1) (flatx2exp xexp2)
flatx2exp (XElem "constr" [] [XElem "freevars" [] xvars, xexp]) =
  Constr (map flatx2var xvars) (flatx2exp xexp)
flatx2exp (XElem "or" [] [xexp1,xexp2]) =
  Or (flatx2exp xexp1) (flatx2exp xexp2)
flatx2exp (XElem "case" [("type",ctype)] (xexp : xbranches)) =
  Case (flatx2CaseType ctype) (flatx2exp xexp) (map flatx2branch xbranches)
 where flatx2CaseType "Rigid" = Rigid
       flatx2CaseType "Flex"  = Flex
flatx2exp (XElem "guardedexpr" [] [XElem "freevars" [] xvars, xexp1, xexp2]) =
  GuardedExpr (map flatx2var xvars) (flatx2exp xexp1) (flatx2exp xexp2)
flatx2exp (XElem "choice" [] [xexp]) = Choice (flatx2exp xexp)
flatx2exp (XElem "let" [] xbindings) =
  let (bindings,exp) = flatx2let xbindings
   in Let bindings exp
flatx2exp (XElem "letrec" [] xbindings) =
  let (bindings,exp) = flatx2let xbindings
   in Let bindings exp

flatx2let [xexp] = ([],flatx2exp xexp)
flatx2let (XElem "binding" [] [XElem "var" [] xvar, xexp] : xb:xbs) =
  let (bindings,exp) = flatx2let (xb:xbs)
   in ((readNat (flatx2String xvar), flatx2exp xexp) : bindings, exp)

flatx2branch (XElem "branch" [] [XElem "pattern" [("name",cons)] xvars,xexp]) =
  Branch (Pattern cons (map flatx2var xvars)) (flatx2exp xexp)
flatx2branch (XElem "branch" [] [XElem "lpattern" [] [xlit], xexp]) =
  Branch (LPattern (flatx2lit xlit)) (flatx2exp xexp)
flatx2branch (XElem "branch" [] [XElem "hpattern" _ _,_]) =
  error "Higher-order patterns not supported in this version of FlatCurry!"

flatx2lit :: XmlExp -> Literal
flatx2lit (XElem "intc" [] xintc) = Intc (readNat (flatx2String xintc))
flatx2lit (XElem "floatc" [] _) =
            error "Reading of floats not yet implemented!"
flatx2lit (XElem "charc" [] xintc) = Charc (chr (readNat (flatx2String xintc)))

flatx2texp :: XmlExp -> TypeExpr
flatx2texp (XElem "tvar" [] xtvar) = TVar (readNat (flatx2String xtvar))
flatx2texp (XElem "functype" [] [xtexp1,xtexp2]) =
   FuncType (flatx2texp xtexp1) (flatx2texp xtexp2)
flatx2texp (XElem "tcons" [("name",tcname)] xtexps) =
   TCons tcname (map flatx2texp xtexps)

flatx2Fixity "InfixOp"  = InfixOp
flatx2Fixity "InfixlOp" = InfixlOp
flatx2Fixity "InfixrOp" = InfixrOp

flatx2CombType "FuncCall" = FuncCall
flatx2CombType "ConsCall" = ConsCall
flatx2CombType "PartCall" = PartCall

flatx2String [] = ""
flatx2String [XText s] = s

------------------------------------------------------------------------------
