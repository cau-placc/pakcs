------------------------------------------------------------------------------
--- This module supports functions to read Curry program in the
--- old AbsCurry representation (based on reading files in the
--- new AbstractCurry format).
---
--- @author Michael Hanus
--- @version Nevember 2006
------------------------------------------------------------------------------

module AbsCurryIO(readCurry,readUntypedCurry,
                  readCurryWithParseOptions,
                  readUntypedCurryWithParseOptions,
                  readAbsCurryFile) where

import AbstractCurry
import qualified AbsCurry as AC
import Distribution(FrontendParams)

------------------------------------------------------------------------------
--- Transforms new AbstractCurry representation into old AbsCurry.

abstract2absCurry :: CurryProg -> AC.CProg
abstract2absCurry (CurryProg mod imps types funcs ops) =
     AC.CProg mod imps
              (map transTypeDecl types)
              (map transFunc funcs)
              (map transOpDecl ops)

transVis Public  = AC.CExported
transVis Private = AC.CPrivate

transTypeDecl (CType (_,name) vis tvars constrs) =
  AC.CType name (transVis vis) (map fst tvars) (map transConsDecl constrs)
transTypeDecl (CTypeSyn (_,name) vis tvars texp) =
  AC.CTypeSyn name (transVis vis) (map fst tvars) (transTExp texp)

transConsDecl (CCons (_,name) arity vis texps) =
  AC.CCons name arity (transVis vis) (map transTExp texps)

transTExp (CTVar (i,_)) = AC.CTVar i
transTExp (CFuncType te1 te2) = AC.CFuncType (transTExp te1) (transTExp te2)
transTExp (CTCons (mod,name) texps) = AC.CTCons mod name (map transTExp texps)

transFunc (CFunc (_,name) arity vis texp rules) =
  AC.CFunc name arity (transVis vis) (transTExp texp) (transRules rules)

transRules (CRules ea rules) = AC.CRules (transEval ea) (map transRule rules)
transRules (CExternal extname) = AC.CExternal extname

transEval CFlex   = AC.CFlex
transEval CRigid  = AC.CRigid
transEval CChoice = AC.CChoice

transRule (CRule patts grhs locals) =
  AC.CRule (map transPattern patts) (map transGRHS grhs)
           (map transLocal locals)

transLocal (CLocalFunc func) = AC.CLocalFunc (transFunc func)
transLocal (CLocalPat patt exp locals) =
  AC.CLocalPat (AC.CTCons "Prelude" "untyped" [])
               (transPattern patt) (transExp exp)
               (map transLocal locals)
transLocal (CLocalVar (i,_)) =
  AC.CLocalVar (AC.CTCons "Prelude" "untyped" []) i

transGRHS (guard,rhs) = (transExp guard, transExp rhs)

transExp (CVar (i,_)) = AC.CVar i
transExp (CLit l) = AC.CLit (transLit l)
transExp (CSymbol (mod,name)) = AC.CSymbol  mod name
transExp (CApply e1 e2) = AC.CApply (transExp e1) (transExp e2)
transExp (CLambda ps exp) = AC.CLambda (map transPattern ps) (transExp exp)
transExp (CLetDecl locals exp) =
                         AC.CLetDecl (map transLocal locals) (transExp exp)
transExp (CDoExpr stats) = AC.CDoExpr (map transStat stats)
transExp (CListComp exp stats) =
                     AC.CListComp (transExp exp) (map transStat stats)
transExp (CCase exp brs) = AC.CCase (transExp exp) (map tbr brs)
 where tbr (CBranch p e) = AC.CBranch (transPattern p) (transExp e)

transStat (CSExpr exp)     = AC.CSExpr (transExp exp)
transStat (CSPat patt exp) = AC.CSPat (transPattern patt) (transExp exp)
transStat (CSLet locals)   = AC.CSLet (map transLocal locals)

transPattern pat = case pat of
  CPVar (i,_)             -> AC.CPVar i
  CPLit lit               -> AC.CPLit (transLit lit)
  CPComb (mod,name) patts -> AC.CPComb mod name (map transPattern patts)
  _ -> error ("AbsCurryIO.abstract2absCurry: cannot pattern into old AbsCurry:\n" ++
              show pat)

transLit (CIntc i)   = AC.CIntc i
transLit (CFloatc f) = AC.CFloatc f
transLit (CCharc c)  = AC.CCharc c

transOpDecl (COp (_,name) fix prec) = AC.COp name (transFix fix) prec

transFix CInfixOp  = AC.CInfixOp
transFix CInfixlOp = AC.CInfixlOp
transFix CInfixrOp = AC.CInfixrOp


------------------------------------------------------------------------------
--- I/O action which parses a Curry program and returns the corresonding
--- typed Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry" and the result is a Curry term representing this
--- program.

readCurry :: String -> IO AC.CProg
readCurry prog = AbstractCurry.readCurry prog >>= return . abstract2absCurry

--- I/O action which parses a Curry program and returns the corresonding
--- untyped Abstract Curry program.
--- Thus, the argument is the file name without suffix ".curry"
--- or ".lcurry" and the result is a Curry term representing this
--- program.

readUntypedCurry :: String -> IO AC.CProg
readUntypedCurry prog =
       AbstractCurry.readUntypedCurry prog >>= return . abstract2absCurry

--- I/O action which reads a typed Curry program from a file (with extension
--- ".acy") with respect to some parser options.
--- This I/O action is used by the standard action <CODE>readCurry</CODE>.
--- It is currently predefined only in Curry2Prolog.
--- @param progfile - the program file name (without suffix ".curry")
--- @param options - parameters passed to the front end

readCurryWithParseOptions :: String -> FrontendParams -> IO AC.CProg
readCurryWithParseOptions progname options =
  AbstractCurry.readCurryWithParseOptions progname options >>=
  return . abstract2absCurry

--- I/O action which reads an untyped Curry program from a file (with extension
--- ".uacy") with respect to some parser options. For more details
--- see function 'readCurryWithParseOptions'
readUntypedCurryWithParseOptions :: String -> FrontendParams -> IO AC.CProg
readUntypedCurryWithParseOptions progname options =
  AbstractCurry.readUntypedCurryWithParseOptions progname options >>=
  return . abstract2absCurry

--- I/O action which reads an Abstract Curry program from a file in ".acy"
--- format. In contrast to <CODE>readCurry</CODE>, this action does not parse
--- a source program. Thus, the argument must be the name of an existing
--- file (with suffix ".acy") containing an AbstractCurry program in ".acy"
--- format and the result is a Curry term representing this program.

readAbsCurryFile :: String -> IO AC.CProg
readAbsCurryFile filename =
  AbstractCurry.readAbstractCurryFile filename >>= return . abstract2absCurry

