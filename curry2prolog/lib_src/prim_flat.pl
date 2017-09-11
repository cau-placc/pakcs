%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Implementation of readFlatCurryFile of module Flat
% (reading FlatCurry programs):

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).
:- (current_module(prim_readshowterm) -> true ; use_module(prim_readshowterm)). % for term en/decoding

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

prim_readFlatCurryFile(FileString,FlatProg) :-
	cp_string(FileString,FileName),
	atom_codes(FileNameAtom,FileName),
	readFlatProg(FileNameAtom,FlatProg),
	!.

readFlatProg(ProgFile,FlatProg) :-
	existsFile(ProgFile), !,
	(verbosityIntermediate
	 -> write(user_error,'>>> Reading '),
	    write(user_error,ProgFile), write(user_error,' ... '),
	    getRunTime(RT1) ; true),
	open(ProgFile,read,Stream),
	readStreamContents(Stream,FcyPrologString), !,
	readTerm(FcyPrologString,unchecked,Tail,Term),
	skipWhiteSpace(Tail,[]),
	(verbosityIntermediate
	 -> getRunTime(RT2),
	    RT is RT2-RT1,
	    write(user_error,RT), write(user_error,' ms.'), nl(user_error)
	  ; true),
	fcy2flatProg(Term,FlatProg).
readFlatProg(ProgFile,_) :-
	write('ERROR: FlatCurry file '), write(ProgFile),
	write(' not found!'), nl,
	!, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read a fcy file and translate it into old Flat format

fcy2flatProg('Prog'(ModName,Imps,Types,Funcs,Ops),
 	     'Flat.Prog'(ModName,Imps,FTypes,FFuncs,FOps,Trans)) :-
	fcy2flatTypes(Types,FTypes),
	map2M(user:fcy2flatFunc,Funcs,FFuncs),
	map2M(user:fcy2flatOp,Ops,FOps),
	fcyGetPublicTypes(Types,PTNames),
	fcyGetPublicFuncs(Funcs,PFNames),
	append(PTNames,PFNames,PNames),
	map2M(user:fcy2flatTrans,PNames,Trans).

fcy2flatTypes([],[]).
fcy2flatTypes(['Type'(TName,_,TParams,Cons)|Types],
	      ['Flat.Type'(FTName,TParams,FCons)|FTypes]) :-
	fcy2flatName(TName,FTName),
	map2M(user:fcy2flatCons,Cons,FCons),
	fcy2flatTypes(Types,FTypes).
fcy2flatTypes(['TypeSyn'(_,_,_,_)|Types],FTypes) :- % ignore type synonyms
	fcy2flatTypes(Types,FTypes).

fcy2flatCons('Cons'(CName,Arity,_,Types),'Flat.Cons'(FCName,Arity,FTypes)) :-
	fcy2flatName(CName,FCName),
	map2M(user:fcy2flatTypeExpr,Types,FTypes).

fcy2flatTypeExpr('TVar'(I),'Flat.TVar'(I)).
fcy2flatTypeExpr('FuncType'(T1,T2),'Flat.FuncType'(FT1,FT2)) :-
	fcy2flatTypeExpr(T1,FT1),
	fcy2flatTypeExpr(T2,FT2).
fcy2flatTypeExpr('TCons'(TName,TEs),'Flat.TCons'(FTName,FTEs)) :-
	fcy2flatName(TName,FTName),
	map2M(user:fcy2flatTypeExpr,TEs,FTEs).

fcyGetPublicTypes([],[]).
fcyGetPublicTypes(['Type'(TName,'Public',_,Cons)|Types],PNames) :- !,
	fcyGetPublicConstrs(Cons,CNames),
	fcyGetPublicTypes(Types,TNames),
	append([TName|CNames],TNames,PNames).
fcyGetPublicTypes(['Type'(_,_,_,Cons)|Types],PNames) :- !,
	fcyGetPublicConstrs(Cons,CNames),
	fcyGetPublicTypes(Types,TNames),
	append(CNames,TNames,PNames).
fcyGetPublicTypes(['TypeSyn'(TName,'Public',_,_)|Types],[TName|TNames]) :- !,
	fcyGetPublicTypes(Types,TNames).
fcyGetPublicTypes([_|Types],TNames) :-
	fcyGetPublicTypes(Types,TNames).

fcyGetPublicConstrs([],[]).
fcyGetPublicConstrs(['Cons'(CName,_,'Public',_)|Cs],[CName|CNames]) :- !,
	fcyGetPublicConstrs(Cs,CNames).
fcyGetPublicConstrs([_|Cs],CNames) :-
	fcyGetPublicConstrs(Cs,CNames).

fcyGetPublicFuncs([],[]).
fcyGetPublicFuncs(['Func'(FName,_,'Public',_,_)|Funcs],[FName|FNames]) :-
	!, fcyGetPublicFuncs(Funcs,FNames).
fcyGetPublicFuncs([_|Funcs],FNames) :-
	fcyGetPublicFuncs(Funcs,FNames).

fcy2flatFunc('Func'(FName,Arity,_,Type,Rule),
	     'Flat.Func'(FFName,Arity,FType,FRule)) :-
	fcy2flatName(FName,FFName),
	fcy2flatTypeExpr(Type,FType),
	fcy2flatTypeRule(Rule,FRule).

fcy2flatTypeRule('External'(N),'Flat.External'(N)).
fcy2flatTypeRule('Rule'(Args,Exp),'Flat.Rule'(Args,FExp)) :-
	fcy2flatExpr(Exp,FExp).

fcy2flatLit('Intc'(I),'Flat.Intc'(I)).
fcy2flatLit('Floatc'(F),'Flat.Floatc'(F)).
fcy2flatLit('Charc'(C),'Flat.Charc'(C)).

fcy2flatExpr('Var'(I),'Flat.Var'(I)).
fcy2flatExpr('Lit'(L),'Flat.Lit'(FL)) :- fcy2flatLit(L,FL).
fcy2flatExpr('Comb'('FuncCall',QName,[E]),'Flat.Choice'(FE)) :-
	fcy2flatQName(QName,"commit"), !,
	fcy2flatExpr(E,FE).
fcy2flatExpr('Comb'('FuncCall',QName,[E1,E2]),'Flat.Apply'(FE1,FE2)) :-
	fcy2flatQName(QName,"apply"), !,
	fcy2flatExpr(E1,FE1),
	fcy2flatExpr(E2,FE2).
fcy2flatExpr('Comb'('FuncCall',QName,[E1,E2]),
	    'Flat.GuardedExpr'([],'Flat.Constr'([],FE1),FE2)) :-
	fcy2flatQName(QName,"cond"), !,
	fcy2flatExpr(E1,FE1),
	fcy2flatExpr(E2,FE2).
fcy2flatExpr('Free'(Vars,'Comb'('FuncCall',QName,[E1,E2])),
	    'Flat.GuardedExpr'(Vars,'Flat.Constr'([],FE1),FE2)) :-
	fcy2flatQName(QName,"cond"), !,
	fcy2flatExpr(E1,FE1),
	fcy2flatExpr(E2,FE2).
fcy2flatExpr('Comb'(CT,QName,Es),'Flat.Comb'(FCT,FQName,FEs)) :-
	fcy2flatName(QName,FQName),
	fcy2flatCombType(CT,FCT),
	map2M(user:fcy2flatExpr,Es,FEs).
fcy2flatExpr('Free'(Vars,Exp),'Flat.Constr'(Vars,FExp)) :-
	fcy2flatExpr(Exp,FExp).
fcy2flatExpr('Or'(E1,E2),'Flat.Or'(FE1,FE2)) :-
	fcy2flatExpr(E1,FE1),
	fcy2flatExpr(E2,FE2).
fcy2flatExpr('Case'(CT,Exp,Branches),'Flat.Case'(FCT,FExp,FBranches)) :-
	fcy2flatCaseType(CT,FCT),
	fcy2flatExpr(Exp,FExp),
	map2M(user:fcy2flatBranch,Branches,FBranches).
fcy2flatExpr('Let'(Bindings,Exp),'Flat.Let'(FBindings,FExp)) :-
        map2M(user:fcy2flatBinding,Bindings,FBindings),
        fcy2flatExpr(Exp,FExp).
fcy2flatExpr(E,E) :-
	write(user_error,'*** Internal error in FlatCurry file: Illegal expression "'),
	write(user_error,E),
	write(user_error,'"!'), nl(user_error),
	pleaseReport, fail.

fcy2flatBinding('Prelude.(,)'(Var,Exp),'Prelude.(,)'(Var,FExp)) :-
	fcy2flatExpr(Exp,FExp).

fcy2flatCaseType('Rigid','Flat.Rigid').
fcy2flatCaseType('Flex','Flat.Flex').

fcy2flatCombType('FuncCall','Flat.FuncCall').
fcy2flatCombType('ConsCall','Flat.ConsCall').
fcy2flatCombType('FuncPartCall'(_),'Flat.PartCall').
fcy2flatCombType('ConsPartCall'(_),'Flat.ConsCall').

fcy2flatBranch('Branch'('LPattern'(L),Exp),'Flat.Branch'('Flat.LPattern'(FL),FExp)) :-
	fcy2flatLit(L,FL), fcy2flatExpr(Exp,FExp).
fcy2flatBranch('Branch'('Pattern'(QName,Vars),Exp),
	       'Flat.Branch'('Flat.Pattern'(FQName,Vars),FExp)) :-
	fcy2flatName(QName,FQName),
	fcy2flatExpr(Exp,FExp).


fcy2flatOp('Op'(OName,Fix,Int),'Flat.Op'(FOName,FlatFix,Int)) :-
	fcy2flatName(OName,FOName),
	fcy2flatFixity(Fix,FlatFix).

fcy2flatFixity('InfixOp','Flat.InfixOp').
fcy2flatFixity('InfixlOp','Flat.InfixlOp').
fcy2flatFixity('InfixrOp','Flat.InfixrOp').

fcy2flatTrans('Prelude.(,)'(Mod,Name),'Flat.Trans'(Name,Name)) :-
	cp_string(Mod,"Prelude"), !.
fcy2flatTrans('Prelude.(,)'(Mod,Name),'Flat.Trans'(Name,FName)) :-
	char_int(Dot,46),
	append(Mod,[Dot|Name],FName).

fcy2flatQName('Prelude.(,)'(Mod,Name),FName) :-
	cp_string(Mod,"Prelude"), !,
	cp_string(Name,FName).
fcy2flatQName('Prelude.(,)'(Mod,Name),FMName) :-
	cp_string(Mod,FMod),
	cp_string(Name,FName),
	append(FMod,[46|FName],FMName).

fcy2flatName('Prelude.(,)'(Mod,Name),Name) :-
	cp_string(Mod,"Prelude"), !.
fcy2flatName('Prelude.(,)'(Mod,Name),FName) :-
	char_int(Dot,46),
	append(Mod,[Dot|Name],FName).




