%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reading .fcy file and translate it into internal old FLC format

:- module(readFlcFromFcy,[readFlcFromFcy/2]).

:- use_module(prologbasics).
:- use_module(basics).
:- prolog(sicstus) -> use_module('lib_src/prim_readshowterm')  % for term en/decoding
                    ; use_module('libswi/prim_readshowterm').

readFlcFromFcy(FileName,FlatProg) :-
	(verbosityIntermediate
	 -> write(user_error,'>>> Reading '),
	    write(user_error,FileName), write(user_error,' ... '),
	    getRunTime(RT1) ; true),
	open(FileName,read,Stream),
	readStreamContents(Stream,FcyPrologString),
	readTerm(FcyPrologString,unchecked,Tail,Term),
	skipWhiteSpace(Tail,[]),
	(verbosityIntermediate
	 -> getRunTime(RT2),
	    RT is RT2-RT1,
	    write(user_error,RT), write(user_error,' ms.'), nl(user_error)
	  ; true),
	fcy2flcProg(Term,FlatProg), !.
readFlcFromFcy(FileName,_) :-
	writeErr('ERROR in readFlcFromFcy during reading of "'),
	writeErr(FileName), writeErr('"!'),
	nlErr, fail.

fcy2flcProg('Prog'(ModName,Imps,Types,Funcs,Ops),
	    'Prog'(FModName,FImps,FTypes,FFuncs,FOps)) :-
        cp_string(ModName,FModName),
        map2M(basics:cp_string,Imps,FImps),
	fcy2flcTypes(Types,FTypes),
	map2M(readFlcFromFcy:fcy2flcFunc,Funcs,FFuncs),
	map2M(readFlcFromFcy:fcy2flcOp,Ops,FOps).

fcy2flcTypes([],[]).
fcy2flcTypes(['Type'(TName,Vis,TParams,Cons)|Types],
	     ['Type'(FTName,Vis,TParams,FCons)|FTypes]) :-
	fcy2flcQName(TName,FTName),
	map2M(readFlcFromFcy:fcy2flcCons,Cons,FCons),
	fcy2flcTypes(Types,FTypes).
fcy2flcTypes(['TypeSyn'(_,_,_,_)|Types],FTypes) :- % ignore type synonyms
	fcy2flcTypes(Types,FTypes).

fcy2flcCons('Cons'(CName,Arity,Vis,Types),'Cons'(FCName,Arity,Vis,FTypes)) :-
	fcy2flcQName(CName,FCName),
	map2M(readFlcFromFcy:fcy2flcTypeExpr,Types,FTypes).

fcy2flcTypeExpr('TVar'(I),'TVar'(I)).
fcy2flcTypeExpr('FuncType'(T1,T2),'FuncType'(FT1,FT2)) :-
	fcy2flcTypeExpr(T1,FT1),
	fcy2flcTypeExpr(T2,FT2).
fcy2flcTypeExpr('TCons'(TName,TEs),'TCons'(FTName,FTEs)) :-
	fcy2flcQName(TName,FTName),
	map2M(readFlcFromFcy:fcy2flcTypeExpr,TEs,FTEs).

fcy2flcFunc('Func'(FName,Arity,Vis,Type,Rule),
	    'Func'(FFName,Arity,Vis,FType,FRule)) :-
	fcy2flcQName(FName,FFName),
	fcy2flcTypeExpr(Type,FType),
	fcy2flcTypeRule(Rule,FRule).

fcy2flcTypeRule('External'(N),'External'(FN)) :-
	cp_string(N,FN).
fcy2flcTypeRule('Rule'(Args,Exp),'Rule'(Args,FExp)) :-
	fcy2flcExpr(Exp,FExp).

fcy2flcLit('Intc'(I),'Intc'(I)).
fcy2flcLit('Floatc'(F),'Floatc'(F)).
fcy2flcLit('Charc'(C),'Charc'(I)) :- char_int(C,I).

fcy2flcExpr('Var'(I),'Var'(I)).
fcy2flcExpr('Lit'(L),'Lit'(FL)) :- fcy2flcLit(L,FL).
fcy2flcExpr('Comb'(CT,QName,Es),'Comb'(FCT,FQName,FEs)) :-
	fcy2flcQName(QName,FQName),
	fcy2flcCombType(CT,FCT),
	map2M(readFlcFromFcy:fcy2flcExpr,Es,FEs).
fcy2flcExpr('Free'(Vars,Exp),'Free'(Vars,FExp)) :-
	fcy2flcExpr(Exp,FExp).
fcy2flcExpr('Let'(Bindings,Exp),'Let'(FBindings,FExp)) :-
	map2M(readFlcFromFcy:fcy2flcBinding,Bindings,FBindings),
	fcy2flcExpr(Exp,FExp).
fcy2flcExpr('Or'(E1,E2),'Or'(FE1,FE2)) :-
	fcy2flcExpr(E1,FE1),
	fcy2flcExpr(E2,FE2).
fcy2flcExpr('Case'(CT,Exp,Branches),'Case'(CT,FExp,FBranches)) :-
	fcy2flcExpr(Exp,FExp),
	map2M(readFlcFromFcy:fcy2flcBranch,Branches,FBranches).
fcy2flcExpr('Typed'(E1,_),FE1) :- % ignore typed expressions
	fcy2flcExpr(E1,FE1).

fcy2flcCombType('FuncCall','FuncCall').
fcy2flcCombType('ConsCall','ConsCall').
fcy2flcCombType('FuncPartCall'(M),'FuncPartCall'(M)).
fcy2flcCombType('ConsPartCall'(M),'ConsPartCall'(M)).
%fcy2flcCombType('ConsPartCall'(m),'ConsCall').

fcy2flcBinding('Prelude.(,)'(Var,Exp),'Prelude.(,)'(Var,FExp)) :-
	fcy2flcExpr(Exp,FExp).

fcy2flcBranch('Branch'('LPattern'(L),Exp),'Branch'('LPattern'(FL),FExp)) :-
	fcy2flcLit(L,FL), fcy2flcExpr(Exp,FExp).
fcy2flcBranch('Branch'('Pattern'(QName,Vars),Exp),
	      'Branch'('Pattern'(FQName,Vars),FExp)) :-
	fcy2flcQName(QName,FQName),
	fcy2flcExpr(Exp,FExp).


fcy2flcOp('Op'(OName,Fix,Int),'Op'(FOName,Fix,Int)) :-
	fcy2flcQName(OName,FOName).

fcy2flcQName('Prelude.(,)'(Mod,Name),FMName) :-
	cp_string(Mod,"prelude"), !,
	cp_string(Name,FName),
	append("Prelude.",FName,FMName).
fcy2flcQName('Prelude.(,)'(Mod,Name),FMName) :-
	cp_string(Mod,FMod),
	cp_string(Name,FName),
	append(FMod,[46|FName],FMName).






