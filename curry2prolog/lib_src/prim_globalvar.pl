%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module GlobalVariable:
%

%:- module(prim_globvar,
%	  [initGlobalVariable/5,prim_writeGVar/5,prim_readGVar/4]).

%:- use_module('../prologbasics').
%:- use_module('../basics').

% initialize the predicate containing the global value if called for the
% first time:
initGlobalVariable(GlobNameInProg,Exp,GlobResult,E0,E) :-
	appendAtom('$GLOBAL_',GlobNameInProg,GlobName),
	user:nf(Exp,Val,E0,E1),
	GlobalHead =.. [GlobNameInProg,_,_,_],
        user:retractClause(GlobalHead,_),
	GlobResult = 'GlobalVariable.GVarValue'(GlobName,Val),
	NewGlobalFact =.. [GlobNameInProg,GlobResult,Ev,Ev],
	% redefine initial clause for global variable:
	asserta(user:NewGlobalFact),
	E=E1,
	!.

% get the value associated to a global variable:
?- block prim_readGVar(?,?,-,?).
prim_readGVar(GV,partcall(1,prim_readGVarWorld,[GV]),E,E).

?- block prim_readGVarWorld(?,?,?,-,?).
prim_readGVarWorld('GlobalVariable.GVarValue'(GV,IVal),_World,
		   '$io'(Val),E0,E) :-
	E0 = eval(Bs),
	(findGVar(Bs,GV,Val) -> E=E0 ; Val=IVal, E = eval([GV/Val|Bs])).

findGVar([Var/Value|_],Var,Value) :- !.
findGVar([_|Bs],Var,Value) :- findGVar(Bs,Var,Value).


% set the value associated to a global value:
?- block prim_writeGVar(?,?,?,-,?).
prim_writeGVar(GV,Val,partcall(1,prim_writeGVarWorld,[Val,GV]),E,E).

?- block prim_writeGVarWorld(?,?,?,?,-,?).
prim_writeGVarWorld('GlobalVariable.GVarValue'(GV,IVal),Val,_World,
		    '$io'('Prelude.()'),E0,E) :-
	E0 = eval(Bs),
	updateGVar(Bs,GV,Val,NewBs),
	E = eval(NewBs).

updateGVar([],Var,Value,[Var/Value]).
updateGVar([Var/_|Bs],Var,Value,[Var/Value|Bs]) :- !.
updateGVar([B|Bs],Var,Value,[B|NBs]) :- updateGVar(Bs,Var,Value,NBs).

