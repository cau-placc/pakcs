%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System:
%
:- module(prim_system,
	  [prim_getEnvironment/1, prim_getPID/1,
	   prim_system/2, prim_exitWith/2, prim_sleep/2]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).

prim_getEnvironment(Result) :-
	catch(findall((Var,Value), system:environ(Var, Value), Reslist), _, Reslist = []),
	allAtom2String(Reslist,Result).

allAtom2String([],[]).
allAtom2String([X|Xs],[Y|Ys]) :- bothAtom2String(X,Y), allAtom2String(Xs, Ys).

bothAtom2String((X1,Y1),'Prelude.(,)'(X2,Y2)) :- basics:atom2String(X1,X2),
                                                 basics:atom2String(Y1,Y2).

prim_getPID(Pid) :- currentPID(Pid).

prim_system(S,Status) :-
	string2Atom(S,Cmd),
	shellCmd(Cmd,Status).

prim_exitWith(Code,_) :- halt(Code).

prim_sleep(S,'Prelude.()') :- sleepSeconds(S).
