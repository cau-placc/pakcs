%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System:
%
:- module(prim_system,
	  [prim_getCPUTime/1,prim_getElapsedTime/1,prim_getArgs/1,
	   prim_getEnviron/2,
	   prim_getHostname/1,prim_getPID/1,prim_getProgName/1,
	   prim_system/2,prim_exitWith/2,prim_sleep/2]).

:- use_module('../prologbasics').
:- use_module('../basics').

prim_getCPUTime(MS) :- getRunTime(MS).

prim_getElapsedTime(MS) :- getElapsedTime(MS).

prim_getArgs(StringArgs) :-
        (rtargs(Args) -> true ; getProgramArgs(Args)),
        map2M(basics:atom2String,Args,StringArgs).

prim_getEnviron(Var,Value) :-
	string2Atom(Var,AtomVar),
	(getEnv(AtomVar,AtomValue) -> atom2String(AtomValue,Value)
	                            ; Value = []). % empty string if undefined

prim_getHostname(String) :-
        getHostname(Name),
        atom2String(Name,String).

prim_getPID(Pid) :- currentPID(Pid).

prim_getProgName(String) :-
        user:currentModule(Name),
        atom2String(Name,String).

prim_system(S,Status) :-
	string2Atom(S,Cmd),
	shellCmd(Cmd,Status).

prim_exitWith(Code,_) :- halt(Code).

prim_sleep(S,'Prelude.()') :- sleepSeconds(S).

