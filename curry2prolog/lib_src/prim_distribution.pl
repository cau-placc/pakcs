%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

:- use_module('../prologbasics').
:- use_module('../version').

prim_curryCompiler(CS) :- atom2String(pakcs,CS).

prim_curryCompilerMajorVersion(V) :- compilerMajorVersion(V1,V2), V is V1*10+V2.

prim_curryCompilerMinorVersion(V) :- compilerMinorVersion(V).

prim_curryRuntime(PrologS) :- prolog(Prolog), atom2String(Prolog,PrologS).

prim_curryRuntimeMajorVersion(V) :- prologMajorVersion(V).

prim_curryRuntimeMinorVersion(V) :- prologMinorVersion(V).

prim_installDir(PHS) :-
	getEnv('PAKCSHOME',PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Distribution.installDir: cannot determine installation directory!').
