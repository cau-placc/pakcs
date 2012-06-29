%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

:- use_module('../prologbasics').
:- use_module('../version').
:- use_module('../pakcsversion').

prim_curryCompiler(CS) :- atom2String(pakcs,CS).

prim_curryCompilerMajorVersion(V) :- compilerMajorVersion(V).

prim_curryCompilerMinorVersion(V) :- compilerMinorVersion(V).

prim_curryCompilerRevisionVersion(V) :- compilerRevisionVersion(V).

prim_curryRuntime(PrologS) :- prolog(Prolog), atom2String(Prolog,PrologS).

prim_curryRuntimeMajorVersion(V) :- prologMajorVersion(V).

prim_curryRuntimeMinorVersion(V) :- prologMinorVersion(V).

prim_installDir(PHS) :-
	getEnv('PAKCSHOME',PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Distribution.installDir: cannot determine installation directory!').
