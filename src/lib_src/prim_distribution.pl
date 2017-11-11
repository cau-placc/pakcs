%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Distribution
%

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(version)      -> true ; use_module('../version')).
:- (current_module(pakcsversion) -> true ; use_module('../pakcsversion')).

prim_curryCompiler(CS) :- atom2String(pakcs,CS).

prim_curryCompilerMajorVersion(V) :- compilerMajorVersion(V).

prim_curryCompilerMinorVersion(V) :- compilerMinorVersion(V).

prim_curryCompilerRevisionVersion(V) :- compilerRevisionVersion(V).

prim_curryRuntime(PrologS) :- prolog(Prolog), atom2String(Prolog,PrologS).

prim_curryRuntimeMajorVersion(V) :- prologMajorVersion(V).

prim_curryRuntimeMinorVersion(V) :- prologMinorVersion(V).

prim_baseVersion(BVS) :- baseVersion(BVA), atom2String(BVA,BVS).

prim_installDir(PHS) :-
	installDir(PH)
	 -> atom2String(PH,PHS)
	  ; raise_exception('Distribution.installDir: cannot determine installation directory!').
