%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Global:
%

:- module(prim_global,
	  [initGlobalValue/4,prim_readGlobal/2,prim_writeGlobal/3]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).
:- (current_module(prim_readshowterm) -> true ; use_module(prim_readshowterm)). % for term reading/showing
:- (current_module(prim_standard) -> true ; ensure_loaded(user:prim_standard)). % for waitUntilGround

% initialize the predicate containing the global value if called for the
% first time:
initGlobalValue(GlobName,'Global.Temporary',Exp,Val) :-
	evalToken(Eval),
	user:nf(Exp,Val,Eval,E1),
	user:waitUntilGround(Val,E1,_), % groundness required
	GlobalHead =.. [GlobName,_],
        user:retractClause(GlobalHead,_),
	NewGlobalCall =.. [GlobName,Val],
	% temporary globals directly contain its value:
	assertz(user:NewGlobalCall),
	!.
initGlobalValue(GlobName,'Global.Persistent'(FExp),Exp,FileName) :-
	evalToken(Eval),
	user:nf(FExp,FileString,Eval,E0),
	user:waitUntilGround(FileString,E0,E1), % groundness required
	string2Atom(FileString,FileName),
	user:nf(Exp,Val,E1,E2),
	user:waitUntilGround(Val,E2,_), % groundness required
	GlobalHead =.. [GlobName,_],
        user:retractClause(GlobalHead,_),
	NewGlobalCall =.. [GlobName,FileName],
	% persistent globals contain the file name where its value is stored:
	assertz(user:NewGlobalCall),
	(existsFile(FileName) -> true ; writeGlobalFile(FileName,Val)),
	!.

% read a global value:
prim_readGlobal('Global.GlobalDef'(GlobName,'Global.Temporary'),Val) :-
	GlobalCall =.. [GlobName,Val],
	call(user:GlobalCall), !.
prim_readGlobal('Global.GlobalDef'(GlobName,'Global.Persistent'),Val) :-
	GlobalCall =.. [GlobName,FileName],
	call(user:GlobalCall),
	readGlobalFile(FileName,Val), !.

% update a global value:
prim_writeGlobal('Global.GlobalDef'(GlobName,'Global.Temporary'),NewVal,
	         'Prelude.()') :-
	GlobalCall =.. [GlobName,_],
	(retract(user:GlobalCall) ; user:retractClause(GlobalCall,_)),
	NewGlobalCall =.. [GlobName,NewVal],
	assertz(user:NewGlobalCall), !.
prim_writeGlobal('Global.GlobalDef'(GlobName,'Global.Persistent'),
	         NewVal,'Prelude.()') :-
	GlobalCall =.. [GlobName,FileName],
	call(user:GlobalCall),
	writeGlobalFile(FileName,NewVal),
	!.

% read the file with the persistent global value:
readGlobalFile(FileName,Val) :-
	lockFileName(FileName,LockFile),
	lockWithFile(LockFile),
	open(FileName,read,Stream),
	readStreamLine(Stream,ValString),
	readTerm(ValString,qualified,_Rest,Val),
	close(Stream),
	unlockWithFile(LockFile).

% write the file with the persistent global value:
writeGlobalFile(FileName,Val) :-
	lockFileName(FileName,LockFile),
	lockWithFile(LockFile),
	(existsFile(FileName)
	 -> appendAtom(FileName,'.bak',BakFileName),
            renameFile(FileName,BakFileName)
	  ; true),
	open(FileName,write,Stream),
	show_term(Val,qualified,ValString,[]),
	writeChars(Stream,ValString),
	put_code(Stream,10),
	% the additional characters are necessary due to a bug in
	% SWI-Prolog when reading short files:
	put_code(Stream,10), put_code(Stream,10), put_code(Stream,10),
	close(Stream),
	unlockWithFile(LockFile).


% lockfile for safe file reading/writing:
lockFileName(FName,LockFile) :- appendAtom(FName,'.LOCK',LockFile).

lockWithFile(LockFile) :-
	appendAtom('lockfile-create --lock-name ',LockFile,LockCmd),
	((existsFile(LockFile), pakcsrc(dynamicmessages,yes))
	 -> writeErr('>>> Waiting for removing lock file \''),
	    writeErr(LockFile), writeErr('\'...'),
	    nlErr ; true),
	shellCmd(LockCmd), !.

unlockWithFile(LockFile) :-
        appendAtom('lockfile-remove --lock-name ',LockFile,LockCmd),
	shellCmd(LockCmd).
