%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Dynamic:
%

:- module(prim_dynamic,
	[prim_assertFact/2,prim_retractFact/2,prim_getDynamicKnowledge/1,
	 prim_abortTransaction/1,prim_commitTransaction/2,
	 prim_startTransaction/1,prim_isKnownAtTime/3,
	 abortTransaction/0,initializeDynamic/0]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).
:- (current_module(version)      -> true ; use_module('../version')).
:- (current_module(pakcsversion) -> true ; use_module('../pakcsversion')).

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

:- dynamic dynamicTime/1, dynamicVersion/3, insideTransaction/0.

dynamicTime(1).
%dynamicVersion(DBdir,P/N,V). % loaded database version of persistent predicate
% insideTransaction. % this fact is asserted during a transaction.

setDynamicVersion(Dir,P/N,V) :- % set info about database version
	(retract(dynamicVersion(Dir,P/N,_)) -> true ; true),
	asserta(dynamicVersion(Dir,P/N,V)).

prim_assertFact('Dynamic.Dynamic'(P),'Prelude.()') :-
	retract(dynamicTime(DT)),
	DT1 is DT+1,
	assertz(dynamicTime(DT1)),
	P =.. [Pred,DType,_,_|Args],
	NP =.. [Pred,DT1,0|Args],
	assertz(user:NP),
	logDynamicEvent(DType,assertz,NP).

prim_retractFact('Dynamic.Dynamic'(P),B) :-
	P =.. [Pred,DType,_,_|Args],
	NP =.. [Pred,Start,Stop|Args],
	reloadNewestDynamicVersionOf(Pred),
	retract(dynamicTime(DT)),
	DT1 is DT+1,
	assertz(dynamicTime(DT1)),
	call(user:NP),
	isInTimeInterval(DT,Start,Stop),
	!,
	NPA =.. [Pred,Start,DT|Args],
	retract(user:NP), !, logDynamicEvent(DType,retract,NP),
	assertz(user:NPA),
	B='Prelude.True'.
prim_retractFact(_,'Prelude.False').


prim_getDynamicKnowledge(IsKnownAtTime) :-
        reloadNewestDynamicVersions,
        dynamicTime(DT),
	term2partcall('Dynamic.isKnownAtTime'(DT),1,IsKnownAtTime),
	(compileWithSharing(function)
	 -> makeShare(IsKnownAtTime,Result)
	  ; Result = IsKnownAtTime).

prim_isKnownAtTime(DT,'Dynamic.Dynamic'(P),'Prelude.True') :-
	P =.. [Pred,_,_,_|Args],
	NP =.. [Pred,Start,Stop|Args],
	call(user:NP),
	isInTimeInterval(DT,Start,Stop).

isInTimeInterval(DT,Start,Stop) :-
	DT>=Start, (Stop=0 -> true; DT=<Stop).


% log dynamic events (assert/retract):
logDynamicEvent('Dynamic.Temporary',_,_) :- !.
logDynamicEvent('Dynamic.Persistent',Op,Pred) :-
	Pred =.. [P,_,_|Args],
	StorePred =.. [P,0,0|Args],
	OpPred =.. [Op,StorePred],
	user:dynamicPredInfo(P/_,Dir),
	logDynamic(OpPred,Dir).

logDynamic(Term,Dir) :-
	%writeq(user_error,Term), nlErr,
	dir2tmplockfile(Dir,LockFile),
	lockWithFileIfNotInTransaction(LockFile),
	executeWithFinalize(
	    (dir2logfile(Dir,LogFile),
	     open(LogFile,append,Stream),
	     writeq(Stream,Term), put_code(Stream,46), nl(Stream),
	     flush_output(Stream), close(Stream),
	    addChangePid(Dir)),
	  unlockWithFileIfNotInTransaction(LockFile)).

% execute a goal with finalizer (the finalizer is also execute if the
% goal fails or produce a run-time error):
executeWithFinalize(Goal,Finalizer) :-
	on_exception(ErrorMsg,
		     Goal,
		     (call(Finalizer),printError(ErrorMsg),!,fail)),
	!, call(Finalizer).
executeWithFinalize(_,Finalizer) :- call(Finalizer), !, fail.

prim_startTransaction(DT1) :-
        (insideTransaction
	 -> writeErr('ERROR: nested transactions not supported!'), nlErr,
	    !, fail
	  ; true),
	assert(insideTransaction),
	on_exception(ErrorMsg,
		     initTransaction(DT1),
		     (unlockAllPersistentDynamics,
		      (retract(insideTransation) -> true ; true),
		      printError(ErrorMsg),fail)),
	!.

initTransaction(DT1) :-
	lockAllPersistentDynamics,
	reloadNewestDynamicVersions,
	retract(dynamicTime(DT)),
	DT1 is DT+1,
	assertz(dynamicTime(DT1)),
	logAllDynamicPreds(startTransaction(DT1)).

prim_commitTransaction(TDT,'Prelude.()') :-
	logAllDynamicPreds(commitTransaction(TDT)),
	unlockAllPersistentDynamics,
	(retract(insideTransaction) -> true ; true).

prim_abortTransaction(_) :-
	logAllDynamicPreds(abortTransaction),
	getDynamicDirs(Dirs), map1partialM(prim_dynamic:addChangePid(0),Dirs),
	unlockAllPersistentDynamics,
	(retract(insideTransaction) -> true ; true),
	!, fail.


% get directories of all dynamic predicates:
getDynamicDirs(SortedDirs) :-
	findall(Dir,isDynamicDir(Dir),Dirs), sort(Dirs,SortedDirs).

isDynamicDir(Dir) :- user:dynamicPredInfo(_,Dir), \+ Dir=''.


% write a term (e.g., transaction marker) to event log of all
% persistent dynamic predicates occurring
% in a dynamic initialization directive:
logAllDynamicPreds(Term) :-
	getDynamicDirs(Dirs),
	map1partialM(prim_dynamic:logDynamic(Term),Dirs).

% initialize dynamic predicates:
initializeDynamic :-
	user:dynamicPredInfo(Name/Arity,Dir),
	initializeDynamic(Dir,Name/Arity),
	fail.
initializeDynamic.

initializeDynamic('',P/N) :- !,	retractAllFacts(P/N).
initializeDynamic(Dir,P/N) :-
	existsDirectory(Dir), !,
	checkDynSpec(Dir,P/N),
	setDynamicVersion(Dir,P/N,0), !.
initializeDynamic(Dir,_) :-
	existsFile(Dir), !,
	appendAtom('ERROR: Directory "',Dir,E1),
	appendAtom(E1,'" for dynamic entries cannot be created!',ErrMsg),
	writeErr(ErrMsg), nlErr, !, fail.
initializeDynamic(Dir,P/N) :-
	upDirectory(Dir,UpDir),
	(existsDirectory(UpDir)
	 -> createDynamicDir(Dir,P/N),
	    setDynamicVersion(Dir,P/N,1)
	  ; writeErr('>>> Warning: persistent dynamic data directory '),
	    writeErr(Dir), writeErr(' not accessible!'), nlErr).

% get the directory up from the given directory:
upDirectory('.','..') :- !.
upDirectory(Dir,UpDir) :-
	atom_codes(Dir,DirS),
	append(UpDirS,[47|Last],DirS), \+ member(47,Last), !,
	atom_codes(UpDir,UpDirS).
upDirectory(_,'.'). % Dir does not contain a '/'

checkDynSpec(Dir,P/N) :-
	dir2specfile(Dir,SpecFile),
	existsFile(SpecFile),
	readPrologTermFile(SpecFile,Spec),
	dynamicSpec(P/N,Spec), !.
checkDynSpec(Dir,_) :-
	appendAtom('ERROR: Directory "',Dir,E1),
	appendAtom(E1,'" contains no matching dynamic entries!',ErrMsg),
	writeErr(ErrMsg), nlErr, !, fail.

% replay all events written to log file:
replayLogFile(Dir,P/N) :-
	dir2tmplockfile(Dir,LockFile),
	lockWithFileIfNotInTransaction(LockFile),
	executeWithFinalize(replayLogFileWhenAlreadyLocked(Dir,P/N),
			    unlockWithFileIfNotInTransaction(LockFile)).

:- dynamic replayStatus/1.

replayLogFileWhenAlreadyLocked(Dir,P/N) :-
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> Restoring persistent dynamic data in directory \''),
	    writeErr(Dir), writeErr('\'...'),
	    nlErr ; true),
	getRunTime(RT1),
	ensurePakcsVersion(Dir),
	dir2dbfile(Dir,DBFile),
	dir2pofile(Dir,POFile),
	consultPrologorPOFile(DBFile,POFile),
	dir2logfile(Dir,LogFile),
	open(LogFile,read,Stream),
	(atEndOfStream(Stream) -> close(Stream)
	  ; assert(replayStatus(unchanged)),
            repeat,
	    on_exception(Exc,readAndReplay(Stream),
                ((printError(Exc)->true;true),
  	         writeErr('ERROR during reading of '),
		 writeErr(LogFile),
		 writeErr(' (restored state might be incomplete)'),
		 nlErr)),
	    close(Stream),
	    retract(replayStatus(ChangeStatus)),
	    writeNewDatabaseIfChanged(P/N,Dir,ChangeStatus)),
	updateDynFactsIfNecessary(Dir,P/N),
	readDynamicVersion(Dir,CurrentVersion),
	setDynamicVersion(Dir,P/N,CurrentVersion),
	writeChangePids(Dir,[]),
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> ...restored in '),
	    getRunTime(RT2),
	    RT is RT2-RT1,
	    writeErr(RT),
	    writeErr(' msec'), nlErr ; true),
	!.

% Ensure that the database is consistent with the current version of PAKCS,
% i.e., delete .po file if it belongs to a different Prolog version.
ensurePakcsVersion(Dir) :-
	readPAKCSVersion(Dir,DBVersion),
	DBVersion = pakcsVersion(_,PrologVersion),
	prologMajor(PrologV),
	\+ PrologV=PrologVersion,
	dir2pofile(Dir,POFile),
	(existsFile(POFile) -> deleteFile(POFile) ; true),
	writePAKCSVersion(Dir), !.
ensurePakcsVersion(_).
	
% is database generated with current version of PAKCS compiler?
isCurrentPakcsVersion(Dir) :-
	readPAKCSVersion(Dir,DBVersion),
	compilerVersion(CVersion), !,
	(DBVersion = compilerVersion(CVersion)
         -> writePAKCSVersion(Dir)
          ; DBVersion = pakcsVersion(CVersion,_)).

% read the current version of PAKCS from the version file (or fail):
readPAKCSVersion(Dir,DBVersion) :-
	dir2pakcsversionfile(Dir,PakcsVersionFile),
	existsFile(PakcsVersionFile),
	readPrologTermFile(PakcsVersionFile,DBVersion).

% write the current version of PAKCS into the version file:
writePAKCSVersion(Dir) :-
	dir2pakcsversionfile(Dir,PakcsVersionFile),
	compilerVersion(PakcsV), prologMajor(PrologV),
	writePrologTermFile(PakcsVersionFile,pakcsVersion(PakcsV,PrologV)).

% update facts of dynamic predicates w.r.t. new Curry2Prolog compiler version:
updateDynFactsIfNecessary(Dir,_) :- isCurrentPakcsVersion(Dir), !.
updateDynFactsIfNecessary(Dir,P/N) :-
	compilerVersion(Version),
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> Updating persistent dynamic data in directory \''),
	    writeErr(Dir), writeErr('\' to compiler version '),
	    writeErr(Version), writeErr('...'),
	    nlErr ; true),
	updateDynFacts(P/N),
	writeNewDatabaseIfChanged(P/N,Dir,changed),
	writePAKCSVersion(Dir).
updateDynFactsIfNecessary(_,_).

updateDynFacts(P/N) :-
	functor(Fact,P,N),
	functor(NewFact,P,N),
	retract(user:Fact),
	arg(1,Fact,A1), arg(1,NewFact,A1),
	arg(2,Fact,A2), arg(2,NewFact,A2),
	translateDynFactArgs(3,N,Fact,NewFact),
	asserta(user:NewFact),
	fail.
updateDynFacts(_).

translateDynFactArgs(I,N,_,_) :- I>N, !.
translateDynFactArgs(I,N,Fact,NewFact) :-
	arg(I,Fact,Arg),
	translateDynFactArg(Arg,NewArg),
	arg(I,NewFact,NewArg),
	I1 is I+1,
	translateDynFactArgs(I1,N,Fact,NewFact).

% add prefix 'Prelude.' to all prelude constructors:
translateDynFactArg(X,X) :- var(X), !.
translateDynFactArg(X,X) :- number(X), !.
translateDynFactArg('True','Prelude.True') :- !.
translateDynFactArg('False','Prelude.False') :- !.
translateDynFactArg('False','Prelude.False') :- !.
translateDynFactArg('Left','Prelude.Left') :- !.
translateDynFactArg('Right','Prelude.Right') :- !.
translateDynFactArg('Nothing','Prelude.Nothing') :- !.
translateDynFactArg('Just','Prelude.Just') :- !.
translateDynFactArg('LT','Prelude.LT') :- !.
translateDynFactArg('GT','Prelude.GT') :- !.
translateDynFactArg('EQ','Prelude.EQ') :- !.
translateDynFactArg(Name,NewName) :-
	atomic(Name),
	atom_codes(Name,[40|_]), !, % tuple constructor
	appendAtom('Prelude.',Name,NewName).
translateDynFactArg(Char,NewChar) :-
	atom(Char), atom_codes(Char,[39|L]), !, % old character representation
	(L=[N]
         -> char_int(NewChar,N)
          ; (L=[78,85,76] % NUL
             -> char_int(NewChar,0)
              ; writeErr('INTERNAL ERROR in translateDynFactArg: unkown char'),
                nlErr)).
translateDynFactArg(Name,Name) :- atomic(Name), !.
translateDynFactArg(T,NT) :-
	functor(T,F,N),
	translateDynFactArg(F,NF),
	functor(NT,NF,N),
	translateDynFactArgs(1,N,T,NT).


% write new version of database from internal predicates if they have been changed
% (indicated by third argument):
writeNewDatabaseIfChanged(P/N,Dir,changed) :- !,
	dir2dbfile(Dir,DBFile),
	dir2pofile(Dir,POFile),
	dir2logfile(Dir,LogFile),
	appendAtom(DBFile,'.bak',DBBakFile),
	tell(DBBakFile),
	writeq((:- dynamic P/N)), put_code(46), nl,
	listing(user:P/N),
	told,
	try_save_predicates(P/N,POFile),
	renameFile(DBBakFile,DBFile),
	appendAtom(LogFile,'.bak',LogBakFile),
	renameFile(LogFile,LogBakFile),
	appendAtom('touch ',LogFile,Cmd5), shellCmd(Cmd5),
	readDynamicVersion(Dir,Version),
	NewVersion is Version+1,
	writeDynamicVersion(Dir,NewVersion),
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> ...new database constructed by merging old database and log file '),
	    nlErr
	  ; true).
writeNewDatabaseIfChanged(P/N,Dir,_) :-
	% nothing has been changed, remove only log file:
	dir2logfile(Dir,LogFile),
	appendAtom(LogFile,'.bak',LogBakFile),
	renameFile(LogFile,LogBakFile),
	appendAtom('touch ',LogFile,Cmd), shellCmd(Cmd).

readAndReplay(Stream) :- atEndOfStream(Stream), !.
readAndReplay(Stream) :- read(Stream,Event), replayEvent(Stream,Event).

replayEvent(Stream,end_of_file) :- !.
replayEvent(Stream,startTransaction(T)) :-
	readAndReplayTransaction(Stream,T,[]), !, fail.
replayEvent(Stream,Event) :-
	retract(replayStatus(_)), assert(replayStatus(changed)),
	call(user:Event),
	!, fail.

readAndReplayTransaction(Stream,T,_) :- atEndOfStream(Stream), !,
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> Warning: ignoring incomplete transaction '),
	    writeErr(T), nlErr
	  ; true).
readAndReplayTransaction(Stream,T,Evs) :-
	read(Stream,Event),
	(Event=commitTransaction(T)
	 -> callRevList(Evs)
	  ; (Event=abortTransaction
	     -> true
	      ; readAndReplayTransaction(Stream,T,[Event|Evs]))),
	!.

callRevList([]).
callRevList([C|Cs]) :-
	callRevList(Cs),
	retract(replayStatus(_)), assert(replayStatus(changed)),
	call(user:C).

% in order to ignore nested transactions:
startTransaction(_).
commitTransaction(_).
abortTransaction.


% lock all persistent data:
lockAllPersistentDynamics :-
	getDynamicDirs(Dirs), map1M(prim_dynamic:lockDynamicDir,Dirs).

lockDynamicDir(Dir) :-
	dir2lockfile(Dir,LockFile), lockWithFile(LockFile).

lockWithFileIfNotInTransaction(LockFile) :-
	insideTransaction -> true ; lockWithFile(LockFile).

lockWithFile(LockFile) :-
	appendAtom('lockfile-create --lock-name ',LockFile,LockCmd),
	((existsFile(LockFile), pakcsrc(dynamicmessages,yes))
	 -> writeErr('>>> Waiting for removing lock file \''),
	    writeErr(LockFile), writeErr('\'...'),
	    nlErr ; true),
	shellCmd(LockCmd), !.
	%writeErr('File lock set: '), writeErr(LockFile), nlErr.

% unlock all persistent data:
unlockAllPersistentDynamics :-
	getDynamicDirs(Dirs), map1M(prim_dynamic:unlockDynamicDir,Dirs).

unlockDynamicDir(Dir) :-
	dir2lockfile(Dir,LockFile), unlockWithFile(LockFile).

unlockWithFileIfNotInTransaction(LockFile) :-
	insideTransaction -> true ; unlockWithFile(LockFile).

unlockWithFile(LockFile) :-
	existsFile(LockFile), !,
        appendAtom('lockfile-remove --lock-name ',LockFile,LockCmd),
        shellCmd(LockCmd).
unlockWithFile(_).


% reload newest versions of all persistent data:
reloadNewestDynamicVersions :-
	user:dynamicPredInfo(P/N,Dir), % reload only currently accessible preds
	dynamicVersion(Dir,P/N,Version),
	reloadNewestDynamicVersion(Dir,P/N,Version),
	fail.
reloadNewestDynamicVersions.

% reload newest version of a particular dynamic predicate, if necessary:
reloadNewestDynamicVersionOf(Pred) :-
	dynamicVersion(Dir,Pred/N,Version), !, % is Pred persistent?
	reloadNewestDynamicVersion(Dir,Pred/N,Version).
reloadNewestDynamicVersionOf(_).

reloadNewestDynamicVersion(Dir,P/N,0) :-
	!, % external facts have not been loaded due to Version=0
	retractAllFacts(P/N), % remove existing facts
	replayLogFile(Dir,P/N),
	!.
reloadNewestDynamicVersion(Dir,P/N,Version) :-
	readDynamicVersion(Dir,FileVersion),
	\+ Version=FileVersion, !,  % somebody put a newer version: reload
	retractAllFacts(P/N), % remove existing facts
	replayLogFile(Dir,P/N),
	!.
reloadNewestDynamicVersion(Dir,_,_) :-
	readChangePids(Dir,Pids), currentPID(Pid),
	(Pids=[] ; Pids=[Pid]), !. % I have the newest version and nobody changed it
reloadNewestDynamicVersion(Dir,P/N,_) :-
	retractAllFacts(P/N),	% somebody changed the current version
	replayLogFile(Dir,P/N).


% create new directory for dynamic data:
createDynamicDir(Dir,P/N) :-
	(pakcsrc(dynamicmessages,yes)
	 -> writeErr('>>> Creating new persistent dynamic data in directory \''),
	    writeErr(Dir), writeErr('\'...'),
	    nlErr ; true),
	appendAtom('mkdir -p ',Dir,Cmd1), shellCmd(Cmd1),
	dir2logfile(Dir,LogFile),
	appendAtom('touch ',LogFile,Cmd2), shellCmd(Cmd2),
	dir2dbfile(Dir,DBFile),
	appendAtom('touch ',DBFile,Cmd3), shellCmd(Cmd3),
	dir2specfile(Dir,SpecFile),
	dynamicSpec(P/N,Spec),
	writePrologTermFile(SpecFile,Spec),
	writePAKCSVersion(Dir).


% read a file containing a single Prolog term:
readPrologTermFile(File,Term) :-
	open(File,read,Stream),
	read(Stream,Term),
	close(Stream).

% write a file with a single Prolog term:
writePrologTermFile(File,Term) :-
	open(File,write,Stream),
	writeq(Stream,Term), put_code(Stream,46), nl(Stream),
	flush_output(Stream), close(Stream).

% read version number from file:
readDynamicVersion(Dir,Version) :-
	dir2versionfile(Dir,VersFile),
	existsFile(VersFile), !,
	% if something goes wrong with reading the file,
	% return an unused version number:
	on_exception(_,readPrologTermFile(VersFile,Version),Version=0).
readDynamicVersion(_,1).

% write version number into file:
writeDynamicVersion(Dir,Version) :-
	dir2versionfile(Dir,VersFile),
	writePrologTermFile(VersFile,Version).


% read changePIDs from file:
readChangePids(Dir,Pids) :-
	dir2changepidsfile(Dir,CPFile),
	existsFile(CPFile), !,
	% if something goes wrong with reading the file,
	% return an unused pid:
	on_exception(_,readPrologTermFile(CPFile,Pids),Pids=[0]).
readChangePids(_,[]).

% write changePIDs into file:
writeChangePids(Dir,Pids) :-
	dir2changepidsfile(Dir,CPFile),
	writePrologTermFile(CPFile,Pids).

addChangePid(Dir) :- currentPID(Pid), addChangePid(Pid,Dir).

addChangePid(Pid,Dir) :-
	readChangePids(Dir,Pids),
	(member(Pid,Pids) -> true ; writeChangePids(Dir,[Pid|Pids])),
	!.


% define specification data stored in directory:
dynamicSpec(P/N,dynamicPredicate(P,N)).

% retract all dynamic facts that are dead at current dynamic time:
retractDeadDynamicFacts(P/N) :-
	length(Args,N),
	Pred =.. [P|Args],
	Args = [_,Stop|_],
	dynamicTime(DT),
	call(user:Pred),
	Stop>0, DT>Stop,
	retract(user:Pred),
	fail. 
retractDeadDynamicFacts(_).


% File for writing all change events to persistent predicates:
dir2logfile(Dir,LogFile) :- appendAtom(Dir,'/eventlog',LogFile).

% Lockfile for transactions:
dir2lockfile(Dir,LockFile) :- appendAtom(Dir,'/LOCK',LockFile).

% Lockfile for change event logging and reconstructing the database
% (i.e., replaying all change events):
dir2tmplockfile(Dir,TmpLockFile) :- appendAtom(Dir,'/LOCK',TmpLockFile).
%dir2tmplockfile(Dir,TmpLockFile) :- appendAtom(Dir,'/LOCK.TMP',TmpLockFile).

% File to store all facts in Prolog format:
dir2dbfile(Dir,DBFile) :- appendAtom(Dir,'/database.pl',DBFile).

% File to store all facts in faster load format:
dir2pofile(Dir,POFile) :- appendAtom(Dir,'/dbpred.po',POFile).

% File to store the specification of the dynamic predicate:
dir2specfile(Dir,SpecFile) :- appendAtom(Dir,'/spec',SpecFile).

% File to store the version of PAKCS compiler that created this database:
% (used for managing PAKCS compiler updates that influences database format)
dir2pakcsversionfile(Dir,VersFile) :- appendAtom(Dir,'/pakcsversion',VersFile).

% File to store the version number of the database:
dir2versionfile(Dir,VersFile) :- appendAtom(Dir,'/version',VersFile).

% File to store the PIDs of all processes that performed change events:
dir2changepidsfile(Dir,CPFile) :- appendAtom(Dir,'/changepids',CPFile).

