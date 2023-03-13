%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some basic data and predicates that are used by diffent modules.

:- module(basics,[exitCode/1, setExitCode/1, failWithExitCode/0,
		  noLoadMessage/0, withColor/1, cpmVersion/1, lastload/1,
                  plprofiling/1,
		  setVerbosity/1, verbosityQuiet/0, verbosityNotQuiet/0,
                  verbosityCommands/0,
		  verbosityIntermediate/0, verbosityDetailed/0,
		  quietmode/1, setQuietMode/1,
                  rtArgs/1,
		  compileWithSharing/1,
		  compileWithDebug/0, compileWithFailPrint/0,
		  hasPrintedFailure/0, printConsFailure/1,
		  evalToken/1, worldToken/1,
		  writeNQ/1, nlNQ/0, writeLnNQ/1,
		  writeIntermediate/1, nlIntermediate/0, writeLnIntermediate/1,
		  writeErr/1, nlErr/0, writeLnErr/1,
		  writeErrNQ/1, nlErrNQ/0, writeLnErrNQ/1,
		  writeBlanks/1,
		  onlySICStusMessage/1, checkSICStusAndWarn/1,
		  onlySWIMessage/1, checkSWIAndWarn/1,
		  putChars/1, putChars/2, writeChars/2,
		  assertPakcsrc/1, writeRCvalues/0,
		  evaluateDynamicPredInfo/3, checkDynamicAccessMethod/2,
		  resetDynamicPreds/0, clearDynamicPreds/0,
		  isCharCons/1, isString/1, char_int/2, cp_string/2,
		  string2Atom/2, atom2String/2, atomic2Atom/2, atomic2Codes/2,
		  removeShares/2, term2partcall/3, isCompleteList/2, isList/1,
		  getNewFileName/2, createNewTmpDir/1, mainPrologFileName/1,
		  extendPath/3, path2Atom/2, pathString2loadPath/2,
		  getLocalCurryPath/1, getCurryPath/1, setCurryPath/1,
		  getSysLibPath/1, shellCmdWithCurryPath/1,
                  shellCmdWithCurryPathWithReport/1,
		  loadPath/2, findSourceFileInLoadPath/3,
		  findFlatProgFileInLoadPath/2,
		  findPrologTargetFileInLoadPath/2, findFilePropertyInPath/4,
		  toAbsPath/2, split2dirbase/3, stripSuffix/2,
		  isIoType/1, isId/1, 
		  versionAtom/2, padVersionAtom/2, padList/4,
		  constructorOrFunctionType/4,
		  flatName2Atom/2, decodePrologName/2,
		  isTupleCons/1, isLetterDigitCode/1,
		  isOperatorName/1, isOpIdChar/1,
                  getHomeDirectory/1,
		  rev/2, concat/2, take/3, drop/3, splitAt/4,
		  memberEq/2, deleteFirst/3, replaceEq/4,
		  union/3, diff/3,
		  foldr/4, foldr1/3, intersperse/3,
		  appendAtoms/2, split2words/2,
		  codes2number/2, isDigit/1,
		  retractAllFacts/1, prefixComma/3,
		  isWritableFile/1, tryWriteFile/1,
                  tryDeleteFile/1, deleteFileIfExists/1,
		  ensureDirOfFile/1, getOutDirectory/1, prog2DirProg/2, prog2PrologFile/2,
		  prog2InterfaceFile/2,
                  prog2FlatCurryFile/2, prog2AbstractCurryFile/2,
		  prog2ICurryFile/2, hierarchical2dirs/2,
		  skipEOL/0, readStreamLine/2, removeBlanks/2, skipblanks/2,
		  numberconst/3, readFileContents/2, readStreamContents/2,
		  printError/1, prologError2Atom/2,
                  readChar/3, readString/3,
                  showTermChar/3, showTermString/3]).

:- use_module(prologbasics).
:- use_module(pakcsversion).

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic withColor/1, cpmVersion/1, lastload/1, plprofiling/1, quietmode/1,
	   rtArgs/1, compileWithSharing/1,
	   compileWithDebug/0, compileWithFailPrint/0, hasPrintedFailure/0,
	   printConsFailure/1, exitCode/1,
	   user:dynamicPredInfo/2, orgDynamicPredInfo/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hook predicates to influence message printing from Prolog:

noLoadMessage :- \+ pakcsrc(_,_), !. % no messages for initial state creation
noLoadMessage :- \+ verbosityIntermediate.

:- multifile user:portray_message/2.
:- dynamic user:portray_message/2.

% suppress loading/restore messages if not desired:
%user:portray_message(I,M) :- writeErr(I/M), nlErr, fail.
user:portray_message(informational,_) :- !, noLoadMessage.
user:portray_message(informational,loading(_,_,_)) :- !, noLoadMessage.
user:portray_message(informational,loaded(_,_,_,_,_,_)) :- !,noLoadMessage.
user:portray_message(informational,created(File,_)) :- !,
	noLoadMessage,
	atom_codes(File,FileS),
	append(_,".po",FileS). % do not show creation message for .po files
user:portray_message(warning,import(_,_,_,_)) :- !, noLoadMessage.
user:portray_message(informational,imported(_,_,_)) :- !, noLoadMessage.
user:portray_message(informational,foreign_resource(_,_,_,_)) :- !, noLoadMessage.
% do not show any restore messages:
user:portray_message(informational,loading(_,restoring,_)) :- !.
user:portray_message(informational,restored(_,_,_)) :- !.
% do not show saved state creation messages:
user:portray_message(informational,created(_,_)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

withColor(yes).      % use colors for output
cpmVersion('').      % the version string of CPM
lastload("Prelude"). % program in last load command
quietmode(no).	% yes if environment should work in quiet mode (option -q)
plprofiling(no). % perform profiling with Prolog profiler
rtArgs([]).	% run-time arguments from script call or ":set args" option
compileWithSharing(variable). % if it should be compiled with variable sharing scheme
%compileWithSharing(function). % if it should be compiled with function sharing scheme
%compileWithDebug.    % include if it should be compiled with debugging code
%compileWithFailPrint.% include this if it should be compiled without code for
                     % printing failure of reductions (predicate "failprint")
printConsFailure(no). % print failures because of missing constructor cases
%pakcsrc(name,value). % definitions from PAKCS rc file
%dynamicPredInfo(Pred/Arity,Directory). % info clauses for dynamic predicates
evalToken(eval([])). % token send through eval arguments for correct concurrency
worldToken('$world'). % token send through world arguments in I/O actions

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exitCode(0). % standard exit code when terminating PAKCS

setExitCode(C) :-
	retract(exitCode(_)), asserta(exitCode(C)).

% set exit code to 1 and fail:
failWithExitCode :- setExitCode(1), !, fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setQuietMode(V) :-
	retract(quietmode(_)), asserta(quietmode(V)).

% verbosity/1 is defined in prologbasics.pl

setVerbosity(N) :-
	retract(verbosity(_)), asserta(verbosity(N)).

% verbosity level = 0 (do not show warnings or messages)?
verbosityQuiet :- verbosity(0).

% verbosity level > 0?
verbosityNotQuiet :- verbosity(N), N>0.

% verbosity level >= 2 (show commands)?
verbosityCommands :- verbosity(N), N>1.

% verbosity level >= 3 (show intermediate messages)?
verbosityIntermediate :- verbosity(N), N>2.

% verbosity level = 4 (show all details and intermediate results)?
verbosityDetailed :- verbosity(N), N>3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writing outputs:

% write on standard out if not in quiet mode:
writeNQ(T) :- quietmode(no) -> write(T); true.
nlNQ :- quietmode(no) -> nl; true.
writeLnNQ(T) :- writeNQ(T), nlNQ.

% write on standard out if not in quiet mode:
writeIntermediate(T) :- verbosityIntermediate -> write(T) ; true.
nlIntermediate :- verbosityIntermediate -> nl ; true.
writeLnIntermediate(T) :- writeIntermediate(T), nlIntermediate.

% write on user error:
writeErr(T) :- write(user_error,T).
nlErr :- nl(user_error), flush_output(user_error).
writeLnErr(T) :- writeErr(T), nlErr.

% write on user error if not in quiet mode:
writeErrNQ(T) :- quietmode(no) -> write(user_error,T); true.
nlErrNQ :- quietmode(no) -> nl(user_error); true.
writeLnErrNQ(T) :- writeErrNQ(T), nlErrNQ.


% write n blanks on standard out:
writeBlanks(N) :- N>0 -> put_code(32), N1 is N-1, writeBlanks(N1)
	               ; true.

% write a Prolog string (list of ASCII values):
putChars([]).
putChars([C|Cs]) :- put_code(C), putChars(Cs).

% write a Prolog string (list of ASCII values) to a stream (Arg 1):
putChars(_,[]).
putChars(Stream,[C|Cs]) :-
	put_code(Stream,C), putChars(Stream,Cs).


% write a Curry string (list of chars) to a stream (Arg 1):
writeChars(_,[]).
writeChars(Stream,[C|Cs]) :-
	char_int(C,N), put_code(Stream,N), writeChars(Stream,Cs).

% check whether this is a SICStus-based implementation and provide warning
% if this is not the case:
checkSICStusAndWarn(Feature) :-
	prolog(sicstus) -> true ; onlySICStusMessage(Feature).

onlySICStusMessage(Feature) :-
	appendAtoms(['"',Feature,'" not available ',
		     '(only available in a PAKCS implementation based on SICStus-Prolog)!'],
		    Message),
	writeErr('WARNING: '), writeErr(Message), nl.


% check whether this is a SICStus-based implementation and provide warning
% if this is not the case:
checkSWIAndWarn(Feature) :-
	prolog(swi) -> true ; onlySWIMessage(Feature).

onlySWIMessage(Feature) :-
	appendAtoms(['"',Feature,'" not available ',
		     '(only available in a PAKCS implementation based on SWI-Prolog)!'],
		    Message),
	writeErr('WARNING: '), writeErr(Message), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% handling pakcsrc settings

assertPakcsrc(prop(Name,Value)) :- assertz(pakcsrc(Name,Value)).

% write all pakcsrc settings:
writeRCvalues :-
	pakcsrc(Name,Value),
	write(Name),
        atom_codes(Name,NameS), length(NameS,L), B is 19-L, writeBlanks(B),
        write(' = '), write(Value), nl, fail.
writeRCvalues.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some auxiliaries for dynamic predicates:

% evaluate an initial dynamicPredInfo clause generated by the compiler
% and replace it by the evaluated clause:
evaluateDynamicPredInfo(P/N,Exp,Dir) :-
	assertz(orgDynamicPredInfo(P/N,Exp)),
	evalToken(Eval),
	user:nf(Exp,NF,Eval,E1),
	user:boolEq(NF,NF,_,E1,_), % groundness required
	string2Atom(NF,DynAccess),
	checkDynamicAccessMethod(DynAccess,Dir),
	user:retractClause(dynamicPredInfo(P/N,_),_),
	asserta(user:dynamicPredInfo(P/N,Dir)).

% check the access method for persistent predicates (currently: "file:")
% and remove it:
checkDynamicAccessMethod(AccName,Name) :-
	atom_codes(AccName,AccNameS),
	append("file:",NameS,AccNameS),
	!,
	atom_codes(Name,NameS).
checkDynamicAccessMethod(AccName,AccName) :-
	appendAtom('Illegal access method for persistent predicate "',AccName,Err1),
	appendAtom(Err1,'"! (must be "file:")',Err2),
	raise_exception(Err2).

% reset all persistent dynamic predicates to their initial form,
% i.e., retract all facts and reset dynamicPredInfo clauses:
resetDynamicPreds :-
	user:dynamicPredInfo(Name/Arity,_),
	retractAllFacts(Name/Arity),
	fail.
resetDynamicPreds :-
	% finally, reset dynamicPredInfo clauses:
	resetDynamicPredInfos.

% reset dynamicPredInfo clauses to their initial compiler-generated definition:
resetDynamicPredInfos :-
	retract(orgDynamicPredInfo(Pred,Exp)),
	retract((user:dynamicPredInfo(Pred,_) :- _)),
	assertz((user:dynamicPredInfo(Pred,Dir) :-
	                  evaluateDynamicPredInfo(Pred,Exp,Dir))),
	fail.
resetDynamicPredInfos.

% clear all non-persistent facts about dynamic predicates occurring
% in a dynamic initialization directive:
clearDynamicPreds :-
	user:dynamicPredInfo(Name/Arity,Dir),
	clearDynamicPred(Name/Arity,Dir),
	fail.
clearDynamicPreds.

clearDynamicPred(Name/Arity,'') :- retractAllFacts(Name/Arity), !.
clearDynamicPred(Name/Arity,_) :- prim_dynamic:retractDeadDynamicFacts(Name/Arity), !.
%clearDynamicPred(Name/Arity,_) :- user:retractDeadDynamicFacts(Name/Arity), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for temporary files:

% get a name of a temporary file (for the current PAKCS process)
% with a given (possibly empty) suffix (and delete an already existing
% file or directory):
getNewFileName(Suffix,NewFile) :-
	currentPID(PID),
	number_codes(PID,PIDS),
        tmpDir(TmpDir), atom_codes(TmpDir,TmpDirS),
	concat([TmpDirS,"/pakcs_file_",PIDS],P1),
	(Suffix=[] -> ProgS=P1 ; append(P1,[46|Suffix],ProgS)),
	atom_codes(NewFile,ProgS),
	append("rm -rf ",ProgS,RmCmdS),
	atom_codes(RmCmd,RmCmdS),
	shellCmd(RmCmd).

% Creates a new temporary directory (for the current PAKCS process)
% (and delete an already existing file or directory):
createNewTmpDir(NewTmpDir) :-
	getNewFileName("",NewTmpDir),
	makeDirectory(NewTmpDir).

% determine (for the current PAKCS process) a file name
% where the clauses for the main predicates (hnf, constrEq,...)
% should be stored (and delete an already existing file):
mainPrologFileName(MainPrologFile) :-
	getNewFileName("pl",NewPrologFile),
	appendAtom(NewPrologFile,'.main',MainPrologFile).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for handling load paths:

% extend a path with a prefix, e.g.:
% extendPath(test,current,'test:current')
extendPath(Path,'',Path) :- !.
extendPath('',Path,Path) :- !.
extendPath(AddPath,OldPath,ExtPath) :-
	appendAtoms([AddPath,':',OldPath],ExtPath).

% translate a string containing a path into list of these directories:
% (e.g., ".:pakcs/lib" -> ['.','pakcs/lib'])
pathString2loadPath(SCP,LP1) :-
	append(SDir1,[58|SDirs],SCP), % 58 = ':'
	!,
	atom_codes(Dir1,SDir1),
	pathString2loadPath(SDirs,LP),
	(Dir1='' -> LP1=LP ; LP1=[Dir1|LP]).
pathString2loadPath(SDir,LP) :-
	atom_codes(Dir,SDir),
	(Dir='' -> LP=[] ; LP=[Dir]).

% transform a path into an atom (e.g., ['.','pakcs/lib'] -> '.:pakcs/lib'):
path2Atom(Path,PathA) :- path2String(Path,PathS), atom_codes(PathA,PathS).

% transform a path into a string (e.g., ['.','pakcs/lib'] -> ".:pakcs/lib"):
path2String([],[]).
path2String([D],DS) :- atom_codes(D,DS).
path2String([D1,D2|Ds],DS) :-
	atom_codes(D1,D1s),
	path2String([D2|Ds],D2S),
	append(D1s,[58|D2S],DS). % 58 = ':'

% compute the path for loading modules w.r.t. a main directory:
loadPath(MainDir,LoadPath) :-
	getCurryPath(LCP),
	getLocalLibPath(LocalLibPath),
	getSysLibPath(SysLibPath),
	append(LCP,LocalLibPath,LocalP),
	append(LocalP,SysLibPath,LP),
	lastload(ProgS),
	atom_codes(Prog,ProgS),
	split2dirbase(Prog,LastLoadDir,_),
	(LastLoadDir='.' -> LoadPath = [MainDir|LP]
	                  ; LoadPath = [MainDir,LastLoadDir|LP]).

% store a local setting of CURRYPATH:
:- dynamic localCurryPath/1.
localCurryPath("").

getLocalCurryPath(LCP) :- localCurryPath(LCPS), atom_codes(LCP,LCPS).

getCurryPath(LCP) :-
	localCurryPath(LocalCP),
	(LocalCP="" -> (getEnv('CURRYPATH',CP) -> atom_codes(CP,CPS) ; CPS="")
                     ; CPS=LocalCP),
	pathString2loadPath(CPS,LCP).

setCurryPath(CP) :-
	retract(localCurryPath(_)),
	atom_codes(CP,CPS),
	asserta(localCurryPath(CPS)).

% get a shell command for some given command where CURRYPATH is exported
% together with the system library path
getCmdWithCurryPath(Cmd,ExpCmd) :-
	loadPath('.',LP),
        path2String(LP,FullPathS),
	concat(["CURRYPATH='",FullPathS,"' && export CURRYPATH && "],Export),
	atom_codes(Cmd,CmdS),
	append(Export,CmdS,ExpCmdS),
	atom_codes(ExpCmd,ExpCmdS).

% execute a shell command where CURRYPATH is exported together with
% the system library path
shellCmdWithCurryPath(Cmd) :-
	getCmdWithCurryPath(Cmd,ExpCmd),
	shellCmd(ExpCmd).

% call "shellCmdWithCurryPath" and report its execution
% if verbosityIntermediate:
shellCmdWithCurryPathWithReport(Cmd) :-
	getCmdWithCurryPath(Cmd,ExpCmd),
	(verbosityIntermediate -> write('Executing: '), write(ExpCmd), nl
                                ; true),
	flush_output(user_output),
	shellCmd(ExpCmd).

getLocalLibPath(LocalPath) :-
	pakcsrc(libraries,LocalLib),
	atom_codes(LocalLib,LibS),
	pathString2loadPath(LibS,LocalPath), !.
getLocalLibPath([]).

% define the system libaries directories
getSysLibPath(LP) :-
	installDir(Root),
	appendAtom(Root,'/lib',Lib),
	LP = [Lib].

% findFilePropertyInPath(Path,Pred,F,PF): find a file name w.r.t. to path
%                                         satisfying a given predicate
% Path: a list of directory names (atoms)
% Pred: a predicate that should be satisfied by the file name in the path
% F   : file name (an atom)
% FP  : file name prefixed by directory found in path
findFilePropertyInPath([Dir|Dirs],Mod:PropPred,File,PathFile) :-
	(atom_codes(File,[47|_])  % already absolute file name?
	 -> DirFile=File
	  ; appendAtom(Dir,'/',DirSlash),
	    appendAtom(DirSlash,File,DirFile)),
	Pred =.. [PropPred,DirFile],
	(call(Mod:Pred)
	 -> PathFile = DirFile
	  ; findFilePropertyInPath(Dirs,Mod:PropPred,File,PathFile)).

% findSourceFileInLoadPath(F,PF): find a source program file w.r.t. to load path
% Prog: program name (an atom with possible file path and suffix)
% Ext: name extension ('.curry' or '.lcurry')
% PathProg: file name prefixed by directory in load path
findSourceFileInLoadPath(Prog,Ext,PathProg) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	prog2DirProg(ProgBase,ProgDBase),
	appendAtoms([ProgDBase,Ext],ProgDBaseE),
	loadPath(ProgDir,LP),
	findFilePropertyInPath(LP,prologbasics:existsFile,ProgDBaseE,PathProg).

% findFlatProgFileInLoadPath(F,PF): find a program name that has an
% existing FlatCurry file in the current load path
% Prog: a program name (an atom)
% PathProg:  directory/module name of the Curry file corresponding to Prog
% If the program name is not found, an error message is issued followed
% by a failure.
findFlatProgFileInLoadPath(Prog,PathProg) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	loadPath(ProgDir,LP),
        (findFlatProgFileInPath(LP,ProgBase,PathProg)
         -> true
          ; writeErr('ERROR: Program (or FlatCurry file) "'),
            writeErr(Prog),
            writeLnErr('" not found in load path:'),
            path2Atom(LP,AP), writeLnErr(AP),
            fail),
        !.

findFlatProgFileInPath([Dir|Dirs],Prog,PathProg) :-
	(atom_codes(Prog,[47|_])  % already absolute file name?
	 -> DirProg=Prog
	  ; appendAtoms([Dir,'/',Prog],DirProg)),
	prog2FlatCurryFile(DirProg,FlatFile),
	(existsFile(FlatFile)
	 -> PathProg = DirProg
	  ; findFlatProgFileInPath(Dirs,Prog,PathProg)).

% findPrologTargetFileInLoadPath(F,PF): find a file name of a Prolog target file
% w.r.t. to load path
% Prog: a program name (an atom)
% PathProg: file name of the Prolog target corresponding to Prog
findPrologTargetFileInLoadPath(Prog,PathProg) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	loadPath(ProgDir,LP),
	findPrologTargetFileInPath(LP,ProgBase,PathProg).

findPrologTargetFileInPath([Dir|Dirs],Prog,PathProg) :-
	(atom_codes(Prog,[47|_])  % already absolute file name?
	 -> DirProg=Prog
	  ; appendAtoms([Dir,'/',Prog],DirProg)),
	prog2PrologFile(DirProg,PrologFile),
	(existsFile(PrologFile)
	 -> PathProg = PrologFile
	  ; findPrologTargetFileInPath(Dirs,Prog,PathProg)).

% transform a path name into an absolute path name:
toAbsPath(Path,Path) :-	atom_codes(Path,[47|_]), !.  % already absolute path?
toAbsPath(Path,AbsPath) :-
        atom_codes(Path,[126,47|RPathS]), % home dir path ~/...?
	!,
	atom_codes(RPath,RPathS),
	(getHomeDirectory(HomeDir) -> true ; HomeDir='~'),
	appendAtoms([HomeDir,'/',RPath],AbsPath).
toAbsPath('~',HomeDir) :- !,
	(getHomeDirectory(HomeDir) -> true ; HomeDir='~').
toAbsPath('.',CurDir) :- !, workingDirectory(CurDir).
toAbsPath(Path,AbsPath) :-
        workingDirectory(CurDir),
	appendAtoms([CurDir,'/',Path],AbsPath).

% split a file name into directory name (possibly '.') and base name:
% (all parameters are atoms)
split2dirbase(File,Dir,Base) :-
	atom_codes(File,FileS),
	rev(FileS,RevFileS),
	append(RevBaseS,[47|RevDirS],RevFileS),  % 47 = '/'
	!,
	rev(RevDirS,DirS), atom_codes(Dir,DirS),
	rev(RevBaseS,BaseS), atom_codes(Base,BaseS).
split2dirbase(File,'.',File).

% strip a suffix (the last suffix starting with a dot) from a file name (atom):
stripSuffix(FileName,BaseName) :-
	atom_codes(FileName,FileNameS),
	rev(FileNameS,RevFileNameS),
	append(_,[46|RevBaseNameS],RevFileNameS),  % 46 = '.'
	!,
	rev(RevBaseNameS,BaseNameS), atom_codes(BaseName,BaseNameS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% is a type expression IO?
isIoType(Type) :- var(Type), !, fail.
isIoType('TCons'('Prelude.IO',_)) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Union of functiontype and constructortype:
constructorOrFunctionType(QName,Name,Arity,Type) :-
	user:constructortype(QName,Name,Arity,_UnqualifiedName,_Index,Type,_),
	!.
constructorOrFunctionType(QName,Name,Arity,Type) :-
	user:functiontype(QName,Name,Arity,_PrologName,_Fixity,Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Further auxiliaries:

% get home directory (fail if it does not exist):
getHomeDirectory(Home) :-
        (getEnv('HOME',Home); getEnv('USERPROFILE',Home)),
        atom_codes(Home,[_|_]),
        existsDirectory(Home).

% linear reverse:
rev(Xs,Ys) :- rev_acc(Xs,Ys,[]).
rev_acc([],Ys,Ys).
rev_acc([X|Xs],Ys,Zs) :- rev_acc(Xs,Ys,[X|Zs]).

% concatenate a list of lists:
concat([],[]).
concat([L|Xs],LXs) :- concat(Xs,Ys), append(L,Ys,LXs).

% take first n elements of a list:
take(0,_,[]) :- !.
take(_,[],[]) :- !.
take(N,[X|Xs],[X|Ys]) :- N1 is N-1, take(N1,Xs,Ys).

% drop first n elements of a list:
drop(0,L,L) :- !.
drop(_,[],[]) :- !.
drop(N,[_|Xs],Ys) :- N1 is N-1, drop(N1,Xs,Ys).

% split list to first n elements and remaining ones:
splitAt(0,Xs,[],Xs) :- !.
splitAt(N,[X|Xs],[X|Ys],Zs) :- N1 is N-1, splitAt(N1,Xs,Ys,Zs).

% is there an identical member in a list?
memberEq(E,[F|_]) :- E==F, !.
memberEq(E,[_|Xs]) :- memberEq(E,Xs).

% delete first occurrence of an element in a list:
deleteFirst(E,[E|L],L) :- !.
deleteFirst(E,[F|L],[F|M]) :- deleteFirst(E,L,M).

% replace identical occurrences of a term in a list by a new term:
replaceEq(_,_,[],[]).
replaceEq(X,Y,[E|L],[Y|M]) :- X==E, !, replaceEq(X,Y,L,M).
replaceEq(X,Y,[E|L],[E|M]) :- replaceEq(X,Y,L,M).

% compute union (without dups) of two lists:
union([],Xs,Xs).
union([X|Xs],Ys,Zs) :- member(X,Ys), !, union(Xs,Ys,Zs).
union([X|Xs],Ys,[X|Zs]) :- union(Xs,Ys,Zs).

% compute difference (without dups) of two lists:
diff([],_,[]).
diff([X|Xs],Ys,Zs) :- member(X,Ys), !, diff(Xs,Ys,Zs).
diff([X|Xs],Ys,[X|Zs]) :- diff(Xs,Ys,Zs).


% foldr: replace '.' and [] in a list by a constructor and constant:
foldr(_,E,[],E).
foldr(C,E,[X|Xs],T) :- foldr(C,E,Xs,XsT), T =.. [C,X,XsT].

% foldr1: replace '.' in a non-empty list by a constructor:
foldr1(_,[X],X).
foldr1(C,[X1,X2|Xs],T) :- foldr1(C,[X2|Xs],XsT), T =.. [C,X1,XsT].

% intersperse: puts a separator (first arg) between all elements in a list:
intersperse(_,[],[]).
intersperse(_,[X],[X]).
intersperse(Sep,[X,Y|Zs],[X,Sep|Xs]) :- intersperse(Sep,[Y|Zs],Xs).

% concatenate a list of atoms into a single atom:
appendAtoms(As,A) :-
	map2M(prologbasics:atomCodes,As,Xs), concat(Xs,Ys), atom_codes(A,Ys).

% transform a list of ASCII digit codes into a number;
% fail if the list does not contain only digit codes:
codes2number(Cs,N) :- Cs=[_|_], map1M(basics:isDigit,Cs), number_codes(N,Cs).

% Is it a digit code?
isDigit(C) :- C >= 48, C =< 57.

% split a list of characters into list of words:
split2words([],[]).
split2words([32|Cs],Words) :- !, split2words(Cs,Words).
split2words([C|Cs],Words) :- split2words_word(Cs,[C],Words).

split2words_word([],Word,[Word]).
split2words_word([32|Cs],Word,[Word|Words]) :- !,
	split2words(Cs,Words).
split2words_word([C|Cs],Word,Words) :-
	append(Word,[C],WordC), % slow, but doesn't matter here
	split2words_word(Cs,WordC,Words).


% retract all (user-module) facts for a predicate:
retractAllFacts(P/N) :-
	length(Args,N),
	Pred =.. [P|Args],
	retractAll(Pred), !.

retractAll(L) :- retract(user:L), fail.
retractAll(_).


% put N commas before a string:
prefixComma(C1,N,C1) :- N=1, !.
prefixComma(C1,N,C2) :- N>1, N1 is N-1, prefixComma([44|C1],N1,C2).

% is an atom an identifier (and not an operator)?
isId(T) :- atom_codes(T,[F|_]),
	(65=<F, F=<90 ; 97=<F, F=<122).

% convert the given version to an atom
versionAtom(Version,Atom) :-
	number_chars(Version,Chars),
	atom_chars(Atom,Chars).

% convert the given version to a zero-padded atom
padVersionAtom(Version,PaddedAtom) :-
	number_chars(Version,Chars),
	padList(Chars,'0',2,PaddedChars),
	atom_chars(PaddedAtom,PaddedChars).

% pads a list with the given element
padList(List,_,Length,PaddedList) :-
	length(List,Length), !,
	PaddedList = List.
padList(List,Pad,Length,PaddedList) :-
	length(List, Length2), Length2 < Length,
	padList([Pad|List],Pad,Length,PaddedList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Axuiliaries for handling translated Prolog files

% check whether a file is writable:
isWritableFile(File) :-
	on_exception(_,
	             (open(File,write,Stream), close(Stream), deleteFile(File)),
		     fail).

% try to write a file (and immediately close it) and catch and show file errors:
tryWriteFile(File) :-
	on_exception(ErrorMsg,
	             (open(File,write,Stream), close(Stream)),
		     printError(ErrorMsg)).

% try to delete a file and catch and show file errors:
tryDeleteFile(File) :-
	on_exception(ErrorMsg,deleteFile(File),printError(ErrorMsg)).

% delete a file if it exists:
deleteFileIfExists(File) :-
	existsFile(File) -> tryDeleteFile(File) ; true.

% ensure that there exists the directory (usually, .curry/pakcs) of the given
% file name, i.e., create it if it does not exist (and catch/show any errors):
ensureDirOfFile(File) :-
	on_exception(ErrorMsg,tryEnsureDirOfFile(File),printError(ErrorMsg)).
tryEnsureDirOfFile(File) :-
	split2dirbase(File,Dir,_),
	(existsDirectory(Dir) -> true ; makeDirectoryWithParents(Dir)).

% create directory and also their parents, if necessary:
makeDirectoryWithParents(Dir) :-
	atom_codes(Dir,DirS),
	(DirS=[47|Dir1S] -> makeDirectoryWithParentsFrom([47],Dir1S)
	                  ; makeDirectoryWithParentsFrom([],DirS)).

makeDirectoryWithParentsFrom(PrefixS,DirS) :-
	append(Dir1S,[47|Dir2S],DirS), \+ append(_,[47|_],Dir1S), !,
	% create first subdir:
	makeDirectoryWithPrefix(PrefixS,Dir1S),
	((PrefixS=[] ; PrefixS=[47]) -> append(PrefixS,Dir1S,SubDirS)
	                              ; append(PrefixS,[47|Dir1S],SubDirS)),
	makeDirectoryWithParentsFrom(SubDirS,Dir2S).
makeDirectoryWithParentsFrom(PrefixS,DirS) :-
	makeDirectoryWithPrefix(PrefixS,DirS).

makeDirectoryWithPrefix(PrefixS,DirS) :-
	(PrefixS=[] -> CompleteDirS = DirS
	             ; append(PrefixS,[47|DirS],CompleteDirS)),
	atom_codes(CompleteDir,CompleteDirS),
	(existsDirectory(CompleteDir) -> true ; makeDirectory(CompleteDir)).

% fetch the name of the directory for compilation artifacts, e.g.
% FlatCurry and compiled Prolog files.
getOutDirectory(OutDir) :-
	compilerMajorVersion(MajorVersion),
	compilerMinorVersion(MinorVersion),
	compilerRevisionVersion(RevVersion),
	versionAtom(MajorVersion,MajorVersionAtom),
	versionAtom(MinorVersion,MinorVersionAtom),
	versionAtom(RevVersion,RevVersionAtom),
	appendAtoms(['.curry/pakcs-',MajorVersionAtom,'.',MinorVersionAtom,'.',RevVersionAtom], OutDir).

% generate the name of the program file for a given Curry program name,
% i.e., replace all dots in a module name by slashes:
prog2DirProg(Prog,ProgDir) :-
	split2dirbase(Prog,ProgDirPrefix,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	(ProgDirPrefix='.'
	 -> ProgDir=DProgBase
	  ; appendAtoms([ProgDirPrefix,'/',DProgBase],ProgDir)).

% generate the name of the Prolog file for a given Curry program name:
prog2PrologFile(Prog,PrologFile) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	getOutDirectory(OutDir),
	appendAtoms([ProgDir,'/',OutDir,'/',DProgBase,'.pl'],PrologFile).

% generate the name of the interface file for a given Curry program name:
prog2InterfaceFile(Prog,IntFile) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	getOutDirectory(OutDir),
	appendAtoms([ProgDir,'/',OutDir,'/',DProgBase,'.fint'],IntFile).

% generate the name of the FlatCurry file for a given Curry program name:
prog2FlatCurryFile(Prog,FlatFile) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	getOutDirectory(OutDir),
	appendAtoms([ProgDir,'/',OutDir,'/',DProgBase,'.fcy'],FlatFile).

% generate the name of the FlatCurry file for a given Curry program name:
prog2AbstractCurryFile(Prog,AcyFile) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	getOutDirectory(OutDir),
	appendAtoms([ProgDir,'/',OutDir,'/',DProgBase,'.acy'],AcyFile).

% generate the name of the InterfaceCurry file for a given Curry program name:
prog2ICurryFile(Prog,ICurryFile) :-
	split2dirbase(Prog,ProgDir,ProgBase),
	hierarchical2dirs(ProgBase,DProgBase),
	getOutDirectory(OutDir),
	appendAtoms([ProgDir,'/',OutDir,'/',DProgBase,'.icurry'],ICurryFile).

% translate a hierarchical module name (atom) into directory/file name,
% i.e., replace dots by slashes:
hierarchical2dirs(HModName,DModName) :-
	atom_codes(HModName,HModNameS),
	map2M(basics:dot2slash,HModNameS,DModNameS),
	atom_codes(DModName,DModNameS).
dot2slash(C1,C2) :- C1=46 -> C2=47 ; C2=C1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read input until newline or end-of-input is encountered.
skipEOL :- skipTo(10).

% read input until a code or end-of-input is encountered.
skipTo(Code) :-
	get_code(C),
        ((C = -1 ; C = Code) -> true ; skiptTo(Code)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read a line from a stream:
readStreamLine(Str,[]) :-
	atEndOfStream(Str), !.
readStreamLine(Str,Line) :-
	get_code(Str,C),
        (C=10 -> Line=[]
               ; Line=[C|Rest], readStreamLine(Str,Rest)).

% remove blanks at the beginning and end of a string:
removeBlanks(L1,L5) :-
	skipblanks(L1,L2),
	rev(L2,L3),
	skipblanks(L3,L4),
	rev(L4,L5).

skipblanks --> " ", !, skipblanks.
skipblanks --> {true}.

% parser for numeric literals
numberconst([45,C|Cs]) --> [45,C], % 45 = '-'
        { C >= "0", C =< "9" }, !,
        numberconstrest(Cs).
numberconst([C|Cs]) --> [C], 
        { C >= "0", C =< "9" },
        numberconstrest(Cs).
numberconstrest([C|Cs]) --> [C], 
        { C >= "0", C =< "9" }, !,
        numberconstrest(Cs).
numberconstrest([46,C|Cs]) --> ".", [C], { C >= "0", C =< "9" }, !,
        floatconstrest(Cs).
numberconstrest([]) --> skipblanks.

floatconstrest([C|Cs]) --> [C], 
        { C >= "0", C =< "9" }, !,
        floatconstrest(Cs).
floatconstrest([C|Cs]) --> [C], {C=69 ; C=101}, !, % exponent
	intconst(Cs).
floatconstrest([]) --> skipblanks.

% parser for integer constants
intconst(Cs) --> ( "-", natconst(NCs), {Cs=[45|NCs]}
		  ; natconst(Cs)
		  ).
natconst([C|Cs]) --> [C], 
        { C >= "0", C =< "9" }, !,
        natconst(Cs).
natconst([]) --> skipblanks.

% read contents of a file:
readFileContents(File,Cs) :- open(File,read,S), readStreamContents(S,Cs).

readStreamContents(Stream,[]) :- atEndOfStream(Stream), !, close(Stream).
readStreamContents(Stream,Cnts) :-
	get_code(Stream,C),
	(C = -1 -> Cnts=[], close(Stream)
	         ; Cnts=[C|Cs], readStreamContents(Stream,Cs)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Conversion functions for characters:
% During (Prolog) runtime, characters are represented as atoms
% with a leading ^ followed by the character or an decimal
% value for special characters, i.e., either three (for ASCII)
% or five digits (for Unicode)

% is the argument internal representation of a character constructor?
isCharCons(S) :- atom(S), atom_codes(S,[94|_]).

% a list is considered as a string if all elements are
% character constants:
isString(L) :- var(L), !, fail.
isString([]).
isString([X|Xs]) :- isCharCons(X), isString(Xs).


% relate a Curry character representation with an ASCII value of a character:
char_int(C,N) :- var(N), !,
	(atom_codes(C,[94,N]) -> true
         ; (atom_codes(C,[94,N1,N2,N3])
            -> N is (N1-48)*100+(N2-48)*10+N3-48
             ; (atom_codes(C,[94,N1,N2,N3,N4,N5])
                -> N is (N1-48)*10000+(N2-48)*1000+(N3-48)*100+(N4-48)*10+N5-48
	         ; writeErr('INTERNAL ERROR in char_int: unknown char "'),
	           writeErr(C), writeErr('"'), nlErr))), !.
char_int(C,N) :-
	((N<32 ; N=96 ; N>126)  % 96 = '`' -> cause problems in SP4
         -> (N<256 % decimal encoding for special chars:
             -> % ASCII:
	        N1 is (N//100)+48,
	        N2 is ((N mod 100)//10)+48,
		N3 is (N mod 10)+48,
		atom_codes(C,[94,N1,N2,N3])
	      ; % Unicode:
	        N1 is (N//10000)+48,
	        N2 is ((N mod 10000)//1000)+48,
	        N3 is ((N mod 1000)//100)+48,
	        N4 is ((N mod 100)//10)+48,
		N5 is (N mod 10)+48,
		atom_codes(C,[94,N1,N2,N3,N4,N5]))
	  ; atom_codes(C,[94,N])), !.

% relate Curry strings (list of chars) and Prolog strings (list of ints):
cp_string(String,Ints) :- map2M(basics:char_int,String,Ints).

% translate a Curry string (list of chars) into Prolog atom:
string2Atom(String,Atom) :-
	map2M(basics:char_int,String,Ints), atom_codes(Atom,Ints).

% translate a Prolog atom into a Curry string (list of chars):
atom2String(Atom,String) :-
	atom_codes(Atom,Ints), map2M(basics:char_int,String,Ints), !.


% translate a Prolog atomic value (atom, number, empty list) into a
% Prolog atom:
atomic2Atom([],'[]') :- !.                     % for SWI-Prolog >= 7
atomic2Atom(Atomic,Atom) :- number(Atomic), !, % transform number into atom
	number_codes(Atomic,AtomicS),
	atom_codes(Atom,AtomicS).
atomic2Atom(A,A).

% translates a Prolog atomic value (atom, number, empty list) to
% a list of character codes, i.e., like atom_codes but works also for
% other atomic values
atomic2Codes(A,Cs) :- atomic2Atom(A,AA), atom_codes(AA,Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% auxiliaries for handling FlatCurry names in the Prolog code:

% transform a FlatCurry name (list of character codes) into an atom used
% in the Prolog target code:
flatName2Atom("Prelude.:",ConsAtom) :- !,
        % translate Curry list cons into Prolog list cons:
        (swi7orHigher -> ConsAtom='[|]' ; ConsAtom='.').
flatName2Atom("Prelude.[]",[]) :- !. % keep name of list (type) constructor
%flatName2Atom("Prelude.[]",'[]') :- !. % keep name of list (type) constructor
flatName2Atom(Name,Atom) :- % keep name of tuple (type) constructor
	isTupleConsString(Name), !, atom_codes(Atom,Name).
%flatName2Atom(Name,Atom) :- atom_codes(Atom,Name).
flatName2Atom(Name,Atom) :-
	encodeName2Ident(Name,EName), atom_codes(Atom,EName).

% encode a (qualified) name into a regular (qualified) Curry identifier:
% start checking module prefix:
encodeName2Ident(Name,EName) :-
	Name=[C|_],
	((isLetterDigitCode(C), encodeName2IdentMod(Name,EName))
	 -> true % the identifier was a qualified one
	  ; (isOperatorName(Name)
	     -> EName=Name % don't encode standard operators
	      ; encodeString2Ident(Name,EName) % encode unqualified name
	    )).

encodeName2IdentMod([],[]) :- fail. % no module prefix found
encodeName2IdentMod([C],[C]) :- fail. % no module prefix found
encodeName2IdentMod([47|Cs],[46|TCs]) :- !, % replace / by . in module name
	encodeName2IdentMod(Cs,TCs).
encodeName2IdentMod([46,C2|Cs],[46|TCs]) :- !,
	% module qualifier found, encode remaining name:
	(isLetterDigitCode(C2)
	 -> encodeString2Ident(Cs,TCs2), TCs = [C2|TCs2]
	  ; (isOperatorName([C2|Cs])
	     -> TCs=[C2|Cs]	% remaining name is an operator -> don't change
	      ; encodeString2Ident([C2|Cs],TCs))).
encodeName2IdentMod([C1|Cs],[C1|TCs]) :-
	(isLetterDigitCode(C1) ; C1=95) % allowed module char?
	 -> encodeName2IdentMod(Cs,TCs)
	  ; fail.

% encode a string into a regular identifier by replacing all special
% characters c (i.e., characters different from letters, digits, '_', '.')
% by 'cx where cs is the 2-byte hex value of (ord c):
encodeString2Ident([],[]).
encodeString2Ident([C|Cs],[C|TCs]) :-
	(isLetterDigitCode(C) ; C=95 ; C=46), !,
	encodeString2Ident(Cs,TCs).
encodeString2Ident([C|Cs],[39,H1,H2|TCs]) :- % encode special char:
	C1 is C // 16, int2hex(C1,H1),
	C2 is C mod 16, int2hex(C2,H2),
	encodeString2Ident(Cs,TCs).

% is the argument a code of a letter or digit?
isLetterDigitCode(C) :- 65=<C, C=<90 ; 97=<C, C=<122 ; 48=<C, C=<57.

int2hex(I,H) :- I<10 -> H is 48+I ; H is 65+I-10.
hex2int(H,I) :- H<65 -> I is H-48 ; I is H-65+10.

% transform an atom of the Prolog target code into the corresponding Flat name
% (i.e., inverse of encoding in flatName2Atom):
decodePrologName(PName,FName) :-
	atom_codes(PName,PNameS),
	decodePrologNameCodes(PNameS,FNameS),
	atom_codes(FName,FNameS).

decodePrologNameCodes([],[]).
decodePrologNameCodes([C1],[C1]).
decodePrologNameCodes([C1,C2],[C1,C2]).
decodePrologNameCodes([C1,C2,C3|Cs],Ds) :-
	(C1=39 -> hex2int(C2,I2), hex2int(C3,I3), C is 16*I2+I3,
	          decodePrologNameCodes(Cs,DCs), Ds=[C|DCs]
                ; decodePrologNameCodes([C2,C3|Cs],DCs), Ds=[C1|DCs]).

		   
% is a list of character codes a tuple constructor
% (i.e., of the form "Prelude.(,,,)")?
isTupleConsString(Name) :- append("Prelude.(",Cs,Name), isTupleConsSuffix(Cs).
isTupleConsSuffix([41]) :- !.
isTupleConsSuffix([44|Cs]) :- isTupleConsSuffix(Cs).

% is an atom a tuple constructor?
isTupleCons(Atom) :- atom_codes(Atom,String), isTupleConsString(String).

% is a list of character codes a Curry operator name?
isOperatorName(Name) :- map1M(basics:isOpIdChar,Name).

% is a character code allowed as a Curry operator character?
isOpIdChar(126).% ~
isOpIdChar(33).	% !
isOpIdChar(64).	% @
isOpIdChar(35).	% #
isOpIdChar(36).	% $
isOpIdChar(37).	% %
isOpIdChar(94).	% ^
isOpIdChar(38).	% &
isOpIdChar(42).	% *
isOpIdChar(43).	% +
isOpIdChar(45).	% -
isOpIdChar(61).	% =
isOpIdChar(60).	% <
isOpIdChar(62).	% >
isOpIdChar(63).	% ?
isOpIdChar(46).	% .
isOpIdChar(47).	% /
isOpIdChar(124).% |
isOpIdChar(92).	% \
isOpIdChar(58).	% :


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some auxiliaries for handling expressions at run time:

% remove all share structures in a term:
removeShares(T,T) :- var(T), !.
removeShares(makeShare(T,_),UT) :- !, removeShares(T,UT).
removeShares(share(M),UT) :- !,
	get_mutable(V,M),
	(V='$eval'(Exp) -> true ; Exp=V),
	removeShares(Exp,UT).
removeShares(T,UT) :-
	T =.. [F|Args],
	map2M(basics:removeShares,Args,UArgs),
	UT =.. [F|UArgs].

% translate term into partcall term with missing arguments
% (used in the compiler and in dynamic predicates):
term2partcall(Term,Missing,TermOrPartCall) :-
	Missing=0 -> TermOrPartCall = Term
         ; (Missing>0 -> Term =.. [F|Args], rev(Args,RevArgs),
	                 TermOrPartCall = partcall(Missing,F,RevArgs)
	     ; Term =.. [F|Args], append(FArgs,[LArg],Args),
               M1 is Missing+1, FTerm =.. [F|FArgs],
	       term2partcall(FTerm,M1,FTermOrPartCall),
	       TermOrPartCall = 'Prelude.apply'(FTermOrPartCall,LArg)).

% is the argument a complete list structure, i.e., ends with the empty list?
% if yes, second arguments contains list without share structures
isCompleteList(Xs,Xs) :- var(Xs), !, fail.
isCompleteList(makeShare(T,_),L) :- !, isCompleteList(T,L).
isCompleteList(share(M),L) :- !, get_mutable(V,M),
	(V='$eval'(Exp) -> true ; Exp=V), isCompleteList(Exp,L).
isCompleteList([],[]).
isCompleteList([X|Xs],[X|L]) :- isCompleteList(Xs,L).

% is the argument a Prolog list structure
isList(Xs) :- var(Xs), !, fail.
isList([]) :- !.
isList([_|Xs]) :- isList(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print an error message:
printError(Error) :-
	prologError2Atom(Error,ErrorA),
	writeErr(ErrorA), nlErr,
        seen, told,
        !,
        fail.

% Transform a Prolog error term into an atom:
prologError2Atom(error(IsoError,_),ErrA) :-
        isoError2Atom(IsoError,ErrA), !.
prologError2Atom(error(_,Context),ErrA) :-
        errorContext2Atom(Context,ErrA), !.
prologError2Atom(ErrorTerm,ErrA) :-
        prologTerm2Atom(ErrorTerm,ErrorTermA),
	appendAtoms(['ERROR: ',ErrorTermA],ErrA).

% Try to format a meaningful ISO error term:
isoError2Atom(existence_error(ObjType,Culprit),ErrA) :-
        atom(ObjType), atom(Culprit), !,
	appendAtoms(['EXISTENCE ERROR: ',ObjType,' "',Culprit,
		     '" does not exist'],ErrA).
isoError2Atom(permission_error(Op,ObjType,Culprit),ErrA) :-
	atom(Op), atom(ObjType), atom(Culprit), !,
	appendAtoms(['PERMISSION ERROR: ',Op,' ',ObjType,' "',Culprit,'"'],
                    ErrA).

% Try to format a context error term:
errorContext2Atom(existence_error(Goal,_,_,_,Message),ErrA) :-
	nonvar(Message), Message = past_end_of_stream, !,
	prologTerm2Atom(Goal,GoalA),
	appendAtoms(['EXISTENCE ERROR: ',GoalA,
		     ': attempt to read past end of stream'],ErrA).
errorContext2Atom(existence_error(_Goal,_,ObjType,Culprit,_),ErrA) :-
	atom(ObjType), atom(Culprit), !,
	appendAtoms(['EXISTENCE ERROR: ',ObjType,' "',Culprit,
		     '" does not exist'],ErrA).
errorContext2Atom(permission_error(_Goal,_,ObjType,Culprit,Msg),ErrA) :-
	atom(ObjType), atom(Culprit), atom(Msg), !,
	appendAtoms(['PERMISSION ERROR: ',ObjType,' "',Culprit,'" ',Msg],ErrA).
errorContext2Atom(system_error(Msg),ErrA) :-
	atom(Msg), !,
	appendAtoms(['SYSTEM ERROR: ',Msg],ErrA).

prologTerm2Atom(V,'_') :- var(V), !.
prologTerm2Atom(A,A) :- atom(A), !.
prologTerm2Atom(N,A) :- number(N), !, number_codes(N,Cs), atom_codes(A,Cs).
prologTerm2Atom(T,A) :-
	T =.. [F|Args],
	map2M(basics:prologTerm2Atom,Args,[ArgA|ArgsA]),
	map2M(basics:prefixComma,ArgsA,CArgsA),
	concat([[F,'(',ArgA],CArgsA,[')']],Atoms),
	appendAtoms(Atoms,A).

prefixComma(Atom,CAtom) :- appendAtom(',',Atom,CAtom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of reading and showing single characters and strings.
% This is contained here since it is used by the primitives of the prelude
% and the library ReadShowTerm.


readChar([92,N|S],T,C) :- N>=48, N<58, !, % read decimal numeric char
	readDecimalChar(0,[N|S],T,C).
readChar([92,N,39|T],T,C) :- !,
	readStringChar(N,NS),
	char_int(C,NS).
readChar([92,78,85,76,39|T],T,C) :- !, char_int(C,0). % '\NUL' character
readChar([92,83,79,72,39|T],T,C) :- !, char_int(C,1). % '\SOH' character
readChar([92,83,84,88,39|T],T,C) :- !, char_int(C,2). % '\STX' character
readChar([92,69,84,88,39|T],T,C) :- !, char_int(C,3). % '\ETX' character
readChar([92,69,79,84,39|T],T,C) :- !, char_int(C,4). % '\EOT' character
readChar([92,69,78,81,39|T],T,C) :- !, char_int(C,5). % '\ENQ' character
readChar([92,65,67,75,39|T],T,C) :- !, char_int(C,6). % '\ACK' character
readChar([92,97,39|T],T,C) :- !, char_int(C,7). % '\a' character
readChar([92,66,69,76,39|T],T,C) :- !, char_int(C,7). % '\BEL' character
readChar([92,98,39|T],T,C) :- !, char_int(C,8). % '\b' character
readChar([92,66,83,39|T],T,C) :- !, char_int(C,8). % '\BS' character
readChar([92,116,39|T],T,C) :- !, char_int(C,9). % '\t' character
readChar([92,72,84,39|T],T,C) :- !, char_int(C,9). % '\HT' character
readChar([92,110,39|T],T,C) :- !, char_int(C,10). % '\n' character
readChar([92,76,70,39|T],T,C) :- !, char_int(C,10). % '\LF' character
readChar([92,118,39|T],T,C) :- !, char_int(C,11). % '\v' character
readChar([92,86,84,39|T],T,C) :- !, char_int(C,11). % '\VT' character
readChar([92,102,39|T],T,C) :- !, char_int(C,12). % '\f' character
readChar([92,70,70,39|T],T,C) :- !, char_int(C,12). % '\FF' character
readChar([92,114,39|T],T,C) :- !, char_int(C,13). % '\r' character
readChar([92,67,82,39|T],T,C) :- !, char_int(C,13). % '\CR' character
readChar([92,83,79,39|T],T,C) :- !, char_int(C,14). % '\SO' character
readChar([92,83,73,39|T],T,C) :- !, char_int(C,15). % '\SI' character
readChar([92,68,76,69,39|T],T,C) :- !, char_int(C,16). % '\DLE' character
readChar([92,68,67,49,39|T],T,C) :- !, char_int(C,17). % '\DC1' character
readChar([92,68,67,50,39|T],T,C) :- !, char_int(C,18). % '\DC2' character
readChar([92,68,67,51,39|T],T,C) :- !, char_int(C,19). % '\DC3' character
readChar([92,68,67,52,39|T],T,C) :- !, char_int(C,20). % '\DC4' character
readChar([92,78,65,75,39|T],T,C) :- !, char_int(C,21). % '\NAK' character
readChar([92,83,89,78,39|T],T,C) :- !, char_int(C,22). % '\SYN' character
readChar([92,69,84,66,39|T],T,C) :- !, char_int(C,23). % '\ETB' character
readChar([92,67,65,78,39|T],T,C) :- !, char_int(C,24). % '\CAN' character
readChar([92,69,77,39|T],T,C) :- !, char_int(C,25). % '\EM' character
readChar([92,83,85,66,39|T],T,C) :- !, char_int(C,26). % '\SUB' character
readChar([92,69,83,67,39|T],T,C) :- !, char_int(C,27). % '\ESC' character
readChar([92,70,83,39|T],T,C) :- !, char_int(C,28). % '\FS' character
readChar([92,71,83,39|T],T,C) :- !, char_int(C,29). % '\GS' character
readChar([92,82,83,39|T],T,C) :- !, char_int(C,30). % '\RS' character
readChar([92,85,83,39|T],T,C) :- !, char_int(C,31). % '\US' character
readChar([92,83,80,39|T],T,C) :- !, char_int(C,32). % '\SP' character
readChar([92,68,69,76,39|T],T,C) :- !, char_int(C,127). % '\DEL' character
readChar([N,39|T],T,C) :- char_int(C,N).

readDecimalChar(N,[39|T],T,C) :- !, char_int(C,N).
readDecimalChar(N,[M|S],T,C) :- M>=48, M<58, !,
	NM is 10*N+M-48,
	readDecimalChar(NM,S,T,C).

readString([34|T],T,[]) :- !.
readString([92|Ns],T,Str) :- !, readStringEscape(Ns,T,Str).
readString([N|Ns],T,[C|Str]) :-
	char_int(C,N),
	readString(Ns,T,Str).

readStringEscape([N|Ns],T,Str) :- N>=48, N<58, !,
        V is N-48, readDecimalCharInString(V,Ns,T,Str).
readStringEscape([69,83,67|Ns],T,[C|Str]) :- !, % '\ESC' character
        char_int(C,27),
        readString(Ns,T,Str).
readStringEscape([68,69,76|Ns],T,[C|Str]) :- !, % '\DEL' character
        char_int(C,127),
        readString(Ns,T,Str).
readStringEscape([N|Ns],T,[C|Str]) :- !,
	readStringChar(N,NS),
	char_int(C,NS),
	readString(Ns,T,Str).

% read a character with decimal number representation, e.g., '\243'
readDecimalCharInString(V,[N|Ns],T,Str) :- N>=48, N<58, !,
        V1 is V*10+N-48,
        readDecimalCharInString(V1,Ns,T,Str).
readDecimalCharInString(V,Ns,T,[C|Str]) :-
        char_int(C,V),
        readString(Ns,T,Str).

readStringChar(97,7) :- !.
readStringChar(98,8) :- !.
readStringChar(116,9) :- !.
readStringChar(110,10) :- !.
readStringChar(118,11) :- !.
readStringChar(102,12) :- !.
readStringChar(114,13) :- !.
readStringChar(34,34) :- !.
readStringChar(39,39) :- !.
readStringChar(92,92) :- !.
readStringChar(N,N) :-
	writeErr('INTERNAL ERROR: unknown character string "'),
	put_code(user_error,92), put_code(user_error,N),
	writeErr('" in readStringChar'), nlErr.

% The show functions use difference lists.

showTermChar( 7,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,97]).  % \a
showTermChar( 8,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,98]).  % \b
showTermChar( 9,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,116]). % \t
showTermChar(10,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,110]). % \n
showTermChar(11,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,118]). % \v
showTermChar(12,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,102]). % \f
showTermChar(13,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,114]). % \r
showTermChar(34,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,34]). % 34="
showTermChar(92,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,92]). % 92=\
showTermChar(N,[C1,C2,C3|E],E) :- N<32, !,
	N1 is (N//10)+48, N2 is (N mod 10)+48,
	cp_string([C1,C2,C3],[92,N1,N2]).
showTermChar(N,E,F) :- N>126, !,
	num2rdigits(N,RDs), rev(RDs,Ds),
	cp_string(DS,[92|Ds]),
	append(DS,F,E).
showTermChar(N,[C|E],E) :- char_int(C,N).

num2rdigits(N,[D]) :- N<10, !, D is N+48.
num2rdigits(N,[D|Ds]) :- D is (N mod 10)+48, N1 is N//10, num2rdigits(N1,Ds).


showTermString([],[Quot|E],E) :- char_int(Quot,34).
showTermString([C|T],S,E) :-
	char_int(C,N),
	showTermChar(N,S,ST),
	showTermString(T,ST,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
