%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% basic predicates related to the SWI-Prolog system

:- module(prologbasics,
	  [prolog/1, prologMajorVersion/1, prologMinorVersion/1, pakcsrc/2,
	   sicstus310orHigher/0,
	   atomCodes/2, atEndOfStream/1,
	   unifyWithOccursCheck/2,
	   waitConcurrentConjunction/6,
	   append/3, member/2,
	   appendAtom/3,
	   map1M/2, map2M/3, map1partialM/2, map2partialM/3,
	   getProgramArgs/1, getEnv/2,
	   noSingletonWarnings/0, noRedefineWarnings/0, noDiscontiguousWarnings/0,
	   getRunTime/1, getElapsedTime/1,
	   getCurrentMemorySize/1, getCurrentCodeSize/1,
	   getCurrentStackSize/1, getCurrentChoiceSize/1,
	   getCurrentHeapSize/1, getCurrentGCs/1,
	   garbageCollectorOn/0, garbageCollectorOff/0, garbageCollect/0,
	   workingDirectory/1, setWorkingDirectory/1,
	   fileModTime/2, fileSize/2, existsFile/1, existsDirectory/1,
	   makeDirectory/1, directoryFiles/2, deleteFile/1, deleteDirectory/1,
	   renameFile/2, renameDirectory/2,
	   fileExistsAndNewer/2, canWriteFile/1, currentPID/1, sleepSeconds/1,
	   getHostname/1, shellCmd/1, shellCmd/2,
	   execCommand/4, forkProcessForGoal/1,
	   isInputStream/1, isOutputStream/1,
	   currentClockTime/1, clocktime2localtime/8, clocktime2utctime/7,
	   date2clocktime/8,
	   connect2socket/4, closeSocketStream/2,
	   listenOnNewSocket/3, socketAccept/4, socketClose/1,
	   waitForInputDataOnStreams/3, waitForSocketClientStream/5,
	   waitForSocketOrInputStreams/6,
	   try_save_program/1, saveprog_entry/2, runtime_entry/0,
	   try_save_predicates/2,
	   compilePrologFile/1, compilePrologFileAndSave/1,
	   consultPrologorPOFile/2, ensure_lib_loaded/1,
	   getNewPrologFileName/1, mainPrologFileName/2,
	   callAndReturnSuspensions/2, writeqWithVars/1,
	   genBlockDecl/4,
	   prolog_flag/2, prolog_flag/3,
	   create_mutable/2, get_mutable/2, update_mutable/2]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Principle kind of Prolog system and version used for this implementation.
prolog(swi).

prologMajorVersion(MV) :-
	current_prolog_flag(version,VN),
	MV is VN//10000.

prologMinorVersion(MV) :-
	current_prolog_flag(version,VN),
	MV is (VN mod 10000)//100.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile pakcsrc/2. % relevant for createSavedState

:- dynamic pakcsrc/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unifyWithOccursCheck(T1,T2) :- unify_with_occurs_check(T1,T2).

append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

member(E,[E|_]).
member(E,[_|Xs]) :- member(E,Xs).

% concatenate two atoms:
appendAtom(A1,A2,A3) :-
	atom_codes(A1,L1), atom_codes(A2,L2),
	append(L1,L2,L3),
	atom_codes(A3,L3).

% atomCodes is defined here in order to use this version for
% higher-order applications:
atomCodes(A,L) :- atom_codes(A,L).

% check the end of a stream:
atEndOfStream(Stream) :- at_end_of_stream(Stream), !.

sicstus310orHigher :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% import the right libraries:

:- use_module(library(unix)).
:- use_module(library(socket)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of suspension for concurrent conjunction (&)

waitConcurrentConjunction(S1,S2,R,E1,E2,E) :-
	when((nonvar(E1) ; nonvar(E2)),
	     waitConcurrentConjunctionBlocked(S1,S2,R,E1,E2,E)).
waitConcurrentConjunctionBlocked(S1,S2,R,E1,E2,E) :- nonvar(E1), !,
	(S1='FAIL'(_) -> R=S1, E=E1 ; waitForEval(S2,R,E2,E)).
waitConcurrentConjunctionBlocked(S1,S2,R,E1,E2,E) :- % E2 must be nonvar
	(S2='FAIL'(_) -> R=S2, E=E2 ; waitForEval(S1,R,E1,E)).

waitForEval(R0,R,E0,E) :- freeze(E0,(R0=R, E0=E)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta-predicates for higher-order programming:

% map a unary predicate on a list:
map1M(P,Xs) :- map1FstM(Xs,P).
map1FstM([],_).
map1FstM([X|Xs],M:P) :- C =.. [P,X], call(M:C), map1FstM(Xs,M:P).

% map a binary predicate on two lists:
map2M(P,Xs,Ys) :- map2FstM(Xs,P,Ys).

% map a binary predicate on two lists with first argument indexing:
map2FstM([],_,[]).
map2FstM([X|Xs],M:P,[Y|Ys]) :- C =.. [P,X,Y], call(M:C), map2FstM(Xs,M:P,Ys).

% map a unary predicate (which might be a partial application) on a list:
map1partialM(P,Xs) :- map1partialFstM(Xs,P).
map1partialFstM([],_).
map1partialFstM([X|Xs],M:P) :-
	P =.. [Pred|PartialArgs],
	append(PartialArgs,[X],Args),
	C =.. [Pred|Args], call(M:C),
	map1partialFstM(Xs,M:P).

% map a binary predicate (which might be a partial application) on two lists:
map2partialM(P,Xs,Ys) :- map2partialFstM(Xs,P,Ys).
map2partialFstM([],_,[]).
map2partialFstM([X|Xs],M:P,[Y|Ys]) :-
	P =.. [Pred|PartialArgs],
	append(PartialArgs,[X,Y],Args),
	C =.. [Pred|Args], call(M:C),
	map2partialFstM(Xs,M:P,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to the system environment:

% get program arguments:
getProgramArgs(Args) :-
	current_prolog_flag(argv,AllArgs),
	(append(_,['--'|Args],AllArgs)
          -> true  % for backward compatibility
           ; dropSWIPL(AllArgs,Args)).

dropSWIPL([],[]).
dropSWIPL([Exec|Args],Args) :-  % for backward compatibility
	atom_codes(Exec,ExecS),
	atom_codes(swipl,SWIPL),
	append(_,SWIPL,ExecS), % first argument ends with 'swipl'?
	!.
dropSWIPL(Args,Args).


% get value of environment variable (fails if it is not set):
getEnv(Var,Val) :- getenv(Var,Val), !.

% no warnings for singleton variables:
noSingletonWarnings :- style_check(-singleton).

% turn off discontiguous clauses warnings:
noDiscontiguousWarnings :- style_check(-discontiguous).

% no warnings for redefining predicates:
noRedefineWarnings :- true. % TODO, no solution yet

% get current run time in msecs:
getRunTime(Time) :- statistics(runtime,[Time,_]).

% get current elapsed time in msecs:
getElapsedTime(Time) :-
	statistics(runtime,[RTime,_]), statistics(system_time,[STime,_]),
	Time is RTime+STime.

% get current size of total memory of Curry process in bytes:
getCurrentMemorySize(S) :- statistics(memory,[S,_]).

% get current size of code area in bytes:
getCurrentCodeSize(S) :- statistics(program,[S,_]).

% get current size of local stack in bytes:
getCurrentStackSize(S) :- statistics(local_stack,[S,_]).

% get current size of choicepoint stack in bytes:
getCurrentChoiceSize(_) :- fail.

% get current size of heap in bytes:
getCurrentHeapSize(S) :- statistics(global_stack,[S,_]).

% get current number of garbage collections:
getCurrentGCs(N) :- statistics(garbage_collection,[N,_,_]).

% turn on garbage collector:
garbageCollectorOn :- set_prolog_flag(gc,true).
% turn off garbage collector:
garbageCollectorOff :- set_prolog_flag(gc,false).
% invoke the garbage collector:
garbageCollect :- garbage_collect.

% get current working directory:
workingDirectory(Dir) :-
	working_directory(CDir,CDir),
	atom_codes(CDir,CDirS),
	(append(DirS,[47],CDirS) % check for trailing slash
	 -> atom_codes(Dir,DirS)
	  ; Dir=CDir).

% set current working directory:
setWorkingDirectory(Dir) :- working_directory(_,Dir).

% get modification time of a file:
fileModTime(File,ClockTime) :-
	time_file(File,FTime), ClockTime is truncate(FTime).


% get modification time of a file:
fileSize(File,Size) :- size_file(File,Size).

% does a file exist?
existsFile(File) :- exists_file(File).

% does a file exist?
existsDirectory(Dir) :- exists_directory(Dir).

% create a new directory:
makeDirectory(Dir) :- make_directory(Dir).

% get all files in a directory:
directoryFiles(Dir,Files) :-
	appendAtom(Dir,'/*',DirPattern),  % */ for correct fontifying
	expand_file_name(DirPattern,DirNotDotFiles),
	appendAtom(Dir,'/.*',DirDotPattern),
	expand_file_name(DirDotPattern,DirDotFiles),
	append(DirDotFiles,DirNotDotFiles,DirFiles),
	map2M(user:file_base_name,DirFiles,Files).

% remove a file from the file system:
deleteFile(File) :- delete_file(File).

% remove a directory from the file system:
deleteDirectory(Dir) :- delete_directory(Dir).

% remove a file from the file system:
renameFile(File1,File2) :- rename_file(File1,File2).

% remove a directory from the file system:
renameDirectory(Dir1,Dir2) :- rename_file(Dir1,Dir2).

% fileExistsAndNewer(f1,f2) is true if file f1 exists and is newer than f2:
fileExistsAndNewer(File1,File2) :-
	existsFile(File1),
	fileModTime(File1,MT1),
	fileModTime(File2,MT2),
	MT1>=MT2.

% can I write a file (i.e., write and immediately close it)?
canWriteFile(File) :-
	on_exception(_ErrorMsg,
	             (open(File,write,Stream), close(Stream)),
		     fail).

% process of identifer of current Prolog process:
currentPID(Pid) :- current_prolog_flag(pid,Pid).

% put the current process asleep for the given amount of seconds:
sleepSeconds(S) :- sleep(S).

% get name of current host:
getHostname(Name) :- gethostname(Name).

% execute a shell command and fail, if not successful:
shellCmd(Cmd) :- shell(Cmd,0).

% execute a shell command and return exit status:
shellCmd(Cmd,Status) :- shell(Cmd,Status).

% execute a shell command in background and return the input, output, and
% error stream connected to this command:
execCommand(Cmd,InWrite,OutRead,ErrRead) :-
	pipe(InRead,InWrite),
	pipe(OutRead,OutWrite),
	(ErrRead==std -> true ; pipe(ErrRead,ErrWrite)),
	fork(Pid),
	(Pid=child
	 -> close(InWrite), dup(InRead,user_input),
	    close(OutRead), dup(OutWrite,user_output),
	    (ErrRead==std -> true ; close(ErrRead), dup(ErrWrite,user_error)),
	    exec(sh('-c',Cmd))
	  ; close(InRead),
	    close(OutWrite),
	    (ErrRead==std -> true ; close(ErrWrite))).


% fork the current program state with an initial goal to execute
% (without producing any output):
forkProcessForGoal(Goal) :-
	fork(Pid),
	(Pid=child
	 -> open('/dev/null',write,Null),
	    dup(Null,user_output), dup(Null,user_error),
	    call(Goal)
	  ; true).


% is a stream a readable stream?
isInputStream(Stream) :- stream_property(Stream,input).

% is a stream a writable stream?
isOutputStream(Stream) :- stream_property(Stream,output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Time and date operations

% get current clocktime (Unix timestamp in seconds):
currentClockTime(ClockTime) :-
	get_time(Timestamp), ClockTime is truncate(Timestamp).

% convert a Unix timestamp into a (local) date:
clocktime2localtime(ClockTime,Year,Month,Day,Hour,Min,Sec,TZ) :-
	convert_time(ClockTime,Year,Month,Day,Hour,Min,Sec,_),
	date2clocktime(Year,Month,Day,Hour,Min,Sec,0,LocalClockTime),
	TZ is LocalClockTime-ClockTime.

% transform a 6-tupel datime into Unix time, i.e., the number of seconds elapsed
% from January 1, 1970:
date2clocktime(Year,Month,Day,Hour,Min,Sec,TZ,ClockTime) :-
	completeYearDaysFrom1970(Year,YearDays),
	completeDaysInYear(Year,Month,Day,MonthDays),
	ClockTime is (YearDays+MonthDays)*24*3600+Hour*3600+Min*60+Sec-TZ.

completeYearDaysFrom1970(1970,0) :- !.
completeYearDaysFrom1970(Year,Days) :-
	Year1 is Year-1,
	daysOfYear(Year1,YearDays),
	completeYearDaysFrom1970(Year1,Year1Days),
	Days is YearDays+Year1Days.

completeDaysInYear(Year,Month,Day,Days) :-
	Month1 is Month-1,
	daysOfMonthsInYear(Month1,Year,Month1Days),
	Days is Month1Days+Day-1.

daysOfMonthsInYear(0,_,0) :- !.
daysOfMonthsInYear(Month,Year,Days) :-
	daysOfMonth(Month,Year,MonthDays),
	Month1 is Month-1,
	daysOfMonthsInYear(Month1,Year,Month1Days),
	Days is MonthDays+Month1Days.

daysOfYear(Year,Days) :-
	daysOfMonth(2,Year,FebDays),
	Days is FebDays+337.

daysOfMonth(2,Year,29) :-
	0 is Year mod 4, (Year mod 100 > 0 ; 0 is Year mod 400), !.
daysOfMonth(2,_,28) :- !.
daysOfMonth(Month,_,Days) :-
	Month1 is Month-1,
	elemAt(Month1,[31,28,31,30,31,30,31,31,30,31,30,31],Days).

% get n-th element of a list (n=0: head):
elemAt(0,[X|_],X) :- !.
elemAt(N,[_|Xs],X) :- N1 is N-1, elemAt(N1,Xs,X).


% convert a Unix timestamp into a UTC time:
clocktime2utctime(ClockTime,Year,Month,Day,Hour,Min,Sec) :-
	Sec is ClockTime mod 60,
	Mins is ClockTime // 60,
	Min is Mins mod 60,
	Hours is Mins // 60,
	Hour is Hours mod 24,
	Days is Hours // 24,
	days2year(Days,1970,Year,DaysInCurrentYear),
	days2month(DaysInCurrentYear,Year,1,Month,Day).

days2year(Days,CYear,Year,DaysInCurrentYear) :-
	daysOfYear(CYear,CYearDays),
	(CYearDays > Days -> Year=CYear, DaysInCurrentYear = Days
	 ; NDays is Days-CYearDays, NYear is CYear+1,
	   days2year(NDays,NYear,Year,DaysInCurrentYear)).

days2month(Days,Year,CMonth,Month,Day) :-
	daysOfMonth(CMonth,Year,CMonthDays),
	(CMonthDays > Days -> Month=CMonth, Day is Days+1
	 ; NDays is Days-CMonthDays, NMonth is CMonth+1,
	   days2month(NDays,Year,NMonth,Month,Day)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to sockets.

% Create a connection to a remote socket and return instream and outstream:
connect2socket(Host,Port,InStream,OutStream) :-
	tcp_socket(Socket),
	tcp_connect(Socket,Host:Port),
	tcp_open_socket(Socket,InStream,OutStream).

% close socket stream connection:
closeSocketStream(InStream,OutStream) :- close(InStream), close(OutStream).

% Create a server socket on a port. The port is an integer or a free variable
% (in this case, it is bound to a free port number).
% The hostname and the new socket is returned.
listenOnNewSocket(Port,Hostname,Socket) :-
	tcp_socket(Socket),
	tcp_bind(Socket,Port),
	tcp_listen(Socket,1024),
	gethostname(Hostname).

% return the read and write stream of a first connection to a socket:
socketAccept(Socket,Client,InStream,OutStream) :-
        tcp_open_socket(Socket,AcceptStream,_),
	tcp_accept(AcceptStream,SocketConnection,ClientIP),
	ip2atom(ClientIP,Client),
	%tcp_host_to_address(Client,ClientIP),
	tcp_open_socket(SocketConnection,InStream,OutStream).

% return the read and write stream of a first connection to a socket:
socketClose(Socket) :- tcp_close_socket(Socket).

ip2atom(ip(B1,B2,B3,B4),A) :-
	number_codes(B1,B1s), number_codes(B2,B2s),
	number_codes(B3,B3s), number_codes(B4,B4s),
	append(B1s,[46|B2s],B12s),
	append(B12s,[46|B3s],B123s),
	append(B123s,[46|B4s],B1234s),
	atom_codes(A,B1234s).

% Wait for input data on a list of streams (Timeout = TOSec:TOMSec or off),
% returns -1 if no data available within Timeout limit, otherwise the index of
% the corresponding stream:
waitForInputDataOnStreams(InStreams,Timeout,Index) :-
	timeoutAsSWI(Timeout,TimeoutSWI),
	wait_for_input(InStreams,ReadyStreams,TimeoutSWI), !,
	(ReadyStreams = [] -> Index = -1
                            ; ReadyStreams = [Stream|_],
                              streamIndex(InStreams,Stream,Index)).

streamIndex([S|_],S,0) :- !.
streamIndex([_|Streams],S,I) :- streamIndex(Streams,S,I1), I is I1+1.

% Wait for a client connection at a socket (Timeout = TOSec:TOMSec or off),
% fails if no data available within Timeout limit
waitForSocketClientStream(Socket,Timeout,Client,InStream,OutStream) :-
	timeoutAsSWI(Timeout,TimeoutSWI),
	tcp_open_socket(Socket,AcceptStream,_),
	wait_for_input([AcceptStream],ReadyStreams,TimeoutSWI), !,
	ReadyStreams = [AcceptStream],
	tcp_accept(AcceptStream,Slave,ClientIP),
	ip2atom(ClientIP,Client),
	tcp_open_socket(Slave,InStream,OutStream).

% Wait for a client connection at a socket or available stream input data.
% If a client connection is established, InPortStream and OutPortStream
% are instantiated to the client stream connection and Client is
% instantiated to the clients address, otherwise Client is instantiated
% to 'no' and the last argument is instantiated to the InStreams index with
% available data.
waitForSocketOrInputStreams(Socket,Client,InPortStream,OutPortStream,
			    InStreams,Index) :-
	tcp_open_socket(Socket,AcceptStream,_),
	wait_for_input([AcceptStream|InStreams],ReadyStreams,infinite), !,
	(member(AcceptStream,ReadyStreams)
	 -> tcp_accept(AcceptStream,Slave,ClientIP),
	    ip2atom(ClientIP,Client),
	    tcp_open_socket(Slave,InPortStream,OutPortStream)
	  ; Client=no,
	    ReadyStreams = [Stream|_],
	    streamIndex(InStreams,Stream,Index)).

timeoutAsSWI(Timeout,infinite) :- Timeout<0, !.
timeoutAsSWI(Timeout,TO) :- TO is Timeout/1000.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Saving program states

saveprog_entry(State,Entry) :-
	pakcsrc(standalone,yes), !,
	(retract(user:rt_entry(_)) -> true ; true),
	asserta(user:rt_entry(Entry)),
	qsave_program(State,[toplevel(runtime_entry),stand_alone(true)]).
saveprog_entry(State,Entry) :-
	(retract(user:rt_entry(_)) -> true ; true),
	asserta(user:rt_entry(Entry)),
	%export(user:functiontype(_,_,_,_,_,_)),
	qsave_program(State,[toplevel(runtime_entry)]).

:- dynamic user:rt_entry/1.

runtime_entry :-
	user:rt_entry(Entry),
	call(Entry).


% try to save a user predicate in a .po file if it is supported by this
% Sicstus version:
try_save_predicates(_,_).


% try to save an already compiled Prolog program in a .po file if it is supported by this
% Sicstus version:
try_save_program(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for compiling programs and loading run-time libraries:

% compile a Prolog file:
compilePrologFile(PrologFileName) :- compile(user:PrologFileName).

% compile a Prolog file and try to save it in fast load format:
compilePrologFileAndSave(PrologFileName) :-
	compilePrologFile(PrologFileName), try_save_program(PrologFileName).

% consult a Prolog file or a .po file if it exists (not in SWI)
consultPrologorPOFile(PrologFileName,_POFileName) :-
	consult(user:PrologFileName).


% directory containing the system run-time modules:
moduleDir(MD) :-
        getEnv('PAKCSHOME',TCP),
        appendAtom(TCP,'/curry2prolog/libswi/',MD).

% ensure that run-time library is loaded:
ensure_lib_loaded(Lib) :- % first, look into working directory:
	workingDirectory(WDir),
	appendAtom(WDir,'/',Dir),
	appendAtom(Dir,Lib,DirLib),
	appendAtom(DirLib,'.pl',DirLibPl),
	existsFile(DirLibPl), !,
	ensure_loaded(user:DirLib).
ensure_lib_loaded(Lib) :-
	moduleDir(Dir),
	appendAtom(Dir,Lib,DirLib),
	ensure_loaded(user:DirLib).


% get name of temporary Prolog file:
getNewPrologFileName(PrologFile) :-
	tmp_file(curry_main,Main),
	atom_codes(Main,MainS),
	append(MainS,".pl",ProgS),
	atom_codes(PrologFile,ProgS),
	append("rm -f ",ProgS,RmCmdS),
	atom_codes(RmCmd,RmCmdS),
	shellCmd(RmCmd).


% determine for a given Prolog file name (of the main module) a file name
% where the clauses for the main predicates (hnf, constrEq,...) should be stored:
mainPrologFileName(_PrologFile,MainPrologFile) :-
	mainPrologFileName(MainPrologFile) -> true
	 ; tmp_file(pakcs_main,MainPrologFile),
	   assertz(mainPrologFileName(MainPrologFile)).

% for storing the file name during the Prolog session:
:- dynamic mainPrologFileName/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call a goal and return list of suspended goals (here always empty):
callAndReturnSuspensions(Goal,Suspensions) :-
	%call(Goal), Suspensions=[]. % for SWI < 5.6.60
	call_residue_vars(Goal,Vars),
	copy_term(Vars,_,SuspGoal),
	omitFreezeGoals(SuspGoal,Suspensions).

omitFreezeGoals([freeze(_,Goal)|Gs],NGs) :- !, omitFreezeGoals([Goal|Gs],NGs).
omitFreezeGoals([user:freeze(_,Goal)|Gs],NGs) :- !,
	omitFreezeGoals([Goal|Gs],NGs).
omitFreezeGoals([Goal|Gs],[Goal|NGs]) :- omitFreezeGoals(Gs,NGs).
omitFreezeGoals([],[]).

% write a Prolog term possibly containing variables:
writeqWithVars(T) :- writeq(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a block declaration for predicate PredName of arity PredArity
% where the positions in the non-empty(!) list GroundPositions must be instantiated.
% Furthermore, the last argument is a possibly new predicate name corresponding
% to PredName which should be coded instead of PredName (this depends on
% the implementation scheme for block declarations).
genBlockDecl(PredName,PredArity,BoundPositions,NewPredName) :-
	appendAtom('blocked_',PredName,NewPredName),
	functor(Literal,PredName,PredArity),
	Literal =.. [_|Args],
	NewLiteral =.. [NewPredName|Args],
	genFreezeLiteral(BoundPositions,NewLiteral,NewLiteral,FreezeLiteral),
	compiler:writeClause((Literal :- FreezeLiteral)).

genFreezeLiteral([],_,Literal,Literal) :- !.
genFreezeLiteral([P|Ps],Literal,FreezeLiteral,NewFreezeLiteral) :-
	arg(P,Literal,Var),
	genFreezeLiteral(Ps,Literal,freeze(Var,FreezeLiteral),NewFreezeLiteral).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mutable values (simulated by open-ended lists)

create_mutable(V,'$mutable'([V|_])).

get_mutable(V,'$mutable'(L)) :- get_mutable_aux(V,L).
get_mutable_aux(V,[H|T]) :- var(T) -> V=H ; get_mutable_aux(V,T).

update_mutable(V,'$mutable'(L)) :- update_mutable_aux(V,L).
update_mutable_aux(V,[_|T]) :- var(T) -> T=[V|_] ; update_mutable_aux(V,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prolog_flag(user_input,user_input) :- !.
prolog_flag(user_output,user_output) :- !.
prolog_flag(user_error,user_error) :- !.
prolog_flag(F,_) :- write('Warning: unknown prolog_flag: '), write(F), nl.

prolog_flag(F,V,V) :- write('Warning: unknown prolog_flag: '), write(F), nl.
