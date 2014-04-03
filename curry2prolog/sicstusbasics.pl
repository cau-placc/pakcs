%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% basic predicates related to the SICStus-Prolog system

:- module(prologbasics,
	  [prolog/1, prologMajorVersion/1, prologMinorVersion/1, pakcsrc/2,
	   sicstus310orHigher/0, generatePrologBasics/0,
%SICS3X	   append/3, member/2,
%SICS37	   atom_codes/2, number_codes/2,
%SICS37	   put_code/1, put_code/2, put_byte/2,
%SICS37	   get_code/1, get_code/2, get_byte/2,
	   atomCodes/2, atEndOfStream/1,
	   unifyWithOccursCheck/2,
	   waitConcurrentConjunction/6,
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
	   waitForInputDataOnStreams/3,
	   waitForSocketClientStream/5, waitForSocketOrInputStreams/6,
	   try_save_program/1, saveprog_entry/2, try_save_predicates/2,
	   ensure_lib_loaded/1, compilePrologFile/1,
	   compilePrologFileAndSave/1, consultPrologorPOFile/2,
	   getNewPrologFileName/1, mainPrologFileName/2,
	   callAndReturnSuspensions/2, writeqWithVars/1,
	   genBlockDecl/4]).

:- use_module(pakcsversion).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Principle kind of Prolog system and version used for this implementation.
prolog(sicstus).

prologMajorVersion(MV) :-
	prolog_flag(version,V,V),
	atom_codes(V,Vs),
	app("SICStus ",[MC|_],Vs),
	MV is MC-48, !.

prologMinorVersion(MV) :-
	prolog_flag(version,V,V),
	atom_codes(V,Vs),
	app("SICStus ",[_,46|Vs1],Vs),
	app(MVs,[46|_],Vs1),
	number_codes(MV,MVs), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile pakcsrc/2. % relevant for createSavedState

:- dynamic pakcsrc/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unifyWithOccursCheck(T1,T2) :-
	sicstus38orHigher -> unify_with_occurs_check(T1,T2)
	                   ; T1=T2. % could be improved...

% concatenate two lists:
%SICS3X	append([],Xs,Xs).
%SICS3X	append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).
%SICS3X	member(E,[E|_]).
%SICS3X	member(E,[_|Xs]) :- member(E,Xs).

% we call it app to avoid name conflicts with SICStus 4:
app([],Xs,Xs).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

% concatenate two atoms:
appendAtom(A1,A2,A3) :-
	atom_codes(A1,L1), atom_codes(A2,L2),
	app(L1,L2,L3),
	atom_codes(A3,L3).

% concatenate a list of lists:
concat([],[]).
concat([L|Xs],LXs) :- concat(Xs,Ys), app(L,Ys,LXs).

%SICS37	atom_codes(A,L) :- atom_chars(A,L).
%SICS37	number_codes(A,L) :- number_chars(A,L).
%SICS37 put_code(C) :- put(C).
%SICS37 put_code(S,C) :- put(S,C).
%SICS37 put_byte(S,C) :- put(S,C).
%SICS37 get_code(C) :- get0(C).
%SICS37 get_code(S,C) :- get0(S,C).
%SICS37 get_byte(S,C) :- get0(S,C).
%SICS37 peek_code(S,C) :- peek_char(S,C).

% atomCodes is defined here in order to use this version for
% higher-order applications:
atomCodes(A,L) :- atom_codes(A,L).

% check the end of a stream:
atEndOfStream(Stream) :- at_end_of_stream(Stream), !.
atEndOfStream(Stream) :- peek_code(Stream,C), C = -1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get Sicstus version (e.g., '3.9'):
% Currently, we support only 3.5, 3.7, 3.8, 3.9, 3.10, 3.11, 3.12, 4.0, 4.1, 4.2:
getSicstusVersion(SV) :-
	prolog_flag(version,V,V),
	atom_chars(V,Vs),
	getSicstusVersionChars(Vs,SV), !.
getSicstusVersion(SV) :-
	prolog_flag(version,V,V),
	atom_codes(V,Vs),
	(app("SICStus 3.8",_,Vs) -> SV='3.8' ;
	 app("SICStus 3.9",_,Vs) -> SV='3.9' ;
	 app("SICStus 3.10",_,Vs) -> SV='3.10' ;
	 app("SICStus 3.11",_,Vs) -> SV='3.11' ;
	 app("SICStus 3.12",_,Vs) -> SV='3.12' ;
	 app("SICStus 4.0",_,Vs)  -> SV='4.0' ;
	 app("SICStus 4.1",_,Vs)  -> SV='4.1' ;
	 app("SICStus 4.2",_,Vs)  -> SV='4.2' ;
	 write(user_error,'ERROR: UNKNOWN SICSTUS PROLOG VERSION:'),
	 nl(user_error),
	 write(user_error,'PLEASE MODIFY pakcs/curry2prolog/sicstusbasics.pl'),
	 nl(user_error), halt(1)).

% special handling since atom_codes are not available in these versions:
getSicstusVersionChars(Vs,'3.5') :- app("SICStus 3.5",_,Vs).
getSicstusVersionChars(Vs,'3.7') :- app("SICStus 3.7",_,Vs).

sicstus37orHigher :-
	getSicstusVersion(SV),
	(SV = '3.7' ; sicstus38orHigher).

sicstus38orHigher :-
	getSicstusVersion(SV),
	(SV = '3.8' ; sicstus39orHigher).

sicstus385orHigher :- sicstus39orHigher, !.
sicstus385orHigher :-
	prolog_flag(version,V,V),
	atom_codes(V,Vs),
	app("SICStus 3.8.",[MV|_],Vs),
	MV>=53.

sicstus39orHigher :-
	getSicstusVersion(SV),
	(SV = '3.9' ; sicstus310orHigher).

sicstus310orHigher :-
	getSicstusVersion(SV),
	(SV = '3.10' ; SV = '3.11' ; SV = '3.12' ; sicstus40orHigher).

sicstus40orHigher :-
	getSicstusVersion(SV),
	(SV = '4.0' ; SV = '4.1' ; SV = '4.2').


generatePrologBasics :-	sicstus40orHigher, !,
	shellCmd('cp sicstusbasics.pl prologbasics.pl').
generatePrologBasics :-	sicstus38orHigher, !,
	system('sed "s/%SICS3X/ /g" < sicstusbasics.pl > prologbasics.pl').
generatePrologBasics :-
	system('sed "s/%SICS3X/ /g" < sicstusbasics.pl | sed "s/%SICS37/ /g" > prologbasics.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% import the right libraries:
:- sicstus40orHigher
   -> use_module(library(file_systems)),
      use_module(library(process))
    ; true.

:- use_module(library(system)).
:- use_module(library(sockets)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of suspension for concurrent conjunction (&)

?- block waitConcurrentConjunction(?,?,?,-,-,?).
waitConcurrentConjunction(S1,S2,R,E1,E2,E) :- nonvar(E1), !,
	reduceConcurrentConjunction(S1,S2,R,E1,E2,E).
waitConcurrentConjunction(S1,S2,R,E1,E2,E) :- % E2 must be nonvar
	reduceConcurrentConjunction(S2,S1,R,E2,E1,E).

% reduce a concurrent conjunction where the first argument is already evaluated
reduceConcurrentConjunction('Prelude.success',S2,R,_,E2,E) :-
	!, % first constraint is successful
	waitForEval(S2,R,E2,E).
reduceConcurrentConjunction('FAIL'(X),_,R,E1,_,E) :-
	!, % first constraint is a failure
	R='FAIL'(X), E=E1.
reduceConcurrentConjunction(_,_,_,_,_,_) :-
	write(user_error,'Internal error in waitConcurrentConjunction'),
	nl(user_error).

?- block waitForEval(?,?,-,?).
waitForEval(R,R,E,E).


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
	app(PartialArgs,[X],Args),
	C =.. [Pred|Args], call(M:C),
	map1partialFstM(Xs,M:P).

% map a binary predicate (which might be a partial application) on two lists:
map2partialM(P,Xs,Ys) :- map2partialFstM(Xs,P,Ys).
map2partialFstM([],_,[]).
map2partialFstM([X|Xs],M:P,[Y|Ys]) :-
	P =.. [Pred|PartialArgs],
	app(PartialArgs,[X,Y],Args),
	C =.. [Pred|Args], call(M:C),
	map2partialFstM(Xs,M:P,Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to the system environment:

% get program arguments:
getProgramArgs(Args) :- prolog_flag(argv,Args,Args).

% get value of environment variable (fails if it is not set):
getEnv(Var,Val) :- environ(Var,Val), !.

% no warnings for singleton variables:
noSingletonWarnings :- prolog_flag(single_var_warnings,_,off).

% no warnings for redefining predicates:
noRedefineWarnings :- prolog_flag(redefine_warnings,_,off).

% turn off discontiguous clauses warnings:
noDiscontiguousWarnings :-
	sicstus38orHigher -> prolog_flag(discontiguous_warnings ,_,off) ; true.

% get current run time in msecs:
getRunTime(Time) :- statistics(runtime,[Time,_]).

% get current elapsed time in msecs:
getElapsedTime(Time) :- statistics(walltime,[Time,_]).

% get current size of total memory of Curry process in bytes:
getCurrentMemorySize(S) :- statistics(memory,[S,_]).

% get current size of code area in bytes:
getCurrentCodeSize(S) :- statistics(program,[S,_]).

% get current size of local stack in bytes:
getCurrentStackSize(S) :- statistics(local_stack,[S,_]).

% get current size of choicepoint stack in bytes:
getCurrentChoiceSize(S) :- statistics(choice,[S,_]).

% get current size of heap in bytes:
getCurrentHeapSize(S) :- statistics(global_stack,[S,_]).

% get current number of garbage collections:
getCurrentGCs(N) :- statistics(garbage_collection,[N,_,_]).

% turn on garbage collector:
garbageCollectorOn :- prolog_flag(gc,_,on).
% turn off garbage collector:
garbageCollectorOff :- prolog_flag(gc,_,off).
% invoke the garbage collector:
garbageCollect :- garbage_collect.

% get current working directory:
workingDirectory(Dir) :-
	sicstus40orHigher
	-> current_directory(CDir),
	   atom_codes(CDir,CDirS),
	   (append(DirS,[47],CDirS) % check for trailing slash
	    -> atom_codes(Dir,DirS)
	     ; Dir=CDir)
	 ; working_directory(Dir,Dir).

% set current working directory:
setWorkingDirectory(Dir) :-
	sicstus40orHigher
	-> current_directory(_,Dir)
	 ; working_directory(_,Dir).

% get modification time of a file:
fileModTime(File,ClockTime) :-
	sicstus40orHigher
	-> file_property(File,modify_timestamp,ClockTime)
	 ; file_property(File,mod_time(ClockTime)).

% get modification time of a file:
fileSize(File,Size) :-
	sicstus40orHigher
	-> file_property(File,size_in_bytes,Size)
	 ; file_property(File,size(Size)).

% does a file exist and is a regular file?
existsFile(File) :- 
	sicstus40orHigher
	-> file_exists(File)
	 ; file_exists(File), file_property(File,type(regular)).

% does a directory exist?
existsDirectory(Dir) :-
	sicstus40orHigher
	-> directory_exists(Dir)
	 ; file_exists(Dir), file_property(Dir,type(directory)).

% create a new directory:
makeDirectory(Dir) :- make_directory(Dir).

% get all files in a directory:
directoryFiles(Dir,Files) :-
	sicstus40orHigher
	-> directory_exists(Dir),
	   absolute_file_name(Dir, AbsDir,
	                      [file_type(directory),access(exist)]),
	   findall(M,absolute_file_name(AbsDir, M,[glob(*),
                                                   solutions(all),
                                                   file_errors(fail)]),
                   AbsFiles),
	   map2partialM(prologbasics:removeDirPrefix(AbsDir),
	                AbsFiles,RelFiles),
	   Files = ['.','..'|RelFiles]
	 ; directory_files(Dir,Files).

removeDirPrefix(Dir,AbsFile,File) :-
	atom_codes(Dir,DirS),
	atom_codes(AbsFile,AbsFileS),
	append(DirS,FileS,AbsFileS),
	atom_codes(File,FileS).
	

% remove a file from the file system:
deleteFile(File) :-
	sicstus40orHigher
	-> delete_file(File)
	 ; delete_file(File,[]).

% remove a directory from the file system:
deleteDirectory(Dir) :-
	sicstus40orHigher
	-> delete_directory(Dir)
	 ; delete_file(Dir,[directory]).

% remove a file from the file system:
renameFile(File1,File2) :- rename_file(File1,File2).

% remove a directory from the file system:
renameDirectory(Dir1,Dir2) :-
	sicstus40orHigher
	-> rename_directory(Dir1,Dir2)
	 ; rename_file(Dir1,Dir2).

% fileExistsAndNewer(f1,f2) is true if file f1 exists and is newer than f2:
fileExistsAndNewer(File1,File2) :-
	file_exists(File1),
	fileModTime(File1,MT1),
	fileModTime(File2,MT2),
	MT1>=MT2.

% can I write a file (i.e., write and immediately close it)?
canWriteFile(File) :-
	on_exception(_ErrorMsg,
	             (open(File,write,Stream), close(Stream)),
		     fail).

% process of identifer of current Prolog process:
currentPID(Pid) :- sicstus40orHigher -> process_id(Pid) ; pid(Pid).

% put the current process asleep for the given amount of seconds:
sleepSeconds(S) :- sleep(S).

% get name of current host:
getHostname(Name) :- sicstus40orHigher -> current_host(Name) ; host_name(Name).

% execute a shell command and fail, if not successful:
shellCmd(Cmd) :- shellCmd(Cmd,0).

% execute a shell command and return exit status:
shellCmd(Cmd,Status) :-
	%write(user_error,Cmd), nl(user_error),
	(sicstus40orHigher
         -> absolute_file_name(path(sh),SH,[access([exist,executable])]),
	    process_create(SH,['-c',Cmd],[process(Pid)]),
            process_wait(Pid,exit(Status))
          ; system(Cmd,Status)).

% execute a shell command in background and return the input, output, and
% error stream connected to this command (if the corresponding stream argument
% is not already instantiated to 'std'):
execCommand(Cmd,InWrite,OutRead,ErrRead) :-
	(var(ErrRead) -> ErrReadArg=pipe(ErrRead) ; ErrReadArg=ErrRead),
	(sicstus40orHigher
	 -> absolute_file_name(path(sh),SH,[access([exist,executable])]),
	    process_create(SH,['-c',Cmd],
			   [stdin(pipe(InWrite)),stdout(pipe(OutRead)),
			    stderr(ErrReadArg)])
 	  ; exec(Cmd,[pipe(InWrite),pipe(OutRead),ErrReadArg],_)).


% fork the current program state with an initial goal to execute
% (without producing any output):
forkProcessForGoal(Goal) :-
	currentPID(PID),
	number_codes(PID,PIDS),
	app("/tmp/pakcs_fork_",PIDS,StateP),
	app(StateP,".state",StateS),
	atom_codes(StateName,StateS),
        saveprog_entry(StateName,Goal),
	app("(",StateS,FC0),
	app(FC0," ; rm -f ", FC1),
	app(FC1,StateS,FC2),
	app(FC2,") > /dev/null 2> /dev/null &",ForkCmdS),
	atom_codes(ForkCmd,ForkCmdS),
	shellCmd(ForkCmd).


% is a stream a readable stream?
isInputStream(Stream) :-
	sicstus38orHigher -> stream_property(Stream,input)
	                   ; current_stream(_,input,Stream).

% is a stream a writable stream?
isOutputStream(Stream) :-
	sicstus38orHigher -> stream_property(Stream,output)
	                   ; current_stream(_,output,Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Time and date operations

% get current clocktime (Unix timestamp):
currentClockTime(ClockTime) :-
	sicstus39orHigher
	 -> now(ClockTime)
	  ; % for older SICStus versions, we can only use the local time:
	    datime(datime(Year,Month,Day,Hour,Min,Sec)),
            date2clocktime(Year,Month,Day,Hour,Min,Sec,0,ClockTime).

% convert a Unix timestamp into a (local) date:
clocktime2localtime(ClockTime,Year,Month,Day,Hour,Min,Sec,TZ) :-
	sicstus39orHigher
	 -> datime(ClockTime,datime(Year,Month,Day,Hour,Min,Sec)),
	    date2clocktime(Year,Month,Day,Hour,Min,Sec,0,LocalClockTime),
	    TZ is LocalClockTime-ClockTime
	  ; % for older SICStus versions, we use UTC time:
	    clocktime2utctime(ClockTime,Year,Month,Day,Hour,Min,Sec),
	    TZ=0.

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
connect2socket(Host,Port,Stream,Stream) :-
	sicstus40orHigher
	 -> socket_client_open(Host:Port,Stream,[type(text)])
	  ; socket('AF_INET',Socket),
 	    socket_connect(Socket,'AF_INET'(Host,Port),Stream).

% close socket stream connection:
closeSocketStream(InStream,_OutStream) :- close(InStream).

% Create a server socket on a port. The port is an integer or a free variable
% (in this case, it is bound to a free port number).
% The hostname and the new socket is returned.
listenOnNewSocket(Port,Hostname,Socket) :-
	sicstus40orHigher
	 -> current_host(Hostname),
	    (var(Port)
	     -> socket_server_open(NewPort,Socket),
		(number(NewPort) -> Port=NewPort
		 ; % SICStus 4.x represents Ports also as atoms:
		   atom_codes(NewPort,NewPortS), number_codes(Port,NewPortS))
	      ; socket_server_open(Port,Socket))
	  ; socket('AF_INET',Socket),
	    socket_bind(Socket,'AF_INET'(Hostname,Port)),
	    socket_listen(Socket,1024).

% return the read and write stream of a first connection to a socket:
socketAccept(Socket,Client,Stream,Stream) :-
        sicstus40orHigher
	 -> socket_server_accept(Socket,Client,Stream,[type(text)])
	  ; socket_accept(Socket,Client,Stream).

% close a server socket.
socketClose(Socket) :-
        sicstus40orHigher
	 -> socket_server_close(Socket)
	  ; socket_close(Socket).

% translate a timeout value in milliseconds (where <0 means no timeout)
% into a Sicstus timeout specification:
timeoutAsSicstus(TimeOut,TO) :-
	(TimeOut<0 -> TO=off
	            ; TOSec is TimeOut//1000,
	              TOMSec is (TimeOut mod 1000) * 1000, % in milliseconds
	              TO = TOSec:TOMSec).
	
% Wait for input data on a list of streams (Timeout = TOSec:TOMSec or off),
% returns -1 if no data available within Timeout limit, otherwise the index of
% the corresponding stream:
waitForInputDataOnStreams(InStreams,Timeout,Index) :-
	timeoutAsSicstus(Timeout,TO),
	(sicstus40orHigher
	  -> socket_select([],_,InStreams,SelStreams,[],_,TO)
	   ; socket_select([],_,_,TO,InStreams,SelStreams)),
	!,
	(SelStreams = [] -> Index = -1
                          ; SelStreams = [Stream|_],
                            streamIndex(InStreams,Stream,Index)).

streamIndex([S|_],S,0) :- !.
streamIndex([_|Streams],S,I) :- streamIndex(Streams,S,I1), I is I1+1.


% Wait for a client connection at a socket (Timeout = TOSec:TOMSec or off),
% fails if no client connection available within Timeout limit.
waitForSocketClientStream(Socket,Timeout,Client,Stream,Stream) :-
	timeoutAsSicstus(Timeout,TO),
	(sicstus40orHigher
	  -> socket_select([Socket],ReadySockets,[],_,[],_,TO),
	     ReadySockets=[Socket], % fail otherwise
	     socket_server_accept(Socket,Client,Stream,[type(text)])
	   ; socket_select([Socket],PortStreams,Clients,TO,[],_),
	     PortStreams=[Stream], Clients=[Client]),
	!.


% Wait for a client connection at a socket or available stream input data.
% If a client connection is established, InPortStream and OutPortStream
% are instantiated to the client stream connection and Client is
% instantiated to the clients address, otherwise Client is instantiated
% to 'no' and the last argument is instantiated to the InStreams index with
% available data.
%
% Note that this implementation works only with
% Sicstus-Prolog 3.8.5 or higher (due to a bug in previous versions
% of Sicstus-Prolog).
waitForSocketOrInputStreams(Socket,Client,PortStream,PortStream,
			    InStreams,Index) :-
	sicstus385orHigher, !,
	(sicstus40orHigher
	  -> socket_select([Socket],ReadySockets,InStreams,SelStreams,[],_,off),
	     (ReadySockets=[Socket]
	      -> socket_server_accept(Socket,Client,PortStream,[type(text)]),
		 Clients=[Client], PortStreams=[PortStream]
	       ; PortStreams=[])
	   ; socket_select([Socket],PortStreams,Clients,off,
			   InStreams,SelStreams)),
	(PortStreams=[PortStream]
	 -> Clients=[Client]
	  ; Client=no,
	    SelStreams = [Stream|_],
	    streamIndex(InStreams,Stream,Index)).
waitForSocketOrInputStreams(_,_,_,_,_,_) :-
	raise_exception('You need Sicstus-Prolog 3.8.5 or higher to execute this program (due to a bug in the socket library of previous versions of Sicstus)').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Due to incompatibilities between the various versions of
% Sicstus-Prolog, we have to use different methods to save states etc.

saveprog_entry(State,Entry) :-
	pakcsrc(standalone,yes), !,
	save_standalone_executable(State,Entry).
saveprog_entry(State,Entry) :-
	getSicstusVersion(SV), SV = '3.5',
	!, % if we use Sicstus 3.5
	save(State), Entry.
saveprog_entry(State,Entry) :-
	sicstus37orHigher, !,   % if we use Sicstus 3.7 or higher
	save_program(State,Entry),
	appendAtom('chmod +x ',State,Chmod),
	shellCmd(Chmod).
% Note for Sicstus 3.9 and higher:
% It works only if the name of the state contains a suffix with a dot,
% like 'xxx.state', otherwise Sicstus adds automatically
% the suffix '.sav'!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for stand-alone executables:
:- dynamic rt_entry/1.

% use this instead of saveprog_entry:
save_standalone_executable(State,Entry) :-
	(sicstus310orHigher -> true
	 ; write(user_error,'ERROR: stand-alone executables require Sictus 3.10 or higher!'),
	   nl(user_error), fail),
	(retract(rt_entry(_)) -> true ; true),
	asserta(rt_entry(Entry)),
	save_program(State).

user:runtime_entry(start) :-
	rt_entry(Entry),
	call(Entry).


% try to save a user predicate in a .po file if it is supported by this
% Sicstus version:
try_save_predicates(P/N,POFile) :-
	sicstus38orHigher, !,
	save_predicates(user:P/N,POFile).
try_save_predicates(_,_).


% try to save an already compiled Prolog program in a .po file if it is supported by this
% Sicstus version:
try_save_program(PrologFileName) :-
	sicstus38orHigher, !,
	atom_codes(PrologFileName,PrologFileNameS),
	app(FileNameS,".pl",PrologFileNameS),
	app(FileNameS,".po",POFileNameS),
	atom_codes(POFileName,POFileNameS),
	((app("/tmp/",_,PrologFileNameS) ; \+ canWriteFile(POFileName)) -> true
	 ; save_files(PrologFileName,POFileName)).
try_save_program(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for compiling programs and loading run-time libraries:

% compile a Prolog file:
compilePrologFile(PrologFileName) :- compile(user:PrologFileName).

% compile a Prolog file and try to save it in fast load format:
compilePrologFileAndSave(PrologFileName) :-
	atom_codes(PrologFileName,PrologFileNameS),
	app(FileNameS,".pl",PrologFileNameS),
	app(FileNameS,".po",POFileNameS),
	atom_codes(POFileName,POFileNameS),
	(fileExistsAndNewer(POFileName,PrologFileName)
	 -> load_files(user:POFileName) % for faster compilation
	  ; compilePrologFile(PrologFileName),
	    try_save_program(PrologFileName)).

% consult a Prolog file or a .po file if it exists
consultPrologorPOFile(PrologFileName,POFileName) :-
	(fileExistsAndNewer(POFileName,PrologFileName)
	 -> load_files(user:POFileName) % for faster compilation
	  ; consult(user:PrologFileName)).


% directory containing the system run-time modules:
moduleDir(MD) :-
        installDir(TCP),
        appendAtom(TCP,'/curry2prolog/lib_src/',MD).

% ensure that run-time library is loaded:
ensure_lib_loaded(Lib) :- % first, look into working directory:
	workingDirectory(WDir),
	appendAtom(WDir,'/',Dir),
	appendAtom(Dir,Lib,DirLib),
	appendAtom(DirLib,'.pl',DirLibPl),
	file_exists(DirLibPl), !,
	ensure_loaded(user:DirLib).
ensure_lib_loaded(Lib) :-
	moduleDir(Dir),
	appendAtom(Dir,Lib,DirLib),
	ensure_loaded(user:DirLib).


% get name of temporary Prolog file:
getNewPrologFileName(PrologFile) :-
	currentPID(PID),
	number_codes(PID,PIDS),
	app("/tmp/pakcsprog",PIDS,P1), app(P1,".pl",ProgS),
	atom_codes(PrologFile,ProgS),
	app("rm -f ",ProgS,RmCmdS),
	atom_codes(RmCmd,RmCmdS),
	shellCmd(RmCmd).


% determine for a given Prolog file name (of the main module) a file name
% where the clauses for the main predicates (hnf, constrEq,...) should be stored:
mainPrologFileName(_PrologFile,MainPrologFile) :-
	getNewPrologFileName(NewPrologFile),
	appendAtom(NewPrologFile,'.main',MainPrologFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call a goal and return list of suspended goals:
callAndReturnSuspensions(Goal,Suspensions) :-
	sicstus40orHigher
         -> call_residue_vars(Goal,Vars),
	    copy_term(Vars,NewVars,SuspGoal),
	    bindVariables(Vars,NewVars),
	    goal2list(SuspGoal,Suspensions)
	  ; call_residue(Goal,VarSusps),
	    map2M(prologbasics:omitVarPart,VarSusps,Suspensions).

bindVariables([],[]).
bindVariables([X|Xs],[Y|Ys]) :- X=Y, bindVariables(Xs,Ys).

goal2list(L,[L]) :- var(L), !.
goal2list((L,G),[L|Gs]) :- !, goal2list(G,Gs).
goal2list(true,[]) :- !.
goal2list(L,[L]).

omitVarPart(_-G,G).

% write a Prolog term possibly containing variables:
writeqWithVars(T) :- \+ \+ (numbervars(T,0,_), writeq(T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a block declaration for predicate PredName of arity PredArity
% where the positions in the non-empty(!) list GroundPositions must be instantiated.
% Furthermore, the last argument is a possibly new predicate name corresponding
% to PredName which should be coded instead of PredName (this depends on
% the implementation scheme for block declarations).
genBlockDecl(PredName,PredArity,BoundPositions,PredName) :-
	map2partialM(prologbasics:genBlockLiteral(PredName,PredArity),
	             BoundPositions,Literals),
	basics:foldr1(',',Literals,LiteralsGoal),
	compiler:writeClause((:- block(LiteralsGoal))).

genBlockLiteral(PredName,PredArity,BlockPos,Literal) :-
	functor(Literal,PredName,PredArity),
	setBlockArgs(PredArity,BlockPos,Literal).
setBlockArgs(0,_,_) :- !.
setBlockArgs(I,P,Literal) :-
	arg(I,Literal,Arg),
	(I=P -> Arg='-' ; Arg='?'),
	I1 is I-1, setBlockArgs(I1,P,Literal).


