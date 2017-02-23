%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of the port primitives.
%
% The clauses in this file are added to each compiled Curry program
% if the port communication primitives are used

:- module(prim_ports,
	  [prim_openPort/3,prim_basicServerLoop/4,prim_sendPort/5,prim_after/4,
	   prim_openProcessPort/2,prim_ping/3,prim_timeoutOnStream/5,
	   prim_openPortOnSocket/3,prim_connectPortAtSocket/4,prim_choiceSPEP/5,
	   checkIncomingPortStreams/3,ifTracePort/1,readPortMessage/4,
	   parse_received_message/4]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).
:- (current_module(prim_standard) -> true ; ensure_loaded(user:prim_standard)). % for normalizeAndCheck,waitUntilGround
:- (current_module(prim_readshowterm) -> true ; use_module(prim_readshowterm)). % for term en/decoding

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dynamic configurations for the port implementation:

:- dynamic only_localhost/1, tracePorts/1,
	   mySocket/1, incomingPortStreams/2.

% the the communication socket for this program
% (will be set by the first invocation of openPortOnSocket):
%mySocket('Ports.internalPort'(Host,SocketNr,MaxPortNumber,Socket)).

% lists of all pending incoming streams at our socket:
% IPNr is the port number of the streams
% Streams is a list of elements of the following form:
% message(Stream): a regular message to be read from Stream
% ping(Stream): a ping request for this port to be answered on Stream
%incomingPortStreams(IPNr,Streams).

% if machine is not connected to the inet (specified by PAKCS_LOCALHOST=yes)
% then local ports get the symbolic hostname "localhost"
only_localhost(LH) :-
	(getEnv('PAKCS_LOCALHOST',PLH) -> true ; PLH=no),
	(PLH=yes -> asserta(only_localhost(yes)), LH=yes
	          ; asserta(only_localhost(no)),  LH=no),
	retract((only_localhost(_) :- _,_)), !.


% trace all port communication if PAKCS_TRACEPORTS=yes
tracePorts(unknown).

ifTracePort(Goal) :-
	tracePorts(Status),
	(Status=unknown
	 -> retract(tracePorts(_)),
	    (getEnv('PAKCS_TRACEPORTS',PTP) -> true ; PTP=no),
	    (PTP=yes -> asserta(tracePorts(yes)), Goal
	              ; asserta(tracePorts(no)))
	  ; Status=yes -> Goal ; true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definitions for port communication:
%
% internal representation of the handle of a port:
% 'Ports.internalPort'(Host,SNr,IPNr,Socket) where:
% - for external ports:
%   Host: Internet name of the host (a Curry list)
%   SNr: socket number of the port at Host (an integer)
%   IPNr: internal port number assigned to the port at Host (an integer)
%   Socket: Sicstus-Prolog handle of this socket
% - for internal ports:
%   Host: not used
%   SNr:  0
%   IPNr: not used
%   Socket: a list with an open end (the stream)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CURRENT RESTRICTIONS w.r.t. sending of logical variables on ports:
% 1. If a message sent over an external port contains lvars, then
%    the sender waits immediately after sending until these variables
%    are bound by the receiver
% 2. The receiver must bind the lvars of a received message
%    (incrementelly) to a ground term,
%    otherwise the binding will not be sent back


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ENCODING OF TERMS:
%
% Terms sent on an external port are encoded as strings as with
% the library function ReadShowTerm.showQTerm with the exception
% that logical variables are represented as (VAR i) where i is
% a unique index for the variable in this term.
%
% For instance, the message (M.Get "foo" x y x) is encoded as the
% string "(M.Get \"foo\" (VAR 0) (VAR 1) (VAR 0))".
%
% Sending bindings of logical variables:
%
% If a message contains logical variables, their bindings will be sent
% back through the same connection via the inverse stream, i.e.,
% if a message is received via an instream of a socket connection,
% the variable bindings are sent via the outstream of the same
% connection. Each binding is encoded as follows:
% "V<i>\n<b>\n" where <i> is the unique index of the variable,
% <b> is the string encoding of the term to which variable <i> is bound.
%
%
% SENDING MESSAGES:
% Messages are sent as strings using the encoding above to the
% socket assigned to the invocation of PAKCS. All port communication
% is done via this single socket. To distinguish different ports,
% each port has a unique number at the socket (this number is assigned
% by the primitive openPortOnSocket, see the implementation of
% prim_openPortOnSocket).
% The socket number is freely chosen or can be defined via the
% environment variable PAKCS_SOCKET.
% In order to determine the port to which a message
% is sent, the port number always precedes the message string.
% In particular, the structure of a message is as follows:
%
% N<pnr>\n<msg>\n : A message encoded as string <msg> is sent to port <pnr>
%
% P<pnr>\n: Ping port <pnr>, i.e., tests whether there is a process which
%           is waiting for messages on this port. In this case, the line
%           "ok\n" is sent back on the same socket connection.
%
% EXAMPLE:
% As an example, we assume that we send the message (GetName "talk" v)
% (where GetName is defined in module "NameServer")
% to a server running at socket 1665 with port number 0. Then we open
% a connection to socket 1665 and send the following string:
% "N0\n(NameServer.GetName "talk" (VAR 0))\n"
% Since a logical variable (v) with index 0 has been sent, the sender
% listens on the input stream of this socket connection for the value
% of this variable. For instance, if the server binds this variable
% to the value 42, it sends the following string to the sender:
% "V0\n42\n". Since the sender has now received all variable values,
% he closes the connection.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%% open

prim_openPort(P,_,_) :-
	nonvar(P), !,
	writeErr('ERROR: openPort: port is not a free variable'), nlErr,
	fail.
prim_openPort(_,S,_) :-
	nonvar(S), !,
	writeErr('ERROR: openPort: stream is not a free variable'), nlErr,
	fail.
prim_openPort('Ports.internalPort'([],0,0,S),S,'Prelude.True').


prim_openPortOnSocket(NewSocketNr,NewPortNr,Result) :-
	(retract(mySocket('Ports.internalPort'(Host,SocketNr,MPN,Socket)))
	 -> MPN1 is MPN+1,
	    asserta(mySocket('Ports.internalPort'(Host,SocketNr,MPN1,Socket))),
	    ((NewSocketNr=SocketNr, NewPortNr=MPN1)
	     -> true
	      ; writeErr('ERROR: inconsistent socket/port numbers in openPortOnSocket!'),
		nlErr)
	  ; trySetSocketNumber(NewSocketNr),
	    % catch connection errors:
	    on_exception(Exc,listenOnNewSocket(NewSocketNr,AHost,Socket),
                         (write('ERROR: openPortOnSocket "'),
			  write(NewSocketNr),
		          write('":'), nl, printError(Exc), !, fail)),
	    atom2String(AHost,Host),
	    (var(NewPortNr) -> NewPortNr=0 ; true),
	    asserta(mySocket('Ports.internalPort'(Host,NewSocketNr,NewPortNr,
						  Socket)))
	),
	mySocket(Port),
	Port = 'Ports.internalPort'(CurrHost,CurrSocketNr,CurrPNr,_),
	ifTracePort((writeErr('TRACEPORTS: Listen on port '),
	             writeErr(CurrPNr),
	             writeErr(' at socket '),
	             writeErr(CurrSocketNr),
	             writeErr(' on host '),
		     (only_localhost(yes) -> writeErr('localhost')
	                                   ; string2Atom(CurrHost,ACurrHost),
			                     writeErr(ACurrHost)),
	             writeErr('...'), nlErr)),
	(compileWithSharing(function)
	 -> makeShare('Ports.basicServerLoop'(Port),Result)
	  ; Result = 'Ports.basicServerLoop'(Port)).

% try setting socket number (if it is unbound) to the value of
% environment variable PAKCS_SOCKET, if it is defined:
trySetSocketNumber(SocketNr) :-
	var(SocketNr),
	getEnv('PAKCS_SOCKET',PSNr),
	atom_codes(PSNr,PSNrS), number_codes(SocketNr,PSNrS),
	!.
trySetSocketNumber(_).
	
% loop for reading the stream of incoming messages at a port:
?- block 'prim_basicServerLoop'(?,?,-,?).
'prim_basicServerLoop'(Port,Result,E0,E) :-
	readPort(Port,ReadResult,-1),
	Result = [ReadResult|BSL],
	(compileWithSharing(function)
	 -> makeShare('Ports.basicServerLoop'(Port),BSL)
	  ; BSL = 'Ports.basicServerLoop'(Port)),
	E0=E.
 

% test whether a stream is instantiated in a particular time period:
?- block prim_timeoutOnStream(?,?,?,-,?).
prim_timeoutOnStream(RN,Stream,R,E0,E) :- user:derefRoot(RN,N),
	prim_timeoutOnStream_exec(N,Stream,R,E0,E).

prim_timeoutOnStream_exec(_,Stream,'Prelude.Nothing',E0,E) :- var(Stream), !, E0=E.
prim_timeoutOnStream_exec(Timeout,share(M),Result,E0,E) :- !,
        get_mutable(T,M),
	(T='$eval'(Exp) -> Result=Exp, E0=E
	 ; prim_timeoutOnStream_exec(Timeout,T,TOResult,E0,E),
	   (TOResult='Prelude.Nothing'
	    -> Result=TOResult
	     ; (compileWithSharing(variable) -> user:propagateShare(TOResult,Result)
	                                      ; TOResult=Result),
	       update_mutable('$eval'(Result),M))).
prim_timeoutOnStream_exec(_,[],'Prelude.Just'([]),E0,E) :- % stream already instantiated
        !, E0=E.
prim_timeoutOnStream_exec(_,[X|Xs],'Prelude.Just'([X|Xs]),E0,E) :- !, E0=E.
prim_timeoutOnStream_exec(Timeout,'Ports.basicServerLoop'(Port),Result,E0,E) :-
	(readPort(Port,ReadResult,Timeout)
	 -> (compileWithSharing(function)
	     -> makeShare('Ports.basicServerLoop'(Port),BSL)
	      ; BSL = 'Ports.basicServerLoop'(Port)),
	    Result='Prelude.Just'([ReadResult|BSL])
	  ; Result='Prelude.Nothing'),
	E0=E.
 

% primitives for binding an external logical variable:
?- block send_extvar_binding(?,-,?,?).
send_extvar_binding(Stream,A,Index,Done) :-
	evalToken(Eval),
	user:normalizeAndCheck(A,NA,Eval,E),
        user:waitUntilGround(NA,E,E1), % wait for message to become bound
	eval_send_extvar_binding(Stream,NA,Index,E1,Done).
?- block eval_send_extvar_binding(?,?,?,-,?).
eval_send_extvar_binding(nostream,Term,Index,_,_) :-
	% this case should usually not occur since we send only ground
	% bindings for variables
	writeErr('ERROR: cannot send binding for variable '),
	writeErr(Index), writeErr(': '),
	writeErr(Term), nlErr,
	!.
eval_send_extvar_binding(Stream,Term,Index,_,Done) :-
	show4sending(Term,MsgString,_),
	ifTracePort((writeErr('TRACEPORTS: send binding for variable '),
                     writeErr(Index), writeErr(': '),
		     writeChars(user_error,MsgString), nlErr
		     %writeErr(Term), nlErr
		    )),
	number_codes(Index,IndexS),
	putChars(Stream,[86|IndexS]), % send variable index
	put_code(Stream,10),
	writeChars(Stream,MsgString),
	put_code(Stream,10),
	flush_output(Stream),
	!, Done=done. % mark successful sending


readPort('Ports.internalPort'(Host,SNr,PNr,Socket),_,_):-
	var(Socket), !,
	string2Atom(Host,AHost),
	writeErr('ERROR: readPort: Port '),
	writeErr(SNr), writeErr('/'), writeErr(PNr),
	writeErr('@'), writeErr(AHost),
	writeErr(' only opened for writing!'), nlErr.
readPort('Ports.internalPort'(_,_,PNr,Socket),Msg,Timeout) :-
	% catch connection errors:
	on_exception(Exc,readFromSocket(PNr,Socket,Msg,Timeout),
                     (printError(Exc)->Msg=[];Msg=[])), !.

% check the pending incoming streams on our socket for messages
% for a given port number and succeed with the stream; otherwise fail:
checkIncomingPortStreams(PNr,InStream,OutStream) :-
	retract(incomingPortStreams(PNr,[message(InStream,OutStream)|Streams])),
	asserta(incomingPortStreams(PNr,Streams)),
	!.
checkIncomingPortStreams(PNr,InStream,OutStream) :-
	retract(incomingPortStreams(PNr,[ping(PingInStream,PingOutStream)|Streams])),
	asserta(incomingPortStreams(PNr,Streams)),
	answerPingOnStream(PingInStream,PingOutStream),
	checkIncomingPortStreams(PNr,InStream,OutStream),
	!.

% answer a ping request on a stream:
answerPingOnStream(InStream,OutStream) :-
	write(OutStream,'ok'), put_code(OutStream,10),
	flush_output(OutStream),
	closeSocketStream(InStream,OutStream),
	ifTracePort((writeErr('TRACEPORTS: Ping request answered.'),
		     nlErr)).

readFromSocket(PNr,Socket,Msg,Timeout) :-
	checkIncomingPortStreams(PNr,InPortStream,OutPortStream),
	!,
	readStreamLine(InPortStream,MsgString),
	(parse_received_message(InPortStream,OutPortStream,MsgString,Msg)
         -> ifTracePort((writeErr('TRACEPORTS: Received message: '),
                         putChars(user_error,MsgString), nlErr
		         %writeErr(Msg), nlErr
			))
          ; writeErr('ERROR: Illegal message received (ignored): '),
            putChars(user_error,MsgString), nlErr,
            readFromSocket(PNr,Socket,Msg,Timeout)),
	!.
readFromSocket(PNr,Socket,Msg,Timeout) :-
	waitForSocketClientStream(Socket,Timeout,Client,InStream,OutStream),
	ifTracePort((writeErr('TRACEPORTS: Connection to client: '),
		     writeErr(Client), nlErr)),
	(readPortMessage(PNr,InStream,OutStream,MsgString)
	 -> (parse_received_message(InStream,OutStream,MsgString,Msg)
            -> ifTracePort((writeErr('TRACEPORTS: Received message: '),
                            putChars(user_error,MsgString), nlErr
		            %writeErr(Msg), nlErr
			   ))
             ; writeErr('ERROR: Illegal message received (ignored): '),
               putChars(user_error,MsgString), nlErr,
	       readFromSocket(PNr,Socket,Msg,Timeout))
	  ; % try reading next message:
	    readFromSocket(PNr,Socket,Msg,Timeout)),
	!.
readFromSocket(_,_Socket,_Msg,_) :-
	ifTracePort((writeErr('TRACEPORTS: Timeout!'),
		     nlErr)), fail.

% read from a (socket) stream a message with port number header;
% delay reading and fail if this message is not intended for the
% given port number:
readPortMessage(PNr,InStream,OutStream,MsgString) :-
	readStreamLine(InStream,MsgHead),
	MsgHead = [FirstHeadChar|ReceivedPNrS],
	checkMessageHeader(FirstHeadChar,InStream,OutStream,DecoratedStream),
	number_codes(ReceivedPNr,ReceivedPNrS),
	!,
	(ReceivedPNr=PNr
	 -> readPortMessageBody(DecoratedStream,MsgString)
	  ; % delay receiving of message and fail:
	    (retract(incomingPortStreams(ReceivedPNr,OldStreams))
	     -> append(OldStreams,[DecoratedStream],NewStreams)
	      ; NewStreams = [DecoratedStream] ),
	    asserta(incomingPortStreams(ReceivedPNr,NewStreams)),
	    !, fail).

checkMessageHeader(78,InStream,OutStream,message(InStream,OutStream)) :- !. % 78 = 'N'
checkMessageHeader(80,InStream,OutStream,ping(InStream,OutStream)) :- !.    % 80 = 'P'
checkMessageHeader(_,_,_,_) :-
	writeErr('ERROR: Illegal message header received.'),
	nlErr,
	!, fail.

readPortMessageBody(message(InStream,_OutStream),MsgString) :-
	readStreamLine(InStream,MsgString).
readPortMessageBody(ping(InStream,OutStream),_) :-
	answerPingOnStream(InStream,OutStream),
	!, fail.
	

%%%% send

?- block prim_sendPort(?,?,?,-,?).
prim_sendPort(RMsg,RPort,R,E0,E) :-
	user:derefAll(RMsg,Msg), user:derefRoot(RPort,Port),
	prim_sendPortExec(Msg,Port,R,E0,E).

prim_sendPortExec(Msg,'Ports.internalPort'(_,0,_,Stream),
		  'Prelude.True',E0,E) :-
	% send to internal port
	!,
	add2Stream(Stream,Msg), E0=E.
prim_sendPortExec('Ports.SP_Put'(Str),'Ports.internalPort'(_,-1,_,WIn),
		  'Prelude.True',E0,E) :-
	% send to stream port
	!,
	user:waitUntilGround(Str,E0,E), % wait for string to become bound
	writeChars(WIn,Str), nl(WIn), flush_output(WIn),
	ifTracePort((writeErr('TRACEPORTS: SP_Put: '),
		     writeChars(user_error,Str),
		     nlErr)).
prim_sendPortExec('Ports.SP_GetLine'(Str),'Ports.internalPort'(WOut,-1,_,_),
		'Prelude.True',E0,E) :-
	% send to stream port
	!,
	readStreamLine(WOut,WOLine),
	map2M(basics:char_int,SPOutLine,WOLine),
	ifTracePort((writeErr('TRACEPORTS: SP_Get: '),
		     writeChars(user_error,SPOutLine),
		     nlErr)),
	user:constrEq(SPOutLine,Str,_,E0,E). % unify SP_Get-Arg with read line
prim_sendPortExec('Ports.SP_GetChar'(Chr),'Ports.internalPort'(WOut,-1,_,_),
		'Prelude.True',E0,E) :-
	% send to stream port
	!,
	get_code(WOut,NC), char_int(NC,C),
	ifTracePort((writeErr('TRACEPORTS: SP_GetChar: '),
		     writeErr(C),
		     nlErr)),
	user:constrEq(C,Chr,_,E0,E). % unify SP_GetChar-Arg with read character
prim_sendPortExec('Ports.SP_EOF'(Bool),'Ports.internalPort'(WOut,-1,_,_),
		'Prelude.True',E0,E) :-
	% send to stream port
	!,
	(atEndOfStream(WOut) -> EOF='Prelude.True' ; EOF='Prelude.False'),
	ifTracePort((writeErr('TRACEPORTS: SP_EOF: '),
		     writeErr(EOF),
		     nlErr)),
	user:constrEq(Bool,EOF,_,E0,E).	% unify SP_EOF-Arg with current value
prim_sendPortExec('Ports.SP_Close','Ports.internalPort'(WOut,-1,_,WIn),
		'Prelude.True',E0,E) :-
	% send to stream port
	!,
	close(WIn), close(WOut), % close input and output streams
	E0=E.
prim_sendPortExec(Msg,'Ports.internalPort'(_,-1,_,_),'Prelude.True',E0,E) :-
	% send to stream port
	!,
	writeErr('ERROR: wrong message received by stream port: '),
	writeErr(Msg), nlErr, E0=E.
prim_sendPortExec(Msg,'Ports.internalPort'(Host,SNr,PNr,_),'Prelude.True',E0,E) :-
	% send to external port
	% catch connection errors:
        string2Atom(Host,AHost),
	on_exception(Exc,send2Socket(AHost,SNr,PNr,Msg),
                     (write('ERROR: send to port '), write(PNr), write('@'),
		      write(AHost), write(':'), nl,
		      (printError(Exc)->true;true))),	% send always succeeds!
	E0=E.

% append a message to the open end of a stream:
add2Stream(Str,Item) :- var(Str), !, Str = [Item|_].
add2Stream([],_) :-
	write('ERROR: send: stream has not a free variable at the end'),
	nl.
add2Stream([_|Str],Item) :- add2Stream(Str,Item).



%%%% ping

prim_ping(_,'Ports.internalPort'(_,0,_,_),'Prelude.Just'(0)) :-
	% ping internal port
	!.
prim_ping(_TimeOut,'Ports.internalPort'(_WOut,-1,_,_WIn),'Prelude.Just'(0)) :-
	% ping to process port: must still be done....
	!.
prim_ping(TimeOut,'Ports.internalPort'(Host,SNr,PNr,_),Result) :-
	% ping external port
	string2Atom(Host,AHost),
	on_exception(_Exc,ping2SocketPort(AHost,SNr,PNr,TimeOut,Result),
                     Result='Prelude.Nothing'). % ping fails in case of an error

ping2SocketPort(Host,SNr,PNr,TimeOut,Result) :-
	connect2socket(Host,SNr,InPortStream,OutPortStream),
	number_codes(PNr,PNrS),
	putChars(OutPortStream,[80|PNrS]), % 80 = 'P'
	put_code(OutPortStream,10),
	flush_output(OutPortStream),
	getElapsedTime(ETime1),
	ifTracePort((writeErr('TRACEPORTS: Ping to "'),
		     writeErr(SNr), writeErr('/'),
		     writeErr(PNr), writeErr('@'),
		     writeErr(Host), writeErr('"'),
		     nlErr)),
	waitForInputDataOnStreams([InPortStream],TimeOut,Index),
        (Index=0
	 -> readStreamLine(InPortStream,_Line),
	    closeSocketStream(InPortStream,OutPortStream),
	    getElapsedTime(ETime2),
	    PingTime is ETime2-ETime1,
	    ifTracePort((writeErr('TRACEPORTS: Ping answer received in '),
		         writeErr(PingTime),
			 writeErr(' ms'), nlErr)),
	    Result = 'Prelude.Just'(PingTime)
	  ; ifTracePort((writeErr('TRACEPORTS: No ping answer received'),
		         nlErr)),
	    Result = 'Prelude.Nothing').


%%%% connect
  
prim_connectPortAtSocket(RSNr,RPNr,RHost,
			 'Ports.internalPort'(Host,SNr,PNr,'Prelude.()')) :-
	user:derefRoot(RSNr,SNr),
	user:derefRoot(RPNr,PNr),
	user:derefAll(RHost,Host), !.


send2Socket(Host,SNr,PNr,Msg) :-
	connect2socket(Host,SNr,InPortStream,OutPortStream),
	number_codes(PNr,PNrS),
	putChars(OutPortStream,[78|PNrS]), % 78 = 'N'
	put_code(OutPortStream,10),
	show4sending(Msg,MsgString,RevVarIndexs),
	writeChars(OutPortStream,MsgString),
	put_code(OutPortStream,10),
	flush_output(OutPortStream),
	ifTracePort((writeErr('TRACEPORTS: Sent to "'),
		     writeErr(SNr), writeErr('/'),
		     writeErr(PNr), writeErr('@'),
		     writeErr(Host), writeErr('": '),
		     writeChars(user_error,MsgString), nlErr
		     %writeErr(Msg), nlErr
		    )),
	rev(RevVarIndexs,VarIndexs),
	receive_extvar_bindings(InPortStream,VarIndexs).

% read all variable bindings from the sockets instream:
receive_extvar_bindings(Str,[]) :-
	!,
	% all variable bindings received, so close the stream:
	ifTracePort((writeErr('TRACEPORTS: Closing send-connection'),
		     nlErr)),
	close(Str).
receive_extvar_bindings(Str,VIs) :-
	readStreamLine(Str,VarLine),
	VarLine = [86|IndexS],  % 86='V'
	number_codes(Index,IndexS),
	readStreamLine(Str,MsgString),
	(deleteIndexVariable(Index,VIs,Var,VIs1)
	 -> true
	  ; writeErr('ERROR: Illegal binding for logical variable received:'),
	    nlErr,
	    VIs1 = VIs),
	(parse_received_message(nostream,nostream,MsgString,Msg)
         -> ifTracePort((writeErr('TRACEPORTS: Received binding for variable '),
                         writeErr(Index), writeErr(': '),
			 putChars(user_error,MsgString), nlErr
		         %writeErr(Msg), nlErr
			))
          ; writeErr('ERROR: Illegal message received (ignored): '),
            putChars(user_error,MsgString), nlErr),
	!,
	Var=Msg,  % unify received (ground!) term with sent variable
	receive_extvar_bindings(Str,VIs1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of Curry terms into string representation:
% the final (result) argument is a list of variable/integer pairs
% of all variables occurring in the term and their uniquely assigned indices:
%

show4sending(T,S,VIs) :- numberVars(T,GT,[],VIs), show_term(GT,qualified,S,[]).


% get the assigned index for a variable:
getVariableIndex(V,[(W,Index)|_],Index) :- V==W, !.
getVariableIndex(V,[_|VIs],Index) :- getVariableIndex(V,VIs,Index).

% get the index assigned to a variable:
getIndexVariable(Index,[(V,Index)|_],V) :- !.
getIndexVariable(Index,[_|VIs],V) :- getIndexVariable(Index,VIs,V).

% get and delete the index assigned to a variable:
deleteIndexVariable(Index,[(V,Index)|VIs],V,VIs) :- !.
deleteIndexVariable(Index,[VI|VIs],V,[VI|VIs1]) :-
	deleteIndexVariable(Index,VIs,V,VIs1).


% conversion of string representations of Curry terms into Curry terms:
parse_received_message(InStream,OutStream,String,Term) :-
	readTerm(String,qualified,T,GroundTerm), skipWhiteSpace(T,[]),
	extractVars(GroundTerm,Term,[],VIs),
	send_extvar_bindings(VIs,Dones,OutStream),
	closeStreamAfterDones(Dones,InStream,OutStream).

send_extvar_bindings([],[],_).
send_extvar_bindings([(V,I)|VIs],[Done|Dones],Stream) :-
	send_extvar_binding(Stream,V,I,Done),
	send_extvar_bindings(VIs,Dones,Stream).

% close the stream after all list elements are instantiated:
closeStreamAfterDones([],nostream,_) :- !.
closeStreamAfterDones([],InStream,OutStream) :-
	ifTracePort((writeErr('TRACEPORTS: Closing receive-connection'),
		     nlErr)),
	closeSocketStream(InStream,OutStream).
closeStreamAfterDones([Done|Dones],InStream,OutStream) :-
	closeStreamAfterDones(Done,Dones,InStream,OutStream).

?- block closeStreamAfterDones(-,?,?,?).
closeStreamAfterDones(_,Dones,InStream,OutStream) :-
	closeStreamAfterDones(Dones,InStream,OutStream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definitions for process communication:
%
% internal representation of the handle of the port to the process:
% 'Ports.internalPort'(WOut,-1,<unused>,WIn) where:
% WOut: a Prolog stream, the output of the process
% WIn : a Prolog stream, the input to the process

%%%%% open a connection to an external process:

prim_openProcessPort(RCmdString,'Ports.internalPort'(WOUT,-1,0,WIN)) :-
	user:derefAll(RCmdString,CmdString),
	string2Atom(CmdString,Cmd),
	execCommand(Cmd,WIN,WOUT,_).


?- block prim_choiceSPEP(?,?,?,-,?), prim_choiceSPEP(?,-,?,?,?).
prim_choiceSPEP(RPort,Msgs,R,E0,E) :- user:derefRoot(RPort,Port),
	prim_choiceSPEP_exec(Port,Msgs,R,E0,E).

prim_choiceSPEP_exec(SPPort,share(M),Result,E0,E) :-
        !,
	get_mutable(V,M),
	(V='$eval'(R) % external message stream has been already evaluated
	 -> E0=E, Result='Prelude.Right'(R)
	  ; prim_choiceSPEP_exec(SPPort,V,CResult,E0,E),
	    (CResult='Prelude.Left'(_)
  	     -> Result=CResult
	      ; CResult='Prelude.Right'(S),
	        (compileWithSharing(variable) -> user:propagateShare(S,TResult)
		                               ; S=TResult),
	        Result='Prelude.Right'(TResult),
	        update_mutable('$eval'(TResult),M))).
prim_choiceSPEP_exec(_,[M|Ms],'Prelude.Right'([M|Ms]),E0,E) :-
	!, E0=E. % stream already evaluated
prim_choiceSPEP_exec('Ports.internalPort'(WOut,-1,_,_),[],Result,E0,E) :-
	% message stream is empty, look at stream port:
	readStreamLine(WOut,WOLine),
	map2M(basics:char_int,SPOutLine,WOLine),
	ifTracePort((writeErr('TRACEPORTS: SP_Get: '),
		     writeChars(user_error,SPOutLine),
		     nlErr)),
	Result='Prelude.Left'(SPOutLine),
	E0=E,
	!.
prim_choiceSPEP_exec(SPort,'Ports.basicServerLoop'(Port),Result,E0,E) :-
        Port='Ports.internalPort'(_,_,PNr,_),
	checkIncomingPortStreams(PNr,InPortStream,OutPortStream),
	!,
	readStreamLine(InPortStream,MsgString),
	(compileWithSharing(function)
	 -> makeShare('Ports.basicServerLoop'(Port),BSL)
	  ; BSL = 'Ports.basicServerLoop'(Port)),
	(parse_received_message(InPortStream,OutPortStream,MsgString,Msg)
	 -> ifTracePort((writeErr('TRACEPORTS: Received message: '),
		         writeErr(Msg), nlErr)),
	    Result='Prelude.Right'([Msg|BSL])
	  ; writeErr('ERROR: Illegal message received (ignored): '),
	    putChars(user_error,MsgString), nlErr,
	    prim_choiceSPEP_exec(SPort,BSL,Result,E0,E)),
	E0=E,
	!.
prim_choiceSPEP_exec('Ports.internalPort'(WOut,-1,_,WIn),
		     'Ports.basicServerLoop'(Port),Result,E0,E) :-
        Port='Ports.internalPort'(_,_,PNr,Socket),
	waitForSocketOrInputStreams(Socket,Client,InPortStream,OutPortStream,[WOut],_),
	(compileWithSharing(function)
	 -> makeShare('Ports.basicServerLoop'(Port),BSL)
	  ; BSL = 'Ports.basicServerLoop'(Port)),
	(Client=no
	 -> % there is input on WOut:
            readStreamLine(WOut,WOLine),
	    map2M(basics:char_int,SPOutLine,WOLine),
	    ifTracePort((writeErr('TRACEPORTS: SP_Get: '),
		         writeChars(user_error,SPOutLine),
		         nlErr)),
	    Result='Prelude.Left'(SPOutLine)
	  ; % there is a client connection:
	    ifTracePort((writeErr('TRACEPORTS: Connection to client: '),
		         writeErr(Client), nlErr)),
	    (readPortMessage(PNr,InPortStream,OutPortStream,MsgString)
	     -> (parse_received_message(InPortStream,OutPortStream,MsgString,Msg)
	         -> ifTracePort((writeErr('TRACEPORTS: Received message: '),
		                 writeErr(Msg), nlErr)),
		    Result='Prelude.Right'([Msg|BSL])
	          ; writeErr('ERROR: Illegal message received (ignored): '),
		    putChars(user_error,MsgString), nlErr,
		    prim_choiceSPEP_exec('Ports.internalPort'(WOut,-1,_,WIn),
					 BSL,Result,E0,E))
	      ; % try reading next message:
		prim_choiceSPEP_exec('Ports.internalPort'(WOut,-1,_,WIn),
		                     BSL,Result,E0,E))
	   ),
	E0=E,
	!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary operations for handling terms with variables:

% numberVars(T1,T2,[],VIs): replace each variable X in T1 by 'VAR'(i)
% where i is a unique index. VIs is a list of variable/integer pairs
% (e.g., it contains (X,i)):
numberVars(V,'VAR'(I),VIs,NVIs) :-
	var(V), !,
	(getVariableIndex(V,VIs,I) % has this variable already replaced?
	 -> NVIs=VIs
	  ; length(VIs,I), NVIs=[(V,I)|VIs] ).
numberVars(T,NT,VIs,NVIs) :-
	isShowableArg(T),
	T =.. [C|Args],
	numberVarsList(Args,NArgs,VIs,NVIs),
	NT =.. [C|NArgs].

numberVarsList([],[],VIs,VIs).
numberVarsList([Arg|Args],[NArg|NArgs],VIs,NVIs) :-
	numberVars(Arg,NArg,VIs,VIs1),
	numberVarsList(Args,NArgs,VIs1,NVIs).


% extractVars(T1,T2,[],VIs): replace each 'VAR'(i) in T1 by a new variable X
% where VIs is the list of variable/integer pairs (e.g., it contains (X,i))
extractVars(V,V,VIs,VIs) :- var(V), !.
extractVars('VAR'(I),Var,VIs,NVIs) :- !,
	(getIndexVariable(I,VIs,Var)
	 -> NVIs=VIs
	  ; NVIs=[(Var,I)|VIs]).
extractVars(T,NT,VIs,NVIs) :-
	T =.. [C|Args],
	extractVarsList(Args,NArgs,VIs,NVIs),
	NT =.. [C|NArgs].

extractVarsList([],[],VIs,VIs).
extractVarsList([Arg|Args],[NArg|NArgs],VIs,NVIs) :-
	extractVars(Arg,NArg,VIs,VIs1),
	extractVarsList(Args,NArgs,VIs1,NVIs).


% not yet implemented in Curry2Prolog:
?- block 'prim_after'(?,?,-,?).
prim_after(_,_,E,E) :- raise_exception('Ports.after not implemented!').
