%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module SOcket:
%

:- module(prim_socket,
	  [prim_listenOn/2,prim_listenOnFresh/1,prim_socketAccept/2,
	   prim_waitForSocketAccept/3,prim_sClose/2,prim_connectToSocket/3]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).


% create a server side socket bound to a port number.
prim_listenOn(Port,Socket) :- listenOnNewSocket(Port,_,Socket).


% create a server side socket with a fresh port.
prim_listenOnFresh('Prelude.(,)'(Port,Socket)) :-
	listenOnNewSocket(Port,_,Socket).


% return the first connection to a socket as a read/write stream:
prim_socketAccept(Socket,
    'Prelude.(,)'(ClientS,'$stream'('$inoutstream'(InStream,OutStream)))) :-
        socketAccept(Socket,Client,InStream,OutStream),
	atom2String(Client,ClientS), !.

% return a connection to a socket within a time limit as a read/write stream,
% otherwise Nothing:
prim_waitForSocketAccept(Socket,TimeOut,Result) :-
	(waitForSocketClientStream(Socket,TimeOut,Client,InStream,OutStream)
	 -> atom2String(Client,ClientS),
	    Result = 'Prelude.Just'('Prelude.(,)'(ClientS,
			      '$stream'('$inoutstream'(InStream,OutStream))))
	  ; Result = 'Prelude.Nothing').


% Closes a server socket.
prim_sClose(Socket,'Prelude.()') :- socketClose(Socket).


% open a connection to a Unix socket:
prim_connectToSocket(SHst,SNr,'$stream'('$inoutstream'(InStream,OutStream))) :-
        string2Atom(SHst,Host), !,
        connect2socket(Host,SNr,InStream,OutStream).


