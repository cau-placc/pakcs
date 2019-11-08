%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module IOExts:
%

%:- module(prim_ioexts,
%	  [prim_setAssoc/3,prim_getAssoc/2,prim_execCmd/2,prim_connectToCmd/2,
%	   prim_newIORef/4,prim_readIORef/4,prim_writeIORef/5]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).

:- dynamic globalAssoc/2.

prim_setAssoc(Key,Val,'Prelude.()') :-
        string2Atom(Key,KeyA),
	(retract(globalAssoc(KeyA,_)) -> true ; true),
	assertz(globalAssoc(KeyA,Val)),
	!.

prim_getAssoc(Key,R) :-
        string2Atom(Key,KeyA),
	(globalAssoc(KeyA,Val) -> R='Prelude.Just'(Val) ; R='Prelude.Nothing'),
	!.

% shell command execution:
prim_execCmd(CmdString,'Prelude.(,,)'(StdIn,StdOut,StdErr)) :-
	string2Atom(CmdString,Cmd),
	execCommand(Cmd,StdIn,StdOut,StdErr).


% shell command execution:
prim_connectToCmd(CmdString,'$stream'('$inoutstream'(StdOut,StdIn))) :-
	string2Atom(CmdString,Cmd),
	execCommand(Cmd,StdIn,StdOut,std).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% primitives implementing IORefs:

% New IORefs are represented as mutable values. The "share" constructor
% is put around to be conform with the remaining implementation where
% all mutables are "marked" by this constructor.

?- block prim_newIORef(?,?,-,?).
prim_newIORef(V,partcall(1,exec_newIORef,[V]),E,E).
?- block exec_newIORef(?,?,?,-,?).
exec_newIORef(Val,_,'$io'('Data.IORef.IORef'(share(MutVal))),E0,E) :-
        var(Val), !,
	create_mutable('$eval'(Val),MutVal), E0=E.
exec_newIORef(Val,_,'$io'('Data.IORef.IORef'(share(MutVal))),E0,E) :-
	create_mutable(Val,MutVal), E0=E.

% When an IORef is read and its value is not evaluated, the current value
% is wrapped into a new mutable in order to implement sharing
% of evaluations of IORefs. The current IORef is updated so that
% it refers to the new mutable (without this indirection, there is
% a risk of creating cyclic structures when the IORef itself is updated).

?- block prim_readIORef(?,?,-,?).
prim_readIORef(R,partcall(1,exec_readIORef,[R]),E,E).
?- block exec_readIORef(?,?,?,-,?).
exec_readIORef(RIORef,_,'$io'(V),E0,E) :-
        user:derefRoot(RIORef,'Data.IORef.IORef'(share(MutVal))),
        get_mutable(Val,MutVal),
	(Val='$eval'(V) -> true
	  ; create_mutable(Val,MutV),
	    update_mutable(share(MutV),MutVal),
	    V=share(MutV)),
	E0=E.

% Assign a new value to an IORef:
?- block prim_writeIORef(?,?,?,-,?).
prim_writeIORef(R,V,partcall(1,exec_writeIORef,[V,R]),E,E).

?- block exec_writeIORef(?,?,?,?,-,?).
exec_writeIORef(RIORef,Val,_,R,E0,E) :-
        user:derefRoot(RIORef,IORef),
	prim_writeIORef_exec(IORef,Val,R), E0=E.

prim_writeIORef_exec('Data.IORef.IORef'(share(MutVal)),Val,'$io'('Prelude.()')) :-
        var(Val), !,
	update_mutable('$eval'(Val),MutVal).
prim_writeIORef_exec('Data.IORef.IORef'(share(MutVal)),Val,'$io'('Prelude.()')) :-
	update_mutable(Val,MutVal).
