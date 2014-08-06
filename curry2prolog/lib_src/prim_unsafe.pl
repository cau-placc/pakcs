%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Unsafe:
%

:- use_module('../prologbasics').
:- use_module(prim_readshowterm).

?- block 'prim_unsafePerformIO'(?,?,-,?).
'prim_unsafePerformIO'(Action,H,E0,E) :-
        worldToken(World),
        prim_apply(Action,World,'$io'(Result),E0,E1),
	user:hnf(Result,H,E1,E).

?- block 'prim_spawnConstraint'(?,?,?,-,?).
'prim_spawnConstraint'(Guard,Exp,H,E0,E) :-
        user:hnf(Guard,S,E0,_), % S='Prelude.success',
	user:hnf(Exp,H,E0,E).

prim_isVar(Term,H) :- var(Term), !, H='Prelude.True'.
prim_isVar('VAR'(_),H) :- !, H='Prelude.True'. % for ports and rewriteAll
prim_isVar(_,'Prelude.False').

prim_identicalVar(Y,X,H) :-
        var(X), var(Y), !,
	(X==Y -> H='Prelude.True' ; H='Prelude.False').
prim_identicalVar(_,X,H) :- var(X), !, H='Prelude.False'.
prim_identicalVar(Y,_,H) :- var(Y), !, H='Prelude.False'.
prim_identicalVar('VAR'(I),'VAR'(J),H) :- !, % for ports and rewriteAll
	(I=J -> H='Prelude.True' ; H='Prelude.False').
prim_identicalVar(_,_,'Prelude.False').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of Curry data terms (with variables) into string representation in
% standard prefix notation

prim_showAnyTerm(Term,String) :-
        copy_term(Term,CTerm),
	groundTermVars(CTerm,0,_),
	show_term(CTerm,unqualified,String,[]).

prim_showAnyQTerm(Term,String) :-
        copy_term(Term,CTerm),
	groundTermVars(CTerm,0,_),
	show_term(CTerm,qualified,String,[]).

?- block prim_showAnyExpression(?,?,-,?).
prim_showAnyExpression(Exp,String,E0,E) :-
        removeShares(Exp,UExp), copy_term(UExp,CExp),
	groundTermVars(CExp,0,_),
	show_term(CExp,unqualified,String,[]), E0=E.

?- block prim_showAnyQExpression(?,?,-,?).
prim_showAnyQExpression(Exp,String,E0,E) :-
        shares2let(Lets,Exp,UExp),
	bindSingleLets(Lets),
	copy_term(UExp,CExp),
	groundTermVars(CExp,0,_),
	show_term(CExp,qualified,String,[]), E0=E.

% replace all share structures in a term by let expressions:
shares2let(_,T,T) :- var(T), !.
shares2let(Lets,makeShare(T,_),UT) :- !, shares2let(Lets,T,UT), %????
	writeErr('MAKESHARE OCCURRED'), nlErr.
shares2let(Lets,share(M),LetVar) :- lookupMutable(Lets,M,LetVar), !.
shares2let(Lets,share(M),ShareVar) :- !,
        get_mutable(V,M),
        (V='$eval'(Exp) -> true ; Exp=V),
        shares2let(Lets,Exp,UT),
	addOL(Lets,(M,_NewVar,UT,ShareVar)).
shares2let(Lets,T,UT) :-
        T =.. [F|Args],
        shares2letL(Lets,Args,UArgs),
        UT =.. [F|UArgs].

shares2letL(_,[],[]).
shares2letL(L,[X|Xs],[Y|Ys]) :- shares2let(L,X,Y), shares2letL(L,Xs,Ys).

% lookup mutable with == in open-ended list:
lookupMutable(Binds,_,_) :- var(Binds), !, fail.
lookupMutable([(M,V,T,ShareVar)|_],Mut,LetVar) :- Mut==M, !,
	LetVar=V, ShareVar=let(LetVar,T). % insert let binding
lookupMutable([_|Binds],Mut,LetVar) :- lookupMutable(Binds,Mut,LetVar).

% add new last element to open-ended list:
addOL(Xs,E) :- var(Xs), !, Xs=[E|_].
addOL([_|Xs],E) :- addOL(Xs,E).

% bind remaining lets with single occurrences:
bindSingleLets(Lets) :- var(Lets), !.
bindSingleLets([(_,_,T,ShareVar)|Lets]) :-
	(var(ShareVar) -> ShareVar=T ; true),
	bindSingleLets(Lets).


% bind free variables in a term to a printable ground representation:
groundTermVars(X,I,I1) :- var(X), !,
	X='_'(I),
	I1 is I+1.
groundTermVars(A,I,I) :- atom(A), !.
groundTermVars(T,I,I1) :-
	T =.. [_|Args],
	groundTermsVars(Args,I,I1).

groundTermsVars([],I,I).
groundTermsVars([A|As],I,I2) :-
	groundTermVars(A,I,I1),
	groundTermsVars(As,I1,I2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of standard prefix string representations of Curry terms
% into Curry terms:

% conversion of string representations of Curry terms into Curry terms:
prim_readsAnyQTerm(String,['Prelude.(,)'(Term,TailString)]) :-
	map2M(basics:char_int,String,PrologString),
	readTerm(PrologString,any_qualified,Tail,GTerm),
	ungroundTermVars(GTerm,Term,_),
	map2M(basics:char_int,TailString,Tail), !.
prim_readsAnyQTerm(_,[]). % parse error

prim_readsAnyUnqualifiedTerm(Prefixes,String,['Prelude.(,)'(Term,TailString)]) :-
	(Prefixes=[] -> PrefixDots=any
	  ; map2M(prim_readshowterm:prefix2prefixdot,Prefixes,PrefixDots)),
	map2M(basics:char_int,String,PrologString),
	readTerm(PrologString,any_unqualified(PrefixDots),Tail,GTerm),
	ungroundTermVars(GTerm,Term,_),
	map2M(basics:char_int,TailString,Tail), !.
prim_readsAnyUnqualifiedTerm(_,_,[]). % parse error


% conversion of string representations into Curry expressions:
prim_readsAnyQExpression(String,['Prelude.(,)'(Term,TailString)]) :-
	map2M(basics:char_int,String,PrologString),
	readTerm(PrologString,any_expression,Tail,GTerm),
	ungroundTermVars(GTerm,LTerm,_),
	let2share(LTerm,Term),
	map2M(basics:char_int,TailString,Tail), !.
prim_readsAnyQExpression(_,[]). % parse error

% replace let contruct by share expressions:
let2share(T,T) :- var(T), !.
let2share(share(M),share(M)) :- !. % ignore if already transformed
let2share(let(Var,T),Var) :- !,
	create_mutable(T,M),
	Var=share(M). % instantiate all other occurrences
let2share(LT,T) :-
        LT =.. [F|LArgs],
        map2M(user:let2share,LArgs,Args),
        T =.. [F|Args].

% replace ground representations by free variables in a term:
ungroundTermVars('_'(I),X,Binds) :- !,
	getVarIndex(Binds,I,X).
ungroundTermVars(A,A,_) :- atom(A), !.
ungroundTermVars(T,VT,Binds) :-
	T =.. [C|Args],
	ungroundTermsVars(Args,VArgs,Binds),
	VT =.. [C|VArgs].

ungroundTermsVars([],[],_).
ungroundTermsVars([A|As],[VA|VAs],Binds) :-
	ungroundTermVars(A,VA,Binds),
	ungroundTermsVars(As,VAs,Binds).

getVarIndex(Binds,Idx,Var) :- var(Binds), !, Binds=[(Idx=Var)|_].
getVarIndex([(Idx=V)|_],Idx,Var) :- !, V=Var.
getVarIndex([_|Binds],Idx,Var) :- getVarIndex(Binds,Idx,Var).

