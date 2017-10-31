% Translate Prolog programs containing block declarations into
% SWI-Prolog's freeze predicates.

% in order to avoid read errors in SWI-Prolog:
:- op(1100,fx,(block)).

% Translate a single Prolog file stored in subdirectory lib_src into
% corresponding result and store in in subdirectory lib:
transFile(FileName) :-
	write('Translating Prolog file: '), write(FileName), nl,
	appendAtom('lib_src/',FileName,InFileName),
	appendAtom('libswi/',FileName,OutFileName),
	open(InFileName,read,In),
	open(OutFileName,write,Out),
	read(In,Clause1),
	% put a setting of the no-singleton-warning flag at the beginning
	% of the module (but after the potential module declaration)
	(Clause1 = :-(module(_,_))
         -> translateClause(Out,Clause1,[],TransNames),
   	    writeClause(Out,(:- style_check(-singleton)))
	  ; writeClause(Out,(:- style_check(-singleton))),
	    translateClause(Out,Clause1,[],TransNames)),
	processClauses(In,Out,TransNames),
	close(In),
	close(Out).

processClauses(In,Out,TransNames) :-
	read(In,Clause),
	(Clause=end_of_file
	 -> true
	  ; translateClause(Out,Clause,TransNames,NewTransNames),
	    processClauses(In,Out,NewTransNames)).

translateClause(Out,(?- block(B)),T1,T2) :- !,
	translateClause(Out,(:- block(B)),T1,T2).
translateClause(Out,(:- block(Decls)),T,[Pred/NewPred|T]) :- !,
        analyzeBlockDecls(Decls,Specs),
	summarizeBlockSpecs(Specs,Pred,Arity,Positions),
	genBlockDecl(Out,Pred,Arity,Positions,NewPred).
translateClause(Out,(:- RHS),T,T) :- !, writeClause(Out,(:- RHS)).
translateClause(Out,(?- RHS),T,T) :- !, writeClause(Out,(?- RHS)).
translateClause(Out,(LHS :- RHS),T,T) :- !,
        translateLHS(T,LHS,NewLHS),
	writeClause(Out,(NewLHS :- RHS)).
translateClause(Out,LHS,T,T) :- !,
        translateLHS(T,LHS,NewLHS),
	writeClause(Out,NewLHS).

translateLHS(Ts,LHS,NewLHS) :-
	LHS =.. [Pred|Args],
	lookupTrans(Ts,Pred,NewPred),
	NewLHS =.. [NewPred|Args].

lookupTrans([],Pred,Pred).
lookupTrans([P/NP|Ts],Pred,NewPred) :-
	P=Pred -> NewPred=NP ; lookupTrans(Ts,Pred,NewPred).


analyzeBlockDecls((BlockSpec,BlockSpecs),[Spec|Specs]) :- !,
	analyzeBlockDecl(BlockSpec,Spec),
	analyzeBlockDecls(BlockSpecs,Specs).
analyzeBlockDecls(BlockSpec,[Spec]) :-
	analyzeBlockDecl(BlockSpec,Spec).

analyzeBlockDecl(BlockSpec,(Pred,Arity,Pos)) :-
	functor(BlockSpec,Pred,Arity),
	minusPos(Arity,BlockSpec,Pos).

% compute position of minus symbol:
minusPos(0,Lit,_) :- !,
	write('minusPos: no minus position: '),
	write(Lit), nl.
minusPos(I,Lit,Pos) :-
	arg(I,Lit,'-') -> Pos=I ; I1 is I-1, minusPos(I1,Lit,Pos).

summarizeBlockSpecs([(Pred,Arity,Pos)|Specs],Pred,Arity,[Pos|Poss]) :-
	collectBlockPositions(Specs,Poss).
collectBlockPositions([],[]).
collectBlockPositions([(_,_,Pos)|Specs],[Pos|Poss]) :-
	collectBlockPositions(Specs,Poss).

writeClause(Out,C) :- writeq(Out,C), put_code(Out,46), nl(Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write a block declaration for predicate PredName of arity PredArity
% where the positions in the non-empty(!) list GroundPositions must be instantiated.
% Furthermore, the last argument is a possibly new predicate name corresponding
% to PredName which should be coded instead of PredName (this depends on
% the implementation scheme for block declarations).
genBlockDecl(Out,PredName,PredArity,BoundPositions,NewPredName) :-
	appendAtom('blocked_',PredName,NewPredName),
	functor(Literal,PredName,PredArity),
	Literal =.. [_|Args],
	NewLiteral =.. [NewPredName|Args],
	genFreezeLiteral(BoundPositions,NewLiteral,NewLiteral,FreezeLiteral),
	writeClause(Out,(Literal :- FreezeLiteral)).

genFreezeLiteral([],_,Literal,Literal) :- !.
genFreezeLiteral([P|Ps],Literal,FreezeLiteral,NewFreezeLiteral) :-
	arg(P,Literal,Var),
	genFreezeLiteral(Ps,Literal,freeze(Var,FreezeLiteral),NewFreezeLiteral).


% concatenate two atoms:
appendAtom(A1,A2,A3) :-
	atom_codes(A1,L1), atom_codes(A2,L2),
	append(L1,L2,L3),
	atom_codes(A3,L3).

append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]) :- append(Xs,Ys,Zs).

