%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read and process a file with the Prolog implementation of external
% operations (e.g., "Mod.pakcs.pl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(readPrimFile,[readPrimFile/1]).

:- use_module(prologbasics).
:- use_module(basics).

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

% Read and process a file with the Prolog implementation of external
% operations and write the contants on the standard output stream.
readPrimFile(PrimPlFile) :- prolog(sicstus), !,
        readFileContents(PrimPlFile,Cs), putChars(Cs).
readPrimFile(PrimPlFile) :- prolog(swi), !, transBlockInFile(PrimPlFile).
readPrimFile(_) :- writeLnErr('ERROR in readPrimFile: unknown Prolog system!').

% Translate a Prolog file with block declarations into freeze
% declarations appropriate for SWI-Prolog:
transBlockInFile(FileName) :-
	%writeErr('Translating Prolog file: '), writeLnErr(FileName),
        % in order to avoid read errors in SWI-Prolog:
        op(1150,fx,(block)),
	open(FileName,read,In),
	read(In,Clause1),
	% put a setting of the no-singleton-warning flag at the beginning
	% of the module (but after the potential module declaration)
	(Clause1 = :-(module(_,_))
         -> translateClause(Clause1,[],TransNames),
   	    writeClause((:- style_check(-singleton)))
	  ; writeClause((:- style_check(-singleton))),
	    translateClause(Clause1,[],TransNames)),
	processClauses(In,TransNames),
	close(In).

processClauses(In,TransNames) :-
	read(In,Clause),
	(Clause=end_of_file
	 -> true
	  ; translateClause(Clause,TransNames,NewTransNames),
	    processClauses(In,NewTransNames)).

translateClause((?- block(B)),T1,T2) :- !,
	translateClause((:- block(B)),T1,T2).
translateClause((:- block(Decls)),T,[Pred/NewPred|T]) :- !,
        analyzeBlockDecls(Decls,Specs),
	summarizeBlockSpecs(Specs,Pred,Arity,Positions),
	genBlockDecl(Pred,Arity,Positions,NewPred).
translateClause((:- RHS),T,T) :- !, writeClause((:- RHS)).
translateClause((?- RHS),T,T) :- !, writeClause((?- RHS)).
translateClause((LHS :- RHS),T,T) :- !,
        translateLHS(T,LHS,NewLHS),
	writeClause((NewLHS :- RHS)).
translateClause(LHS,T,T) :- !,
        translateLHS(T,LHS,NewLHS),
	writeClause(NewLHS).

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

writeClause(C) :- writeq(C), put_code(46), nl.
