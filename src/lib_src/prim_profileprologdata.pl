%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module System:
%

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).

prim_profileReset('Prelude.()') :-
	checkPlProfiling,
	profile_reset([user:_]).

prim_profilePredicates(SelS,TData) :-
	checkPlProfiling,
        transSelection(SelS,Selection),
	profile_data([user:_],Selection,predicate,Data),
	transPredData(Data,TData).

transPredData([],[]).
transPredData([-(_,0)|Ds],TDs) :- !, % don't show zero values
	transPredData(Ds,TDs).
transPredData([-(:(user,Pred/_),C)|Ds],['Prelude.(,)'(PredString,C)|TDs]) :-
	decodePrologName(Pred,FName),
	atom2String(FName,PredString),
	transPredData(Ds,TDs).

prim_profileClauses(SelS,PredS,TData) :-
	checkPlProfiling,
        transSelection(SelS,Selection),
        string2Atom(PredS,Pred),
	profile_data([user:Pred],Selection,clause,Data),
	transClauseData(Data,TData).

transClauseData([],[]).
transClauseData([-(:(user,_/_/CNr),C)|Ds],['Prelude.(,)'(CNr,C)|TDs]) :-
	transClauseData(Ds,TDs).


prim_printProfileData('Prelude.()') :-
	checkPlProfiling,
	print_profile. % for printing profile data


prim_profileView('Prelude.()') :-
	checkPlProfiling,
	use_module(library(gauge)), % for visualization of profile data
	view([user:_]).


transSelection('Debug.Profile.PrologData.Calls',calls).
transSelection('Debug.Profile.PrologData.Backtracks',backtracks).
transSelection('Debug.Profile.PrologData.ChoicePoints',choice_points).
transSelection('Debug.Profile.PrologData.ExecTime',execution_time).


% check for profiling mode and terminate with error:
checkPlProfiling :-
	prolog_flag(profiling,Mode,Mode),
	(Mode = on -> true
	  ; raise_exception('No in profiling mode, use ":set +plprofile"!')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get the list of hnf definitions in a translated Curry program:

prim_getHnfDefinitions(HnfInfos) :-
	checkPlProfiling,
	lastload(ProgS),
	atom_codes(Prog,ProgS),
	(Prog=[] -> write('ERROR: no program loaded for profiling'), nl,
	            !, fail
                  ; true),
	% this does no longer work since the main Prolog file is deleted
	% by this operation:
	mainPrologFileName(MainPlFile),
	see(MainPlFile),
	repeat,
	read(Clause),
	isHnfClause(Clause),
	!,
	hnfClause2Info(Clause,I1),
	hnfClauses2InfoList(2,Is),
	seen,
	map2M(user:hnfInfo2Curry,[1/I1|Is],HnfInfos).

hnfInfo2Curry(N/A,'Prelude.(,)'(N,AS)) :- atom2String(A,AS).
	
hnfClauses2InfoList(N,[N/I|Is]) :-
	read(Clause),
	isHnfClause(Clause), !,
	hnfClause2Info(Clause,I),
	N1 is N+1,
	hnfClauses2InfoList(N1,Is).
hnfClauses2InfoList(_,[]).

isHnfClause((:- hnf(_,_,_,_))) :- !.
isHnfClause((hnf(_,_,_,_):-_)) :- !.
isHnfClause(hnf(_,_,_,_)) :- !.

hnfClause2Info((hnf(Arg,_,_,_) :- _),'LOGVAR') :- var(Arg), !.
hnfClause2Info((hnf(FCall,_,_,_) :- _),Func) :- !,
        functor(FCall,PFunc,_), decodePrologName(PFunc,Func).
hnfClause2Info(hnf(_,_,_,_),'CONS').

