% Slightly change the headers of Prolog modules so that conditional imports
% of the form
% 
%     :- (current_module(basics) -> true ; use_module('../basics')).
% 
% are replaced by unconditional ones:
% 
%     :- use_module('../basics').
% 
% This is necessary due to a strange behavior of use_module declarations
% in SICStus-Prolog

% Translate a single Prolog file stored in subdirectory `lib_src` into
% corresponding result and store in in subdirectory `libsicstus`:
transFile(FileName) :-
	write('Translating Prolog file: '), write(FileName), nl,
	appendAtom('lib_src/',FileName,InFileName),
	appendAtom('libsicstus/',FileName,OutFileName),
	open(InFileName,read,In),
	open(OutFileName,write,Out),
	processClauses(In,Out),
	close(In),
	close(Out).

processClauses(In,Out) :-
	read(In,Clause),
	(Clause=end_of_file
	 -> true
	  ; translateClause(Out,Clause),
	    processClauses(In,Out)).

translateClause(Out,(:- current_module(_) -> true ; use_module(M))) :- !,
        writeClause(Out,(:- use_module(M))).
translateClause(Out,(:- RHS)) :- !, writeClause(Out,(:- RHS)).
translateClause(Out,(?- RHS)) :- !, writeClause(Out,(?- RHS)).
translateClause(Out,(LHS :- RHS)) :- !,	writeClause(Out,(LHS :- RHS)).
translateClause(Out,LHS) :- !, writeClause(Out,LHS).


writeClause(Out,C) :- writeq(Out,C), put_code(Out,46), nl(Out).

% concatenate two atoms:
appendAtom(A1,A2,A3) :-
	atom_codes(A1,L1), atom_codes(A2,L2),
	append(L1,L2,L3),
	atom_codes(A3,L3).
