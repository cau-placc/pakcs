%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This program defines primitive operations used in the Curry
% library CHRcompiled.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module('../basics').
:- use_module('../prologbasics').
:- (prolog(swi) ; (prolog(sicstus),prologMajorVersion(4)))
	-> use_module(library('chr/chr_runtime'))
 	 ; writeErr('CHR(Prolog) not available with this Prolog version!'),
	   nlErr.

warnSuspendedConstraints(ShowAll,R) :-
	find_chr_constraint(C), !,
	write(user_error,'WARNING: residual CHR constraints:'),
	writeSuspendedCHRConstraints(ShowAll),
	R='Prelude.True'.
warnSuspendedConstraints(_,'Prelude.True').

writeSuspendedCHRConstraints(ShowAll) :-
	find_chr_constraint(C), write(user_error,' '), write(user_error,C),
	ShowAll='Prelude.False', !, write(user_error,'...'), nl(user_error).
writeSuspendedCHRConstraints(_) :- nl(user_error).
