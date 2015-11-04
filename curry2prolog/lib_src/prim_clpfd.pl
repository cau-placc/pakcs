%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface to the CLP(FD) solver library of Sicstus-Prolog
%
% The clauses in this file are added to each compiled Curry program
% if finite domains constraints are used

:- use_module('../prologbasics').
:- use_module(library(clpfd)).

prim_FD_domain(L,A,B,R) :-
	(prolog(sicstus) -> domain(L,A,B) ; ins(L,'..'(A,B))),
	R='Prelude.True'.

prim_FD_sum(Vs,RelCall,V,R) :-
        checkSICStusAndWarn('CLPFD.sum'),
	RelCall=partcall(2,FD_Rel,[]),
	translateFD_Rel(FD_Rel,Rel),
	sum(Vs,Rel,V), R='Prelude.True'.

prim_FD_scalar_product(Cs,Vs,RelCall,V,R) :-
        checkSICStusAndWarn('CLPFD.scalar_product'),
	RelCall=partcall(2,FD_Rel,[]),
	translateFD_Rel(FD_Rel,Rel),
	scalar_product(Cs,Vs,Rel,V), R='Prelude.True'.

prim_FD_count(Val,Vs,RelCall,C,R) :-
        checkSICStusAndWarn('CLPFD.count'),
	RelCall=partcall(2,FD_Rel,[]),
	translateFD_Rel(FD_Rel,Rel),
	count(Val,Vs,Rel,C), R='Prelude.True'.

translateFD_Rel('CLPFD.=#',#=) :- !.
translateFD_Rel('CLPFD./=#',#\=) :- !.
translateFD_Rel('CLPFD.<#',#<) :- !.
translateFD_Rel('CLPFD.<=#',#=<) :- !.
translateFD_Rel('CLPFD.>#',#>) :- !.
translateFD_Rel('CLPFD.>=#',#>=) :- !.
translateFD_Rel(FD_Rel,_) :- writeErr('ERROR: Illegal FD constraint: '),
	writeErr(FD_Rel), nlErr, !, fail.

prim_FD_all_different(L,R) :- all_different(L), R='Prelude.True'.

prim_FD_indomain(Var,R) :-
	(prolog(sicstus) -> indomain(Var) ; label([Var])),
	R='Prelude.True'.

prim_FD_labeling(Options,L,R) :-
	map2M(user:translateLabelingOption,Options,PlOptions),
	labeling(PlOptions,L),
	R='Prelude.True'.

translateLabelingOption('CLPFD.LeftMost',leftmost).
translateLabelingOption('CLPFD.FirstFail',ff).
translateLabelingOption('CLPFD.FirstFailConstrained',ffc).
translateLabelingOption('CLPFD.Min',min).
translateLabelingOption('CLPFD.Max',max).
translateLabelingOption('CLPFD.Step',step).
translateLabelingOption('CLPFD.Enum',enum).
translateLabelingOption('CLPFD.Bisect',bisect).
translateLabelingOption('CLPFD.Up',up).
translateLabelingOption('CLPFD.Down',down).
translateLabelingOption('CLPFD.All',all) :- sicsLabel.
translateLabelingOption('CLPFD.Minimize'(DomVar),minimize(DomVar)) :- sicsLabel.
translateLabelingOption('CLPFD.Maximize'(DomVar),maximize(DomVar)) :- sicsLabel.
translateLabelingOption('CLPFD.Assumptions'(Var),assumptions(Var)) :- sicsLabel.
translateLabelingOption('CLPFD.RandomVariable'(Seed),random_variable(Seed)).

sicsLabel :- checkSICStusAndWarn('CLPFD.labeling: labeling options').

prim_FD_plus(Y,X,R) :- #=(R,X+Y).

prim_FD_minus(Y,X,R) :- #=(R,X-Y).

prim_FD_times(Y,X,R) :- #=(R,X*Y).

prim_FD_equal(Y,X,'Prelude.True') :- #=(X,Y).

prim_FD_notequal(Y,X,'Prelude.True') :- #\=(X,Y).

prim_FD_le(Y,X,'Prelude.True') :- #<(X,Y).

prim_FD_leq(Y,X,'Prelude.True') :- #=<(X,Y).

prim_FD_ge(Y,X,'Prelude.True') :- #>(X,Y).

prim_FD_geq(Y,X,'Prelude.True') :- #>=(X,Y).

prim_FD_solve_reify(Constraint,R) :-
	translateConstraint(Constraint,PrologConstraint),
	call(PrologConstraint),
	R='Prelude.True'.

translateConstraint(V,V) :- var(V), !.
translateConstraint(X,X) :- integer(X), !.
translateConstraint('CLPFD.FDEqual'(A,B),#=(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDNotEqual'(A,B),#\=(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDLess'(A,B),#<(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDLessOrEqual'(A,B),#=<(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDGreater'(A,B),#>(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDGreaterOrEqual'(A,B),#>=(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDNeg'(A),#\(C)) :-
	translateConstraint(A,C).
translateConstraint('CLPFD.FDAnd'(A,B),#/\(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDOr'(A,B),#\/(C,D)) :-
	translateConstraint(A,C), translateConstraint(B,D).
translateConstraint('CLPFD.FDImply'(A,B),R) :-
	translateConstraint(A,C), translateConstraint(B,D),
	(prolog(sicstus) -> R = #=>(C,D) ; R = #==>(C,D)).
translateConstraint('CLPFD.FDEquiv'(A,B),R) :-
	translateConstraint(A,C), translateConstraint(B,D),
	(prolog(sicstus) -> R = #<=>(C,D) ; R = #<==>(C,D)).
