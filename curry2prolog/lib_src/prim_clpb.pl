% Provides an interface to the CLP(B) solver library of Sicstus-Prolog.
%
% The clauses in this file are added to each compiled Curry program
% if boolean constraints are used.

:- use_module('../prologbasics').
:- prolog(sicstus) -> use_module(library(clpb))
                    ; onlySICStusMessage('CLPB constraints').

clpb_neg(B,NB) :- =(~(B),NB).

clpb_and(B2,B1,B) :- =(B1*B2,B).

clpb_or(B2,B1,B) :- =(B1+B2,B).

clpb_xor(B2,B1,B) :- =(#(B1,B2),B).

clpb_card(Ns,Bs,B) :- =(B,card(Ns,Bs)).

clpb_exists(V,B,R) :- =(V^B,R).

clpb_sat(B,C) :- sat(B), C='Prelude.True'.

clpb_check(B,R) :- taut(B,R).

clpb_labeling(Bs,C) :- labeling(Bs), C='Prelude.True'.

