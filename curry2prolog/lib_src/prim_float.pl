%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of library Float

prim_Float_plus(Y,X,R) :- R is X+Y.

prim_Float_minus(Y,X,R) :- R is X-Y.

prim_Float_times(Y,X,R) :- R is X*Y.

prim_Float_div(Y,X,R) :- R is X/Y.

% transform an integer into a float:
prim_i2f(X,R) :- R is X*1.0.

% transform a float to an integer:
prim_truncate(X,R) :- R is integer(X).

% round a float to an integer:
prim_round(X,R) :- R is integer(round(X)).

prim_sqrt(X,R) :- R is sqrt(X).

prim_log(X,R) :- R is log(X).

prim_exp(X,R) :- R is exp(X).

prim_sin(X,R) :- R is sin(X).

prim_cos(X,R) :- R is cos(X).

prim_tan(X,R) :- R is tan(X).

prim_atan(X,R) :- R is atan(X).
