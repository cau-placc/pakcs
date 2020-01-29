%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module ReadShowTerm:
%

:- module(prim_readshowterm,
	  [prim_showQTerm/2, prim_showTerm/2, show_term/4,
           prim_readNatLiteral/2, prim_readFloatLiteral/2,
           prim_readCharLiteral/2, prim_readStringLiteral/2,
	   prim_readsQTerm/2, prim_readsUnqualifiedTerm/3, readTerm/4,
	   skipWhiteSpace/2, isShowableArg/1]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of Curry data terms into string representation in
% standard prefix notation

prim_showQTerm(Term,String) :- show_term(Term,qualified,String,[]), !.

prim_showTerm(Term,String) :- show_term(Term,unqualified,String,[]), !.

% with difference lists to speed up:
show_term(V,_,S,S) :-
	var(V), !,
	writeErr('*** Internal error in ReadShowTerm.showTerm: free variable'),
	nlErr.
% special case for Unsafe.showAny(Q)Term:
show_term('_'(I),_,[Underscore|S],E) :- !,
	char_int(Underscore,95),
	number_codes(I,SI), map2M(basics:char_int,String,SI), diffList(String,S,E).
show_term([],_,[OpSqBracket,ClSqBracket|E],E) :- !,
	char_int(OpSqBracket,91), char_int(ClSqBracket,93).
show_term(I,_,S,E) :-
	integer(I), !, showNumber(I,S,E).
show_term(F,_,S,E) :-
	float(F), !, showNumber(F,S,E).
show_term(C,_,[Apo|S],E) :- % 39='''
	isCharCons(C), !,
	char_int(Apo,39),
	char_int(C,N),
	(N=39 -> char_int(BS,92), S=[BS,C|SE] % '
	 ; (N=34 -> S=[C|SE] % "
	          ; show_termchar(N,S,SE))),
	SE = [Apo|E].
show_term('Prelude.()',_,[Op,Cl|E],E) :- !, char_int(Op,40), char_int(Cl,41).
show_term('VAR',_,S,E) :-
	!,
	atom2String('VAR',SV), diffList(SV,S,E).
show_term(T,Q,S,E) :-
	atom(T), !,
	atom2String(T,ST),
	(Q=qualified -> ShowT=ST ; removeQualifier(ST,ShowT)),
	(isId(T) -> diffList(ShowT,S,E)
	          ; % enclose in parentheses:
	            char_int(Op,40), char_int(Cl,41),
	            append([Op|ShowT],[Cl|E],S)),
	!.
show_term(L,_,[Quot|S],E) :- % 34 = '"'
	isString(L), !,
	char_int(Quot,34),
	show_termstring(L,S,E).
show_term([H|T],Q,[OpSqBracket|SH],E) :-
	isCompleteList(T,TS), !,
	char_int(OpSqBracket,91),
	show_term(H,Q,SH,ST),
	show_termcomplist(TS,Q,ST,E).
show_term([H|T],Q,[Op|SH],E) :- !,
	char_int(Op,40), char_int(Cl,41),
	show_termlist([H|T],Q,SH,SHE),
	SHE=[Cl|E].
show_term('Dynamic.Dynamic'(P),Q,S,E) :- !, % specific show for dynamic preds.
	functor(P,DN,_),
	atom_codes(DN,DNS),
	(append("$DYN_",N,DNS) -> atom_codes(TN,N) ; TN=DN), !,
	show_term(TN,Q,S,E).
show_term(T,Q,[Op|SC],E) :- % 40='('
	isShowableArg(T), !,
	functor(T,C,N),
	char_int(Op,40), char_int(Cl,41),
	(isTupleCons(C) -> show_termtuple(1,N,T,Q,SC,SArgsE)
	 ; show_term(C,Q,SC,SCE),
	   show_termargs(1,N,T,Q,SCE,SArgsE)),
	SArgsE=[Cl|E]. % 41=')'

% show for numbers:
showNumber(N,S,E) :-
	number_codes(N,SN), map2M(basics:char_int,String,SN),
	(N>=0 -> diffList(String,S,E)
	       ; % enclose negative number in parentheses:
	         char_int(Op,40), char_int(Cl,41),
	         append([Op|String],[Cl|E],S)).

show_termstring([],[Quot|E],E) :- char_int(Quot,34).
show_termstring([C|T],S,E) :-
	char_int(C,N),
	show_termchar(N,S,ST),
	show_termstring(T,ST,E).

show_termchar( 7,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,97]).  % \a
show_termchar( 8,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,98]).  % \b
show_termchar( 9,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,116]). % \t
show_termchar(10,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,110]). % \n
show_termchar(11,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,118]). % \v
show_termchar(12,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,102]). % \f
show_termchar(13,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,114]). % \r
show_termchar(34,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,34]). % 34="
show_termchar(92,[C1,C2|E],E) :- !, cp_string([C1,C2],[92,92]). % 92=\
show_termchar(N,[C1,C2,C3|E],E) :- N<32, !,
	N1 is (N//10)+48, N2 is (N mod 10)+48,
	cp_string([C1,C2,C3],[92,N1,N2]).
show_termchar(N,E,F) :- N>126, !,
	num2rdigits(N,RDs), rev(RDs,Ds),
	cp_string(DS,[92|Ds]),
	append(DS,F,E).
show_termchar(N,[C|E],E) :- char_int(C,N).

num2rdigits(N,[D]) :- N<10, !, D is N+48.
num2rdigits(N,[D|Ds]) :- D is (N mod 10)+48, N1 is N//10, num2rdigits(N1,Ds).

show_termlist(L,Q,SH,E) :- nonvar(L), L=[H|T], !,
	show_term(H,Q,SH,SHE),
	char_int(Colon,58), % 58=':'
	SHE=[Colon|ST],
	show_termlist(T,Q,ST,E).
show_termlist(L,Q,SH,E) :-
	show_term(L,Q,SH,E).

show_termcomplist([],_,[ClSqBracket|E],E) :- char_int(ClSqBracket,93).
show_termcomplist([H|T],Q,[Comma|SH],E) :- % 44=','
	char_int(Comma,44),
	show_term(H,Q,SH,ST),
	show_termcomplist(T,Q,ST,E).

show_termargs(I,N,Term,Q,S,E) :-
	I>N -> S=E ;
	char_int(Blank,32),
	S=[Blank|SA],
	arg(I,Term,AI),
	show_term(AI,Q,SA,SAE),
	I1 is I+1,
	show_termargs(I1,N,Term,Q,SAE,E).

show_termtuple(I,N,Term,Q,SA,E) :-
	I=N -> arg(I,Term,AI), show_term(AI,Q,SA,E) ;
	arg(I,Term,AI),
	show_term(AI,Q,SA,SA1),
	char_int(Comma,44),
	SA1 = [Comma|SAE], % 44=','
	I1 is I+1,
	show_termtuple(I1,N,Term,Q,SAE,E).

% check argument whether it can be encoded as a string:
isShowableArg('Ports.internalPort'(_,SNr,_,_)) :-
	SNr=<0, % is internal port or process port?
	writeErr('ERROR: cannot serialize internal port!'),
	nlErr,
	!, fail.
isShowableArg(_).

% remove module qualifier from internal name:
removeQualifier(N,UT) :- char_int(Dot,46), removeQualifier(N,Dot,N,UT).
removeQualifier(N,Dot,UT) :-
	startWithModId(N)
	-> removeQualifier(N,Dot,N,UT)
	 ; UT=N.

removeQualifier([],_,N,N).  % no qualifier in name, keep original name
removeQualifier([C|Cs],Dot,N,UN) :-
	C=Dot
	 -> removeQualifier(Cs,Dot,UN)
	  ; (isModIdChar(C) -> removeQualifier(Cs,Dot,N,UN)
	                     ; UN=Cs).

startWithModId([C|_]) :- isModIdChar(C).

isModIdChar(C) :- char_int(C,N),
	(65=<N, N=<90 ; 97=<N, N=<122 ; 48=<N, N=<57 ; N=95).

% convert list (arg 1) into difference list (arg 2+3):
diffList([],E,E).
diffList([H|T],[H|DT],DTE) :- diffList(T,DT,DTE).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% conversion of string representations of nat literals into Curry terms:
prim_readNatLiteral([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        natconst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
prim_readNatLiteral(_,[]). % parse error

natconst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        natconst(Cs).
natconst([]) --> skipblanks.

% conversion of string representations of float literals into Curry terms:
prim_readFloatLiteral([CC|String],['Prelude.(,)'(Num,TailString)]) :-
	char_int(CC,C), C>47, C<58,
        floatconst(NumStr,String,TailString),
        number_codes(Num,[C|NumStr]), !.
prim_readFloatLiteral(_,[]). % parse error

floatconst([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatconst(Cs).
floatconst([46,C|Cs]) --> [PC], { char_int(PC,46) }, [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatconstrest(Cs).

floatconstrest([C|Cs]) --> [CC],
        { char_int(CC,C), C>47, C<58 }, !,
        floatconstrest(Cs).
floatconstrest([C|Cs]) --> [CC], {char_int(CC,C), C=69 ; C=101}, !, % exponent
	intconst(Cs).
floatconstrest([]) --> skipblanks.

intconst(Cs) --> ( [CC], {char_int(CC,45)}, natconst(NCs), {Cs=[45|NCs]}
		  ; natconst(Cs)
		  ).

% conversion of string representations of char literals into Curry terms:
% TODO: avoid char_int conversion
prim_readCharLiteral(String,['Prelude.(,)'(Char,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=39, readChar(PrologString,Tail,Char),
	map2M(basics:char_int,TailString,Tail), !.
prim_readCharLiteral(_,[]). % parse error

% conversion of string representations of string literals into Curry terms:
% TODO: avoid char_int conversion
prim_readStringLiteral(String,['Prelude.(,)'(Result,TailString)]) :-
	map2M(basics:char_int,String,[C|PrologString]),
	C=34, readString(PrologString,Tail,Result),
	map2M(basics:char_int,TailString,Tail), !.
prim_readStringLiteral(_,[]). % parse error

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% conversion of standard prefix string representations of Curry terms
% into Curry terms:

% conversion of string representations of Curry terms into Curry terms:
prim_readsQTerm(String,['Prelude.(,)'(Term,TailString)]) :-
	map2M(basics:char_int,String,PrologString),
	readTerm(PrologString,qualified,Tail,Term),
	map2M(basics:char_int,TailString,Tail), !.
prim_readsQTerm(_,[]). % parse error

prim_readsUnqualifiedTerm(Prefixes,String,['Prelude.(,)'(Term,TailString)]) :-
	(Prefixes=[] -> PrefixDots=any
	  ; map2M(prim_readshowterm:prefix2prefixdot,Prefixes,PrefixDots)),
	map2M(basics:char_int,String,PrologString),
	readTerm(PrologString,unqualified(PrefixDots),Tail,Term),
	map2M(basics:char_int,TailString,Tail), !.
prim_readsUnqualifiedTerm(_,_,[]). % parse error

prefix2prefixdot(CPrefix,PrefixDot) :-
	map2M(basics:char_int,CPrefix,Prefix), append(Prefix,[46],PrefixDot).

readTerm(S,Q,T,Term) :-
	skipWhiteSpace(S,T1),
	readTerm0(T1,Q,T,Term).

readTerm0([C|Cs],Q,T,Term) :-
	isLetter(C), !,
	readTermS([C|Cs],Q,T2,Func),
	skipWhiteSpace(T2,T3),
	(Func=partcall % specific handling of partial applications
	 -> readTerm(T3,Q,T4,Missing), skipWhiteSpace(T4,T5),
	    readPartCallFunc(T5,T6,PCFs), atom_codes(PCF,PCFs),
	    readTermArgs(T6,Q,T,[PArgs]),
	    Term = partcall(Missing,PCF,PArgs)
	  ; readTermArgs(T3,Q,T,Args),
	    Term =.. [Func|Args]).
readTerm0([36|Cs],Q,T,Term) :-
	Cs=[68,89,78|_], !, % specific handling of dynamic predicate names
	(readQVarOpId(Cs,T2,IdString) -> true ; readParseError([36|Cs])),
	atom_codes(Func,[36|IdString]),
	skipWhiteSpace(T2,T3),
	readTermArgs(T3,Q,T,Args),
	Term =.. [Func|Args].
readTerm0(S,Q,T,Term) :-
	readTermS(S,Q,T,Term).


% special case for Unsafe.readAny(Q)Term:
readTermS([95|S],Q,T,'_'(Num)) :- % variable encoding
	(Q=any_qualified ; Q=any_unqualified(_) ; Q=any_expression), !,
	numberconst(NumStr,S,T), !, % use c2p parser
	number_codes(Num,NumStr).
readTermS([C|S],_,T,Num) :- % number
	C>47, C<58,
	numberconst(NumStr,[C|S],T), !, % use c2p parser
	number_codes(Num,NumStr).
readTermS([45,C|S],_,T,Num) :- % negative number
	C>47, C<58,
	numberconst(NumStr,[C|S],T), !, % use c2p parser
	number_codes(PNum,NumStr),
	Num is 0-PNum.
readTermS([91,93|T],_,T,[]) :- !. % empty list
readTermS([91|S],Q,T,L) :- !, readCompList(S,Q,T,L). % non-empty list
readTermS([39|S],Q,T,C) :- !, % character
	(readChar(S,T,C) -> true ; readCharParseError(Q,[39|S])).
readTermS([34|S],_,T,String) :- !, % string
	readString(S,T,String).
readTermS([40,41|T],_,T,'Prelude.()') :- !.
readTermS([40|S],Q,T,Term) :- !,	% open bracket
	readTerm(S,Q,SA1,Term1), skipWhiteSpace(SA1,SA2),
	(SA2=[58|SA3] -> readList(SA3,Q,SA4,Tail),
	                 Term=[Term1|Tail], SA4=[41|T]
	 ; (SA2=[44|SA3] -> readTuple(SA3,Q,SA4,Tail), SA4=[41|T],
	                    Args=[Term1|Tail], length(Args,NA),
	                    Cons=")", prefixComma(Cons,NA,ConsComma),
	                    append("Prelude.(",ConsComma,TupleConsString),
	                    atom_codes(TupleCons,TupleConsString),
	                    Term =.. [TupleCons|Args]
         ; (SA2=[41|T] -> Term=Term1
 	                ; readTermArgs(SA2,Q,[41|T],Args),
         	          Term =.. [Term1|Args]))).
readTermS(S,Q,T,QId) :-
        (readQVarOpId(S,T,IdString) -> true ; readParseError(S)),
        atom_codes(Id,IdString),
        readIdTerm(Id,Q,S,QId).

% read partially applied function name (which is any string until the first blank):
readPartCallFunc([C|Cs],Cs1,[]) :- isWhiteSpace(C), !, skipWhiteSpace(Cs,Cs1).
readPartCallFunc([C|Cs],Cs1,[C|Xs]) :- readPartCallFunc(Cs,Cs1,Xs).

readCharParseError(unchecked,S) :- % show always error in .fcy file reading
	!,
	writeErr('ERROR: FlatCurry file contains illegal character: ...'),
	take(20,S,Line),
	putChars(user_error,Line), writeErr('...'), nlErr,
	writeErr('Hint: do not use UTF encoding but 8bit chars (check your locale settings)'), nlErr,
	raise_exception('parse error').
readCharParseError(_,S) :-
	pakcsrc(readtermerrors,yes),
	writeErr('ERROR in ReadShowTerm.readTerm: illegal character in remaining string:'),
	nlErr,
	putChars(user_error,S), nlErr,
	raise_exception('parse error in ReadShowTerm.readTerm').

readParseError(S) :-
	pakcsrc(readtermerrors,yes),
	writeErr('ERROR in ReadShowTerm.readTerm: cannot parse remaining string:'),
	nlErr,
	putChars(user_error,S), nlErr,
	!, fail.

readIdTerm('VAR',_,_,'VAR') :- !.
readIdTerm(Id,unchecked,_,Id) :- !. % don't check constructors (for readFlcFromFcy.pl)
readIdTerm(Id,qualified,_,QId) :-
	(constructorOrFunctionType(Id,_,_,_) -> QId=Id
	                                      ; tryAddQualifier(Id,QId)),
	!.
readIdTerm(Id,unqualified(Prefixes),_,QId) :-
	addQualifier(Prefixes,Id,QId).
readIdTerm(Id,any_qualified,_,Id) :-
	constructorOrFunctionType(Id,_,_,_), !.
readIdTerm(Id,any_expression,_,Id) :-
	constructorOrFunctionType(Id,_,_,_), !.
readIdTerm(let,any_expression,_,let) :- !.
readIdTerm(Id,any_unqualified(Prefixes),_,QId) :-
	addQualifier(Prefixes,Id,QId).
readIdTerm(Id,_,S,_) :-
	pakcsrc(readtermerrors,yes),
	writeErr('ERROR in ReadShowTerm.readTerm: Unknown symbol: '),
	writeErr(Id), nlErr,
	writeErr('in remaining term string: '),
        take(1000,S,ShortS),
	putChars(user_error,ShortS), nlErr,
	!, fail.

% try to add a qualifier to a unique unqualified constructor symbol
% (used to avoid problems readQTerm if other Curry implementations,
% like KiCS2, write them without qualifiers):
tryAddQualifier(Id,QId) :-
	user:constructortype(QId,_,_,Id,_,_,_),
	user:constructortype(QJ,_,_,Id,_,_,_),
	\+ QId=QJ, !,
	writeErr('WARNING: Unqualified symbol "'), writeErr(Id),
	writeErr('" not unique due to multiple imports.'), nlErr.
tryAddQualifier(Id,QId) :-
	user:constructortype(QId,_,_,Id,_,_,_), !.

addQualifier(any,Id,QId) :-
	constructorOrFunctionType(QId,Id,_,_),
	constructorOrFunctionType(QJ,Id,_,_),
	\+ QId=QJ, !,
	writeErr('WARNING: Unqualified symbol "'), writeErr(Id),
	writeErr('" not unique due to multiple imports.'), nlErr.
addQualifier(any,Id,QId) :-
	constructorOrFunctionType(QId,Id,_,_), !.
addQualifier([Prefix|_],Id,QId) :-
	atom_codes(Id,IdS),
	append(Prefix,IdS,QIdS), atom_codes(QId,QIdS),
	constructorOrFunctionType(QId,_,_,_), !.
addQualifier([_|Prefixes],Id,QId) :- addQualifier(Prefixes,Id,QId).
addQualifier([],Id,_) :-
	writeErr('ERROR: Unknown unqualified symbol: '),
	writeErr(Id), nlErr, fail.

readCompList(S,Q,T,[Elem|Tail]) :-
	readTerm(S,Q,SA1,Elem), skipWhiteSpace(SA1,SA2),
	(SA2=[93|SA3] -> T=SA3, Tail=[] ;
	 SA2=[44|SA3], readCompList(SA3,Q,T,Tail)).

readList(S,Q,T,L) :-
	readTerm(S,Q,SA1,Elem), skipWhiteSpace(SA1,SA2),
	(SA2=[58|SA3] -> L=[Elem|Tail], readList(SA3,Q,T,Tail)
	               ; L=Elem, T=SA2).

readTuple(S,Q,T,L) :-
	readTerm(S,Q,SA1,Elem), skipWhiteSpace(SA1,SA2),
	(SA2=[44|SA3] -> L=[Elem|Tail], readTuple(SA3,Q,T,Tail)
	               ; L=[Elem], T=SA2).

readTermArgs([],_,[],[]) :- !.
readTermArgs([41|S],_,[41|S],[]) :- !. % 41 = )
readTermArgs([44|S],_,[44|S],[]) :- !. % 44 = ,
readTermArgs([58|S],_,[58|S],[]) :- !. % 58 = :
readTermArgs([93|S],_,[93|S],[]) :- !. % 93 = ]
readTermArgs(S,Q,T,[Arg|Args]) :-
	readTermS(S,Q,SA1,Arg), skipWhiteSpace(SA1,SA2),
	readTermArgs(SA2,Q,T,Args).

readChar([92,N|S],T,C) :- N>=48, N<58, !, % read decimal numeric char
	readDecimalChar(0,[N|S],T,C).
readChar([92,N,39|T],T,C) :- !,
	readStringChar(N,NS),
	char_int(C,NS).
readChar([92,78,85,76,39|T],T,C) :- !, char_int(C,0). % '\NUL' character
readChar([92,83,79,72,39|T],T,C) :- !, char_int(C,1). % '\SOH' character
readChar([92,83,84,88,39|T],T,C) :- !, char_int(C,2). % '\STX' character
readChar([92,69,84,88,39|T],T,C) :- !, char_int(C,3). % '\ETX' character
readChar([92,69,79,84,39|T],T,C) :- !, char_int(C,4). % '\EOT' character
readChar([92,69,78,81,39|T],T,C) :- !, char_int(C,5). % '\ENQ' character
readChar([92,65,67,75,39|T],T,C) :- !, char_int(C,6). % '\ACK' character
readChar([92,97,39|T],T,C) :- !, char_int(C,7). % '\a' character
readChar([92,66,69,76,39|T],T,C) :- !, char_int(C,7). % '\BEL' character
readChar([92,98,39|T],T,C) :- !, char_int(C,8). % '\b' character
readChar([92,66,83,39|T],T,C) :- !, char_int(C,8). % '\BS' character
readChar([92,116,39|T],T,C) :- !, char_int(C,9). % '\t' character
readChar([92,72,84,39|T],T,C) :- !, char_int(C,9). % '\HT' character
readChar([92,110,39|T],T,C) :- !, char_int(C,10). % '\n' character
readChar([92,76,70,39|T],T,C) :- !, char_int(C,10). % '\LF' character
readChar([92,118,39|T],T,C) :- !, char_int(C,11). % '\v' character
readChar([92,86,84,39|T],T,C) :- !, char_int(C,11). % '\VT' character
readChar([92,102,39|T],T,C) :- !, char_int(C,12). % '\f' character
readChar([92,70,70,39|T],T,C) :- !, char_int(C,12). % '\FF' character
readChar([92,114,39|T],T,C) :- !, char_int(C,13). % '\r' character
readChar([92,67,82,39|T],T,C) :- !, char_int(C,13). % '\CR' character
readChar([92,83,79,39|T],T,C) :- !, char_int(C,14). % '\SO' character
readChar([92,83,73,39|T],T,C) :- !, char_int(C,15). % '\SI' character
readChar([92,68,76,69,39|T],T,C) :- !, char_int(C,16). % '\DLE' character
readChar([92,68,67,49,39|T],T,C) :- !, char_int(C,17). % '\DC1' character
readChar([92,68,67,50,39|T],T,C) :- !, char_int(C,18). % '\DC2' character
readChar([92,68,67,51,39|T],T,C) :- !, char_int(C,19). % '\DC3' character
readChar([92,68,67,52,39|T],T,C) :- !, char_int(C,20). % '\DC4' character
readChar([92,78,65,75,39|T],T,C) :- !, char_int(C,21). % '\NAK' character
readChar([92,83,89,78,39|T],T,C) :- !, char_int(C,22). % '\SYN' character
readChar([92,69,84,66,39|T],T,C) :- !, char_int(C,23). % '\ETB' character
readChar([92,67,65,78,39|T],T,C) :- !, char_int(C,24). % '\CAN' character
readChar([92,69,77,39|T],T,C) :- !, char_int(C,25). % '\EM' character
readChar([92,83,85,66,39|T],T,C) :- !, char_int(C,26). % '\SUB' character
readChar([92,69,83,67,39|T],T,C) :- !, char_int(C,27). % '\ESC' character
readChar([92,70,83,39|T],T,C) :- !, char_int(C,28). % '\FS' character
readChar([92,71,83,39|T],T,C) :- !, char_int(C,29). % '\GS' character
readChar([92,82,83,39|T],T,C) :- !, char_int(C,30). % '\RS' character
readChar([92,85,83,39|T],T,C) :- !, char_int(C,31). % '\US' character
readChar([92,83,80,39|T],T,C) :- !, char_int(C,32). % '\SP' character
readChar([92,68,69,76,39|T],T,C) :- !, char_int(C,127). % '\DEL' character
readChar([N,39|T],T,C) :- char_int(C,N).

readDecimalChar(N,[39|T],T,C) :- !, char_int(C,N).
readDecimalChar(N,[M|S],T,C) :- M>=48, M<58, !,
	NM is 10*N+M-48,
	readDecimalChar(NM,S,T,C).

readString([34|T],T,[]) :- !.
readString([92|Ns],T,Str) :- !, readStringEscape(Ns,T,Str).
readString([N|Ns],T,[C|Str]) :-
	char_int(C,N),
	readString(Ns,T,Str).

readStringEscape([N|Ns],T,Str) :- N>=48, N<58, !,
        V is N-48, readDecimalCharInString(V,Ns,T,Str).
readStringEscape([69,83,67|Ns],T,[C|Str]) :- !, % '\ESC' character
        char_int(C,27),
        readString(Ns,T,Str).
readStringEscape([68,69,76|Ns],T,[C|Str]) :- !, % '\DEL' character
        char_int(C,127),
        readString(Ns,T,Str).
readStringEscape([N|Ns],T,[C|Str]) :- !,
	readStringChar(N,NS),
	char_int(C,NS),
	readString(Ns,T,Str).

% read a character with decimal number representation, e.g., '\243'
readDecimalCharInString(V,[N|Ns],T,Str) :- N>=48, N<58, !,
        V1 is V*10+N-48,
        readDecimalCharInString(V1,Ns,T,Str).
readDecimalCharInString(V,Ns,T,[C|Str]) :-
        char_int(C,V),
        readString(Ns,T,Str).

readStringChar(97,7) :- !.
readStringChar(98,8) :- !.
readStringChar(116,9) :- !.
readStringChar(110,10) :- !.
readStringChar(118,11) :- !.
readStringChar(102,12) :- !.
readStringChar(114,13) :- !.
readStringChar(34,34) :- !.
readStringChar(39,39) :- !.
readStringChar(92,92) :- !.
readStringChar(N,N) :-
	writeErr('INTERNAL ERROR: unknown character string "'),
	put_code(user_error,92), put_code(user_error,N),
	writeErr('" in readStringChar'), nlErr.

readQVarOpId([C|Cs],T,[C|Str]) :-
	isOpIdChar(C) ->  readOpId(Cs,T,Str) ; readModOrVar(Cs,T,Str).

readModOrVar([C|Cs],T,[C|Str]) :-
	isVarIdChar(C), !,
	(C=46 -> readQVarOpId(Cs,T,Str) ; readModOrVar(Cs,T,Str)).
readModOrVar(T,T,[]).


readOpId([C|Cs],T,[C|Str]) :-
	isOpIdChar(C), !,
	readOpId(Cs,T,Str).
readOpId(T,T,[]).

skipWhiteSpace([C|Cs],Cs1) :- isWhiteSpace(C), !, skipWhiteSpace(Cs,Cs1).
skipWhiteSpace([123,45|Cs],Cs1) :- !, skipComment(Cs,Cs1).
skipWhiteSpace(Cs,Cs).

skipComment([],[]) :- !,
 	(pakcsrc(readtermerrors,yes)
	 -> writeErr('ERROR in ReadShowTerm.readTerm: incomplete comment'),
	    nlErr, fail
	  ; fail).
skipComment([45,125|Cs],Cs1) :- !, skipWhiteSpace(Cs,Cs1).
skipComment([_|Cs],Cs1) :- !, skipComment(Cs,Cs1).

isWhiteSpace(32).
isWhiteSpace(10).
isWhiteSpace(13).
isWhiteSpace(12).
isWhiteSpace( 9).

% is a character of a variable identifier, i.e.,
% A..Z a..z 0..9 . _ '
%isVarIdChar(C) :-
%	65=<C, C=<90 ; 97=<C, C=<122 ; 48=<C, C=<57 ; C=46 ; C=95 ; C=39.

% generated by:
% mapIO_ (putStrLn . ("isVarIdChar("++)  . (++").") . show) ([65..90]++[97..122]++[48..57]++[46,95,39])
isVarIdChar(65).
isVarIdChar(66).
isVarIdChar(67).
isVarIdChar(68).
isVarIdChar(69).
isVarIdChar(70).
isVarIdChar(71).
isVarIdChar(72).
isVarIdChar(73).
isVarIdChar(74).
isVarIdChar(75).
isVarIdChar(76).
isVarIdChar(77).
isVarIdChar(78).
isVarIdChar(79).
isVarIdChar(80).
isVarIdChar(81).
isVarIdChar(82).
isVarIdChar(83).
isVarIdChar(84).
isVarIdChar(85).
isVarIdChar(86).
isVarIdChar(87).
isVarIdChar(88).
isVarIdChar(89).
isVarIdChar(90).
isVarIdChar(97).
isVarIdChar(98).
isVarIdChar(99).
isVarIdChar(100).
isVarIdChar(101).
isVarIdChar(102).
isVarIdChar(103).
isVarIdChar(104).
isVarIdChar(105).
isVarIdChar(106).
isVarIdChar(107).
isVarIdChar(108).
isVarIdChar(109).
isVarIdChar(110).
isVarIdChar(111).
isVarIdChar(112).
isVarIdChar(113).
isVarIdChar(114).
isVarIdChar(115).
isVarIdChar(116).
isVarIdChar(117).
isVarIdChar(118).
isVarIdChar(119).
isVarIdChar(120).
isVarIdChar(121).
isVarIdChar(122).
isVarIdChar(48).
isVarIdChar(49).
isVarIdChar(50).
isVarIdChar(51).
isVarIdChar(52).
isVarIdChar(53).
isVarIdChar(54).
isVarIdChar(55).
isVarIdChar(56).
isVarIdChar(57).
isVarIdChar(46).
isVarIdChar(95).
isVarIdChar(39).

isLetter(65).
isLetter(66).
isLetter(67).
isLetter(68).
isLetter(69).
isLetter(70).
isLetter(71).
isLetter(72).
isLetter(73).
isLetter(74).
isLetter(75).
isLetter(76).
isLetter(77).
isLetter(78).
isLetter(79).
isLetter(80).
isLetter(81).
isLetter(82).
isLetter(83).
isLetter(84).
isLetter(85).
isLetter(86).
isLetter(87).
isLetter(88).
isLetter(89).
isLetter(90).
isLetter(97).
isLetter(98).
isLetter(99).
isLetter(100).
isLetter(101).
isLetter(102).
isLetter(103).
isLetter(104).
isLetter(105).
isLetter(106).
isLetter(107).
isLetter(108).
isLetter(109).
isLetter(110).
isLetter(111).
isLetter(112).
isLetter(113).
isLetter(114).
isLetter(115).
isLetter(116).
isLetter(117).
isLetter(118).
isLetter(119).
isLetter(120).
isLetter(121).
isLetter(122).

