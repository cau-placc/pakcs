%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reading .acy files

:- module(readACY,[readAcy/3]).

:- use_module(prologbasics).
:- use_module(basics).
:- use_module(readShowTerm).  % for term en/decoding

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

% read an AbstractCurry program:
readAcy(LoadPath,Prog,AcyProg) :-
	readAcyInLoadPath(LoadPath,Prog,AcyProg), !.
readAcy(LoadPath,Prog,_) :-
	write('ERROR: AbstractCurry file '), write(Prog),
	write('.acy not found!'), nl,
	write('Current load path: '), write(LoadPath), nl,
	!, fail.

readAcyInLoadPath([Dir|Dirs],Prog,AcyProg) :-
	appendAtoms([Dir,'/',Prog],DirProg),
	prog2AbstractCurryFile(DirProg,DirProgFile),
	(existsFile(DirProgFile)
	 -> readAcy(DirProgFile,AcyProg)
	  ; readAcyInLoadPath(Dirs,Prog,AcyProg)).

readAcy(FileName,AProg) :-
	(verbosityIntermediate
	 -> write(user_error,'>>> Reading '),
	    write(user_error,FileName), write(user_error,' ... '),
	    getRunTime(RT1) ; true),
	open(FileName,read,Stream),
	readStreamContents(Stream,AcyPrologString),
	readTerm(AcyPrologString,unchecked,Tail,AProg),
	skipWhiteSpace(Tail,[]),
	(verbosityIntermediate
	 -> getRunTime(RT2),
	    RT is RT2-RT1,
	    write(user_error,RT), write(user_error,' ms.'), nl(user_error)
	  ; true), !.
readAcy(FileName,_) :-
	writeErr('ERROR in readAcy during reading of "'),
	writeErr(FileName), writeErr('"!'),
	nlErr, fail.
