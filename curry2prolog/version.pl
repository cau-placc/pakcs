%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Information about version of Curry2Prolog.

:- module(version,[prologMajor/1, printPakcsHeader/0, printVersionNumber/0]).

:- use_module(prologbasics).
:- use_module(basics).
:- use_module(pakcsversion).

% Included in headers of compiler Prolog files.
% Used to check already compiled Prolog files for correct
% version w.r.t. run-time system
%compilerVersion('PAKCS1.10').

%compilerMajorVersion(1).
%compilerMinorVersion(10).
%compilerRevisionVersion(0).
%buildVersion(2).
%buildDate('06/06/12').

% Prolog compiler version included in .pl headers to distinguish
% code for different Prolog systems
prologMajor(PrologVersion) :-
	prolog(Prolog), atom_codes(Prolog,PrologS),
	prologMajorVersion(MVN), number_codes(MVN,MVS),
	append(PrologS,MVS,PrologVersionS),
	atom_codes(PrologVersion,PrologVersionS), !.

printVersionNumber :-
	compilerMajorVersion(V1),
	compilerMinorVersion(V2),
	compilerRevisionVersion(V3),
	writeNQ(V1), writeNQ('.'),
	writeNQ(V2), writeNQ('.'),
	writeNQ(V3),
        buildVersion(B),
        (B=0 -> true ; writeNQ('-b'), writeNQ(B)).

	
printPakcsHeader :-
	prolog(Prolog),
	prologMajorVersion(MajV),
	prologMinorVersion(MinV),
	writeNQ('  ______      __       _    _    ______   _______     '), nlNQ,
	writeNQ(' |  __  |    /  \\     | |  / /  |  ____| |  _____|   Portland Aachen Kiel'), nlNQ,
	writeNQ(' | |  | |   / /\\ \\    | |_/ /   | |      | |_____    Curry System'), nlNQ,
	writeNQ(' | |__| |  / /__\\ \\   |  _  |   | |      |_____  |   '), nlNQ,
	writeNQ(' |  ____| / ______ \\  | | \\ \\   | |____   _____| |   Version '), printVersionNumber, nlNQ,
	writeNQ(' |_|     /_/      \\_\\ |_|  \\_\\  |______| |_______|   '), nlNQ,
        writeNQ(' ***WITH TYPECLASSES***'), nlNQ,
	nlNQ,
        writeNQ('Curry2Prolog('), writeNQ(Prolog),
	writeNQ(' '), writeNQ(MajV), writeNQ('.'), writeNQ(MinV),
	writeNQ(') Compiler Environment (Version of '),
	buildDate(BD), writeNQ(BD), writeNQ(')'), nlNQ,
	writeNQ('(RWTH Aachen, CAU Kiel, Portland State University)'), nlNQ.
