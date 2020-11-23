%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Information about version of PAKCS.

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
	writeNQ(V3).

printVersionNumberWithBuild :-
        printVersionNumber,
        buildVersion(B),
        (B=0 -> true ; writeNQ('-b'), writeNQ(B)).
	
printPakcsHeader :-
        withColor(no), !,
	writeNQ(' __    _'), nlNQ,
	writeNQ('|_ \\  | |            PAKCS - the Portland Aachen Kiel Curry System'), nlNQ,
	writeNQ('  \\ \\ | |____'), nlNQ,
	writeNQ('  /  \\|  ____|       '), printPAKCSVersion, nlNQ,
	writeNQ(' / /\\ \\ |'), nlNQ,
	writeNQ('/_/  \\_\\|            '),  printCPMVersion, nlNQ,
        nlNQ.
printPakcsHeader :-
	writeNQ('\e[34m __\e[31m    _'), nlNQ,
	writeNQ('\e[34m|_ \\\e[31m  | |            \e[0mPAKCS - the Portland Aachen Kiel Curry System'), nlNQ,
	writeNQ('\e[34m  \\ \\\e[31m | |____'), nlNQ,
	writeNQ('\e[34m  /  \\\e[31m|  ____|       \e[0m'), printPAKCSVersion, nlNQ,
	writeNQ('\e[34m / /\\ \\\e[31m |'), nlNQ,
	writeNQ('\e[34m/_/  \\_\\\e[31m|            \e[0m'),  printCPMVersion, nlNQ,
	writeNQ('\e[0m'), nlNQ,
        nlNQ.

printPAKCSVersion :-
	prolog(Prolog),
	prologMajorVersion(MajV),
	prologMinorVersion(MinV),
        buildDate(BD),
        writeNQ('Version '),
        printVersionNumberWithBuild, writeNQ(' of '), writeNQ(BD),
        writeNQ(' ('), writeNQ(Prolog),
	writeNQ(' '), writeNQ(MajV), writeNQ('.'), writeNQ(MinV),
        writeNQ(')').

printCPMVersion :-
        cpmVersion(CV),
        (CV = '' -> true ; writeNQ('(using '), writeNQ(CV), writeNQ(')')).
