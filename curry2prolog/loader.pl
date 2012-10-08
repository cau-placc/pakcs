%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operations to load a translated Curry application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(loader,
	  [currentModule/1, loadedModule/2, initializationsInProg/1,
	   currentCostCenters/1, curryModule/1, costCenters/1,
	   initializeBeforeLoad/0, initializationsInModule/1,
	   loadAndCompile/3, importModule/1, startCPNSD/0]).

:- dynamic currentModule/1, loadedModule/2, importedModule/1,
	   initializationsInProg/1, currentCostCenters/1.

:- use_module(prologbasics).
:- use_module(basics).
%:- use_module(compiler). % for generateMainPlFile in interactive mode

currentModule(''). % name of currently loaded module
initializationsInProg(true). % intializations to be done after loading Curry program
currentCostCenters(['']). % list of current cost centers


% set name of current Curry module:
curryModule(ModName) :-
	retract(currentModule(_)),
	asserta(currentModule(ModName)).

% set list of cost centers of the current Curry module:
costCenters(CCs) :-
	retract(currentCostCenters(_)),
	asserta(currentCostCenters(CCs)).

% initialize PAKCS before loading a new Curry application:
initializeBeforeLoad :-
	retractAllFacts(dynamicPredInfo/2),
	retractAllFacts(functiontype/6),
	retractAllFacts(constructortype/6),
	retractAllFacts(evaluation/2),
	retractAllFacts(loadedModule/2),
	retractAllFacts(importedModule/1),
	retract(initializationsInProg(_)),
	asserta(initializationsInProg(true)),
	retract(currentCostCenters(_)),
	asserta(currentCostCenters([''])).

% add initializations for currently loaded module:
initializationsInModule(ModInit) :-
	retract(initializationsInProg(ProgInit)),
	asserta(initializationsInProg((ProgInit,ModInit))).


% load a complete translated application by compiling the Prolog programs:
loadAndCompile(PrologFile,AddImports,CreateMain) :-
	initializeBeforeLoad,
	compilePrologFileAndSave(PrologFile),
	currentModule(MainMod),
	assertz(loadedModule(MainMod,PrologFile)),
	map1M(loader:importModule,AddImports),
	loadAndCompileImports,
	curryModule(MainMod),
	(CreateMain=create -> compiler:loadMain(PrologFile) ;
	 CreateMain=load(MainPrologFile) -> compilePrologFile(MainPrologFile)
	                    ; true),
	!.

loadAndCompileImports :-
	retract(importedModule(Mod)),
	loadImportedModule(Mod),
	!,
	loadAndCompileImports.
loadAndCompileImports.

importModule(Mod) :- loadedModule(Mod,_), !. % already loaded
importModule(Mod) :- importedModule(Mod), !. % already in import set
importModule(Mod) :- assertz(importedModule(Mod)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for loading run-time libraries:

loadImportedModule(Mod) :- loadedModule(Mod,_), !. % already loaded
loadImportedModule(Mod) :-
	(findPrologTargetFileInLoadPath(Mod,PrologFile)
	 -> assertz(loadedModule(Mod,PrologFile)),
	    compilePrologFileAndSave(PrologFile)
	  ; write(user_error,'ERROR: Compiled code for Curry module '),
	    write(user_error,Mod),
	    write(user_error,' not found!'),
	    nl(user_error), !, fail).

% start the CPNS demon (used in initialization of library CPNS):
startCPNSD :-
	currentModule('CPNS'),
	% don't automatically start the CPNS demon if the main program is
	% the CPNS demon itself (to avoid infinite recursion)
	!.
startCPNSD :-
	getEnv('PAKCSHOME',TCP),
        appendAtom(TCP,'/cpns/start',CPNSD),
	shellCmd(CPNSD).


