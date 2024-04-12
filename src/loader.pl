%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Operations to load a translated Curry application.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(loader,
	  [loadedModule/2, initializationsInProg/1,
	   currentCostCenters/1, curryModule/1, costCenters/1,
	   initializationsInModule/1,
	   loadAndCompile/3, importModule/1, checkPrologTarget/2]).

:- dynamic loadedModule/2, importedModule/1, currentPrologLoadFile/1,
	   initializationsInProg/1, currentCostCenters/1.

:- use_module(prologbasics).
:- use_module(pakcsversion).
:- use_module(basics).
%:- use_module(compiler). % for generateMainPlFile in interactive mode

currentPrologLoadFile(''). % name of Prolog file loaded by loadAndCompile/3
initializationsInProg(true). % intializations to be done after loading Curry program
currentCostCenters(['']). % list of current cost centers


% set name of current Curry module:
curryModule(ModName) :-
        (loadedModule(ModName,ModFile)
         -> true
          ; currentPrologLoadFile(ModFile)),
        retract(currentModuleFile(_,_)),
        asserta(currentModuleFile(ModName,ModFile)).

% set list of cost centers of the current Curry module:
costCenters(CCs) :-
	retract(currentCostCenters(_)),
	asserta(currentCostCenters(CCs)).

% initialize PAKCS before loading a new Curry application:
initializeBeforeLoad :-
	retractAllFacts(dynamicPredInfo/2),
	retractAllFacts(functiontype/6),
	retractAllFacts(constructortype/7),
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
        retract(currentPrologLoadFile(_)),
        assert(currentPrologLoadFile(PrologFile)),
	initializeBeforeLoad,
	compilePrologFileAndSave(PrologFile),
	currentModuleFile(MainMod,_),
	assertz(loadedModule(MainMod,PrologFile)),
        % add Prelude for interactive REPL if it is not already loaded:
        (loadedModule('Prelude',_) -> AllImports = AddImports
                                    ; AllImports = ['Prelude'|AddImports]),
	map1M(loader:importModule,AllImports),
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
	checkPrologTarget(Mod,PrologFile),
	assertz(loadedModule(Mod,PrologFile)),
	compilePrologFileAndSave(PrologFile).

% Check existence of the Prolog target file for a module:
% If it exists, return name of target file, otherwise fail with error message.
checkPrologTarget(Mod,PrologFile) :-
	findPrologTargetFileInLoadPath(Mod,PrologFile), !.
checkPrologTarget(Mod,_) :-
	write(user_error,'ERROR: Compiled code for Curry module '),
	write(user_error,Mod),
	write(user_error,' not found!'),
	nl(user_error), !, fail.
