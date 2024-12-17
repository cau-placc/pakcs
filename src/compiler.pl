%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transformation of Flat-Curry programs into Prolog programs
%
% main predicate: (initializeCompilerState,) c2p(<progname>)

:- module(compiler,
	  [c2p/1, c2p/2,
	   loadMain/1, generateMainPlFile/2, deleteMainPrologFile/1,
	   forbiddenModules/1, writeClause/1,
	   readProg/5, desugarNewTypesInExp/2,
	   checkProgramHeader/1, deletePrologTarget/1,
	   maxTupleArity/1, tryXml2Fcy/1, varIndex2VarExp/2]).

:- use_module(prologbasics).
:- use_module(pakcsversion).
:- use_module(basics).
:- use_module(version).
:- use_module(pakcsversion).
:- use_module(external). % specification of external functions
:- use_module(readFlcFromFcy).
:- use_module(loader).
:- use_module(readPrimFile).

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

:- dynamic numberOfShares/1,
	   maxTupleArity/1, includePrelude/0,
	   bugInFlcFile/0,
	   allFunctions/1, allConstructors/1, allNewTypeConstructors/1,
	   externalFuncs/1, currentFunction/1,
	   newFunctionCounter/2, newAuxFunctions/1,
	   dynamicPredNames/1, forbiddenModules/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parameters for the transformation:

numberOfShares(0).   % count number of shared variables in a compiled program
includePrelude.      % Uncomment this if prelude should not be included
                     % (in this case, only the main program will be translated
                     %  which is useful for compiler debugging)
hnfTailCallOptim(no).% Should top-level hnf-calls to local functions in right-hand sides
                     % replaced by direct calls to predicates?
completeCases(yes).  % Should case expressions be completed with missing
                     % constructor branches and calls to reportFailure4PAKCS?
failForwarding(yes). % Include forward clause for failures in each case branch?
failCheckFunc(yes).  % Should code for forward failures in each function be
                     % included in hnf clauses?
maxTupleArity(15).   % The maximal arity of tuples

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

allModules([]).  % list of all modules (main + imported)
allFunctions([]).     % list of all defined functions
allConstructors([]).  % list of all constructors (name/arity)
allNewTypeConstructors([]).  % list of the names of all newtype constructors
externalFuncs([]).    % list of all external functions
currentFunction(unknown). % name of currently translated function (for errors)
newFunctionCounter([],0). % counter for generating new function names
newAuxFunctions([]). % list of generated auxiliary functions
                     % (for nested case/or)
dynamicPredNames([]). % list of pred names/file names for dynamic predicates
forbiddenModules([]). % module names that are not allowed (e.g., Unsafe)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for profiling:

% get cost center from function name:
getCostCenterOfName(NameS,CC) :-
	append(_,[123|NCC],NameS), % 123={
	append(CCS,[125|_],NCC), % 125=}
	append([123|CCS],[125],CCD),
	atom_codes(CC,CCD), !.
getCostCenterOfName(_,'').

deleteCostCenterInPrologName(NameCC,Name) :-
	atom_codes(NameCC,NameCCS),
	append(Name0,[39,55,66|NCC],NameCCS), % [39,55,66] = encoding of '{'
	append(_,[39,55,68|Name1],NCC), % [39,55,66] = encoding of '}'
	append(Name0,Name1,NameS),
	atom_codes(Name,NameS), !.
deleteCostCenterInPrologName(Name,Name).

% get all cost centers from list of defined functions:
addCostCenterOfFuncs(CCs,[],CCs).
addCostCenterOfFuncs(CCs,['Func'(Name,_,_,_,_)|Funcs],NewCCs) :-
	getCostCenterOfName(Name,CC),
	(member(CC,CCs) -> CCs1=CCs ; append(CCs,[CC],CCs1)),
	addCostCenterOfFuncs(CCs1,Funcs,NewCCs).

% write a cost center name without decoration:
writeCostCenter(Stream,CC) :-
	atom_codes(CC,CCS),
	append([123|CCNS],[125],CCS),
	atom_codes(CCN,CCNS),
	write(Stream,' '), write(Stream,CCN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reportLiftBug :-
	writeLnErr('> Probably, this is due to a compiler bug in transforming'),
	writeLnErr('> do/let/where expressions. Suggested solution:'),
	writeLnErr('> Simplify dependencies in the do/let/where expression.'),
	setFlcBug.

pleaseReport :-
	writeLnErr('*** Please report this error to the PAKCS developers'),
	writeLnErr('*** if you have used the standard compiler!'),
	setFlcBug.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c2p(Prog) :-
	prog2PrologFile(Prog,PrologFile),
	c2p(Prog,PrologFile).

c2p(Prog,PrologFile) :-
	split2dirbase(Prog,ProgDir,ModName),
	loadPath(ProgDir,LoadPath),
	(includePrelude
	 -> readImportedEntities(LoadPath,[ModName],[],
				 [],[],[],_ImpTypes,_ImpFuncs,_ImpOps)
	  ; readProg(LoadPath,ModName,FlatProg,_,PrimPlFile),
	    generateProg(FlatProg,[],[],[],PrologFile,PrimPlFile)),
	!.
c2p(Prog,PrologFile) :-
	writeErr('ERROR during compilation of program "'),
	writeErr(Prog),
	writeLnErr('"!'),
	deleteFileIfExists(PrologFile).

% read all imported entities (types/functions/operators) of a set of modules:
readImportedEntities(_,[],_,ImpTypes,ImpFuncs,ImpOps,ImpTypes,ImpFuncs,ImpOps) :- !.
readImportedEntities(_,[Imp|_],_,_,_,_,_,_,_) :-
	forbiddenModules(ForbiddenModules),
	member(Imp,ForbiddenModules),
	writeErr('Module "'), writeErr(Imp),
	writeLnErr('" not allowed as import!'),
	setExitCode(3),
	!, fail.
readImportedEntities(LoadPath,[Imp|Imps],ProcessedImps,
		     ImpTypes,ImpFuncs,ImpOps,AllImpTypes,AllImpFuncs,AllImpOps) :-
	member(Imp,ProcessedImps),
	!,
	%write('*** already read: '), write(Imp), nl,
	readImportedEntities(LoadPath,Imps,ProcessedImps,
			     ImpTypes,ImpFuncs,ImpOps,AllImpTypes,AllImpFuncs,AllImpOps).
readImportedEntities(LoadPath,[Imp|Imps],ProcessedImps,
		     ImpTypes,ImpFuncs,ImpOps,AllImpTypes,AllImpFuncs,AllImpOps) :-
	%write('*** read next: '), write(Imp), nl,
        readInterface(LoadPath,Imp,ImpInterface,DirProg),
	ImpInterface = 'Prog'(_,ImportStrings,Types,Funcs,Ops),
	map2M(prologbasics:atomCodes,NewImports,ImportStrings),
	union(Imps,NewImports,AllImports),
	append(ImpTypes,Types,NewImpTypes),
	append(ImpFuncs,Funcs,NewImpFuncs),
	append(ImpOps,Ops,NewImpOps),
	readImportedEntities(LoadPath,AllImports,[Imp|ProcessedImps],
			     NewImpTypes,NewImpFuncs,NewImpOps,
			     AllImpTypes,AllImpFuncs,AllImpOps),
	prog2PrologFile(DirProg,PrologFile),
	(doesPrologTranslationExists(DirProg,PrologFile) -> true
	 ; readProg(LoadPath,Imp,ImpProg,AbsFlatProgFile,PrimPlFile),
	   (verbosityIntermediate
             -> appendAtoms(['Compiling \'',AbsFlatProgFile,'\' into \'',
	                     PrologFile,'\'...'],CompileMsg),
                writeErr(CompileMsg)
              ; true),
	   generateProg(ImpProg,AllImpTypes,AllImpFuncs,AllImpOps,PrologFile,
                        PrimPlFile),
	   (verbosityIntermediate -> writeLnErr('done') ; true)
        ).

doesPrologTranslationExists(DirProg,PrologFile) :-
	checkProgramHeader(PrologFile),
	prog2FlatCurryFile(DirProg,DirProgFlatFile),
	% check fcy file for date since interface might be older:
	fileExistsAndNewer(PrologFile,DirProgFlatFile),
        %writeErrNQ('Compiled Curry program \''),
	%writeErrNQ(PrologFile),
	%writeLnErrNQ('\' is up-to-date.'),
	!.


% initialize the state of the compiler, i.e., clean all tables and
% set all parameters to their default values:
initializeCompilerState :-
	%(retract(includePrelude) -> true ; true),
	%asserta(includePrelude),
	(retract(numberOfShares(_)) -> true ; true),
	asserta(numberOfShares(0)),
	(retract(allFunctions(_)) -> true ; true),
	asserta(allFunctions([])),
	(retract(allConstructors(_)) -> true ; true),
	asserta(allConstructors([])),
	(retract(allNewTypeConstructors(_)) -> true ; true),
	asserta(allNewTypeConstructors([])),
	(retract(externalFuncs(_)) -> true ; true),
	asserta(externalFuncs([])),
	(retract(newAuxFunctions(_)) -> true ; true),
	asserta(newAuxFunctions([])),
	(retract(dynamicPredNames(_)) -> true ; true),
	asserta(dynamicPredNames([])),
	% remove all non-standard infix declarations to avoid syntax errors
	% in compiled program:
	op(0,xfx,(+#)),
	op(0,xfx,(-#)),
	op(0,xfx,(*#)),
	op(0,xfx,(/#)),
	op(0,xfx,(=#)),
	op(0,xfx,(/=#)),
	op(0,xfx,(<#)),
	op(0,xfx,(<=#)),
	op(0,xfx,(>#)),
	op(0,xfx,(>=#)),
	op(0,xfx,(+.)),
	op(0,xfx,(-.)),
	op(0,xfx,(*.)),
	op(0,xfx,(/.)),
	op(0,xfx,(<.)),
	op(0,xfx,(<=.)),
	op(0,xfx,(>.)),
	op(0,xfx,(>=.)).


generateProg(Prog,ImpTypes,ImpFuncs,ImpOps,PrologFile,PrimPlFile) :-
	initializeCompilerState,
	(compileWithDebug
	 -> writeLnErr('...including code for debugging')
	  ; true),
	(compileWithFailPrint
	 -> writeLnErr('...including code for failure printing')
	  ; true),
	ensureDirOfFile(PrologFile),
        (existsFile(PrologFile)
         -> (isWritableFile(PrologFile)
             -> generateProgOnFile(Prog,ImpTypes,ImpFuncs,ImpOps,PrologFile,PrimPlFile)
              ; writeLnErr('WARNING: target file not updated (exists but not writable):'),
                writeLnErr(PrologFile))
          ; tryWriteFile(PrologFile),
            generateProgOnFile(Prog,ImpTypes,ImpFuncs,ImpOps,PrologFile,PrimPlFile)).


generateProgOnFile('Prog'(Mod,Imports,MainTypes,MainFuncs,MainOps),
             ImpTypes,ImpFuncs,ImpOps,PrologFile,PrimPlFile) :-
	tell(PrologFile),
	writePrologHeader,
	writeClause((:- noSingletonWarnings)),
	writeClause((:- noRedefineWarnings)),
	writeClause((:- noDiscontiguousWarnings)),
	nl,
	map1M(compiler:writeLoadImport,Imports), nl,
	writeProg(Mod,Imports,MainTypes,MainFuncs,MainOps,
		  ImpTypes,ImpFuncs,ImpOps),
	nl,
	dynamicPredNames(DynPreds),
	map1M(compiler:writeDynamicInfoClause,DynPreds),
	nl,
	write('%%%%% Number of shared variables: '),
	numberOfShares(SC), write(SC), nl, nl,
        (PrimPlFile = '' -> true
         ; (verbosityIntermediate
            -> writeErr('Adding code of Prolog file: '), writeLnErr(PrimPlFile)
             ; true),
           readPrimFile(PrimPlFile)),
	told, !.
generateProgOnFile(_,_,_,_,PrologFile,_) :-
	told,
	writeLnErr('ERROR during compiling, no program generated!'),
	deleteFileIfExists(PrologFile).

% write head of generated Prolog file:
writePrologHeader :-
	write('%'),
	compilerVersion(CV),
	write(CV), put_code(32),
	prologMajor(Prolog), write(Prolog),
	(compileWithSharing(variable) -> write(' VARIABLESHARING') ; true),
	(compileWithSharing(function) -> write(' FUNCTIONSHARING') ; true),
	nl, nl.

writeLoadImport(ImpModS) :-
	atom_codes(ImpMod,ImpModS),
	writeClause((:- importModule(ImpMod))).

% compute all external libraries to be included for externally defined functions:
getExternalLibraries([],Libs,Libs) :- !.
getExternalLibraries(['Func'(_,_,_,_,'External'(EStr))|Funcs],Libs,AllLibs) :-
	append(LibS,[32|_],EStr), % ext.names have the form "lib ename"
	!,
	atom_codes(Lib,LibS),
        % LibS=[] if the external function is defined without a specification
        % so that its code must be in a standard Prolog file
	((LibS=[] ; member(Lib,Libs)) -> Libs1=Libs ; Libs1=[Lib|Libs]),
	getExternalLibraries(Funcs,Libs1,AllLibs).
getExternalLibraries([_|Funcs],Libs,AllLibs) :-
	getExternalLibraries(Funcs,Libs,AllLibs).

% write directive for necessary inclusion of standard libraries (ports,...):
writeLibraryInclusion(Lib) :-
	writeClause((:- ensure_lib_loaded(Lib))),
	(Lib = prim_dynamic
	 -> writeClause((:- initializationsInModule(prim_dynamic:initializeDynamic)))
	  ; true).


% try translating XML file into .fcy file:
tryXml2Fcy(Prog) :-
	prog2FlatCurryFile(Prog,FlatFile),
	appendAtom(Prog,'.curry',CurryFile),
	appendAtom(Prog,'_flat.xml',XmlFile),
	\+ existsFile(CurryFile),
	\+ existsFile(FlatFile),
	existsFile(XmlFile),
	% translate XML file into .fcy file:
	installDir(PH),
	appendAtoms([PH,'/tools/curry2xml -fcy ',Prog],Cmd),
	%write('Executing: '), write(Cmd), nl,
	shellCmdWithCurryPath(Cmd),
	!.
tryXml2Fcy(_).


% read a FlatCurry interface (or .fcy file if interface does not exist):
readInterface(LoadPath,Prog,FlatInt,DirProgName) :-
	readInterfaceInLoadPath(LoadPath,Prog,FlatInt,DirProgName), !.
readInterface(LoadPath,Prog,_,_) :-
	write('ERROR: Interface or FlatCurry file '), write(Prog),
	write('.[fcy|fint] not found!'), nl,
	write('Current load path: '), write(LoadPath), nl,
	!, fail.

readInterfaceInLoadPath([Dir|Dirs],Prog,FlatInt,DirProgName) :-
	appendAtoms([Dir,'/',Prog],DirProg),
	prog2InterfaceFile(DirProg,DirProgFint),
	(existsFile(DirProgFint)
	 -> DirProgFile=DirProgFint
	  ; prog2FlatCurryFile(DirProg,DirProgFile)),
	(existsFile(DirProgFile)
	 -> readFlcFromFcy(DirProgFile,FlatInt),
	    DirProgName = DirProg,
	    (verbosityIntermediate
	     -> checkForFurtherFcyProgs(Dir,Dirs,Prog)
	      ; true)
	  ; readInterfaceInLoadPath(Dirs,Prog,FlatInt,DirProgName)).

% read a FlatCurry program:
readProg(LoadPath,Prog,FlatProg,AbsFlatProgFile,PrimPlFile) :-
	readProgInLoadPath(LoadPath,Prog,FlatProg,AbsFlatProgFile,PrimPlFile), !.
readProg(LoadPath,Prog,_,_,_) :-
	write('ERROR: FlatCurry file '), write(Prog),
	write('.fcy not found!'), nl,
	write('Current load path: '), write(LoadPath), nl,
	!, fail.

readProgInLoadPath([Dir|Dirs],Prog,FlatProg,AbsFlatProgFile,PrimPlFile) :-
	appendAtoms([Dir,'/',Prog],DirProg),
	prog2FlatCurryFile(DirProg,DirProgFile),
	tryXml2Fcy(DirProg),
	(existsFile(DirProgFile)
	 -> preprocessFcyFile(DirProgFile),
            readFlcFromFcy(DirProgFile,PlainFlatProg),
	    AbsFlatProgFile = DirProgFile,
	    mergeWithPrimitiveSpecs(PlainFlatProg,DirProg,FlatProg),
	    (verbosityIntermediate
	     -> checkForFurtherFcyProgs(Dir,Dirs,Prog) ; true),
            findPrimPrologFile(DirProg,PrimPlFile)
	  ; readProgInLoadPath(Dirs,Prog,FlatProg,AbsFlatProgFile,PrimPlFile)).

% Pre-process the FlatCurry program before loading for compilation.
% Currently, the binding optimizer (replace =:=/2 by ==/2) is applied.
preprocessFcyFile(_) :- pakcsrc(bindingoptimization,no), !.
preprocessFcyFile(FcyFile) :-
    installDir(PH),
    appendAtoms([PH,'/currytools/optimize/BindingOpt'],OptProg),
    existsFile(OptProg), !,
    verbosity(VL),
    (VL<3 -> OptVL=48 ; OptVL is VL+46),
    atom_codes(VParam,[45,118,OptVL,32]), % define -vN
    (pakcsrc(bindingoptimization,fast) -> FParam='-f ' ; FParam=' '),
    appendAtoms(['"',OptProg,'" ',VParam,FParam,'"',FcyFile,'"'],OptCmd),
    (verbosityIntermediate -> write('Executing: '), write(OptCmd),nl ; true),
    (shellCmdWithCurryPath(OptCmd) -> true
     ; writeLnErr('WARNING: binding optimization failed for file:'),
       writeLnErr(FcyFile)).
preprocessFcyFile(FcyFile) :-
    writeLnErr('WARNING: no binding optimization performed for file:'),
    writeLnErr(FcyFile).

checkForFurtherFcyProgs(_,[],_).
checkForFurtherFcyProgs(FcyDir,[Dir|Dirs],Prog) :-
	appendAtoms([Dir,'/',Prog],DirProg),
	prog2FlatCurryFile(DirProg,DirProgFile),
	((existsFile(DirProgFile), \+ equalDirectories(FcyDir,Dir))
	 -> writeErrNQ('WARNING: further FlatCurry file found (but ignored): '),
	    writeLnErrNQ(DirProgFile),
	    checkForFurtherFcyProgs(FcyDir,Dirs,Prog)
	  ; checkForFurtherFcyProgs(FcyDir,Dirs,Prog)).

% are two directories identical?
equalDirectories(Dir1,Dir2) :-
	% a bit complicate solution, but this works also for links
	absolute_file_name(Dir1,AbsDir1),
	absolute_file_name(Dir2,AbsDir2),
	workingDirectory(WDir),
	setWorkingDirectory(AbsDir1), absolute_file_name('.',AbsDir1a),
	setWorkingDirectory(AbsDir2), absolute_file_name('.',AbsDir2a),
	setWorkingDirectory(WDir),
	!,
	AbsDir1a=AbsDir2a.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% try to read primitive function specification file and merge infos into
% external functions:
mergeWithPrimitiveSpecs(PlainFlatProg,DirProg,FlatProg) :-
        prog2DirProg(DirProg,RealDirProg),
	PlainFlatProg = 'Prog'(ModName,_,_,_,_),
	findPrimXmlFile(RealDirProg,PrimXmlFile),
	!,
	(verbosityIntermediate
	 -> writeErr('>>> Reading '),
	    writeErr(PrimXmlFile), writeErr(' ... '),
	    getRunTime(RT1) ; true),
	readPrimitiveXmlSpecs(PrimXmlFile,PrimSpecs),
	(verbosityIntermediate
	 -> getRunTime(RT2),
	    RT is RT2-RT1,
	    writeErr(RT), writeLnErr(' ms.')
	  ; true),
        append(ModName,[46],ModNameDotS), atom_codes(ModNameDot,ModNameDotS),
	map2partialM(compiler:addModuleName2PrimSpecs(ModNameDot),PrimSpecs,QPrimSpecs),
	addPrimitiveSpecs2FlatProg(PlainFlatProg,QPrimSpecs,FlatProg).
mergeWithPrimitiveSpecs(PlainFlatProg,_,FlatProg) :-
	addPrimitiveSpecs2FlatProg(PlainFlatProg,[],FlatProg).

findPrimXmlFile(RealDirProg,PrimXmlFile) :-
        appendAtom(RealDirProg,'.pakcs',PrimXmlFile),
	existsFile(PrimXmlFile), !.
findPrimXmlFile(RealDirProg,PrimXmlFile) :-
        appendAtom(RealDirProg,'.prim_c2p',PrimXmlFile),
	existsFile(PrimXmlFile), !.

% look for a Prolog file (with suffix .pakcs.pl) containing code
% for primitive operations to be added to the generated Prolog file
findPrimPrologFile(DirProg,PrimPrologFile) :-
        prog2DirProg(DirProg,RealDirProg),
        appendAtom(RealDirProg,'.pakcs.pl',PrimPrologFile),
	existsFile(PrimPrologFile),
        !.
findPrimPrologFile(_,'').

addModuleName2PrimSpecs(ModNameDot,primitive(F,N,Mod,Entry),primitive(QF,N,Mod,Entry)) :-
	ModNameDot='prelude.', !, appendAtom('Prelude.',F,QF).
addModuleName2PrimSpecs(ModNameDot,primitive(F,N,Mod,Entry),primitive(QF,N,Mod,Entry)) :-
	appendAtom(ModNameDot,F,QF).
addModuleName2PrimSpecs(ModNameDot,ignore(F,N),ignore(QF,N)) :-
	ModNameDot='prelude.', !, appendAtom('Prelude.',F,QF).
addModuleName2PrimSpecs(ModNameDot,ignore(F,N),ignore(QF,N)) :-
	appendAtom(ModNameDot,F,QF).

addPrimitiveSpecs2FlatProg('Prog'(ModName,Imps,Types,Funcs,Ops),PrimSpecs,
                           'Prog'(ModName,Imps,Types,ModFuncs,Ops)) :-
        addPrimitiveSpecs2Funcs(PrimSpecs,Funcs,ModFuncs).

% format a primitive specification (e.g., for error messages):
writePrimSpec(primitive(F,Arity,ELib,_EName)) :-
	writeErr(F), writeErr('/'), writeErr(Arity), writeErr(' in library "'),
	writeErr(ELib), writeLnErr('"').
writePrimSpec(ignore(F,Arity)) :-
	writeErr(F), writeErr('/'), writeLnErr(Arity).

% process list of function declarations w.r.t. primitive specification:
addPrimitiveSpecs2Funcs([],[],[]) :- !.
addPrimitiveSpecs2Funcs(PrimSpecs,[],[]) :-
	writeErrNQ('WARNING: specifications of primitive functions '),
	writeLnErrNQ('without source code found:'),
	(verbosityNotQuiet -> map1M(compiler:writePrimSpec,PrimSpecs) ; true),
        !.
addPrimitiveSpecs2Funcs(PrimSpecs,['Func'(Name,Arity,_,_,_)|Funcs],MFuncs) :-
	flatName2Atom(Name,F),
	deleteCostCenterInPrologName(F,FWOCC),
	deleteFirst(ignore(FWOCC,EArity),PrimSpecs,DPrimSpecs), !,
	checkArityConsistency(F,Arity,EArity),
	addPrimitiveSpecs2Funcs(DPrimSpecs,Funcs,MFuncs).
addPrimitiveSpecs2Funcs(PrimSpecs,
			['Func'(Name,Arity,Vis,Type,_)|Funcs],
     	                ['Func'(Name,Arity,Vis,Type,'External'(EStr))|MFuncs]) :-
	flatName2Atom(Name,F),
	deleteCostCenterInPrologName(F,FWOCC),
	deleteFirst(primitive(FWOCC,EArity,EMod,EName),PrimSpecs,DPrimSpecs), !,
	checkArityConsistency(F,Arity,EArity),
	atom_codes(EMod,EModS),
	atom_codes(EName,ENameS),
	append(EModS,[32|ENameS],EStr),
	addPrimitiveSpecs2Funcs(DPrimSpecs,Funcs,MFuncs).
addPrimitiveSpecs2Funcs(PrimSpecs,
			['Func'(Name,Arity,Vis,Type,'External'(EStr))|Funcs],
			['Func'(Name,Arity,Vis,Type,'External'(EStr))|MFuncs]) :-
	append(_,[32|_],EStr), % has the form of an already known primitive
	!,
	addPrimitiveSpecs2Funcs(PrimSpecs,Funcs,MFuncs).
addPrimitiveSpecs2Funcs(PrimSpecs,
			['Func'(Name,Arity,Vis,Type,'External'(_))|Funcs],
     	                ['Func'(Name,Arity,Vis,Type,'External'(EStr))|MFuncs]) :-
	flatName2Atom(Name,F),
	deleteCostCenterInPrologName(F,PWOCC), decodePrologName(PWOCC,FWOCC),
        appendAtom(' ',FWOCC,EName), atom_codes(EName,EStr), !,
        (verbosityIntermediate
         -> writeErr('*** No specification of primitive function '),
            writeErr(FWOCC), writeErr('/'), writeLnErr(Arity),
            writeErr('*** Using external name:'), writeLnErr(EName)
          ; true),
     	addPrimitiveSpecs2Funcs(PrimSpecs,Funcs,MFuncs).
   
%addPrimitiveSpecs2Funcs(_,['Func'(Name,Arity,_,_,'External'(_))|_],_) :-
%	flatName2Atom(Name,F),
%	deleteCostCenterInPrologName(F,PWOCC), decodePrologName(PWOCC,FWOCC),
%	writeErr('ERROR: specification of primitive function '),
%	writeErr(FWOCC), writeErr('/'), writeErr(Arity),
%	writeLnErr(' not found!'), !, setFlcBug, fail.
addPrimitiveSpecs2Funcs(PrimSpecs,[Func|Funcs],[Func|MFuncs]) :-
	addPrimitiveSpecs2Funcs(PrimSpecs,Funcs,MFuncs).

checkArityConsistency(FName,FArity,EArity) :-
	(FArity=EArity -> true
         ; writeErr('ERROR in specification of primitive function '),
	   writeErr(FName), writeLnErr(' : inconsistent arities!'),
	   setFlcBug).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main compiler for individual FlatCurry modules:

writeProg(Mod,Imports,MainTypes,MainFuncsO,MainOps,ImpTypes,ImpFuncs,ImpOps) :-
	atom_codes(ModName,Mod),
	append(MainTypes,ImpTypes,AllTypes),
	getConstructors(AllTypes,ConsList),
	retract(allConstructors(_)), asserta(allConstructors(ConsList)),
	getNewTypeConstructors(AllTypes,NewConsList),
	retract(allNewTypeConstructors(_)),
        asserta(allNewTypeConstructors(NewConsList)),
	(map2M(compiler:desugarNewTypes,MainFuncsO,MainFuncs)
	 -> true
	  ; writeErr('INTERNAL COMPILER ERROR in newtype desugarer'),
	    writeErr(' IN MODULE '), writeLnErr(ModName), fail),
        
	append(MainOps,ImpOps,AllOps),
	writeClause((:- curryModule(ModName))), nl,
	getExternalLibraries(MainFuncs,[],ExtLibs),
	map1M(compiler:writeLibraryInclusion,ExtLibs), nl,
	% redundant checking, could be omitted if front end is correct:
	(retract(bugInFlcFile) -> true ; true),
	map1M(compiler:check_flcFunction,MainFuncs),
	\+ bugInFlcFile,
	(plprofiling(yes) -> addCostCenterOfFuncs([''],MainFuncs,CCs)
                           ; CCs=['']),
	CCs=[_|NewCCs],
	(NewCCs=[] -> true
	 ; writeErr('...including code for profiling cost centers:'),
	   map1partialM(compiler:writeCostCenter(user_error),NewCCs),
	   nlErr),
	computeAllExternalFunctions(MainFuncs,ExtFuncs),
	(retract(externalFuncs(_)) -> true ; true),
	asserta(externalFuncs(ExtFuncs)),
	(map2M(compiler:elimNestedOrCases,MainFuncs,CodeFuncsWOnestedcase)
	 -> true
	  ; writeLnErr('INTERNAL COMPILER ERROR in or/case lifter!'),
	    fail),
	newAuxFunctions(TmpAuxFuns), rev(TmpAuxFuns,AuxFuns),
	append(CodeFuncsWOnestedcase,AuxFuns,CodeFuncsOIS),
	(completeCases(no) -> CodeFuncsOISTotal=CodeFuncsOIS
	 ; (map2partialM(compiler:completeCaseExpressions(AllTypes),
		         CodeFuncsOIS,CodeFuncsOISTotal)
 	    -> true
	     ; writeErr('INTERNAL COMPILER ERROR in case branch completion'),
               writeErr(' IN MODULE '), writeLnErr(ModName), fail)),
	append(CodeFuncsOISTotal,ImpFuncs,AllFuncs),
	map2M(compiler:flcFunc2FA,AllFuncs,AllFuncsArities),
	retract(allFunctions(_)), asserta(allFunctions(AllFuncsArities)),
	write('%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%'), nl,
	writeClause((:- multifile functiontype/6)),
	writeClause((:- dynamic functiontype/6)),
	map1partialM(compiler:writeFTypeClause(Mod,ExtFuncs,AllOps),
                     CodeFuncsOISTotal), nl,
	write('%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%'), nl,
	writeClause((:- multifile constructortype/7)),
	writeClause((:- dynamic constructortype/7)),
	(member("Prelude",Imports) -> true
	 ; % generate type clause for partcall in the prelude:
	   writeClause(constructortype(partcall,partcall,3,partcall,0,
                                       notype,[]))),
	map1partialM(compiler:writeDTypeClause(Mod),MainTypes), nl,

	write('%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%'), nl,
	map1M(compiler:writeFunc,CodeFuncsOISTotal),

	writeClause((:- costCenters(CCs))), nl,
	!,
	\+ bugInFlcFile,
	nl.

writeGenericClauses(CCs) :-
	write('%%%%%%%%%%%% clauses for generic operations %%%%%%%%%%%%%%%%%%%'), nl,
	map1M(compiler:transConstrEq,CCs),
	%map1M(compiler:transBoolEq,CCs), % no longer necessary due to class Eq
	map1M(compiler:transNf,CCs),
	(compileWithSharing(variable) -> map1M(compiler:transpropshar,CCs) ; true),
	(compileWithSharing(function) -> map1M(compiler:genMakeFunctionShare,CCs) ; true),
	transDeref(CCs).

% "Linker" for separate compilation:
% create a file containing hnf clauses for all currently loaded functions
% and the generic clauses to define =:=, ==, nf, etc:
generateMainPlFile(_PrologFile,MainFile) :-
	currentCostCenters(CCs),
	mainPrologFileName(MainFile),
	tell(MainFile),
	writeClause((:- noSingletonWarnings)),
	writeClause((:- noDiscontiguousWarnings)),
	nl,
	writeClause((retractClause(LHS,RHS) :- retract((LHS :- RHS)))),
	nl,
	write('%%%%%%%%%%%% hnf clauses %%%%%%%%%%%%%%%%%%%'), nl,
	transHnfCurrent(CCs),
	writeGenericClauses(CCs),
	told.

% create main file, load it and delete it if not in profiling mode:
loadMain(PrologFile) :-
	generateMainPlFile(PrologFile,MainPrologFile),
	compilePrologFile(MainPrologFile),
	deleteMainPrologFile(MainPrologFile).

deleteMainPrologFile(MainPrologFile) :-
	((plprofiling(yes) ; pakcsrc(keepfiles,yes))
	 -> true % prog.pl.main needed later for PlProfileData.getHnfDefinitions
          ; % delete MainPrologFile:
	    deleteFile(MainPrologFile)).
	
% a call to this predicate sets the global error flat so that no compiled
% program is produced:
setFlcBug :- bugInFlcFile -> true ; assertz(bugInFlcFile).

% check for deprecated functions and print warning:
checkForDeprecatedFunction(Name) :-
	member(Name,['Term.readTerm','Term.readQTerm','Term.showTerm',
		     'Term.showQTerm',
		     'ReadShowTerm.readTerm','ReadShowTerm.readsTerm',
		     'System.getDate','System.showDate',
		     'FlatTools.writeFLC','FlatCurryTools.writeFLC',
		     'FileGoodies.findFileInPath',
		     'HTML.Form','HTML.HtmlElem',
		     'HTML.showHtmlDoc','HTML.showHtmlDocCSS',
		     'Ports.openSocketConnectPort']), !,
	writeErrNQ('WARNING: do not use deprecated function "'),
	writeErrNQ(Name),
	writeLnErrNQ('".').
checkForDeprecatedFunction(_).

% check for ocurrences of constructors of Dynamic or Global types
% and print error in this case:
checkForIllegalOperationUsage(Name) :-
	member(Name,['Dynamic.dynamic','Dynamic.persistent',
		     'Global.global','GlobalVariable.gvar']),
	currentFunction(QFunc),
	!,
	writeErr('ERROR in "'),
	writeErr(QFunc),
	writeErr('": Function "'),
	writeErr(Name),
	writeLnErr('" is not allowed in this context!'),
	setFlcBug.
checkForIllegalOperationUsage(_).

% check for maximal arity of tuples and print warning:
checkForTupleArity([40,44|Name]) :-
	length([44|Name],TA),
	maxTupleArity(MA),
	TA>MA, !,
	currentFunction(QFunc),
	writeErr('ERROR in "'),
	writeErr(QFunc),
	writeErr('": arity of '), writeErr(TA),
	writeLnErr('-tupel too large.'),
	writeErr('The maximal arity of tuples is '), writeErr(MA),
	writeLnErr('.'),
	writeLnErr('This can only be changed by reconfiguring your installation'),
	setFlcBug.
checkForTupleArity(_).


% compute all external functions in this program:
computeAllExternalFunctions([],[]).
computeAllExternalFunctions(['Func'(Name,Arity,_,_Type,'External'(EStr))|Funcs],
			    [(F/Arity,EStr)|EFs]) :- !,
	flatName2Atom(Name,F),
	computeAllExternalFunctions(Funcs,EFs).
computeAllExternalFunctions([_|Funcs],EFs) :-
	computeAllExternalFunctions(Funcs,EFs).


% transform FlatCurry representation of a function into name/arity pair
% where the arity is the arity of the final (eta-expanded) function:
flcFunc2FA('Func'(Name,Arity,_,_Type,_),F/Arity) :- flatName2Atom(Name,F).

% get all constructors (in the form Con/Arity) contained in a list
% of type declarations.
getConstructors([],Cs) :-
	TupleCons = ['Prelude.()'/0,'Prelude.(,)'/2,'Prelude.(,,)'/3,
		     'Prelude.(,,,)'/4,'Prelude.(,,,,)'/5],
	(includePrelude
          -> Cs=[partcall/3,'$stream'/1] % all standard constructors defined in prelude
           ; Cs=[partcall/3,'Prelude.True'/0,'Prelude.False'/0,
		 []/0,'.'/2|TupleCons]).
getConstructors(['Type'(_,_,_,DataCons)|Types],AllCons) :-
	getDataCons(DataCons,DCs),
	getConstructors(Types,TCs),
	append(DCs,TCs,AllCons).
getConstructors(['TypeNew'(_,_,_,'NewCons'(Name,_,_))|Types],AllCons) :-
	flatName2Atom(Name,Con),
	getConstructors(Types,TCs),
	append([Con/1],TCs,AllCons).

getDataCons([],[]).
getDataCons(['Cons'(Name,Arity,_,_)|DataCons],[Con/Arity|DCs]) :-
	flatName2Atom(Name,Con),
	getDataCons(DataCons,DCs).

% get the names of all newtype constructors contained in a list
% of type declarations.
getNewTypeConstructors([],[]).
getNewTypeConstructors(['TypeNew'(_,_,_,'NewCons'(Name,_,_))|Types],
                       [Con|TCs]) :-
	flatName2Atom(Name,Con),
	getNewTypeConstructors(Types,TCs).
getNewTypeConstructors(['Type'(_,_,_,_)|Types],TCs) :-
	getNewTypeConstructors(Types,TCs).

% compute the arity of a constructor from the list of all stored constructors
% (fail if arity not found):
getConsArity(Cons,Arity) :-
	allConstructors(CAs), getArity(CAs,Cons,Arity).

% compute the arity of a function from the list of all stored functions:
% (error message and fail if arity not found):
getFuncArity(Func,Arity) :-
	allFunctions(FAs), getArity(FAs,Func,Arity), !.
getFuncArity(Func,Arity) :- % special case for tuples:
	atom_codes(Func,[40,44|Commas]),
	length([44|Commas],Arity), !.
getFuncArity(Func,_) :-
	writeErr('ERROR (illegal FlatCurry file?): Arity of function '),
	writeErr(Func),
	writeErr(' (used in function '),
	currentFunction(CF), writeErr(CF), writeErr(')'),
	writeLnErr(' not found!'), !, fail.

getArity([Name/Arity|_],Name,Arity) :- !.
getArity([_|FAs],Name,Arity) :- getArity(FAs,Name,Arity).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pre-transformation of defined functions:
% possibly add missing arguments in case of higher-order equations,
% i.e., in this implementation all functions are defined by first-order
% rules

% compute argument number (of uncurryfied function) from the functions type:
argnum('FuncType'(_,T2),N) :-
	argnum(T2,N2), N is N2+1.
argnum('TVar'(_),0).
argnum('TCons'(TC,_),N) :- TC="Prelude.IO" -> N=1 ; N=0.

% compute maximum variable index in a term (list):
maxVarIndex('Var'(V),V).
maxVarIndex('Lit'(_),-1).
maxVarIndex('Comb'(_,_,Es),M) :- map2M(compiler:maxVarIndex,Es,Ms), maxList(Ms,M).
maxVarIndex('Free'(Vs,E),M) :-
	maxList(Vs,Ms), maxVarIndex(E,ME), max(Ms,ME,M).
maxVarIndex('Or'(E1,E2),M) :-
	maxVarIndex(E1,M1), maxVarIndex(E2,M2), max(M1,M2,M).
maxVarIndex('Case'(_,E,Cs),M) :-
	maxVarIndex(E,ME), map2M(compiler:maxVarCase,Cs,Ms), maxList(Ms,MC),
	max(MC,ME,M).

maxVarCase('Branch'('Pattern'(_,Vs),E),M) :-
	maxList(Vs,Ms), maxVarIndex(E,ME), max(Ms,ME,M).
maxVarCase('Branch'('LPattern'(_),E),M) :-
	maxVarIndex(E,M).

% maximum of a list of naturals:
maxList([],-1).
maxList([X|Xs],M) :- maxList(Xs,Ms), max(X,Ms,M).

max(X,Y,Z) :- X>Y -> Z=X ; Z=Y.

% for simplicity, we generate negative variable indices here:
genVarIndices(_,0,[]).
genVarIndices(Max,N,[I|Is]) :-
	N>0, I is Max+1, N1 is N-1,
	genVarIndices(I,N1,Is).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% complete case branches with fail branches for missing constructors:
completeCaseExpressions(Types,
			'Func'(Name,Arity,Vis,Type,'Rule'(Args,RHS)),
			'Func'(Name,Arity,Vis,Type,'Rule'(Args,NewRHS))) :-
	!,
	%atom_codes(A,Name), writeLnErr(completeCase(A)),
        %writeLnErr(RHS),
        %allNewTypeConstructors(NTS), writeLnErr(NTS),
	completeCaseInExp(Name,Types,RHS,NewRHS).
completeCaseExpressions(_,Func,Func).

completeCaseInExp(_,_,'Var'(V),'Var'(V)).
completeCaseInExp(_,_,'Lit'(V),'Lit'(V)).
completeCaseInExp(FName,Types,'Comb'(CT,CF,Args),'Comb'(CT,CF,NewArgs)) :-
	map2partialM(compiler:completeCaseInExp(FName,Types),Args,NewArgs).
completeCaseInExp(FName,Types,'Free'(Vs,E),'Free'(Vs,NE)) :-
	completeCaseInExp(FName,Types,E,NE).
completeCaseInExp(_,_,'Case'(CT,C,Cases),'Case'(CT,C,Cases)) :-
        % keep unchanged if it is a newtype constructor:
        Cases = ['Branch'('Pattern'(Cons,_),_)],
        atom_codes(ACons,Cons),
        allNewTypeConstructors(NTCs), member(ACons,NTCs), !.
completeCaseInExp(FName,Types,'Case'(CT,C,Cases),'Case'(CT,C,NewCases)) :- !,
	getMissingBranchConstructors(Types,Cases,Cs),
	(Cs=[] -> MissingCases=[]
	        ; map2partialM(compiler:generateMissingBranch(FName),
		               Cs,MissingCases)),
	map2partialM(compiler:completeCaseInBranch(FName,Types,CT),
                     Cases,NCases),
	append(NCases,MissingCases,NewCases).
completeCaseInExp(FName,Types,'Or'(E1,E2),'Or'(NE1,NE2)) :- !,
	completeCaseInExp(FName,Types,E1,NE1),
	completeCaseInExp(FName,Types,E2,NE2).
completeCaseInExp(FName,Types,'Let'(Bs,E),'Let'(NBs,NE)) :- !,
	map2partialM(compiler:completeCaseInBinding(FName,Types),Bs,NBs),
	completeCaseInExp(FName,Types,E,NE).

completeCaseInBinding(FName,Types,'Prelude.(,)'(V,E),'Prelude.(,)'(V,NE)) :-
	completeCaseInExp(FName,Types,E,NE).

completeCaseInBranch(FName,_,_,
		     'Branch'('Pattern'(Cons,Args),
			      'Comb'('FuncCall',FailedName,[])),
		     'Branch'('Pattern'(Cons,Args),FailureExp)) :-
	atom_codes('Prelude.failed',FailedName), !,
	% change Prelude.failed branch which might be inserted by the front-end
	% in case expressions:
	atom_codes('reportFailure4PAKCS',FailFuncName),
	atom_codes('Prelude.[]',EmptyList),
	atom_codes('Prelude.:',ConsList),
	FNameExp = 'Comb'('FuncCall',FName,[]),
	map2M(compiler:varIndex2VarExp,Args,ArgExps),
	ConsExp = 'Comb'('ConsCall',Cons,ArgExps),
	ArgExp = 'Comb'('ConsCall',ConsList,
                        [ConsExp,'Comb'('ConsCall',EmptyList,[])]),
	FailureExp = 'Comb'('FuncCall',FailFuncName,[FNameExp,ArgExp]).
completeCaseInBranch(FName,Types,_,'Branch'(Pat,Exp),'Branch'(Pat,NewExp)) :-
	completeCaseInExp(FName,Types,Exp,NewExp).

generateMissingBranch(FName,Cons/Arity,'Branch'('Pattern'(Cons,Args),Failed)) :-
        writeErr('INFO: function "'), atom_codes(FN,FName), writeErr(FN),
        writeErr('" add failed branch for constructor "'),
        atom_codes(ConsA,Cons), writeErr(ConsA), writeLnErr('"'),
	length(Args,Arity), numberVarList(100,Args),
	atom_codes('reportFailure4PAKCS',FailFuncName),
	atom_codes('Prelude.[]',EmptyList),
	atom_codes('Prelude.:',ConsList),
	FNameExp = 'Comb'('FuncCall',FName,[]),
	map2M(compiler:varIndex2VarExp,Args,ArgExps),
	ConsExp = 'Comb'('ConsCall',Cons,ArgExps),
	ArgExp = 'Comb'('ConsCall',ConsList,[ConsExp,
					     'Comb'('ConsCall',EmptyList,[])]),
	Failed = 'Comb'('FuncCall',FailFuncName,[FNameExp,ArgExp]).

numberVarList(_,[]).
numberVarList(N,[N|Vs]) :- N1 is N+1, numberVarList(N1,Vs).


getMissingBranchConstructors(Type,Branches,MissingCons) :-
	getBranchConstructors(Branches,BCons),
	getMissingConstructors(Type,BCons,MissingCons).

getMissingConstructors(_,[],[]). % nothing missing in case of LPatterns
getMissingConstructors(Types,[C|Cs],MissingCons) :-
	getConsOfType(Types,C,TypeCons),
	diff(TypeCons,[C|Cs],MissingCons).

getConsOfType(['Type'(_,_,_,TypeCons)|_],C/A,Cons) :-
	member('Cons'(C,A,_,_),TypeCons), !,
	map2M(compiler:getDataConsNameArity,TypeCons,Cons).
getConsOfType([_|Types],C,TypeCons) :- getConsOfType(Types,C,TypeCons).

getDataConsNameArity('Cons'(C,A,_,_),C/A).

getBranchConstructors([],[]).
getBranchConstructors(['Branch'('Pattern'(Cons,Args),_)|Bs],[Cons/A|Cs]) :-
	length(Args,A),
	getBranchConstructors(Bs,Cs).
getBranchConstructors(['Branch'('LPattern'(_),_)|Bs],Cs) :-
	getBranchConstructors(Bs,Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% desugar newtype constructors, i.e.:
% - replace (NEWTYPECONS e) by `e` in constructed expressions
% - replace (NEWTYPECONS) by `Prelude.id` in constructed expressions
% - replace `(f)case x of { NEWTYPECONS y -> e}` by `{y |-> x}(e)`

desugarNewTypes('Func'(Name,Arity,Vis,Type,'Rule'(Args,RHS)),
                'Func'(Name,Arity,Vis,Type,'Rule'(Args,NewRHS))) :-
	!,
	desugarNewTypesInExp(RHS,NewRHS).
desugarNewTypes('Func'(Name,Arity,Vis,Type,'External'(EName)),
		'Func'(Name,Arity,Vis,Type,'External'(EName))) :- !.
desugarNewTypes(Arg,Arg) :-
	writeErr('ERROR: Illegal argument in desugarNewTypes: '),
	writeLnErr(Arg).

% remove newtype constructors in expressions:
desugarNewTypesInExp('Case'(_,CE,Cases),NCase) :-
        Cases = ['Branch'('Pattern'(Cons,[V]),BE)],
        atom_codes(ACons,Cons),
        allNewTypeConstructors(NTCs), member(ACons,NTCs), !,
        replaceVarInExp(V,CE,BE,NBE),
        desugarNewTypesInExp(NBE,NCase).
desugarNewTypesInExp('Case'(CT,CE,Cases),'Case'(CT,NCE,NewCases)) :- !,
	desugarNewTypesInExp(CE,NCE),
	desugarNewTypesInBranches(Cases,NewCases).
desugarNewTypesInExp('Comb'('ConsCall',Cons,[Arg]),Arg) :-
        atom_codes(ACons,Cons),
        allNewTypeConstructors(NTCs), member(ACons,NTCs), !.
desugarNewTypesInExp('Comb'('ConsPartCall'(1),Cons,[]),NE) :-
        atom_codes(ACons,Cons),
        allNewTypeConstructors(NTCs), member(ACons,NTCs), !,
        atom_codes('Prelude.id',PreId),
        NE = 'Comb'('FuncPartCall'(1),PreId,[]).
desugarNewTypesInExp('Comb'(CT,Cons,Args),_) :-
        atom_codes(ACons,Cons),
        allNewTypeConstructors(NTCs), member(ACons,NTCs), !,
        writeErr('ERROR: Illegal use of newtype cons in desugarNewTypes: '),
        writeLnErr('Comb'(CT,Cons,Args)), fail.
             
desugarNewTypesInExp('Comb'(CT,CF,Args),'Comb'(CT,CF,NewArgs)) :-
	map2M(compiler:desugarNewTypesInExp,Args,NewArgs).
desugarNewTypesInExp('Var'(V),'Var'(V)).
desugarNewTypesInExp('Lit'(L),'Lit'(L)).
desugarNewTypesInExp('Or'(E1,E2),'Or'(NE1,NE2)) :- !,
	desugarNewTypesInExp(E1,NE1),
	desugarNewTypesInExp(E2,NE2).
desugarNewTypesInExp('Free'(Vs,E),'Free'(Vs,NE)) :-
	desugarNewTypesInExp(E,NE).
desugarNewTypesInExp('Let'(Bindings,E),'Let'(NBindings,NE)) :-
	map2M(compiler:desugarNewTypesInBinding,Bindings,NBindings),
	desugarNewTypesInExp(E,NE).

desugarNewTypesInBinding('Prelude.(,)'(V,E),'Prelude.(,)'(V,NE)) :-
	desugarNewTypesInExp(E,NE).

desugarNewTypesInBranches([],[]).
desugarNewTypesInBranches(['Branch'(Pat,Exp)|Cs],
	                  ['Branch'(Pat,NewExp)|NCs]) :-
	desugarNewTypesInExp(Exp,NewExp),
	desugarNewTypesInBranches(Cs,NCs).

% replace a variable by another expression in an expression:
replaceVarInExp(X,Y,'Case'(CT,CE,Cases),'Case'(CT,NCE,NewCases)) :- !,
	replaceVarInExp(X,Y,CE,NCE),
	replaceVarInBranches(X,Y,Cases,NewCases).
replaceVarInExp(X,Y,'Comb'(CT,CF,Args),'Comb'(CT,CF,NewArgs)) :-
	map2partialM(compiler:replaceVarInExp(X,Y),Args,NewArgs).
replaceVarInExp(X,Y,'Var'(V),NE) :- X = V -> NE = Y ; NE = 'Var'(V).
replaceVarInExp(_,_,'Lit'(L),'Lit'(L)).
replaceVarInExp(X,Y,'Or'(E1,E2),'Or'(NE1,NE2)) :- !,
	replaceVarInExp(X,Y,E1,NE1),
	replaceVarInExp(X,Y,E2,NE2).
replaceVarInExp(X,Y,'Free'(Vs,E),'Free'(Vs,NE)) :-
	replaceVarInExp(X,Y,E,NE).
replaceVarInExp(X,Y,'Let'(Bindings,E),'Let'(NBindings,NE)) :-
	map2partialM(compiler:replaceVarInBinding(X,Y),Bindings,NBindings),
	replaceVarInExp(X,Y,E,NE).

replaceVarInBinding(X,Y,'Prelude.(,)'(V,E),'Prelude.(,)'(V,NE)) :-
	replaceVarInExp(X,Y,E,NE).

replaceVarInBranches(_,_,[],[]).
replaceVarInBranches(X,Y,['Branch'(Pat,Exp)|Cs],['Branch'(Pat,NewExp)|NCs]) :-
	replaceVarInExp(X,Y,Exp,NewExp),
	replaceVarInBranches(X,Y,Cs,NCs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eliminate nested or/case expressions in right-hand sides of function
% definitions:
elimNestedOrCases('Func'(Name,Arity,Vis,Type,'Rule'(Args,RHS)),
		  'Func'(Name,Arity,Vis,Type,'Rule'(Args,NewRHS))) :-
	!,
	(retract(newFunctionCounter(_,_)) -> true ; true),
	asserta(newFunctionCounter(Name,0)),
	elimNestedCasesInRHS(RHS,NewRHS).
elimNestedOrCases('Func'(Name,Arity,Vis,Type,'External'(EName)),
		  'Func'(Name,Arity,Vis,Type,'External'(EName))) :- !.
elimNestedOrCases(Arg,Arg) :-
	writeErr('ERROR: Illegal argument in elimNestedOrCases: '),
	writeLnErr(Arg).

% remove nested cases in right-hand side:
elimNestedCasesInRHS('Case'(CT,CE,Cases),'Case'(CT,NCE,NewCases)) :- !,
	elimCasesInExp(CE,NCE),
	elimNestedCasesInBranches(Cases,NewCases).
elimNestedCasesInRHS('Or'(E1,E2),'Or'(NE1,NE2)) :- !,
	elimNestedCasesInRHS(E1,NE1),
	elimNestedCasesInRHS(E2,NE2).
elimNestedCasesInRHS(Exp,NewExp) :- elimCasesInExp(Exp,NewExp).

elimNestedCasesInBranches([],[]).
elimNestedCasesInBranches(['Branch'(Pat,Exp)|Cs],
	                  ['Branch'(Pat,NewExp)|NCs]) :-
	elimNestedCasesInRHS(Exp,NewExp),
	elimNestedCasesInBranches(Cs,NCs).


elimCasesInExp('Var'(V),'Var'(V)).
elimCasesInExp('Lit'(L),'Lit'(L)).
elimCasesInExp('Comb'(CT,CF,Args),'Comb'(CT,CF,NewArgs)) :-
	map2M(compiler:elimCasesInExp,Args,NewArgs).
elimCasesInExp('Free'(Vs,E),'Free'(Vs,NE)) :-
	elimCasesInExp(E,NE).
elimCasesInExp('Or'(E1,E2),'Comb'('FuncCall',FNew,FVExps)) :-
	% elimCasesInExp(E1,NE1), no need to do that.
	% elimCasesInExp(E2,NE2),
	freeVarsInExp('Or'(E1,E2),FVs),
	map2M(compiler:varIndex2VarExp,FVs,FVExps),
	genAuxFuncName(FNew),
	length(FVs,FNArity),
        elimNestedOrCases('Func'(FNew,FNArity,'Private',
				 'TVar'(0), % here I am too lazy
			                    % to compute the correct type
				 'Rule'(FVs,'Or'(E1,E2))),ElimF),
        addAuxFunction(ElimF).
elimCasesInExp('Case'(CT,CE,Branches),
	       'Comb'('FuncCall',FNew,[NCE|FVExps])) :-
	elimCasesInExp(CE,NCE),
	% elimCasesInBranches(Branches,NewBranches),
	freeVarsInBranches(Branches,FVs),
	map2M(compiler:varIndex2VarExp,FVs,FVExps),
	genAuxFuncName(FNew),
	length(FVs,FVLength),
	FNArity is FVLength+1,
	newVarIndex(0,FVs,CVI),
	elimNestedOrCases('Func'(FNew,FNArity,'Private',
				 'TVar'(0), % here I am too lazy
			                    % to compute the correct type
				 'Rule'([CVI|FVs],'Case'(CT,'Var'(CVI),Branches))),
                         ElimF),
        addAuxFunction(ElimF).
% preliminary solution (hack?): transform (Let [(x1,e1),...,(xn,en)] e) into
% (let x1,...,xn free in (letrec x1 e1 &>...&> letrec xn en &> e))
elimCasesInExp('Let'(Bindings,E),'Free'(Vs,BindingList)) :-
	map2M(compiler:bindingVar,Bindings,Vs),
	map2M(compiler:elimCasesInBinding,Bindings,NBindings),
	elimCasesInExp(E,NE),
	letbindings2constr(NBindings,NE,BindingList).

bindingVar('Prelude.(,)'(V,_),V).

letbindings2constr([],Exp,Exp).
letbindings2constr(['Prelude.(,)'(X,E)|Bs],Exp,
		   'Comb'('FuncCall',Cond,['Comb'('FuncCall',LetRec,['Var'(X),E]),BsL])) :-
	atom_codes('letrec4PAKCS',LetRec),
        atom_codes('Prelude.cond',Cond),
        letbindings2constr(Bs,Exp,BsL).
	
elimCasesInBinding('Prelude.(,)'(V,E),'Prelude.(,)'(V,NE)) :-
	elimCasesInExp(E,NE).

elimCasesInBranches([],[]).
elimCasesInBranches(['Branch'(Pat,Exp)|Cs],
	            ['Branch'(Pat,NewExp)|NCs]) :-
	elimCasesInExp(Exp,NewExp),
	elimCasesInBranches(Cs,NCs).

% find a new variable index not occurring in a list of variables:
newVarIndex(Index,Vars,NewIndex) :-
	member(Index,Vars)
	 -> I1 is Index+1, newVarIndex(I1,Vars,NewIndex)
	  ; NewIndex=Index.

% generate new name for auxiliary function:
genAuxFuncName(FauxName) :-
	retract(newFunctionCounter(FNameS,FC)),
	FC1 is FC+1,
	asserta(newFunctionCounter(FNameS,FC1)),
	number_codes(FC,FCS),
	append(FNameS,"._#caseor",FNameAux),
	append(FNameAux,FCS,FauxName).

% add new auxiliary function:
addAuxFunction(AF) :-
	retract(newAuxFunctions(AFs)),
	asserta(newAuxFunctions([AF|AFs])).

varIndex2VarExp(I,'Var'(I)).

% compute the free variables in an expression (where variables are represented
% as integers):
freeVarsInExp('Var'(V),[V]).
freeVarsInExp('Lit'(_),[]).
freeVarsInExp('Comb'(_,_,Terms),V) :-
	freeVarsInExps(Terms,V).
freeVarsInExp('Free'(CVars,Exp),V) :-
	freeVarsInExp(Exp,VE),
	diff(VE,CVars,V).
freeVarsInExp('Or'(E1,E2),V) :-
	freeVarsInExp(E1,V1),
	freeVarsInExp(E2,V2),
	union(V1,V2,V).
freeVarsInExp('Case'(_,CE,Branches),V) :-
	freeVarsInExp(CE,V1),
	freeVarsInBranches(Branches,V2),
	union(V1,V2,V).
freeVarsInExp('Let'(Bindings,Exp),V) :-
	freeVarsInBindings(Bindings,BVs),
	freeVarsInExp(Exp,EVs),
	union(BVs,EVs,BEVs),
	map2M(compiler:bindingVar,Bindings,Vs),
	diff(BEVs,Vs,V).

freeVarsInExps([],[]).
freeVarsInExps([E|Es],V) :-
	freeVarsInExp(E,V1),
	freeVarsInExps(Es,V2),
	union(V1,V2,V).

freeVarsInBindings([],[]).
freeVarsInBindings(['Prelude.(,)'(_,E)|Bs],V) :-
	freeVarsInExp(E,V1),
	freeVarsInBindings(Bs,V2),
	union(V1,V2,V).

freeVarsInBranches([],[]).
freeVarsInBranches(['Branch'('Pattern'(_,PVs),E)|Bs],V) :-
	freeVarsInExp(E,V1),
	diff(V1,PVs,V2),
	freeVarsInBranches(Bs,V3),
	union(V2,V3,V).
freeVarsInBranches(['Branch'('LPattern'(_),E)|Bs],V) :-
	freeVarsInExp(E,V1),
	freeVarsInBranches(Bs,V2),
	union(V1,V2,V).


% does a (logical) variable occures in an expression (where variables
% are represented as logical variables)?
occursInExp(V,'Var'(X)) :- X==V.
occursInExp(V,'Comb'(_,_,Terms)) :- occursInExps(V,Terms).
occursInExp(V,'Free'(_,Exp)) :- occursInExp(V,Exp).
occursInExp(V,'Or'(E1,E2)) :- occursInExp(V,E1) ; occursInExp(V,E2).
occursInExp(V,'Case'(_,CE,Branches)) :-
	occursInExp(V,CE) ; occursInBranches(V,Branches).

occursInExps(V,[E|Es]) :- occursInExp(V,E) ; occursInExps(V,Es).

occursInBranches(V,['Branch'(_,E)|Bs]) :-
	occursInExp(V,E) ; occursInBranches(V,Bs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for handling global values:

% translate global specification into run-time form:
translateGlobalSpec(_,'Global.Temporary','Global.Temporary').
translateGlobalSpec(_,'Global.Persistent'(_),'Global.Persistent').
translateGlobalSpec(FName,_,_) :-
	writeErr('ERROR: Global declaration "'),
	writeErr(FName),
	writeLnErr('" has illegal specification of storage mechanism!'),
	setFlcBug.

% check the type of Global predicates (i.e., result type Global and
% monomorphism restriction):
checkGlobalType(PredName,'TCons'("Global.Global",[T])) :- !,
	checkGlobalTypeForCorrectTypes(PredName,T).
checkGlobalType(PredName,_) :-
	writeErr('ERROR: Global declaration "'),
	writeErr(PredName),
	writeLnErr('" has not result type "Global"!'),
	setFlcBug.

checkGlobalTypeForCorrectTypes(PredName,'FuncType'(T1,T2)) :-
	checkGlobalTypeForCorrectTypes(PredName,T1),
	checkGlobalTypeForCorrectTypes(PredName,T2).
checkGlobalTypeForCorrectTypes(PredName,'TCons'(TC,_)) :-
	atom_codes(TCA,TC),
	member(TCA,['Prelude.IO','Data.IORef.IORef','Dynamic.Dynamic','Ports.Port',
		    %'System.IO.Handle',
		    'Network.Socket.Socket']),
	!,
	nlErr,
	writeErr('ERROR: Type of global declaration "'),
	writeErr(PredName),
	writeErr('" contains illegal type: '), writeLnErr(TCA),
	setFlcBug.
checkGlobalTypeForCorrectTypes(PredName,'TCons'(_,Ts)) :-
	map1partialM(compiler:checkGlobalTypeForCorrectTypes(PredName),Ts).
checkGlobalTypeForCorrectTypes(PredName,'TVar'(_)) :-
	writeErr('ERROR: Type of global declaration "'),
	writeErr(PredName),
	writeLnErr('" contains type variable!'),
	setFlcBug.


% check the type of Data.Global predicates (i.e., result type
% Data.Global.GlobalT and monomorphism restriction):
checkGlobalTemporaryType(PredName,'TCons'("Data.Global.GlobalT",[T])) :- !,
	checkGlobalTmpTypeForCorrectTypes(PredName,T).
checkGlobalTemporaryType(PredName,_) :-
	writeErr('ERROR: Global declaration "'),
	writeErr(PredName),
	writeLnErr('" has not result type "Data.Global.GlobalT"!'),
	setFlcBug.

checkGlobalTmpTypeForCorrectTypes(PredName,'FuncType'(T1,T2)) :-
	checkGlobalTmpTypeForCorrectTypes(PredName,T1),
	checkGlobalTmpTypeForCorrectTypes(PredName,T2).
checkGlobalTmpTypeForCorrectTypes(PredName,'TCons'(_,Ts)) :-
	map1partialM(compiler:checkGlobalTmpTypeForCorrectTypes(PredName),Ts).
checkGlobalTmpTypeForCorrectTypes(PredName,'TVar'(_)) :-
	writeErr('ERROR: Type of global declaration "'),
	writeErr(PredName),
	writeLnErr('" contains type variable!'),
	setFlcBug.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for handling global variables:

% check the type of GlobalVariable predicates (i.e., result type Dynamic and
% monomorphism restriction):
checkGVarType(PredName,'TCons'("GlobalVariable.GVar",[T])) :- !,
	checkGlobalTypeForCorrectTypes(PredName,T).
checkGVarType(PredName,_) :-
	writeErr('ERROR: GVar declaration "'),
	writeErr(PredName),
	writeLnErr('" has not result type "GVar"!'),
	setFlcBug.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries for handling dynamic data:

% Add new initialization directives for dynamic data (used for saving states):
addDynamicNameInfo(PredName,FileName) :-
	retract(dynamicPredNames(Ds)),
	asserta(dynamicPredNames([(PredName,FileName)|Ds])), !.

% Write initial info clause for dynamic predicates:
writeDynamicInfoClause((PredName,'')) :- !,
	writeClause((:- assertz(dynamicPredInfo(PredName,'')))).
writeDynamicInfoClause((PredName,FileNameExp)) :-
	DynInfoClause =
	  (dynamicPredInfo(PredName,RF) :-
	               evaluateDynamicPredInfo(PredName,FileNameExp,RF)),
	writeClause((:- assertz(DynInfoClause))).


% is the rhs of a rule a dynamic declaration?
isDynamicRuleDecl([],'Comb'('FuncCall',"Dynamic.dynamic",[]),'') :- !.
isDynamicRuleDecl([],'Comb'('FuncCall',"Dynamic.persistent",[S]),S) :- !.
isDynamicRuleDecl(Args,'Comb'('FuncCall',"Prelude.apply",[Exp,'Var'(A)]),S) :- !,
	append(As,[A],Args),
	isDynamicRuleDecl(As,Exp,S).


% check the type of dynamic predicates (i.e., result type Dynamic and
% monomorphism restriction):
checkDynamicType(PredName,'FuncType'(T1,T2)) :- !,
	checkDynamicTypeForCorrectTypes(PredName,T1),
	checkDynamicType(PredName,T2).
checkDynamicType(_,'TCons'("Dynamic.Dynamic",[])) :- !.
checkDynamicType(PredName,_) :-
	writeErr('ERROR: Dynamic predicate "'),
	writeErr(PredName),
	writeLnErr('" has not result type "Dynamic"!'),
	setFlcBug.

checkDynamicTypeForCorrectTypes(PredName,'FuncType'(T1,T2)) :-
	checkDynamicTypeForCorrectTypes(PredName,T1),
	checkDynamicTypeForCorrectTypes(PredName,T2).
checkDynamicTypeForCorrectTypes(PredName,'TCons'(TC,_)) :-
	atom_codes(TCA,TC),
	member(TCA,['Data.IORef.IORef','Dynamic.Dynamic','Ports.Port',%'System.IO.Handle',
		    'Network.Socket.Socket']),
	!,
	nlErr,
	writeErr('ERROR: Type of dynamic predicate "'),
	writeErr(PredName),
	writeErr('" contains illegal type: '), writeLnErr(TCA),
	setFlcBug.
checkDynamicTypeForCorrectTypes(PredName,'TCons'(_,Ts)) :-
	map1partialM(compiler:checkDynamicTypeForCorrectTypes(PredName),Ts).
checkDynamicTypeForCorrectTypes(PredName,'TVar'(_)) :-
	writeErr('ERROR: Type of dynamic predicate "'),
	writeErr(PredName),
	writeLnErr('" contains type variable!'),
	setFlcBug.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of a defined function

writeFunc('Func'(_,_,_,_,'External'(ExtNameS))) :-
	append(_,"[raw]",ExtNameS),
	% don't generate standard external interface for externals
	% annotated with [raw]
	!.

writeFunc('Func'(Name,FArity,_,Type,'External'(ExtNameS))) :-
	isIOAction(Type), !,
	% generate standard external interface for I/O operations:
	flatName2Atom(Name,FName),
        length(Args,FArity),
	rev(Args,RevArgs),
	appendAtom(FName,'$WORLD',FNameWorld),
	append(Args,[partcall(1,FNameWorld,RevArgs),E,E],PFArgs),
	PrimFact =.. [FName|PFArgs],
	writeClause(PrimFact),
	FArity3 is FArity+3,
	FArity4 is FArity+4,
	genBlockDecl(FNameWorld,FArity4,[FArity3],NewFNameWorld),
	length(RefArgs,FArity),
	append(RefArgs,[_,'$io'(Result),E0,E],PredArgs),
	LHS =.. [NewFNameWorld|PredArgs],
	append(_,[32|PredNameS],ExtNameS),
	atom_codes(PredName,PredNameS),
	append(Args,[Result],PrimArgs),
	PrimCall =.. [PredName|PrimArgs],
	genDerefCalls(Type,RefArgs,Args,(PrimCall, E0=E),RHS),
	writeClause((LHS :- RHS)),
	nl, !.

writeFunc('Func'(Name,FArity,_,Type,'External'(ExtNameS))) :-
	% generate standard external interface for non I/O operations:
	flatName2Atom(Name,FName),
	FArity2 is FArity+2,
	FArity3 is FArity+3,
	genBlockDecl(FName,FArity3,[FArity2],NewFName),
	length(RArgs,FArity),
	append(RArgs,[Result,E0,E],PredArgs),
	LHS =.. [NewFName|PredArgs],
	length(Args,FArity),
	append(Args,[Result],PrimArgs),
	append(_,[32|PredNameS],ExtNameS),
	atom_codes(PredName,PredNameS),
	PrimCall =.. [PredName|PrimArgs],
	genDerefCalls(Type,RArgs,Args,(PrimCall, E0=E),RHS),
	writeClause((LHS :- RHS)),
	nl, !.

% special code for dynamic predicates:
writeFunc('Func'(Name,FArity,_Vis,Type,'Rule'(Args,Exp))) :-
	isDynamicRuleDecl(Args,Exp,DynDeclArg), !,
	flatName2Atom(Name,FName),
	checkDynamicType(FName,Type),
	atom_codes(FName,FNameS),
	append(_,[46|UQNameS],FNameS),
	append("$DYN_",UQNameS,DynNameS),
	atom_codes(DynName,DynNameS),
        (DynDeclArg = ''
	  -> DynType = 'Dynamic.Temporary'
	   ; exp2Term([],DynDeclArg,ENameTerm),
	     DynType = 'Dynamic.Persistent'),
        length(Xs,FArity),
	length(Ys,FArity),
	PYs =.. [DynName,DynType,0,0|Ys],
	append(Xs,['Dynamic.Dynamic'(PYs),E0,E],HeadArgs),
	Head =.. [FName|HeadArgs],
	genNfArgs(Xs,Ys,E0,E,Body),
	writeClause((Head :- Body)),
	argnum(Type,TArity),
	DArity is TArity+2,
	writeClause((:- dynamic DynName/DArity)),
	(DynType = 'Dynamic.Temporary'
	 -> addDynamicNameInfo(DynName/DArity,'')
	  ; addDynamicNameInfo(DynName/DArity,ENameTerm)),
	nl, !.

% special code for global declarations w.r.t. (deprecated) module Global:
writeFunc('Func'(Name,0,_Vis,Type,
	  'Rule'([],'Comb'('FuncCall',"Global.global",[V,S])))) :- !,
	flatName2Atom(Name,FName),
	checkGlobalType(FName,Type),
	appendAtom('$GLOBAL_',FName,GlobName),
        exp2Term([],V,ValueTerm),
        exp2Term([],S,GSpecTerm),
	translateGlobalSpec(FName,GSpecTerm,GSpec),
	Head =.. [FName,'Global.GlobalDef'(GlobName,GSpec),E,E],
	writeClause(Head),
	writeClause((:- dynamic GlobName/1)),
	GlobClauseHead =.. [GlobName,IVal],
	writeClause((GlobClauseHead :-
                         initGlobalValue(GlobName,GSpecTerm,ValueTerm,IVal))),
	nl, !.

% special code for global variable declarations:
writeFunc('Func'(Name,0,_Vis,Type,
	  'Rule'([],'Comb'('FuncCall',"GlobalVariable.gvar",[V])))) :- !,
	flatName2Atom(Name,FName),
	checkGVarType(FName,Type),
	%appendAtom('$GLOBVAR_',FName,GlobName),
        exp2Term([],V,ValueTerm),
	Head =.. [FName,GlobValue,E0,E],
	writeClause((:- dynamic FName/3)),
	writeClause((Head :-
                       initGlobalVariable(FName,ValueTerm,GlobValue,E0,E))),
	nl, !.

writeFunc('Func'(Name,FArity,_Vis,Type,'Rule'(FlatArgs,FlatExp))) :-
	flatName2Atom(Name,FName),
        checkFuncType(FName,FArity,Type),
	retract(currentFunction(_)),
	decodePrologName(FName,FlatName),
	asserta(currentFunction(FlatName)),
	% only for debugging:
	  %writeErr('*** Translating rule: '), writeErr(FName), writeErr(' '),
	  %writeErr(FlatArgs), writeErr(' = '), writeErr(FlatExp), nlErr,
	FArity2 is FArity+2,
	FArity3 is FArity+3,
	genBlockDecl(FName,FArity3,[FArity2],NewFName),
	%write(':- block '), writeq(FName), write('(?,'),
	%writeNTimes(FArity,'?,'), write('-,?).'), nl,
	flatargs2var(FlatArgs,Env,Args),
	flatexp2var(Env,FlatExp,RHS),
	transExp(NewFName,'',Args,Args,nocut,RHS), nl.

% check function type for correct use of module Data.Global:
checkFuncType(FName,0,Type) :-
        Type = 'TCons'(GlobalT,[_]),
        atom_codes('Data.Global.GlobalT',GlobalT), !,
        atom_codes(FName,FNameS),
        (append("Data.Global._impl",_,FNameS)
         -> true
	  ; checkGlobalTemporaryType(FName,Type)), !.
checkFuncType(_,_,_).

% is a function type the type of a (parameterized) IO action?
isIOAction('FuncType'(_,T)) :- isIOAction(T).
isIOAction('TCons'("Prelude.IO",_)) :- !.

% generate deref-calls for external function interface:
genDerefCalls('FuncType'(Type1,Type2),[RA|RArgs],[A|Args],LastGoal,
	      (DerefCall, LastGoal2)) :- !,
	genDerefCalls(Type2,RArgs,Args,LastGoal,LastGoal2),
	type2derefPred(Type1,DerefPred),
	DerefCall =.. [DerefPred,RA,A].
genDerefCalls('TCons'(TCName,_),[RA],[A],LastGoal,LastGoal) :-
	TCName="Prelude.IO", !, % IO type is handled as a function type
	RA=A. % world argument is not dereferenced
genDerefCalls(_,[],[],LastGoal,LastGoal).
%genDerefCalls(Type,RArgs,Args,LastGoal,(RArgs=Args, LastGoal)) :-
%	writeErr(Type), nlErr.

% create deref predicate for corresponding type, i.e.,
% derefRoot for primitive types and derefAll for other types
type2derefPred('TCons'(Name,_),derefRoot) :-
	member(Name,["Prelude.Int","Prelude.Float","Prelude.Char","Prelude.Bool",
		     "Prelude.Ordering",
		     "IO.Handle","IO.IOMode","IO.SeekMode",
		     "PlProfileData.ProfileSelection","Ports.Port","Socket.Socket"]), !.
type2derefPred('FuncType'(_,_),derefRoot) :-
	% functional arguments are only evaluated to head normal form
	!.
type2derefPred(_,derefAll).


genNfArgs([],[],E,E,true).
genNfArgs([X|Xs],[Y|Ys],E0,E,(nf(X,Y,E0,E1),Goal)) :-
	genNfArgs(Xs,Ys,E1,E,Goal).

% assign to each FlatCurry argument (an integer) in an argument list a new
% logic variable and unify second argument with this assignment, e.g.,
% flatargs2var([1,2,3],E,L) --> E=[(1,X),(2,Y),(3,Z)],L=[X,Y,Z]
flatargs2var([],[],[]).
flatargs2var([I|Is],[(I,Var)|E],[Var|Vars]) :- flatargs2var(Is,E,Vars).

% replace in a FlatCurry expression all variable indices by logic variables:
flatexp2var(Env,'Var'(I),'Var'(V)) :- !,
	getVarInEnv(I,Env,V).
flatexp2var(_,'Lit'(L),'Lit'(L)) :- !.
flatexp2var(Env,'Comb'(CT,CF,Exps),'Comb'(CT,CF,VarExps)) :- !,
	map2partialM(compiler:flatexp2var(Env),Exps,VarExps).
flatexp2var(Env,'Free'(VarIs,Exp),'Free'(Vars,VarExp)) :- !,
	flatargs2var(VarIs,VarEnv,Vars),
	append(VarEnv,Env,Env1),
	flatexp2var(Env1,Exp,VarExp).
flatexp2var(Env,'Or'(E1,E2),'Or'(VE1,VE2)) :- !,
	flatexp2var(Env,E1,VE1),
	flatexp2var(Env,E2,VE2).
flatexp2var(Env,'Case'(CT,Exp,Cases),'Case'(CT,VarExp,VarCases)) :- !,
	flatexp2var(Env,Exp,VarExp),
	map2partialM(compiler:flatcases2var(Env),Cases,VarCases).
flatexp2var(_,Expr,Expr) :-
	writeErr('ERROR in FlatCurry file: Unknown expression "'),
	writeErr(Expr),
	writeErr('" in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	pleaseReport,
	put_code(37), write('ERROR in FlatCurry file: Unknown expression "'),
	write(Expr), write('" in function "'),
	write(FName), write('"!'), nl.

flatcases2var(Env,'Branch'('Pattern'(L,VarIs),Exp),
	          'Branch'('Pattern'(L,Vars),VarExp)) :-
	flatargs2var(VarIs,VarEnv,Vars),
	append(VarEnv,Env,Env1),
	flatexp2var(Env1,Exp,VarExp).
flatcases2var(Env,'Branch'('LPattern'(L),Exp),
	          'Branch'('LPattern'(L),VarExp)) :-
	flatexp2var(Env,Exp,VarExp).

getVarInEnv(_,[],_) :-
	writeErr('ERROR in FlatCurry file: '),
	writeErr('undeclared variable in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	reportLiftBug,
	put_code(37), write('ERROR in FlatCurry file: '),
	write('undeclared variable in function "'),
	write(FName), write('"!'), nl.
getVarInEnv(I,[(I,Var)|_],Var) :- !.
getVarInEnv(I,[_|Env],Var) :- getVarInEnv(I,Env,Var).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% write clauses with type (and evaluation) information

writeFTypeClause(ModS,ExtFuncs,_Ops,'Func'(Name,_FArity,Vis,_,_)) :-
	flatName2Atom(Name,FName),
	getExternalNameFromVisibility(ModS,Name,Vis,EName),
	getFuncArity(FName,Arity),
	getPrologNameFromExtFuncs(FName,Arity,ExtFuncs,PrologName),
	writeClause(functiontype(FName,EName,Arity,PrologName,nofix,notype)).

getPrologNameFromExtFuncs(FName,Arity,ExtFuncs,PrologName) :-
	member((FName/Arity,PName),ExtFuncs), !,
	(append(_,[32|PrimNameS],PName)
	 -> (append(EPrimNameS,"[raw]",PrimNameS)
	     -> atom_codes(PrologName,EPrimNameS)
	      ; PrologName=FName)
	  ; writeErr('ERROR: Specification of primitive function '),
	    deleteCostCenterInPrologName(FName,PNameWOCC),
	    decodePrologName(PNameWOCC,FNameWOCC),
	    writeErr(FNameWOCC), writeErr('/'), writeErr(Arity),
	    writeLnErr(' not found!'),
	    setFlcBug, fail).
getPrologNameFromExtFuncs(FName,_,_,FName).

getExternalNameFromVisibility(_,Name,'Private',FName) :- atom_codes(FName,Name).
getExternalNameFromVisibility(ModS,Name,'Public',FName) :-
	append(ModS,[46|F],Name), !, atom_codes(FName,F).
getExternalNameFromVisibility(_,Name,'Public',FName) :- atom_codes(FName,Name).

getUnqualifiedName(Name,UQName) :-
	append([_|_],[46|F],Name), !, atom_codes(UQName,F).
getUnqualifiedName(Name,UQName) :- atom_codes(UQName,Name).

writeDTypeClause(ModS,'Type'(_TypeName,_Vis,_TypeArgs,ConsExprs)) :-
	writeDTypeClauses(ModS,0,ConsExprs,ConsExprs).
writeDTypeClause(_,'TypeNew'(_,_,_,_)).

index2tvar(I,'TVar'(I)). % transform tvar index into type expression

writeDTypeClauses(_,_,[],_).
writeDTypeClauses(ModS,Index,
                  ['Cons'(ConsName,Arity,Vis,_ArgTypes)|Cs],AllConstrs) :-
	flatName2Atom(ConsName,Cons),
	getExternalNameFromVisibility(ModS,ConsName,Vis,EName),
	getUnqualifiedName(ConsName,UQName),
	getOtherConstructors(ConsName,AllConstrs,OtherConstrs),
	writeClause(constructortype(Cons,EName,Arity,UQName,Index,notype,
				    OtherConstrs)),
	Index1 is Index+1,
	writeDTypeClauses(ModS,Index1,Cs,AllConstrs).

getOtherConstructors(_,[],[]).
getOtherConstructors(ConsName,['Cons'(ConsName,_,_,_)|Cs],OCs) :- !,
	getOtherConstructors(ConsName,Cs,OCs).
getOtherConstructors(ConsName,['Cons'(CN,CA,_,_)|Cs],[CNA/CA|OCs]) :-
	flatName2Atom(CN,CNA),
	getOtherConstructors(ConsName,Cs,OCs).

typelist2flattype([Type],Type) :- !.
typelist2flattype([T1|T2L],'FuncType'(T1,T2)) :-
	!,
	typelist2flattype(T2L,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of clauses for evaluation to head normal form

transHnfCurrent(CCs) :-
	findall(FE,functiontypeWithExternal(FE),Funcs),
	transHnf(Funcs,CCs).

functiontypeWithExternal((FName/FArity,EName)) :-
	user:functiontype(FName,_,FArity,EName,_,_).

transHnf(Funcs,CCs) :-
	genBlockDecl(hnf,4,[3],HNF),
	%writeClause((:- block hnf(?,?,-,?))),
	HnfLHS =.. [HNF,T,H,E0,E],
	writeClause((HnfLHS :- var(T), !, H=T, E0=E)),
	(compileWithSharing(variable)
	 -> map1partialM(compiler:genVariableShareHnfClause(HNF),CCs)
	  ; (compileWithSharing(function)
	     -> map1partialM(compiler:genFunctionShareHnfClause(HNF),CCs)
	      ; true)),
        SpecialFuncs = [('reportFailure4PAKCS'/2,prim_failure),
                        ('letrec4PAKCS'/2,prim_letrec)],
        append(SpecialFuncs,Funcs,AllHnfFuncs),
	map1partialM(compiler:genHnfClause(HNF),AllHnfFuncs),
	HnfFact =.. [HNF,T,T,E,E],
	writeClause(HnfFact), nl.

genVariableShareHnfClause(HNF,Suffix) :-
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	appendAtom(propagateShare,Suffix,PropShare),
	PropShare_HV_R =.. [PropShare,HV,R],
	HnfLHS =.. [HNF,Share_M,R,E0,E],
	ShareGoal = (PropShare_HV_R, update_mutable('$eval'(R),M)),
	(printConsFailure(no)
	 -> ShareHNF = ShareGoal
	  ; ShareHNF = % no sharing for FAIL:
	     ((nonvar(HV), functor(HV,'FAIL',_)) -> R=HV ; ShareGoal)),
	writeClause((HnfLHS :- !, get_mutable(V,M),
                        (V='$eval'(Expr)
                         -> R=Expr, E0=E
                          ; hnf(V,HV,E0,E1),
			    ShareHNF,
			    E1=E))).

genFunctionShareHnfClause(HNF,Suffix) :-
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	HnfLHS =.. [HNF,Share_M,R,E0,E],
	writeClause((HnfLHS :- !, get_mutable(V,M),
                        (V='$eval'(Expr)
                         -> R=Expr, E0=E
                          ; hnf(V,R,E0,E1),
			    update_mutable('$eval'(R),M),
			    E1=E))).

% generate hnf clause for a function:
genHnfClause(HNF,(FName/FArity,PredName)) :-
	length(Args,FArity),
	LHS =.. [FName|Args],
	append(Args,[R,E0,E],RArgs),
	PredCall =.. [PredName|RArgs],
	(compileWithDebug ->
	      Goal = (traceCall(LHS,Skip), PredCall, traceExit(LHS,R,E,Skip))
            ; (((\+ printConsFailure(no)), failCheckFunc(yes),
		(\+ FName='Prelude.failure'),
		(\+ FName='reportFailure4PAKCS'),
                (\+ FName='Prelude.apply'))
	       -> append(Args,[IR,E0,E1],IRArgs),
		  PredCall1 =.. [PredName|IRArgs],
		  Goal = (PredCall1, checkFailValue(LHS,IR,R,E1,E))
		; Goal = PredCall)),
	HnfLHS =.. [HNF,LHS,R,E0,E],
	(compileWithFailPrint
	 -> writeClause((HnfLHS :- !, (Goal; failprint(LHS,E0,E))))
	  ; writeClause((HnfLHS :- !, Goal))).
genHnfClause(_,_) :- !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of clauses for solving equational constraints

transConstrEq(Suffix) :-
	appendAtom('Prelude.constrEq',Suffix,ConstrEqOrg),
	genBlockDecl(ConstrEqOrg,5,[4],ConstrEq),
	appendAtom(constrEqHnf,Suffix,ConstrEqHnfOrg),
	ConstrEq_A_B_R_E0_E =.. [ConstrEq,A,B,R,E0,E],
	ConstrEqHnf_HA_HB_R_E2_E =.. [ConstrEqHnfOrg,HA,HB,R,E2,E],
	(compileWithDebug ->
            writeClause((ConstrEq_A_B_R_E0_E :-
			   traceCall('Prelude.=:='(A,B),Skip),
	                   hnf(A,HA,E0,E1), hnf(B,HB,E1,E2),
	                   ConstrEqHnf_HA_HB_R_E2_E,
			   traceExit('Prelude.=:='(A,B),'Prelude.True',
			             E,Skip)))
          ; writeClause((ConstrEq_A_B_R_E0_E :- hnf(A,HA,E0,E1),hnf(B,HB,E1,E2),
	                                 ConstrEqHnf_HA_HB_R_E2_E))),
	nl,
	genBlockDecl(ConstrEqHnfOrg,5,[4],ConstrEqHnf),
	ConstrEqHnf_X_H_R_E0_E =.. [ConstrEqHnf,X,H,R,E0,E],
	ConstrEqHnf_H_X_R_E0_E =.. [ConstrEqHnf,H,X,R,E0,E],
	appendAtom(bindTryNf,Suffix,BindTryNf),
	BindTryNf_X_H_R_E0_E =.. [BindTryNf,X,H,R,E0,E],
	writeClause((ConstrEqHnf_X_H_R_E0_E :- var(X),!,BindTryNf_X_H_R_E0_E)),
	writeClause((ConstrEqHnf_H_X_R_E0_E :- var(X),!,BindTryNf_X_H_R_E0_E)),
	ConstrEqHnf_T1_T2_E0_E =.. [ConstrEqHnf,T1,T2,'Prelude.True',E0,E],
	ConstrEqHnf_A_B_R_E0_E =.. [ConstrEqHnf,A,B,R,E0,E],
	(printConsFailure(no)
	 -> writeClause((ConstrEqHnf_T1_T2_E0_E :- number(T1),!,T1=T2,E0=E))
	  ; ConstrEqHnf_FAIL_X_E_E =.. [ConstrEqHnf,'FAIL'(Src),X,
					'FAIL'(Src),E,E],
	    writeClause((ConstrEqHnf_FAIL_X_E_E :- !)),
	    ConstrEqHnf_X_FAIL_E_E =.. [ConstrEqHnf,X,'FAIL'(Src),
					'FAIL'(Src),E,E],
	    writeClause((ConstrEqHnf_X_FAIL_E_E :- !)),
	    writeClause((ConstrEqHnf_A_B_R_E0_E :- number(A), !,
             (A=B -> R='Prelude.True', E0=E
	           ; prim_failure(partcall(2,'Prelude.=:=',[Dict]),[A,B],R,E0,E))))),
	appendAtom(genConstrEqHnfBody,Suffix,GenConstrEqHnfBody),
	GenConstrEqHnfBody_1_NA =.. [GenConstrEqHnfBody,1,NA,A,B,EqBody],
	writeClause((ConstrEqHnf_A_B_R_E0_E :-
		        functor(A,FA,NA), functor(B,FB,NB),
		        FA==FB, NA==NB, !, GenConstrEqHnfBody_1_NA,
		        hnf(EqBody,R,E0,E))),
	(printConsFailure(no) -> true
	 ; ConstrEqHnf_A_B_FAIL_E0_E =.. [ConstrEqHnf,A,B,R,E0,E],
	   writeClause((ConstrEqHnf_A_B_FAIL_E0_E :-
		          prim_failure(partcall(2,'Prelude.=:=',[Dict]),[A,B],R,E0,E)))),
	nl,
	GenConstrEqHnfBody_N_NA_Succ =..
             [GenConstrEqHnfBody,N,NA,_,_,'Prelude.True'],
	writeClause((GenConstrEqHnfBody_N_NA_Succ :- N>NA,!)),
	appendAtom('Prelude.constrEq',Suffix,Eq),
	Eq_ArgA_ArgB =.. [Eq,ArgA,ArgB],
	GenConstrEqHnfBody_N_NA_Eq =.. [GenConstrEqHnfBody,N,NA,A,B,Eq_ArgA_ArgB],
	writeClause((GenConstrEqHnfBody_N_NA_Eq :- N=NA, !,
		       arg(N,A,ArgA), arg(N,B,ArgB))),
	appendAtom('Prelude.&',Suffix,ConcAnd),
	ConcAnd_Eq =.. [ConcAnd,Eq_ArgA_ArgB,RemBody],
	GenConstrEqHnfBody_N_NA_And =.. [GenConstrEqHnfBody,N,NA,A,B,ConcAnd_Eq],
	GenConstrEqHnfBody_N1_NA =.. [GenConstrEqHnfBody,N1,NA,A,B,RemBody],
	writeClause((GenConstrEqHnfBody_N_NA_And :-
		       arg(N,A,ArgA), arg(N,B,ArgB),
		       N1 is N+1, GenConstrEqHnfBody_N1_NA)),
	nl,
	% optimization: try to normalize the term before binding:
	BindTryNf_X_T_R_E0_E =.. [BindTryNf,X,T,R,E0,E],
	appendAtom(nf,Suffix,Nf),
	Nf_T_NT_E0_E1 =.. [Nf,T,NT,E0,E1],
	appendAtom(bindDirect,Suffix,BindDirect),
	appendAtom(bind,Suffix,Bind),
	BindDirect_X_NT_R_E1_E =.. [BindDirect,X,NT,R,E1,E],
	Bind_X_T_R_E0_E =.. [Bind,X,T,R,E0,E],
	writeClause((BindTryNf_X_T_R_E0_E :- Nf_T_NT_E0_E1,
	                       (nonvar(E1) -> BindDirect_X_NT_R_E1_E
                                            ; Bind_X_T_R_E0_E))),
	BindDirect_X_T_R_E0_E =.. [BindDirect,X,T,R,E0,E],
	appendAtom(occursNot,Suffix,OccursNot),
	OccursNot_X_T =.. [OccursNot,X,T],
	writeClause((BindDirect_X_T_R_E0_E :- var(T), !, X=T,
		                              R='Prelude.True', E0=E)),
	(printConsFailure(no)
	 -> writeClause((BindDirect_X_T_R_E0_E :-
			     OccursNot_X_T, X=T, R='Prelude.True', E0=E))
	  ; BindDirect_X_FAIL_E_E =..
	                      [BindDirect,X,'FAIL'(Src),'FAIL'(Src),E,E],
	    writeClause((BindDirect_X_FAIL_E_E :- !)),
	    writeClause((BindDirect_X_T_R_E0_E :-
			     OccursNot_X_T, !, X=T, R='Prelude.True', E0=E)),
	    writeClause((BindDirect_X_T_R_E0_E :-
		       prim_failure(partcall(2,'Prelude.=:=',[]),[X,T],R,E0,E)))),
	nl,
	Bind_X_T_E0_E =.. [Bind,X,T,'Prelude.True',E0,E],
	writeClause((Bind_X_T_E0_E :- var(T), !, X=T, E0=E)),
	writeClause((Bind_X_T_E0_E :- number(T), !, X=T, E0=E)),
	(printConsFailure(no) -> true
	 ; Bind_X_FAIL_E_E =.. [Bind,X,'FAIL'(Src),'FAIL'(Src),E,E],
	   writeClause((Bind_X_FAIL_E_E :- !))),
	Bind_A_B_R_E0_E =.. [Bind,A,B,R,E0,E],
	appendAtom(occursNotArgs,Suffix,OccursNotArgs),
	OccursNotArgs_1_NB_A_B =.. [OccursNotArgs,1,NB,A,B],
	appendAtom(bindArgs,Suffix,BindArgs),
	BindArgs_1_NB_A_B_R_E0_E =.. [BindArgs,1,NB,A,B,R,E0,E],
	(printConsFailure(no)
	 -> writeClause((Bind_A_B_R_E0_E :-
		        functor(B,FB,NB), OccursNotArgs_1_NB_A_B,
		        functor(A,FB,NB), BindArgs_1_NB_A_B_R_E0_E))
	  ; writeClause((Bind_A_B_R_E0_E :-
		        functor(B,FB,NB), OccursNotArgs_1_NB_A_B, !,
		        functor(A,FB,NB), BindArgs_1_NB_A_B_R_E0_E)),
	    writeClause((Bind_A_B_R_E0_E :-
		       prim_failure(partcall(2,'Prelude.=:=',[]),[A,B],R,E0,E)))),
	nl,
	OccursNotArgs_N_NA_A_B =.. [OccursNotArgs,N,NA,A,B],
	OccursNotArgs_N1_NA_A_B =.. [OccursNotArgs,N1,NA,A,B],
	OccursNot_A_ArgB =.. [OccursNot,A,ArgB],
	writeClause((OccursNotArgs_N_NA_A_B :- N>NA,!)),
	writeClause((OccursNotArgs_N_NA_A_B :- arg(N,B,ArgB),
		        OccursNot_A_ArgB,
		        N1 is N+1, OccursNotArgs_N1_NA_A_B)),
	nl,
	BindArgs_N_NA_A_B_E0_E =.. [BindArgs,N,NA,A,B,'Prelude.True',E0,E],
	BindArgs_N_NA_A_B_R_E0_E =.. [BindArgs,N,NA,A,B,R,E0,E],
	Bind_ArgA_HArgB_R_E1_E2 =.. [Bind,ArgA,HArgB,R,E1,E2],
	Bind_ArgA_HArgB_R0_E1_E2 =.. [Bind,ArgA,HArgB,R0,E1,E2],
	BindArgs_N1_NA_A_B_R_E2_E =.. [BindArgs,N1,NA,A,B,R,E2,E],
	writeClause((BindArgs_N_NA_A_B_E0_E :- N>NA,!, E0=E)),
	(printConsFailure(no)
	 -> writeClause((BindArgs_N_NA_A_B_R_E0_E :-
		        arg(N,A,ArgA), arg(N,B,ArgB),
		        hnf(ArgB,HArgB,E0,E1), Bind_ArgA_HArgB_R_E1_E2,
		        N1 is N+1, BindArgs_N1_NA_A_B_R_E2_E))
	  ; writeClause((BindArgs_N_NA_A_B_R_E0_E :-
		        arg(N,A,ArgA), arg(N,B,ArgB),
		        hnf(ArgB,HArgB,E0,E1), Bind_ArgA_HArgB_R0_E1_E2,
		        N1 is N+1,
			freeze(E2,(R0='FAIL'(_) -> R=R0, E2=E
				                 ; BindArgs_N1_NA_A_B_R_E2_E))))),
	nl,
	OccursNot_X_Y =.. [OccursNot,X,Y],
	writeClause((OccursNot_X_Y :- var(Y), !, X\==Y)),
	OccursNotArgs_1_NY_X_Y =.. [OccursNotArgs,1,NY,X,Y],
	writeClause((OccursNot_X_Y :- functor(Y,FY,NY),
		                      constructortype(FY,_,NY,_,_,_,_),
		                      !, OccursNotArgs_1_NY_X_Y)),
	writeClause(OccursNot_X_Y),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of clauses for (Boolean) strict equality test
	
transBoolEq(Suffix) :-
	appendAtom(boolEq,Suffix,BoolEqOrg),
	genBlockDecl(BoolEqOrg,5,[4],BoolEq),
	appendAtom(boolEqHnf,Suffix,BoolEqHnfOrg),
	BoolEq_A_B_R_E0_E =.. [BoolEq,A,B,R,E0,E],
	BoolEqHnf_HA_HB_R_E2_E =.. [BoolEqHnfOrg,HA,HB,R,E2,E],
	writeClause((BoolEq_A_B_R_E0_E :- hnf(A,HA,E0,E1),hnf(B,HB,E1,E2),
	                                  BoolEqHnf_HA_HB_R_E2_E)),
	nl,
	%genBlockDecl(BoolEqHnfOrg,5,[1,2,4],BoolEqHnf),
	genBlockDecl(BoolEqHnfOrg,5,[4],BoolEqHnf),
	BoolEqHnf_A_B_R_E0_E =.. [BoolEqHnf,A,B,R,E0,E],
	BoolEqHnf_B_A_R_E0_E =.. [BoolEqHnf,B,A,R,E0,E],
	% wait if both arguments are variables:
	writeClause((BoolEqHnf_A_B_R_E0_E :- var(A), var(B), !,
		      evaluator:addSuspensionReason('Comparing (with ==) two free variables'),
		      when((nonvar(A);nonvar(B)),BoolEqHnf_A_B_R_E0_E))),
	writeClause((BoolEqHnf_A_B_R_E0_E :- var(A), !,
		                             BoolEqHnf_B_A_R_E0_E)),
	(printConsFailure(no) -> true
	 ; BoolEqHnf_FAIL_X_E_E =.. [BoolEqHnf,'FAIL'(Src),X,'FAIL'(Src),E,E],
	   writeClause((BoolEqHnf_FAIL_X_E_E :- !)),
	   BoolEqHnf_X_FAIL_E_E =.. [BoolEqHnf,X,'FAIL'(Src),'FAIL'(Src),E,E],
	   writeClause((BoolEqHnf_X_FAIL_E_E :- !))),
	writeClause((BoolEqHnf_A_B_R_E0_E :-
		% we cannot narrow numbers or characters, so we wait:
		(number(A) ; basics:isCharCons(A)), !,
		((A=B, R='Prelude.True', E0=E) ;
		    evaluator:addSuspensionReason('Comparing (with ==) a free variable with a number or character'),
		    when(nonvar(B),(A\=B, R='Prelude.False', E0=E))))),
	appendAtom(genBoolEqHnfBody,Suffix,GenBoolEqHnfBody),
	GenBoolEqHnfBody_1_NA =.. [GenBoolEqHnfBody,1,NA,A,B,SeqBody],
	writeClause((BoolEqHnf_A_B_R_E0_E :- var(B), !, % bind variable
	    functor(A,FA,NA),
	    ((functor(B,FA,NA),GenBoolEqHnfBody_1_NA,hnf(SeqBody,R,E0,E))
	     ; (constructortype(FA,_,NA,_,_,_,OtherCons),
		member(OC/OCA,OtherCons),
		functor(B,OC,OCA), R='Prelude.False',E0=E)))),
	writeClause((BoolEqHnf_A_B_R_E0_E :-
		       functor(A,FA,NA),
		       ((functor(B,FA,NA),GenBoolEqHnfBody_1_NA)
		          -> hnf(SeqBody,R,E0,E)
		           ; R='Prelude.False',E0=E))),
	nl,
	GenBoolEqHnfBody_N_NA_True =.. [GenBoolEqHnfBody,N,NA,_,_,'Prelude.True'],
	writeClause((GenBoolEqHnfBody_N_NA_True :- N>NA,!)),
	appendAtom('Prelude.==',Suffix,Eq),
	Eq_ArgA_ArgB =.. [Eq,ArgA,ArgB],
	appendAtom('Prelude.&&',Suffix,BoolAnd),
	BoolAnd_Eq =.. [BoolAnd,Eq_ArgA_ArgB,RemBody],
	GenBoolEqHnfBody_N_NA_And =.. [GenBoolEqHnfBody,N,NA,A,B,BoolAnd_Eq],
	GenBoolEqHnfBody_N1_NA =.. [GenBoolEqHnfBody,N1,NA,A,B,RemBody],
	writeClause((GenBoolEqHnfBody_N_NA_And :-
		       arg(N,A,ArgA), arg(N,B,ArgB),
		       N1 is N+1, GenBoolEqHnfBody_N1_NA)),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of clauses for evaluation to normal form
	
transNf(Suffix) :-
	appendAtom(nf,Suffix,NfOrg),
	genBlockDecl(NfOrg,4,[3],Nf),
	appendAtom(nfHnf,Suffix,NfHnfOrg),
	%NfMode =.. [Nf,?,?,-,?],
	%writeClause((:- block NfMode)),
	Nf_A_V_E0_E =.. [Nf,A,V,E0,E],
	NfHnf_HA_V_E1_E =.. [NfHnfOrg,HA,V,E1,E],
	writeClause((Nf_A_V_E0_E :- hnf(A,HA,E0,E1), NfHnf_HA_V_E1_E)),
	nl,
	genBlockDecl(NfHnfOrg,4,[3],NfHnf),
	%NfHnfMode =.. [NfHnf,?,?,-,?],
	%writeClause((:- block NfHnfMode)),
	NfHnf_T_V_E0_E =.. [NfHnf,T,V,E0,E],
	writeClause((NfHnf_T_V_E0_E :- var(T), !, V=T, E0=E)),
	NfHnf_A_A_E0_E =.. [NfHnf,A,A,E0,E],
	writeClause((NfHnf_A_A_E0_E :- number(A), !, E0=E)),
	NfHnf_IORefA_IORefA_E0_E =..
                           [NfHnf,'IOExts.IORef'(A),'IOExts.IORef'(A),E0,E],
        writeClause((NfHnf_IORefA_IORefA_E0_E :- !, E0=E)),
	% do not normalize partcall arguments:
	NfHnf_partcall_partcall_E0_E =..
                           [NfHnf,partcall(A,B,C),partcall(A,B,C),E0,E],
        writeClause((NfHnf_partcall_partcall_E0_E :- !, E0=E)),
	(printConsFailure(no) -> true
	 ; NfHnf_FailA_FailA_E0_E =.. [NfHnf,'FAIL'(A),'FAIL'(A),E0,E],
	   writeClause((NfHnf_FailA_FailA_E0_E :- !, E0=E))),
	NfHnf_A_R_E0_E =.. [NfHnf,A,R,E0,E],
	appendAtom(nfHnfArgs,Suffix,NfHnfArgs),
	NfHnfArgs_1_NA_A_NR_R_E0_E =.. [NfHnfArgs,1,NA,A,NR,R,E0,E],
	writeClause((NfHnf_A_R_E0_E :-
		       functor(A,FA,NA), functor(NR,FA,NA),
		       NfHnfArgs_1_NA_A_NR_R_E0_E)),
	nl,
	NfHnfArgs_N_NA_A_NR_R_E0_E =.. [NfHnfArgs,N,NA,A,NR,R,E0,E],
	writeClause((NfHnfArgs_N_NA_A_NR_R_E0_E :- N>NA,!,R=NR,E0=E)),
	Nf_ArgA_ArgR_E0_E1 =.. [Nf,ArgA,ArgR,E0,E1],
	NfHnfArgs_N1_NA_A_NR_R_E1_E =.. [NfHnfArgs,N1,NA,A,NR,R,E1,E],
	(printConsFailure(no)
	 -> writeClause((NfHnfArgs_N_NA_A_NR_R_E0_E :-
		       arg(N,A,ArgA), arg(N,NR,ArgR),
		       Nf_ArgA_ArgR_E0_E1,
		       N1 is N+1, NfHnfArgs_N1_NA_A_NR_R_E1_E))
	  ; writeClause((NfHnfArgs_N_NA_A_NR_R_E0_E :-
		       arg(N,A,ArgA), arg(N,NR,ArgR),
		       Nf_ArgA_ArgR_E0_E1,
		       N1 is N+1,
		       freeze(E1,((nonvar(ArgR),ArgR='FAIL'(_))
				  -> R=ArgR, E1=E
				   ; NfHnfArgs_N1_NA_A_NR_R_E1_E))))),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of clauses for create, propagate, and dereference sharing
	
genMakeFunctionShare(Suffix) :-
	appendAtom(makeShare,Suffix,MakeShare),
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	MakeShare_X_ShM =.. [MakeShare,X,Share_M],
	writeClause((MakeShare_X_ShM :- create_mutable(X,M))),
	nl.

transpropshar(Suffix) :-
	appendAtom(propagateShare,Suffix,PropShare),
	appendAtom(propagateShareArg,Suffix,PropShareArg),
	PropShare_X_X =.. [PropShare,X,X],
	writeClause((PropShare_X_X :- var(X), !)),
	PC1=partcall(N,F,PArg),
	PC2=partcall(N,F,SPArg),
	PropShare_PC_PC =.. [PropShare,PC1,PC2],
	writeClause((PropShare_PC_PC :- !, map2M(user:PropShareArg,PArg,SPArg))),
	IP='Network.Ports.InternalPort'(_,_,_,_),
	PropShare_IP_IP =.. [PropShare,IP,IP],
	writeClause((PropShare_IP_IP :- !)),
	Str='$stream'(_),
	PropShare_Str_Str =.. [PropShare,Str,Str],
	writeClause((PropShare_Str_Str :- !)),
	(printConsFailure(no) -> true
	 ; Fail='FAIL'(_),
	   PropShare_Fail_Fail =.. [PropShare,Fail,Fail],
	   writeClause((PropShare_Fail_Fail :- !))),
	writeClause((PropShare_X_X :- number(X),!)),
	PropShare_A_R =.. [PropShare,A,R],
	appendAtom(propagateShareArgs,Suffix,PropShareArgs),
	PropShareArgs_1_NA_A_R =.. [PropShareArgs,1,NA,A,R],
	writeClause((PropShare_A_R :-
		       functor(A,FA,NA), functor(R,FA,NA),
		       PropShareArgs_1_NA_A_R)),
	nl,
	PropShareArg_ArgA_ArgR =.. [PropShareArg,ArgA,ArgR],
	PropShareArgs_N_NA_A_R =.. [PropShareArgs,N,NA,A,R],
	PropShareArgs_N1_NA_A_R =.. [PropShareArgs,N1,NA,A,R],
	writeClause((PropShareArgs_N_NA_A_R :- N>NA,!)),
	writeClause((PropShareArgs_N_NA_A_R :-
		       arg(N,A,ArgA), arg(N,R,ArgR),
		       PropShareArg_ArgA_ArgR,
		       N1 is N+1, PropShareArgs_N1_NA_A_R)),
	nl,
	PropShareArg_A_A =.. [PropShareArg,A,A],
	writeClause((PropShareArg_A_A :- var(A), !)),
	writeClause((PropShareArg_A_A :-
                       functor(A,'Network.Ports.InternalPort',4), !)),
	PropShareArg_A_SA =.. [PropShareArg,A,SA],
	appendAtom(makeShare,Suffix,MakeShare),
	MakeShare_A_SA =.. [MakeShare,A,SA],
	writeClause((PropShareArg_A_SA :- MakeShare_A_SA)),
	nl,
	MakeShare_X_X =.. [MakeShare,X,X],
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	MakeShare_X_Y =.. [MakeShare,X,Y],
	MakeShare_X_ShM =.. [MakeShare,X,Share_M],
	writeClause((MakeShare_X_X :- var(X), !)),
	writeClause((MakeShare_X_Y :- atomic(X), !, (functiontype(X,_,0,_,_,_) -> create_mutable(X,M), Y=Share_M ; Y=X))),
	writeClause((MakeShare_X_X :- X=Share_M, !)),
	writeClause((MakeShare_X_ShM :- create_mutable(X,M))),
	nl.

transDeref(CCs) :-
	write('% dereference a term, i.e., remove all top-level sharing structures:'),
	nl,
	writeClause((derefRoot(R,V) :- var(R), !, V=R)),
	map1M(compiler:transDerefClause,CCs),
	writeClause((derefRoot('FAIL'(Src),_) :- !,
	                                      evaluator:writeFailSource(Src))),
	writeClause(derefRoot(R,R)),
	nl,
	write('% completely dereference a term, i.e., remove all sharing structures'),
	nl,
	write('% also inside subterms:'), nl,
	writeClause((derefAll(R,V) :- var(R), !, V=R)),
	map1M(compiler:transDerefAllClause,CCs),
	writeClause((derefAll(R,V) :- functor(R,F,N), functor(V,F,N), derefArgs(N,R,V))),
	writeClause((derefArgs(0,_,_) :- !)),
	writeClause((derefArgs(I,R,V) :-
		         arg(I,R,RI), derefAll(RI,VI), arg(I,V,VI),
		         I1 is I-1, derefArgs(I1,R,V))),
	nl.

transDerefClause(Suffix) :-
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	writeClause((derefRoot(Share_M,V) :- !,
	             get_mutable(E,M), (E='$eval'(R) -> V=R ; derefRoot(E,V)))).

transDerefAllClause(Suffix) :-
	appendAtom(share,Suffix,Share),
	Share_M =.. [Share,M],
	writeClause((derefAll(Share_M,V) :- !,
	       get_mutable(E,M), (E='$eval'(R) -> derefAll(R,V) ; derefAll(E,V)))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translation of expressions:
% here we assume that all cases and ors occur only outermost and not
% at nested inner positions
transExp(FName,Aux,Patterns,Vars,Cut,'Case'(CType,CaseExp,Cases)) :- !,
	(CaseExp='Var'(X)
	 -> % case expression is variable:
	    selectVar(X,Vars,I,VarsX),
	    % check whether the case variable occurs in any case branch
	    (occursInBranches(X,Cases)
              -> CaseTerm=ShareX, UMShares=[makeShare(X,ShareX)],
                 map2partialM(compiler:replaceShareVars(UMShares),Vars,NewVars),
                 map2partialM(compiler:insertShareInBranch(UMShares),Cases,
                                                           ShareCases)
                ; NewVars=VarsX, CaseTerm=X, UMShares=[], ShareCases=Cases),
	    number_codes(I,IS), atom_codes(IA,IS) % transform number into atom
	  ; % case expression is complex (not a variable)
	    (compileWithSharing(variable)
	     -> %exp2ShareExpr(CaseExp,UMShares,SCExp),
		getSharedVarsOfCaseArg('Case'(CType,CaseExp,Cases),UMShares),
		insertShare(CaseExp,UMShares,SCExp),
		map2partialM(compiler:replaceShareVars(UMShares),Vars,NewVars),
		exp2Term(NewVars,SCExp,CaseTerm),
		map2partialM(compiler:insertShareInBranch(UMShares),Cases,ShareCases)
	      ; (compileWithSharing(function)
	         -> exp2FuncShareTerm(Vars,CaseExp,UMShares,CaseTerm),
		    NewVars=Vars, ShareCases=Cases
	          ; exp2Term(Vars,CaseExp,CaseTerm),
		    UMShares=[], NewVars=Vars, ShareCases=Cases)),
	    IA='ComplexCase'  % naming needs to be improved!
	),
	atom_codes(FName,FNameS), getCostCenterOfName(FNameS,CC),
	map2partialM(compiler:addSuffix2MakeShare(CC),UMShares,UShares),
	append(Patterns,[H,E0,E],LArgs),
	LHS =.. [FName|LArgs],
	appendAtom(FName,Aux,FNaux),
	appendAtom(FNaux,'_',FNameUS),
	appendAtom(FNameUS,IA,F_I),
	(var(CaseTerm) -> replaceEq(CaseTerm,Y,NewVars,ArgVars)
	                ; ArgVars=NewVars),
	append(ArgVars,[H,E1,E],F_I_Args),
	F_I_Call =.. [F_I,Y|F_I_Args],
	(Cut=withcut -> CutUShares=['!'|UShares] ; CutUShares=UShares),
	writeClauseWithInitGoals(LHS,CutUShares,
	                         (hnf(CaseTerm,Y,E0,E1), F_I_Call)),
	nl,
	length(NewVars,LenNewVars), NumVars is LenNewVars+1,
	F_I_Arity is NumVars+3,
	F_I_Arity1 is NumVars+2,
	(CType='Rigid'
	 -> genBlockDecl(F_I,F_I_Arity,[1,F_I_Arity1],NewF_I)
	  ; % CType='Flex'
	    genBlockDecl(F_I,F_I_Arity,[F_I_Arity1],NewF_I)),
	transCases(NewF_I,NewVars,ShareCases,CType,FName).

transExp(FName,Aux,Patterns,Vars,_,'Or'(Exp1,Exp2)) :- !,
	appendAtom(Aux,'_or1',Aux1),
	appendAtom(Aux,'_or2',Aux2),
	transExp(FName,Aux1,Patterns,Vars,nocut,Exp1),
	transExp(FName,Aux2,Patterns,Vars,nocut,Exp2).

transExp(FName,Aux,Patterns,Vars,Cut,'Comb'('FuncCall',"commit",[Exp])) :- !,
	writeErr('ERROR: "'),
	writeErr(FName),
	writeLnErr(' eval choice" not yet supported!'),
	transExp(FName,Aux,Patterns,Vars,Cut,Exp),
	setFlcBug.

transExp(FName,_Aux,Patterns,Vars,Cut,Exp) :-
	append(Patterns,[H,E0,E],LArgs),
	LHS =.. [FName|LArgs],
	(compileWithSharing(variable)
	 -> exp2ShareExpr(Exp,UMShares,SExp),
            map2partialM(compiler:replaceShareVars(UMShares),Vars,NewVars),
	    exp2Term(NewVars,SExp,Term)
	  ; (compileWithSharing(function)
	     -> exp2FuncShareTerm(Vars,Exp,UMShares,Term)
	      ; exp2Term(Vars,Exp,Term), UMShares=[])),
	atom_codes(FName,FNameS), getCostCenterOfName(FNameS,CC),
	map2partialM(compiler:addSuffix2MakeShare(CC),UMShares,UShares),
	(Cut=withcut -> CutUShares=['!'|UShares] ; CutUShares=UShares),
	(isConstructorRooted(Exp)
	 -> % right-hand side is constructor-rooted -> no hnf call necessary:
            H=Term,
	    (UShares=[]
		-> E0=E,
		   (Cut=withcut -> writeClause((LHS :- !))
                                 ; writeClause(LHS))
		 ; writeClauseWithInitGoals(LHS,CutUShares,E0=E))
	 ; ((var(Term) ; hnfTailCallOptim(no) ; (\+ localFunCall(FName,Exp)))
	    -> writeClauseWithInitGoals(LHS,CutUShares,hnf(Term,H,E0,E))
             ; % if the right-hand side is operation-rooted ->
	       % generate direct call to root predicate:
  	       functor(Term,TF,TA),
	       Term =.. [TF|TArgs],
	       append(TArgs,[H,E0,E],TArgsH),
	       externalFuncs(EFs),
	       (member((TF/TA,PrimName),EFs)
	        -> append(_,[32|TPS],PrimName), atom_codes(TP,TPS) ; TP=TF),
	       TPCall =.. [TP|TArgsH],
               writeClauseWithInitGoals(LHS,CutUShares,TPCall) )).

localFunCall(DefFuncName,'Comb'('FuncCall',FName,_)) :-
	atom_codes(DefFuncName,DefFuncNameS),
	fromSameModule(DefFuncNameS,FName), !.
localFunCall(DefFuncName,'Free'(_,Exp)) :- % since we ignore Constr in the code:
	localFunCall(DefFuncName,Exp).

fromSameModule(F1,F2) :-
	append(Mod,[46|_],F1),
	append(Mod,[46|_],F2), !.

isConstructorRooted('Lit'(_)).
isConstructorRooted('Comb'('ConsCall',_,_)).
isConstructorRooted('Free'(_,Exp)) :- % since we ignore Constr in the code:
	isConstructorRooted(Exp).


transCases(_,_,[],_,_) :- failForwarding(no), !.
transCases(F_I,VarsX,[],CType,FName) :- % generate clause for Fail forwarding:
	(failCheckFunc(no)
	 -> append(['FAIL'(Src)|VarsX],['FAIL'([FName|Src]),E,E],ClauseArgs)
	  ; append(['FAIL'(Src)|VarsX],['FAIL'(Src),E,E],ClauseArgs)),
	LHS =.. [F_I|ClauseArgs],
	% avoid instantiation with FAIL in case of flexible branches:
	(CType='Flex' -> RHS=nonvar(Src) ; RHS=true),
	writeClause((LHS :- RHS)).
transCases(F_I,VarsX,['Branch'('Pattern'(Name,ConsVars),Exp)|Cases],
	   CType,FName) :-
	atom_codes('reportFailure4PAKCS',FailFuncName),
	((Exp='Comb'('FuncCall',FailFuncName,_), CType='Flex')
	 -> NCType='Rigid'
	  ; NCType=CType),
	flatName2Atom(Name,Cons),
	Pattern =.. [Cons|ConsVars],
	append(ConsVars,VarsX,NewVars),
	atomic2Atom(Cons,ConsA),
	appendAtom('_',ConsA,Aux),
	(noFurtherNonFailingCase(CType,Cases) -> Cut=withcut ; Cut=nocut),
	transExp(F_I,Aux,[Pattern|VarsX],NewVars,Cut,Exp),
	transCases(F_I,VarsX,Cases,NCType,FName).
transCases(F_I,VarsX,['Branch'('LPattern'(Lit),Exp)|Cases],CType,FName) :-
	transCaseLit2Cons(Lit,Cons),
	atomic2Atom(Cons,ConsA),
	appendAtom('_',ConsA,Aux),
	transExp(F_I,Aux,[Cons|VarsX],VarsX,nocut,Exp),
	transCases(F_I,VarsX,Cases,CType,FName).

% are the further case branches only failure branches?
noFurtherNonFailingCase(_,[]).
noFurtherNonFailingCase(CType,['Branch'('Pattern'(_,_),
                                        'Comb'('FuncCall',FuncName,_))|Bs]) :-
        CType='Flex',
	atom_codes('reportFailure4PAKCS',ReportFailFuncName),
	atom_codes('Prelude.failed',FailedFuncName),
        (FuncName = ReportFailFuncName ; FuncName = FailedFuncName), !,
        noFurtherNonFailingCase(CType,Bs).

% extract constructor of literal:
transCaseLit2Cons('Intc'(I),I) :- !.
transCaseLit2Cons('Floatc'(F),F) :- !.
transCaseLit2Cons('Charc'(N),C) :- !, char_int(C,N).
transCaseLit2Cons(Lit,Lit) :-
	writeErr('ERROR in FlatCurry file: Illegal argument "'),
	writeErr(Lit),
	writeLnErr('" in case branch!'),
	pleaseReport.

% translate FlatCurry expression into corresponding Prolog term:
% (first argument are the currently visible variables, only used
% to check inconsistencies in FlatCurry)
exp2Term(Vars,'Var'(V),V) :- !,
	(memberEq(V,Vars) -> true
	  ; writeErr('ERROR in FlatCurry file in function "'),
	    currentFunction(FuncName), writeErr(FuncName),
	    writeLnErr('":'),
	    writeErr('variable "'),
	    writeErr(V),
	    writeLnErr('" does not occur in left-hand side.'),
	    reportLiftBug,
	    put_code(37), write('ERROR in FlatCurry file: variable "'),
	    write(V),
	    write('" does not occur in left-hand side!'), nl).
exp2Term(_,'Lit'('Intc'(I)),I) :- !.
exp2Term(_,'Lit'('Floatc'(F)),F) :- !.
exp2Term(_,'Lit'('Charc'(N)),C) :- !, char_int(C,N).
exp2Term(_,'Lit'('Ident'(S)),A) :- !,
	writeErr('ERROR in FlatCurry file in function "'),
	currentFunction(FuncName), writeErr(FuncName),
	writeLnErr('":'),
	flatName2Atom(S,A),
	writeErr('Expression "'),
	writeErr('Lit'('Ident'(A))),
	writeLnErr('" should not occur in FlatCurry expressions.'),
	pleaseReport,
	put_code(37), write('ERROR in FlatCurry file: Expression "'),
	write('Lit'('Ident'(A))),
	write('" should not occur in FlatCurry expressions.'),
	nl.
exp2Term(Vars,'Comb'(CombType,NameS,Exprs),Term) :- !,
	flatName2Atom(NameS,Name),
	checkForDeprecatedFunction(Name),
	checkForIllegalOperationUsage(Name),
	checkForTupleArity(NameS),
	map2partialM(compiler:exp2Term(Vars),Exprs,Terms),
	%length(Terms,TArity),
	((CombType='ConsCall' ; CombType='ConsPartCall'(_))
          %getConsArity(Name,Arity)
	 -> Missing=0 % constructors are not represented by partial applications
	 ; (CombType='FuncCall' -> Missing=0
                                 ; CombType='FuncPartCall'(Missing))),
           %(getFuncArity(Name,Arity) -> Missing is Arity-TArity ; Missing=0)),
	(Missing=0 -> Term =.. [Name|Terms] ;
         (Missing>0 -> rev(Terms,RevTerms), Term = partcall(Missing,Name,RevTerms)
	             ; writeLnErr('INTERNAL COMPILER ERROR: over-application occured in exp2Term!'),
	               writeErr('Function: '),
	               currentFunction(QFunc), writeLnErr(QFunc),
	               writeErr('Expression: '),
	               writeLnErr('Comb'(CombType,NameS,Exprs)),
	               setFlcBug)).
exp2Term(Vars,'Free'(LocalVars,Exp),ET) :- !,
	append(Vars,LocalVars,NewVars),
	exp2Term(NewVars,Exp,ET).
exp2Term(_,Expr,'***unknown expression***') :-
	writeErr('ERROR in FlatCurry file: Unknown expression "'),
	writeErr(Expr),
	writeLnErr('" in FlatCurry file!'),
	pleaseReport.


% implement sharing scheme for variable sharing according to [Antoy/Hanus FroCos'2000]
% replace multiple variables by new variables connected to the old
% ones via a makeShare goal:
exp2ShareExpr(Exp,Shares,SExp) :-
	getSharedVars(Exp,Shares),
	insertShare(Exp,Shares,SExp).

insertShare('Var'(V),Shares,Share) :- !,
	varToShare(V,Shares,Share).
insertShare('Lit'(L),_,'Lit'(L)) :- !.
insertShare('Comb'(CT,Name,Exprs),Shares,'Comb'(CT,Name,SExprs)) :- !,
	insertShares(Exprs,Shares,SExprs).
insertShare('Free'(Vars,Exp),Shares,'Free'(NewVars,SExp)) :- !,
        map2partialM(compiler:replaceShareVars(Shares),Vars,NewVars),
	insertShare(Exp,Shares,SExp).
insertShare('Case'(CType,Exp,Branches),Shares,'Case'(CType,SExp,SBranches)) :-
	!,
	insertShare(Exp,Shares,SExp),
	map2partialM(compiler:insertShareInBranch(Shares),Branches,SBranches).
insertShare('Or'(Exp1,Exp2),Shares,'Or'(SExp1,SExp2)) :-
	!,
	insertShare(Exp1,Shares,SExp1),
	insertShare(Exp2,Shares,SExp2).
insertShare(Exp,_,Exp) :-
	writeLnErr('INTERNAL ERROR in "insertShare"!'),
	writeLnErr('Unknown expression in FlatCurry file:'),
	writeLnErr(Exp),
	pleaseReport.

insertShareInBranch(Shares,'Branch'(Pattern,Exp),'Branch'(Pattern,SExp)) :-
        insertShare(Exp,Shares,SExp).

insertShares([],_,[]).
insertShares([T|Ts],Shares,[ST|STs]) :-
	insertShare(T,Shares,ST),
	insertShares(Ts,Shares,STs).

varToShare(V,[],'Var'(V)).
varToShare(V,[makeShare(W,NV)|_],'Var'(NV)) :- V==W, !.
varToShare(V,[_|Shares],Share) :- varToShare(V,Shares,Share).

replaceShareVars([],V,V).
replaceShareVars([makeShare(V,NV)|_],W,NV) :- V==W, !.
replaceShareVars([_|UVs],W,NV) :- replaceShareVars(UVs,W,NV).

% implement sharing scheme for function sharing (classical technique):
exp2TermInMakeShare(Vars,makeShare(Exp,V),makeShare(Term,V)) :-
	exp2Term(Vars,Exp,Term).

exp2FuncShareTerm(Vars,Exp,Shares,Term) :-
	exp2FuncShareTerm(top,Vars,Exp,SharesR,Term),
	rev(SharesR,Shares).

exp2FuncShareTerm(_,Vars,'Var'(V),[],V) :- !,
	(memberEq(V,Vars) -> true
	  ; writeErr('ERROR in FlatCurry file in function "'),
	    currentFunction(FuncName), writeErr(FuncName),
	    writeLnErr('":'),
	    writeErr('variable "'),
	    writeErr(V),
	    writeLnErr('" does not occur in left-hand side.'),
	    reportLiftBug,
	    put_code(37), write('ERROR in FlatCurry file: variable "'),
	    write(V),
	    write('" does not occur in left-hand side!'), nl).
exp2FuncShareTerm(_,_,'Lit'('Intc'(I)),[],I) :- !.
exp2FuncShareTerm(_,_,'Lit'('Floatc'(F)),[],F) :- !.
exp2FuncShareTerm(_,_,'Lit'('Charc'(N)),[],C) :- !, char_int(C,N).
exp2FuncShareTerm(_,_,'Lit'('Ident'(S)),[],A) :- !,
	writeErr('ERROR in FlatCurry file in function "'),
	currentFunction(FuncName), writeErr(FuncName),
	writeLnErr('":'),
	flatName2Atom(S,A),
	writeErr('Expression "'),
	writeErr('Lit'('Ident'(A))),
	writeLnErr('" should not occur in FlatCurry expressions.'),
	pleaseReport,
	put_code(37), write('ERROR in FlatCurry file: Expression "'),
	write('Lit'('Ident'(A))),
	write('" should not occur in FlatCurry expressions.'),
	nl.
exp2FuncShareTerm(Level,Vars,'Comb'(CombType,NameS,Exprs),NewShares,NewTerm) :- !,
	exp2FuncShareTerms(Vars,Exprs,Shares,Terms),
	flatName2Atom(NameS,Name),
	checkForDeprecatedFunction(Name),
	checkForIllegalOperationUsage(Name),
	checkForTupleArity(NameS),
	length(Terms,TArity),
	(getConsArity(Name,Arity)
	 -> Missing=0 % constructors are not represented by partial applications
	 ; (getFuncArity(Name,Arity) -> Missing is Arity-TArity ; Missing=0)),
	(Missing=0 -> Term =.. [Name|Terms] ;
         (Missing>0 -> rev(Terms,RevTerms), Term = partcall(Missing,Name,RevTerms)
	             ; writeLnErr('INTERNAL COMPILER ERROR: over-application occured in exp2FuncShareTerm!'))),
	((CombType='FuncCall', Level=subterm)
	 -> NewShares = [makeShare(Term,V)|Shares], NewTerm = V
	  ; NewShares = Shares, NewTerm = Term).
exp2FuncShareTerm(Level,Vars,'Free'(LocalVars,Exp),Shares,Term) :- !,
	append(Vars,LocalVars,NewVars),
	exp2FuncShareTerm(Level,NewVars,Exp,Shares,Term).
exp2FuncShareTerm(_,_,Exp,[],'***unknown expression***') :-
	writeLnErr('INTERNAL ERROR in "exp2FuncShareTerm"!'),
	writeLnErr('Unknown expression in FlatCurry file:'),
	writeLnErr(Exp),
	pleaseReport.

exp2FuncShareTerms(_,[],[],[]).
exp2FuncShareTerms(Vars,[T|Ts],Shares,[ST|STs]) :-
	exp2FuncShareTerm(subterm,Vars,T,Shares1,ST),
	exp2FuncShareTerms(Vars,Ts,Shares2,STs),
	append(Shares1,Shares2,Shares).

% add suffix to "makeShare":
addSuffix2MakeShare(Suffix,makeShare(V,NV),MSs) :-
	appendAtom(makeShare,Suffix,MakeShare),
	MSs =.. [MakeShare,V,NV].


% get the variables in a term which occurs more than once and must be shared:
getSharedVars(Exp,Shares) :-
	countVarsInTerm(Exp,[],Vars),
	varsIntoShares(Vars,Shares).

% get the variables of a case argument which occurs more tha once in the case expression
% and, thus, must be shared:
getSharedVarsOfCaseArg('Case'(_,CaseExp,Branches),Shares) :-
	countVarsInTerm(CaseExp,[],ArgVars),
	countVarsInBranches(Branches,ArgVars,CaseVars),
	prefixVars(ArgVars,CaseVars,MultArgVars),
	varsIntoShares(MultArgVars,Shares).

% prefixVars(Xs,Ys,Zs): Zs is prefix of Ys that has the same length as Xs
prefixVars([],_,[]).
prefixVars([_|Xs],[Y|Ys],[Y|Zs]) :- prefixVars(Xs,Ys,Zs).
	
varsIntoShares([],[]).
varsIntoShares([V/C|Vs],[makeShare(V,_)|Shs]) :-
	C>1, !,
	  % for statistics:
	  retract(numberOfShares(SC)),
	  SC1 is SC+1,
	  asserta(numberOfShares(SC1)),
	varsIntoShares(Vs,Shs).
varsIntoShares([_|Vs],Shs) :-
	varsIntoShares(Vs,Shs).

countVarsInTerm('Var'(V),Vars,IVars) :- !, incVarCount(V,Vars,IVars).
countVarsInTerm('Lit'(_),Vars,Vars) :- !.
countVarsInTerm('Comb'(_,_,Terms),Vars,IVars) :- !,
	countVarsInTerms(Terms,Vars,IVars).
countVarsInTerm('Free'(_,Exp),Vars,IVars) :- !,
	countVarsInTerm(Exp,Vars,IVars).
countVarsInTerm('Case'(_,CaseExp,Branches),Vars,IVars) :- !,
	countVarsInTerm(CaseExp,Vars,CVars),
	countVarsInBranches(Branches,CVars,IVars).
countVarsInTerm('Or'(E1,E2),Vars,IVars) :- !,
	countVarsInTerm(E1,Vars,Vars1),
	countVarsInTerm(E2,Vars1,IVars).
countVarsInTerm(Exp,Vars,Vars) :- !,
	writeLnErr('INTERNAL ERROR in "countVarsInTerm"!'),
	writeLnErr('Unknown expression in FlatCurry file:'),
	writeLnErr(Exp),
	pleaseReport.

countVarsInTerms([],Vars,Vars).
countVarsInTerms([X|Xs],Vars,IVars) :-
	countVarsInTerm(X,Vars,XVars),
	countVarsInTerms(Xs,XVars,IVars).

countVarsInBranches([],Vars,Vars).
countVarsInBranches(['Branch'(_,X)|Xs],Vars,IVars) :-
	countVarsInTerm(X,Vars,XVars),
	countVarsInBranches(Xs,XVars,IVars).

incVarCount(V,[],[V/1]).
incVarCount(V,[X/T|Vars],[X/T1|Vars]) :- V==X, !, T1 is T+1.
incVarCount(V,[X/T|Vars],[X/T|IVars]) :- incVarCount(V,Vars,IVars).



% select a variable in a list of variables.
% return position in the list and list of variables without this variable
selectVar(X,[Y|Ys],1,Ys) :- X==Y, !.
selectVar(X,[Y|Ys],XNr,[Y|Zs]) :- selectVar(X,Ys,Nr,Zs), XNr is Nr+1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check correctness of FlatCurry rules and expressions.
% This checking is redundant and should only ensure
% that the front-end produces syntactically correct code.

check_flcFunction('Func'(Name,_,_,_,Rule)) :-
	flatName2Atom(Name,FName),
	retract(currentFunction(_)),
	decodePrologName(FName,FlatName),
	asserta(currentFunction(FlatName)),
        check_flcRule(Rule), !.
check_flcFunction(_) :-
	writeErr('INTERNAL ERROR in FlatCurry file "'),
	writeErr('" in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	pleaseReport.

check_flcRule('Rule'(Args,Expr)) :-
	map1M(user:integer,Args),
	check_flcExpr(Expr).
check_flcRule('External'(Name)) :-
	check_flcString(Name).

check_flcString(S) :- map1M(user:integer,S).

check_flcCaseType('Rigid').
check_flcCaseType('Flex').

check_flcCombType('FuncCall').
check_flcCombType('ConsCall').
check_flcCombType('FuncPartCall'(_)).
check_flcCombType('ConsPartCall'(_)).

check_flcExpr('Var'(_)) :- !.
check_flcExpr('Lit'(L)) :- !, check_flcLit(L).
check_flcExpr('Comb'(CT,Name,Es)) :- !,
	check_flcCombType(CT),
	check_flcString(Name),
	map1M(compiler:check_flcExpr,Es).
check_flcExpr('Free'(Xs,E)) :- !,
	map1M(user:integer,Xs),
	check_flcExpr(E).
check_flcExpr('Let'(Bindings,E)) :- !,
	map1M(compiler:check_flcBinding,Bindings),
	check_flcExpr(E).
check_flcExpr('Or'(E1,E2)) :- !,
	check_flcExpr(E1), check_flcExpr(E2).
check_flcExpr('Case'(CT,E,Cs)) :- !,
	check_flcCaseType(CT),
	check_flcExpr(E),
	map1M(compiler:check_flcCase,Cs).
check_flcExpr(E) :-
	writeErr('ERROR in FlatCurry file: Illegal expression "'),
	writeErr(E),
	writeErr('" in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	reportLiftBug.

check_flcBinding('Prelude.(,)'(V,E)) :- integer(V), check_flcExpr(E).

check_flcLit('Intc'(_)) :- !.
check_flcLit('Floatc'(_)) :- !.
check_flcLit('Charc'(_)) :- !.
check_flcLit(Lit) :-
	writeErr('ERROR in FlatCurry file: Illegal literal "'),
	writeErr(Lit),
	writeErr('" in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	pleaseReport.

check_flcCase('Branch'('Pattern'(Name,Xs),E)) :- !,
	check_flcString(Name),
	map1M(user:integer,Xs),
	check_flcExpr(E).
check_flcCase('Branch'('LPattern'(C),E)) :- !,
	check_flcLit(C),
	check_flcExpr(E).
check_flcCase(Branch) :-
	writeErr('ERROR in FlatCurry file: Illegal case pattern "'),
	writeErr(Branch),
	writeErr('" in function "'),
	currentFunction(FName), writeErr(FName),
	writeLnErr('"!'),
	pleaseReport.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries:

% generate N new free variables:
genVars(N,[]) :- N=0, !.
genVars(N,[_|Xs]) :- N1 is N-1, genVars(N1,Xs).

writeNTimes(0,_) :- !.
writeNTimes(N,S) :- N>0, write(S), N1 is N-1, writeNTimes(N1,S).

% write a clause and puts some initialization goals at the beginning
% of the right-hand side:
writeClauseWithInitGoals(LHS,Init,RHS) :-
	rev(Init,RInit),
	writeClauseWithRevInitGoals(LHS,RInit,RHS).
writeClauseWithRevInitGoals(LHS,[],RHS) :-
	writeClause((LHS :- RHS)).
writeClauseWithRevInitGoals(LHS,[G|Gs],RHS) :-
	writeClauseWithRevInitGoals(LHS,Gs,(G,RHS)).

writeClause(C) :- deleteLastTrue(C,C1), writeqWithVars(C1), write('.'), nl.

% delete a possibly generated last literal "true" in the clause:
deleteLastTrue((Head :- true),Head) :- !.
deleteLastTrue((Head :- B),(Head :- B1)) :- deleteLastTrueInBody(B,B1).
deleteLastTrue(Head,Head).

deleteLastTrueInBody((L,true),L) :- !.
deleteLastTrueInBody((L,B),(L,B1)) :- deleteLastTrueInBody(B,B1).


% replace all dots by underscores in an atom:
replaceDotByUnderscore(DA,UA) :-
	atom_codes(DA,DAS),
	replaceDotsByUnderscores(DAS,UAS),
	atom_codes(UA,UAS).
replaceDotsByUnderscores(DAS,UAS) :-
	append(S1,[46|S2],DAS), !,
	replaceDotsByUnderscores(S2,S3),
	append(S1,[95|S3],UAS).
replaceDotsByUnderscores(S,S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check the header of a generated (Prolog) program and delete it
% if it is not appropriate:
checkProgramHeader(PrologProg) :-
	existsFile(PrologProg),
	% catch read errors:
	on_exception(_Exc,readProgramHeader(PrologProg,Header),
		     (deletePrologTarget(PrologProg),fail)),
	(checkHeaderParams(Header) -> true ; deletePrologTarget(PrologProg)),
	!.
checkProgramHeader(_).

% delete an existing Prolog target file:
deletePrologTarget(PrologProg) :-
	existsFile(PrologProg),
	writeErrNQ('Deleting old target file \''),
	writeErrNQ(PrologProg),
	writeLnErrNQ('\'.'),
	tryDeleteFile(PrologProg), !.
deletePrologTarget(_).

% check header parameters for current settings:
checkHeaderParams(Header) :-
	compilerVersion(CV),
	atom_codes(CV,CVL),
	member([37|CVL],Header), % 37 = '%'
	prologMajor(Prolog), atom_codes(Prolog,PrologS),
	member(PrologS,Header),
	(compileWithSharing(variable) -> member("VARIABLESHARING",Header)
	                               ; \+ member("VARIABLESHARING",Header)),
	(compileWithSharing(function) -> member("FUNCTIONSHARING",Header)
	                               ; \+ member("FUNCTIONSHARING",Header)).

% read the header of a generated (Prolog) program:
readProgramHeader(Prog,Header) :-
	open(Prog,read,ProgStream),
	readStreamLine(ProgStream,Line),
	close(ProgStream),
	split2words(Line,Header).


