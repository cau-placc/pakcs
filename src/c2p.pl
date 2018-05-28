%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Curry2Prolog interactive system.
%

:- use_module(prologbasics).
:- use_module(pakcsversion).
:- use_module(basics).
:- use_module(version).
:- use_module(loader).
:- use_module(evaluator).
:- use_module(compiler). % compiler from FlatCurry into Prolog

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic compileWithCompact/1,
	   parser_warnings/1, parserOptions/1,
	   freeVarsUndeclared/1, varDefines/1,
	   addImports/1, safeMode/1.

compileWithCompact([]).  % parsecurry options for compactification
parser_warnings(yes). % no if the warnings of the parser should be suppressed
parserOptions(''). % additional options passed to front end
freeVarsUndeclared(no). % yes if free variables need not be declared in initial goals
addImports([]). % additional imports defined by the ":add" command
varDefines([]). % list of top-level bindings
safeMode(no). % safe execution without IO actions at top level?

% Read the PAKCS rc file to define some constants
readRcFile(ArgProps) :-
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
        getHomeDirectory(Home),
	appendAtom(Home,'/.pakcsrc',HomeConfigFile),
	% first, try to install local .pakcsrc file:
        (existsFile(HomeConfigFile)
	 -> readConfigFile(HomeConfigFile,HomeProps)
	  ; appendAtoms(['cp ',ConfigFile,' ',HomeConfigFile],CpCmd),
	    shellCmd(CpCmd),
	    writeNQ('>>> '),
	    writeNQ(HomeConfigFile), writeNQ(' installed.'), nlNQ,
	    HomeProps=[] ),
	(existsFile(ConfigFile)
	 -> readConfigFile(ConfigFile,GlobalProps),
	    updateConfigFile(ConfigFile,HomeProps,HomeConfigFile)
	  ; GlobalProps=[]),
	concat([ArgProps,HomeProps,GlobalProps],AllProps),
	deletePropDups(AllProps,UniqueProps),
	map1M(basics:assertPakcsrc,UniqueProps), !,
	(pakcsrc(verboserc,yes)
         -> writeNQ('>>> Reading RC files:'),
            (existsFile(HomeConfigFile)
             -> writeNQ(' '), writeNQ(HomeConfigFile) ; true),
            (existsFile(ConfigFile)
             -> writeNQ(' '), writeNQ(ConfigFile) ; true),
            nlNQ,
            writeNQ('Current configurations: '), nlNQ,
            writeRCvalues
          ; true).
readRcFile(ArgProps) :-
	% maybe the environment variable HOME is not set since the system
	% is not started by a regular user:
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
	(existsFile(ConfigFile)
	 -> readConfigFile(ConfigFile,GlobalProps)
	  ; GlobalProps=[]),
	concat([ArgProps,GlobalProps],AllProps),
	deletePropDups(AllProps,UniqueProps),
	map1M(basics:assertPakcsrc,UniqueProps).


deletePropDups([],[]).
deletePropDups([prop(Name,Value)|Props],[prop(Name,Value)|DProps]) :-
	deleteEqualProps(Name,Props,Props1),
	deletePropDups(Props1,DProps).

deleteEqualProps(_,[],[]).
deleteEqualProps(Name,[prop(Name,_)|Props],DProps) :- !,
	deleteEqualProps(Name,Props,DProps).
deleteEqualProps(Name,[Prop|Props],[Prop|DProps]) :-
	deleteEqualProps(Name,Props,DProps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start main interactive environment:
pakcsMain :-
	getProgramArgs(DArgs),
	processDArgs(DArgs,Props,Args),
	readRcFile(Props),
	pakcsrc(defaultparams,DefParamA),
	atom_codes(DefParamA,DefParamS),
	split2words(DefParamS,DefParamsS),
	map2M(prologbasics:atomCodes,DefParamsA,DefParamsS),
	processArgs(no,DefParamsA), % process default parameters from .pakcsrc
	processArgs(no,Args), % process current parameters
	rtArgs(RTArgs),
        exitCode(EC),
        (EC=0 -> true ; halt(EC)), % halt if error occurred
	((RTArgs=[], \+ verbosityIntermediate) -> true
	  ; writeNQ('Run-time parameters passed to application: '),
	    writeNQ(RTArgs), nlNQ),
	(verbosityNotQuiet
         -> printPakcsHeader, nlNQ,
            writeNQ('Type ":h" for help (contact: pakcs@curry-language.org)'),
            nlNQ
          ; true),
	flush_output,
	main.
pakcsMain :- halt(1).  % halt if failure (in parameters) occurred


% extract the initial arguments of the form "-Dprop=value" as a property list:
processDArgs([],[],[]).
processDArgs([Arg|DArgs],[prop(Name,Val)|Props],Args) :-
	atom_codes(Arg,[45,68|Def]), !, % [45,68] = "-D"
	append(NameS,[61|ValS],Def), % 61 = '='
	atom_codes(Name,NameS),
	atom_codes(Val,ValS),
	processDArgs(DArgs,Props,Args).
processDArgs(Args,[],Args).

% process the remaining run-time arguments:
processArgs(Halt,[]) :- Halt=yes -> halt(0) ; true.
processArgs(Halt,['--nocypm'|Args]) :-
	processArgs(Halt,Args).            % ignore since already processed
processArgs(Halt,['--noreadline'|Args]) :-
	processArgs(Halt,Args).            % ignore since already processed
processArgs(_,[Arg|Args]) :-
	(Arg='--version' ; Arg='-V'), !, % show version and quit:
	printPakcsHeader,
	processArgs(yes,Args).
processArgs(_,[Arg|Args]) :-
	Arg='--compiler-name', !, % show compiler name (pakcs) and quit:
	write(pakcs), nl,
	processArgs(yes,Args).
processArgs(_,[Arg|Args]) :-
	Arg='--numeric-version', !, % show compiler version number and quit:
	printVersionNumber, nl,
	processArgs(yes,Args).
processArgs(_,[Arg|Args]) :-
	Arg='--base-version', !, % show base libraries version number and quit:
	baseVersion(BaseVersion),
        write(BaseVersion), nl,
	processArgs(yes,Args).
processArgs(_,[Arg|Args]) :-
	(Arg='--help' ; Arg='-h' ; Arg='-?'),
	writeMainHelp,
	processArgs(yes,Args).
processArgs(Halt,[Arg|Args]) :-
	(Arg='--quiet' ; Arg='-quiet' ; Arg='-q'),
	setQuietMode(yes), !,
	setVerbosity(0),
	processArgs(Halt,Args).
processArgs(Halt,[Arg|Args]) :- % command option
	atom_codes(Arg,[58|CmdS]), !, % 58=':'
	expandCommand(CmdS,FullCmd),
	extractReplCmdParameters(Args,Params,RArgs),
	processReplCmd(FullCmd,Params),
        exitCode(EC),
        (EC=0 -> true ; halt(EC)), % halt if error occurred
	processArgs(Halt,RArgs).
processArgs(Halt,[Arg|Args]) :- % run-time arguments (starting with '--'):
	atom_codes(Arg,"--"), !,
	retract(rtArgs(_)),
	assertz(rtArgs(Args)),
        processArgs(Halt,[]).
processArgs(_,Args) :-
	writeErr('ERROR: Illegal arguments: '),
	printArguments(Args),
        nlErr, nlErr,
	writeLnErr('Run "pakcs --help" for usage infos'),
	halt(1).

printArguments([]).
printArguments([Arg|Args]) :-
        writeErr(Arg), writeErr(' '), printArguments(Args).

% extract REPL command parameters, i.e., everything until next REPL command
% or parameter "--":
extractReplCmdParameters([],[],[]).
extractReplCmdParameters([Arg|Args],[],[Arg|Args]) :-
	atom_codes(Arg,[58|_]), !. % 58=':'
extractReplCmdParameters([Arg|Args],[],[Arg|Args]) :-
	atom_codes(Arg,"--"), !.
extractReplCmdParameters([Arg|Args],[ArgS|Params],RArgs) :-
	atom_codes(Arg,ArgS),
	extractReplCmdParameters(Args,Params,RArgs).

% process a REPL command parameter:
processReplCmd("quit",Args) :- !,
	(Args=[] -> exitCode(EC), halt(EC)
	  ; writeErr('ERROR: Arguments after ":quit"!'), halt(1)).
processReplCmd(Cmd,Params) :- !,
	combine2cmd([Cmd|Params],CmdS),
	(process([58|CmdS]) -> true ; true). % due to failure loop of REPL

combine2cmd([],[]).
combine2cmd([X],X).
combine2cmd([X1,X2|Xs],CmdS) :-
	combine2cmd([X2|Xs],X2s),
	append(X1,[32|X2s],CmdS).
	      

% Show the main options of calling pakcs:
writeMainHelp :-
	nlErr,
	writeLnErr('Invoke interactive environment:'),
	nlErr,
	writeLnErr('    pakcs <options> [ -- <run-time arguments>]'),
	nlErr,
	writeLnErr('with options:'),
	nlErr,
	writeLnErr('-h|--help|-?      : show this message and quit'),
	writeLnErr('-V|--version      : show version and quit'),
	writeLnErr('--compiler-name   : show the compiler name "pakcs" and quit'),
	writeLnErr('--numeric-version : show the compiler version number and quit'),
	writeLnErr('--base-version    : show the version of the base libraries and quit'),
	writeLnErr('-q|--quiet        : work silently'),
	writeLnErr('--nocypm          : do not invoke "cypm" to compute package load path'),
	writeLnErr('--noreadline      : do not use input line editing via command "rlwrap"'),
	writeLnErr('-Dprop=val        : define pakcsrc property "prop" as "val"'),
	writeLnErr(':<cmd> <args>     : command of the PAKCS environment'),
	nlErr,
	nlErr,
	writeLnErr('Invoke some tool:'),
	nlErr,
	writeLnErr('    pakcs <tool> <tool specific options>'),
	nlErr,
	writeLnErr('where <tool> is one of:'),
	nlErr,
	writeLnErr('browse    : browse and analyze'),
	writeLnErr('check     : check properties'),
	writeLnErr('doc       : generate documentation for Curry programs'),
	writeLnErr('frontend  : Curry front end'),
	writeLnErr('makecgi   : translate Curry HTML program into CGI program'),
	writeLnErr('pp        : Curry preprocessor'),
        nlErr,
        writeLnErr('To get more help about the usage of a tool, type'),
	nlErr,
	writeLnErr('    pakcs <tool> -h').


% Compute the prompt of the interactive loop:
pakcsPrompt('') :- quietmode(yes), !.
pakcsPrompt(Prompt) :-
	currentModuleFile(MN,_),
	currentprogram(CPS),
	atom_codes(CP,CPS),
	split2dirbase(CP,_,BaseProg),
	addImports(AddImps),
	intersperse(' ',[BaseProg|AddImps],AllMods),
	appendAtoms(AllMods,CurrMods),
	(MN=BaseProg -> appendAtoms([CurrMods,'> '],Prompt)
	 ; appendAtoms([CurrMods,' (module: ',MN,')> '],Prompt)),
	!.

% main read-eval-print loop of the environment:
main :-
	prompt(_,''), % clear standard Prolog prompt
	repeat,
	pakcsPrompt(Prompt), write(Prompt),
	flush_output(user_output),
	flush_output(user_error),
	readLine(Input),
	(Input = end_of_file -> true
            ; removeBlanks(Input,ShortInput),
	      process(ShortInput)),
	cleanupAtEnd,
	exitCode(EC),
	halt(EC).

% things to do when leaving PAKCS interactive environment:
cleanupAtEnd :-
	% ignore connection errors:
	(on_exception(_ErrorMsg,cleanSourceCodeGUIs,true) -> true ; true).

% Expand a command that may be shortened to its full name:
expandCommand(AShortCmd,Cmd) :-
	map2M(user:isLowerCaseOf,ShortCmd,AShortCmd),
	allCommands(AllCmds),
	findall(FullCmd,prefixOf(ShortCmd,AllCmds,FullCmd),FullCmds),
	(FullCmds=[Cmd] -> true ;
	 (FullCmds=[]
	  -> writeErr('ERROR: unknown command: ":'),
	     atom_codes(ASC,AShortCmd), writeErr(ASC), writeLnErr('"'),
	     fail
	   ; writeErr('ERROR: ambiguous command: ":'),
	     atom_codes(ASC,AShortCmd), writeErr(ASC), writeLnErr('"'),
	     fail)).

prefixOf(Prefix,[Full|_],Full) :- append(Prefix,_,Full).
prefixOf(Prefix,[_|FullS],Full) :- prefixOf(Prefix,FullS,Full).

% all possible commands:
allCommands(["add","browse","cd","compile","coosy",
             "edit","eval","fork","help",
	     "interface","load","modules","peval","programs","quit","reload",
	     "save","set","show","source","type","usedimports"]).

% Expand an option that may be shortened to its full name:
expandOption(ShortOpt,FullOpt) :-
	(append(OptFirst,[32|OptRest],ShortOpt)
         -> true ; OptFirst=ShortOpt,OptRest=[]),
	map2M(user:isLowerCaseOf,LOptFirst,OptFirst),
	allOptions(AllOpts),
	findall(FullOptF,prefixOf(LOptFirst,AllOpts,FullOptF),FullOpts),
	(FullOpts=[Opt] -> (OptRest=[] -> FullOpt=Opt
                                        ; append(Opt,[32|OptRest],FullOpt))
         ; (FullOpts=[]
	     -> writeErr('ERROR: unknown option: '),
	        atom_codes(OF,OptFirst), writeLnErr(OF),
		writeLnErr('Type :set for help'), fail
  	      ; writeErr('ERROR: option not unique: '),
	        atom_codes(OF,OptFirst), writeLnErr(OF),
		writeLnErr('Type :set for help'), fail)).

% all possible options:
allOptions(["+allfails","-allfails",
            "+compact","-compact",
            "+consfail","-consfail",
            "+debug","-debug",
            "+free","-free",
            "+interactive","-interactive",
            "+first","-first",
            "+plprofile","-plprofile",
            "+printfail","-printfail",
            "+profile","-profile",
            "+suspend","-suspend",
            "+time","-time",
            "+verbose","-verbose",
            "+warn","-warn",
            "path","printdepth","v0","v1","v2","v3","parser","safe","args",
            "+single","-single",
            "+spy","-spy","spy",
            "+trace","-trace"]).

isLowerCaseOf(L,C) :- 65=<C, C=<90, !, L is C+32.
isLowerCaseOf(C,C).

% execute command (in repeat-fail loop):
process([]) :- !, fail.
process("?") :-	!, ioAdmissible, write('Type :h for help'), nl, fail.
process(S) :- append(":!",STail,S), !, ioAdmissible,
	removeBlanks(STail,ShellString),
	atom_codes(Cmd,ShellString),
	shellCmd(Cmd),
	!, fail.
process([58|Cs]) :- !, % 58=':'
	(append(ShortCmd,[32|Rest],Cs) -> true ; ShortCmd=Cs, Rest=[]),
	expandCommand(ShortCmd,Cmd),
	removeBlanks(Rest,Params),
	(member(Cmd,["load","reload","compile","quit","eval"])
          -> true
           ; ioAdmissible),
	processCommand(Cmd,Params),
	!,
	Cmd="quit".
process(Input) :- % ignore inputs starting with a comment:
        removeBlanks(Input,[45,45|_]), !, fail.
process(Input) :-
	processExpression(Input,ExprGoal),
	call(ExprGoal).

% check whether arbitrary top-level IO actions are allowed:
ioAdmissible :- safeMode(yes), !,
	write('Only initial expressions of non I/O type are allowed!'), nl,
	setExitCode(3),
	fail.
ioAdmissible.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process a given main expression:

processExpression(Input,ExprGoal) :-
        processExpressionWithType(Input,none,ExprGoal).

processExpressionWithType(Input,InitMainExpType,ExprGoal) :-
	parseMainExpression(Input,InitMainExpType,Term,Type,Vs,
                            _,DfltTypeAtom,Overloaded),
	processOrDefaultMainExpression(Input,Term,Type,Vs,DfltTypeAtom,
                                       Overloaded,ExprGoal).

% If the main expression is overloaded, try to default it to some
% numeric type:
processOrDefaultMainExpression(_Input,Term,Type,Vs,_,false,ExprGoal) :-
	% we can directly process non-overloaded expressions:
	!,
	(isIoType(Type) -> ioAdmissible ; true),
	(verbosemode(yes) ->
	    (verbosityQuiet -> writeCurryTermWithFreeVarNames(Vs,Term), nl
	      ; write('Evaluating expression: '),
	        writeCurryTermWithFreeVarNames(Vs,Term),
		write(' :: '),
		numbersmallvars(97,_,Type), writeType(Type), nl,
		% print free goal variables if present:
		writeFreeVars(Vs))
	 ; true),
	ExprGoal = evaluateMainExpression(Term,Type,Vs).
processOrDefaultMainExpression(Input,_,_,_,DfltTypeAtom,_,ExprGoal) :-
        \+ DfltTypeAtom=none, !,
	(verbosityDetailed -> write('Defaulted type of main expression: '),
                              write(DfltTypeAtom), nl
                            ; true),
	processExpressionWithType(Input,DfltTypeAtom,ExprGoal).
processOrDefaultMainExpression(_,_,Type,_,_,_,_) :-
	(verbosityDetailed -> write('Overloaded type: '), writeq(Type), nl
                            ; true),
        writeErr('Cannot handle arbitrary overloaded top-level expressions'),
	nlErr,
        writeErr('Hint: add type annotation to overloaded entity'), nlErr,
        fail.

% translate a flat type into an atom in Curry syntax:
flatType2Atom(Type,CurryType) :-
        % small hack to avoid redefinition of writeType/2 to writeType/3...
        getNewFileName("maintype",NewFile),
        tell(NewFile),
	  numbersmallvars(97,_,Type), writeType(Type), nl, 
	told,
	open(NewFile,read,Stream),
	  readStreamLine(Stream,TypeString),
	close(Stream),
	appendAtoms(['rm -rf ',NewFile],RmCmd), shellCmd(RmCmd),
	atom_codes(CurryType,TypeString).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check whether the first argument is the (FlatCurry) representation
% of some class contexts and return the context type in the second
% argument and the qualified class name (string) in the third (module)
% and fourth (class name) argument:
classDict(T,_,_,_) :- var(T), !, fail.
classDict('TCons'(FCDict,[A]),A,ModS,DictS) :-
        atomic2Codes(FCDict,FCDictS), % hack for SWI 7.x if FCDict == []
        % dictionary argument types are prefixed by "_Dict#":
        atom_codes('._Dict\'23',FCDictPrefixS),
        append(ModFCDictPrefixS,DictS,FCDictS),
        append(ModS,FCDictPrefixS,ModFCDictPrefixS), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% process a given expression by writing it into a main module
% and calling the front end:
parseMainExpression(Input,InitMainExpType,MainExp,Type,Vs,
                    MainExpType,DfltTypeAtom,Ovld) :-
	getNewFileName("",MainExprDir),
	makeDirectory(MainExprDir),
	parseExpressionWithFrontend(MainExprDir,Input,InitMainExpType,
                                    MainExp,Type,Vs,
                                    MainExpType,DfltTypeAtom,Ovld),
	(verbosityDetailed
          -> write('Translated expression: '), writeq(MainExp), nl
           ; true).

parseExpressionWithFrontend(MainExprDir,Input,InitMainFuncType,MainExp,
                            Type,Vs,MainFuncType,DfltTypeAtom,Ovld) :-
        getMainProgPath(MainProgName,MainPath),
	appendAtoms([MainExprDir,'/PAKCS_Main_Exp'],MainExprMod),
	appendAtoms([MainExprMod,'.curry'],MainExprModFile),
	splitWhereFree(Input,InputExp,FreeVars),
	writeMainExprFile(MainExprModFile,MainProgName,InputExp,FreeVars,
                          InitMainFuncType),
	(verbosityIntermediate -> PVerb=1 ; PVerb=0),
	workingDirectory(CurDir),
	toAbsPath(MainPath,AbsMainPath),
	getCurryPath(CP0), path2String(CP0,CP1), atom_codes(LCP,CP1),
        extendPath(AbsMainPath,LCP,NewLCP),
	setCurryPath(NewLCP),
	setWorkingDirectory(MainExprDir),
	(parseProgram("PAKCS_Main_Exp",PVerb,no) -> Parse=ok ; Parse=failed),
	setCurryPath(LCP), % restore old settings
	setWorkingDirectory(CurDir),
	Parse=ok, % proceed only in case of successful parsing
	loadPath(AbsMainPath,LoadPath),
	setCurryPath(NewLCP),
	setWorkingDirectory(MainExprDir),
	readProg(['.'|LoadPath],'PAKCS_Main_Exp',FlatProg,_),
	setCurryPath(LCP), % restore old settings
	setWorkingDirectory(CurDir),
	FlatProg = 'Prog'(_,_Imps,_TDecls,FDecls,_),
	length(FreeVars,NumFreeVars),
	FDecls = ['Func'(_,_,_,FuncType,'Rule'(RuleArgs,RuleExp))|MoreFs],
	!,
	((MoreFs=[], simpleFlatExp(RuleExp))
          -> FlatExp = RuleExp
           ; setCurryPath(NewLCP),
	     compileMainExpression(MainExprMod),
	     setCurryPath(LCP),
	     map2M(compiler:varIndex2VarExp,RuleArgs,RuleVars),
	     FlatExp = 'Comb'('FuncCall',
	                      "PAKCS_Main_Exp.pakcsMainGoal",RuleVars)),
        flatType2MainType([],FuncType,_,MFuncType),
        copy_term(MFuncType,MainFuncType),
        defaultTypeExpr(MFuncType,DfltType,DfltTypeAtom),
        stripFuncTypes(NumFreeVars,DfltType,Type),
	flatExp2MainExp([],FlatExp,EVs,MainExp),
	replaceFreeVarInEnv(FreeVars,RuleArgs,EVs,Vs),
	length(FreeVars,FVL), length(RuleArgs,RAL),
	(FVL=RAL -> Ovld=false ; Ovld=true),
	!,
	deleteMainExpFiles(MainExprDir).
parseExpressionWithFrontend(MainExprDir,_,_,_,_,_,_,_,_) :-
	deleteMainExpFiles(MainExprDir),
	!, failWithExitCode.

% get the name and path to the source code of the currently loaded main module
% (or fail with an error message if there is no source code):
getMainProgPath(MainProgName,MainPath) :-
	lastload(MainProgS),
	findSourceProgPath(MainProgS,MainPath), !,
	atom_codes(MainProg,MainProgS),
	split2dirbase(MainProg,_,MainProgName).
getMainProgPath(CurrMod,MainPath) :-
	currentModuleFile(CurrMod,_), atom_codes(CurrMod,CurrModS),
	findSourceProgPath(CurrModS,MainPath), !,
	(verbosityQuiet -> true ;
	    lastload(LoadProgS), atom_codes(LoadProg,LoadProgS),
	    writeErr('*** Warning: module loaded from                : '),
	    writeLnErr(LoadProg),
	    writeErr('    main expression parsed w.r.t. source module: '),
	    writeLnErr(CurrMod)).
getMainProgPath(_,_) :-
	lastload(MainProgS), atom_codes(MainProg,MainProgS),
	writeErr('Source program for module "'), writeErr(MainProg),
	writeLnErr('" not found!'),
	!, fail.

% delete all auxiliary files for storing main expression:
deleteMainExpFiles(MainExprDir) :-
	appendAtoms([MainExprDir,'/PAKCS_Main_Exp'],MainExprMod),
	prog2FlatCurryFile(MainExprMod,MainExprFcyFile),
	deleteFileIfExists(MainExprFcyFile),
	prog2InterfaceFile(MainExprMod,MainExprFintFile),
	deleteFileIfExists(MainExprFintFile),
	prog2ICurryFile(MainExprMod,MainExprIcurryFile),
	deleteFileIfExists(MainExprIcurryFile),
	appendAtoms(['rm -rf ',MainExprDir],RmdirCmd),
	(pakcsrc(keepfiles,yes) -> true ; shellCmd(RmdirCmd)).

% compile and load the "main expression program" since it contains some
% functions generated from the main expression, e.g., let bindings:
compileMainExpression(MainExprMod) :-
	prog2PrologFile(MainExprMod,PrologFile),
	c2p(MainExprMod,PrologFile),
	currentModuleFile(CurrMod,_),
	on_exception(ErrorMsg,
                     (addImports(AddImps),
		      loadAndCompile(PrologFile,AddImps,create)),
	             printError(ErrorMsg) ),
	curryModule(CurrMod).

% Try to default a type by instantiating type context variables
% to default types (e.g., numeric types) and removing such type contexts.
% The defaulted type is returned in the second argument.
% The third argument contains the defaulted type as an atom
% or 'none' if the type is still overloaded.
defaultTypeExpr(OrgType,DfltType,DfltTypeAtom) :-
        defaultNumType(OrgType,DefNumType),
        removeDefaultedTypes(DefNumType,DfltType),
        (isOverloadedType(DfltType) -> DfltTypeAtom=none    % can't default
                                     ; flatType2Atom(DfltType,DfltTypeAtom)).

% try to default overloaded numerical types:
defaultNumType(Type,Type) :- var(Type), !.
defaultNumType('FuncType'(AType,RType),DType) :-
	classDict(AType,TVar,ModName,DictName),
        ModName="Prelude", member(DictName,["Num","Integral","Fractional"]), !,
	(DictName="Fractional"
         -> (var(TVar) -> TVar = 'TCons'('Prelude.Float',[]) ; true),
	    defaultNumType(RType,DType)
	  ; (var(TVar) -> TVar = 'TCons'('Prelude.Int',[]) ; true),
	    defaultNumType(RType,DType)).
defaultNumType('FuncType'(AType,RType),'FuncType'(AType,DType)) :- !,
        defaultNumType(RType,DType).
defaultNumType(Type,Type).

% remove type class context of defaulted types:
removeDefaultedTypes(Type,Type) :- var(Type), !.
removeDefaultedTypes('FuncType'(AType,RType),DType) :-
	classDict(AType,TVar,ModName,DictName),
        ModName="Prelude", nonvar(TVar),
	( member(DictName,["Eq","Ord","Read","Show"])
        ; DictName="Enum",
          member(TVar,['TCons'('Prelude.Int',[]),'TCons'('Prelude.Float',[])])
        ), !,
        removeDefaultedTypes(RType,DType).
removeDefaultedTypes('FuncType'(AType,RType),'FuncType'(AType,DType)) :- !,
        removeDefaultedTypes(RType,DType).
removeDefaultedTypes(Type,Type).

% Is the type overloaded, i.e., does it contain class dictioniary parameters?
isOverloadedType(Type) :- var(Type), !, fail.
isOverloadedType('FuncType'(AType,_)) :- classDict(AType,_,_,_), !.
isOverloadedType('FuncType'(_,RType)) :- !, isOverloadedType(RType).
isOverloadedType(_) :- fail.

% strip the first n function types (except for class dictionaries)
% from a type expression:
stripFuncTypes(0,Type,Type) :- !.
stripFuncTypes(N,'FuncType'(_,RType),Type) :-
        N1 is N-1, stripFuncTypes(N1,RType,Type).

replaceFreeVarInEnv(_,_,[],[]).
replaceFreeVarInEnv(FreeVars,RuleArgs,[(EVN=EV)|Env],[(EVN1=EV)|TEnv]) :-
        atom_codes(EVN,[95,120|Vs]), number_codes(V,Vs),
	replaceFreeEnvVar(FreeVars,RuleArgs,V,EVN1),
        replaceFreeVarInEnv(FreeVars,RuleArgs,Env,TEnv).

replaceFreeEnvVar([],_,V,NV) :- !,
        number_codes(V,Vs), atom_codes(NV,[95|Vs]).
replaceFreeEnvVar([FV|FreeVars],[RA|RuleArgs],V,NV) :-
        (V=RA -> appendAtoms(['_',FV],NV)
               ; replaceFreeEnvVar(FreeVars,RuleArgs,V,NV)).

splitWhereFree(Input,Exp,Vs) :-
        append(IWOfree," free",Input),
	append(IWOvars,Vars,IWOfree),
	append(Exp," where ",IWOvars), !,
	splitWhereVars(Vars,Vs).
splitWhereFree(Input,Input,[]).

splitWhereVars(VarsS,[V|Vs]) :- 
        append(VarS,[44|Vars],VarsS), !,
	removeBlanks(VarS,VarS1), atom_codes(V,VarS1),
	splitWhereVars(Vars,Vs).
splitWhereVars(VarS,[V]) :- removeBlanks(VarS,VarS1), atom_codes(V,VarS1).

flatType2MainType(Vs,'TVar'(I),Vs1,TV) :-
	number_codes(I,IS), atom_codes(VN,[97|IS]), addVar(VN,Vs,TV,Vs1).
flatType2MainType(Vs,'FuncType'(FT1,FT2),Vs2,'FuncType'(T1,T2)) :-
        flatType2MainType(Vs,FT1,Vs1,T1),
        flatType2MainType(Vs1,FT2,Vs2,T2).
flatType2MainType(Vs,'TCons'(TNameS,TArgs),Vs1,'TCons'(TName,Args)) :-
	flatName2Atom(TNameS,TName),
	flatTypes2MainTypes(Vs,TArgs,Vs1,Args).

flatTypes2MainTypes(Vs,[],Vs,[]).
flatTypes2MainTypes(Vs,[FE|FEs],Vs2,[E|Es]) :-
	flatType2MainType(Vs,FE,Vs1,E),
	flatTypes2MainTypes(Vs1,FEs,Vs2,Es).

% is a FlatCurry expression simple, i.e., does not contains Let/Case/Or/Typed?
simpleFlatExp('Var'(_)).
simpleFlatExp('Lit'(_)).
simpleFlatExp('Comb'(_,_,Args)) :- simpleFlatExps(Args).
simpleFlatExp('Free'(_,Exp)) :- simpleFlatExp(Exp).
simpleFlatExp('Let'(_,_)) :- fail.
simpleFlatExp('Or'(_,_)) :- fail.
simpleFlatExp('Typed'(_,_)) :- fail.
simpleFlatExp('Case'(_,_,_)) :- fail.
simpleFlatExps([]).
simpleFlatExps([E|Es]) :- simpleFlatExp(E), simpleFlatExps(Es).

% translate FlatCurry expression from fcy reader into internal format:
flatExp2MainExp(Vs,'Var'(I),Vs1,V) :-
	number_codes(I,IS), atom_codes(VN,[95,120|IS]), addVar(VN,Vs,V,Vs1).
flatExp2MainExp(Vs,'Lit'('Intc'(I)),Vs,I).
flatExp2MainExp(Vs,'Lit'('Floatc'(F)),Vs,F).
flatExp2MainExp(Vs,'Lit'('Charc'(I)),Vs,I_Atom) :- char_int(I_Atom,I).
flatExp2MainExp(Vs,'Comb'(_,FNameS,[FA1,FA2]),Vs1,Exp) :-
	atom_codes('Prelude.apply',FNameS), !,
	flatExps2MainExps(Vs,[FA1,FA2],Vs1,[A1,A2]),
	(compileWithSharing(function)
	 -> Exp = makeShare('Prelude.apply'(A1,A2),_)
	  ; Exp = 'Prelude.apply'(A1,A2)).
flatExp2MainExp(Vs,'Comb'(CType,FNameS,FArgs),Vs1,Exp) :-
	flatName2Atom(FNameS,FName),
	flatExps2MainExps(Vs,FArgs,Vs1,Args),
	FExp =.. [FName|Args],
	((CType='FuncCall' ; CType='ConsCall' ; CType='ConsPartCall'(_))
	 -> Missing=0
	  ; CType='FuncPartCall'(Missing)),
	term2partcall(FExp,Missing,ExpOrPartCall),
	(compileWithSharing(function)
	 -> Exp = makeShare(ExpOrPartCall,_)
	  ; Exp = ExpOrPartCall).
flatExp2MainExp(Vs,'Free'(_,FExp),Vs1,Exp) :-
	flatExp2MainExp(Vs,FExp,Vs1,Exp).
flatExp2MainExp(_,'Let'(_,_),_,_) :-
        writeLnErr('ERROR: Let not allowed in main expressions!'),
	!, fail.
flatExp2MainExp(_,'Or'(_,_),_,_) :-
        writeLnErr('ERROR: Or not allowed in main expressions!'),
	!, fail.
flatExp2MainExp(_,'Typed'(_,_),_,_) :-
        writeLnErr('ERROR: Typed not allowed in main expressions!'),
	!, fail.
flatExp2MainExp(_,'Case'(_,_,_),_,_) :-
        writeLnErr('ERROR: Case not allowed in main expressions!'),
	!, fail.

flatExps2MainExps(Vs,[],Vs,[]).
flatExps2MainExps(Vs,[FE|FEs],Vs2,[E|Es]) :-
	flatExp2MainExp(Vs,FE,Vs1,E),
	flatExps2MainExps(Vs1,FEs,Vs2,Es).

writeMainExprFile(ExprFile,MainProg,Input,FreeVars,InitMainFuncType) :-
	(verbosityIntermediate
          -> write('Writing Curry main expression file: '), write(ExprFile), nl
           ; true),
	fileOpenOptions(FOptions),
	open(ExprFile,write,S,FOptions),
	% suppress parser warnings:
	write(S,'{-# OPTIONS_CYMAKE -Wnone #-}'), nl(S),
	(MainProg='Prelude' -> true
          ; write(S,'import '), write(S,MainProg), nl(S)),
	addImports(Imps), writeMainImports(S,Imps),
        (InitMainFuncType = none -> true
         ; write(S,'pakcsMainGoal :: '), write(S,InitMainFuncType), nl(S)),
	write(S,'pakcsMainGoal'),
	writeFreeVarArgs(S,FreeVars),
	write(S,' = '),
	varDefines(VarDefs), writeVarDefs(S,VarDefs),
	putChars(S,Input), nl(S),
	close(S).

writeVarDefs(_,[]).
writeVarDefs(S,[Var=Exp|VarDefs]) :-
	exp2Term(Exp,[],Term,_),
	write(S,'let '), write(S,Var), write(S,' = '),
	writeCurryOnStream(S,Term), write(S,' in '),
	writeVarDefs(S,VarDefs).

writeFreeVarArgs(_,[]).
writeFreeVarArgs(S,[V|Vs]) :-
        write(S,' '), write(S,V), writeFreeVarArgs(S,Vs).

writeMainImports(_,[]).
writeMainImports(S,[Imp|Imps]) :-
	write(S,'import '), write(S,Imp), nl(S),
	writeMainImports(S,Imps).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processCommand("quit",[]) :- !.
processCommand("help",[]) :- !,
	write('Commands (can be abbreviated to a prefix if unique):'), nl,
	write(':load <prog>      - compile and load program "<prog>.curry" and all imports'),nl,
	write(':add <m1> .. <mn> - add modules <m1> to <mn> to currently loaded modules'),nl,
	write(':reload           - recompile currently loaded modules'),nl,
	write(':compile <prog>   - alias for ":load <prog>"'),nl,
	write(':eval <expr>      - evaluate expression <expr>'), nl,
	%write(':define <v>=<exp> - define variable binding for subsequent expressions'), nl,
	write(':type <expr>      - show the type of <expression>'),nl,
	write(':browse           - browse program and its imported modules'),nl,
	write(':interface        - show interface of current program'),nl,
	write(':interface <m>    - show interface of module <m>'),nl,
	write(':usedimports      - show all used imported functions/constructors'),nl,
	write(':edit             - load source of currently loaded module into editor'), nl,
	write(':edit <m>         - load source of module <m> into editor'), nl,
	write(':modules          - show list of currently loaded modules'), nl,
	write(':show             - show source of currently loaded Curry program'), nl,
	write(':show <m>         - show source code of module <m>'), nl,
	write(':source <f>       - show source code of (visible!) function <f>'), nl,
	write(':source <m>.<f>   - show source code of function <f> in module <m>'), nl,
	write(':programs         - show names of all Curry programs available in load path'), nl,
	write(':cd <dir>         - change current directory to <dir>'), nl,
	write(':!<command>       - execute <command> in shell'), nl,
	write(':save             - save executable with main expression "main"'), nl,
	write(':save <expr>      - save executable with main expression <expr>'), nl,
	write(':fork <expr>      - fork new process evaluating <expr> (of type "IO ()")'), nl,
	write(':coosy            - start Curry Object Observation System'), nl,
	%write(':xml              - translate current program into XML format'), nl,
	write(':peval            - partially evaluate current program'), nl,
	write(':set <option>     - set a command line option'), nl,
	write(':set              - help on :set command'), nl,
	write(':help             - show this message'), nl,
	write(':quit             - leave the PAKCS environment'), nl, nl,
	write('... or type any <expression> to evaluate'), nl,
	nl, fail.

processCommand("set",[]) :- !,
	write('Options for ":set" command:'), nl,
	write('+/-allfails     - show all failures if printfail is turned on'), nl,
	write('+/-compact      - reduce size of target program during compilation'), nl,
	write('+/-consfail     - show pattern matching/unification failures'), nl,
	write('                  ("+consfail int": interactive mode to show fail trace)'), nl,
	write('                  ("+consfail all": show complete fail trace)'), nl,
	write('                  ("+consfail file:F": store complete fail trace in file F)'), nl,
	write('+/-debug        - debug mode (compile with debugging information)'), nl,
	write('+/-interactive  - turn on/off interactive execution of initial expression'), nl,
	write('+/-first        - turn on/off printing only first value'), nl,
	write('+/-plprofile    - use Prolog profiler'), nl,
	write('+/-printfail    - show failures in top-level evaluation'), nl,
	write('+/-profile      - show profile data in debug mode'), nl,
	write('+/-suspend      - show suspended goals at end of suspended computation'), nl,
	write('+/-time         - show execution time'), nl,
	write('+/-verbose      - verbose mode (printing initial expressions)'), nl,
	write('+/-warn         - show parser warnings'), nl,
	write('path <path>     - set additional search path for loading modules'), nl,
	write('printdepth <n>  - set print depth to <n> (0 = unlimited)'), nl,
	write('v<n>            - verbosity level'), nl,
	write('                   0: quiet (errors only)'), nl,
	write('                   1: status messages (default)'), nl,
	write('                   2: intermediate messages and commands'), nl,
	write('                   3: all intermediate results'), nl,
	write('safe            - safe execution mode without I/O actions'), nl,
	write('parser <opts>   - additional options passed to Curry front end'), nl,
	write('args   <args>   - run-time arguments passed to main program'), nl,
	nl,
	write('Options in debug mode:'), nl,
	write('+/-single         - single step mode'), nl,
	write('+/-spy            - spy mode'), nl,
	write('+/-trace          - trace mode'), nl,
	write('spy <function>    - set spy point on <function>'), nl,
	nl,
	write('Current settings: '), nl,
	(printAllFailures -> write('+') ; write('-')),
	write(allfails), write('   '),
	(compileWithCompact([]) -> write('-') ; write('+')),
	write(compact),	write('  '),
	printConsFailure(PrintConsFail),
	(PrintConsFail=no -> write('-') ; write('+')),
	write(consfail),
	(PrintConsFail=no -> write('    ')
	  ; write('('), write(PrintConsFail), write(') ')),
	(compileWithDebug -> write('+') ; write('-')),
	write(debug),	write('    '),
	(firstSolutionMode(yes) -> write('+') ; write('-')),
	write(first), write('  '),
	(interactiveMode(yes) -> write('+') ; write('-')),
	write(interactive), write('  '),
	nl,
	(compileWithFailPrint -> write('+') ; write('-')),
	write(printfail), write('  '),
	profiling(P), (P=yes -> write('+') ; write('-')),
	write(profile),	write('  '),
	plprofiling(PLP), (PLP=yes -> write('+') ; write('-')),
	write(plprofile), write('  '),
	suspendmode(BM), (BM=yes -> write('+') ; write('-')),
	write(suspend),	write('   '),
	timemode(T), (T=yes -> write('+') ; write('-')),
	write(time),	write('  '),
	verbosemode(V), (V=yes -> write('+') ; write('-')),
	write(verbose),	write('  '),
	parser_warnings(W), (W=yes -> write('+') ; write('-')),
	write(warn), write('  '),
	nl,
	loadPath('.',LP), path2String(LP,SP),
	atom_codes(AP,SP),     write('loadpath          : '), write(AP), nl,
	printDepth(PD),        write('printdepth        : '),
	(PD=0 -> write(PD) ; PD1 is PD-1, write(PD1)), nl,
	verbosity(VL),         write('verbosity         : '), write(VL), nl,
	parserOptions(POpts),  write('parser options    : '), write(POpts), nl,
	rtArgs(RTArgs),        write('run-time arguments: '),
	intersperse(' ',RTArgs,RTBArgs),
	appendAtoms(RTBArgs,AllArgs), write(AllArgs), nl,
	(compileWithDebug ->
	  (singlestep -> write('+') ; write('-')), write(single), write('  '),
	  (spymode    -> write('+') ; write('-')), write(spy), write('  '),
	  (tracemode  -> write('+') ; write('-')), write(trace), write('  '),
	  write('/ spy points: '), spypoints(SPs), write(SPs), nl
	 ; true).

processCommand("set",Option) :- !,
	expandOption(Option,CompletedOption),
	processSetOption(CompletedOption).

processCommand("add",Arg) :- !,
	split2words(Arg,Args),
	addImports(OldAddImps), !,
	map1M(user:addImportModule,Args),
	addImports(NewAddImps), !,
	((OldAddImps=NewAddImps) -> true
	 ; (processCommand("reload",[]) -> true
             ; retract(addImports(_)), asserta(addImports(OldAddImps)))).

processCommand("compile",Arg) :- !,
        processCommand("load",Arg).

processCommand("load",Arg) :- !,
	extractProgName(Arg,Prog),
	isValidProgramName(Prog),
	retract(lastload(OldLL)), asserta(lastload(Prog)),
	retract(addImports(OldImps)), asserta(addImports([])),
	(verbosemode(yes) -> write('Loading program "'),
	                     atom_codes(ProgA,Prog), write(ProgA),
	                     write('"...'), nl ; true),
        (processCommand("reload",[])
         -> true
          ; retract(lastload(_)), asserta(lastload(OldLL)),
	    retract(addImports(_)), asserta(addImports(OldImps))).

processCommand("reload",[]) :- !,
	lastload(Prog),
	(Prog="" -> writeLnErr('ERROR: no load command to repeat'),
	            !, fail
                  ; true),
	processCompile(Prog,PrologFile),
	!,
	existsFile(PrologFile),
	addImports(AddImps),
	map2M(loader:checkPrologTarget,AddImps,_),
	on_exception(ErrorMsg,
                     (loadAndCompile(PrologFile,AddImps,create),
		      atom_codes(PrologFile,PrologFileL),
		      (append("/tmp/",_,PrologFileL)
		       -> % remove temporary Prolog file:
			  deleteFile(PrologFile)
		        ; true),
		      retract(currentprogram(_)),
	              asserta(currentprogram(Prog)),
		      initializationsInProg(ProgInits), call(ProgInits)),
	             printError(ErrorMsg) ),
	retract(varDefines(_)), asserta(varDefines([])),
	(compileWithDebug -> retract(spypoints(_)),
	                     asserta(spypoints([])),
	                     singleOn, traceOn, spyOff
	                   ; true).

processCommand("eval",ExprInput) :- !,
	processExpression(ExprInput,ExprGoal),
	call(ExprGoal).

processCommand("type",ExprInput) :- !,
	parseMainExpression(ExprInput,none,Term,_,Vs,Type,_,Ovld),
	(Ovld=true -> atom_codes(EI,ExprInput), write(EI)
                    ; writeCurryTermWithFreeVarNames(Vs,Term)),
	write(' :: '),
	numbersmallvars(97,_,Type), writeType(Type), nl.

processCommand("usedimports",[]) :- !,
	checkCpmTool('curry-usedimports','importusage',UsedImports),
        lastload(Prog),
	(Prog="" -> write('ERROR: no program loaded for analysis'), nl, !, fail
                  ; true),
        atom_codes(ProgA,Prog),
	appendAtoms([UsedImports,' ',ProgA],AnaCmd),
        shellCmdWithCurryPathWithReport(AnaCmd).

processCommand("interface",[]) :- !,
	lastload(Prog),
	(Prog="" -> processCommand("interface","Prelude")
                  ; processCommand("interface",Prog)).
processCommand("interface",IFTail) :- !,
        checkCpmTool('curry-showflat','showflatcurry',ShowFlat),
	extractProgName(IFTail,Prog),
	isValidProgramName(Prog),
        atom_codes(ProgA,Prog),
	appendAtoms([ShowFlat,' -int ',ProgA],GenIntCmd),
        shellCmdWithCurryPathWithReport(GenIntCmd).

processCommand("browse",[]) :- !,
        checkWish,
        checkCpmTool('curry-browse','currybrowse',BrowseProg),
	writeNQ('Starting Curry Browser in separate window...'), nlNQ,
	lastload(LastProg),
	(LastProg="" -> Prog="Prelude" ; Prog=LastProg),
	atom_codes(ProgA,Prog),
	(prog_exists(ProgA)
	 -> RealProg = ProgA
	  ; loadPath('.',LoadPath),
	    (findFilePropertyInPath(LoadPath,user:prog_exists,ProgA,RealProg)
	     -> true
	      ; write('ERROR: program "'),
		write(ProgA), write('" does not exist!'), nl, fail)),
	!,
	appendAtoms(['"',BrowseProg,'" ',RealProg,' & '],BrowseCmd),
        shellCmdWithCurryPathWithReport(BrowseCmd).

processCommand("coosy",[]) :- !,
        checkWish,
        checkCpmTool('coosy-gui','coosy',CoosyGuiProg),
	writeNQ('Starting Curry Object Observation System in separate window...'),
        nlNQ,
	appendAtoms(['"',CoosyGuiProg,'" &'],GuiCmd),
        shellCmdWithCurryPathWithReport(GuiCmd),
	(waitForFile('COOSYLOGS/READY',3) -> true
          ; writeLnErr('ERROR: COOSy startup failed'), fail),
        readFileContents('COOSYLOGS/SRCPATH',CoosyPathNl),
        (append(CoosyPath,[10],CoosyPathNl) -> true ; CoosyPath=CoosyPathNl),
        !,
        atom_codes(CoosySrc,CoosyPath),
	getCurryPath(SLP),
	(SLP=[] -> setCurryPath(CoosySrc)
	         ; path2String([CoosySrc|SLP],PathS), atom_codes(Path,PathS),
	           setCurryPath(Path)),
	printCurrentLoadPath.

% processCommand("xml",[]) :- !,
% 	lastload(Prog),
% 	(Prog="" -> writeLnErr('ERROR: no program loaded for XML translation'),
% 	            !, fail
%                   ; true),
%         atom_codes(ProgA,Prog),
%         installDir(PH),
% 	appendAtoms(['"',PH,'/tools/curry2xml" ',ProgA],XmlCmd),
%         shellCmdWithCurryPathWithReport(XmlCmd).

processCommand("peval",[]) :- !,
	lastload(Prog),
	(Prog="" -> writeLnErr('ERROR: no program loaded for partial evaluation'),
	            !, fail
                  ; true),
        atom_codes(ProgA,Prog),
        installDir(PH),
	appendAtoms(['"',PH,'/tools/Peval/peval" ',ProgA],PevalCmd),
        shellCmdWithCurryPathWithReport(PevalCmd),
	!,
	append(Prog,"_pe",ProgPE), % name of partially evaluated program
	write('Loading partially evaluated program "'),
	atom_codes(ProgPEA,ProgPE), write(ProgPEA),
	write('"...'), nl,
	processCommand("load",ProgPE).


processCommand("edit",[]) :- !, % edit current main module
	currentprogram(Prog),
	findSourceProg(Prog,SourceFileName),
	processCommand("edit",SourceFileName).

processCommand("edit",EdTail) :-
	extractProgName(EdTail,EntityS),
	findSourceProg(EntityS,FileS), !,
	getEditor(Editor),
	atom_codes(Editor,EditorS),
	concat([EditorS," ",FileS," &"],EditCmdS),
	atom_codes(EditCmd,EditCmdS),
	shellCmdWithReport(EditCmd).

% show a list of all Curry programs available in the load path:
processCommand("programs",[]) :- !,
	loadPath('.',LP),
	write('Curry programs available in the load path:'), nl,
	map1M(user:showProgramsInDirectory,LP).

processCommand("modules",[]) :- !, % show list of currently loaded modules
	findall((M,P),loadedModule(M,P),Mods),
	write('Currently loaded modules:'), nl,
	map1M(user:writeModuleFile,Mods).

processCommand("show",[]) :- !, % show source code of current module
	currentprogram(Prog),
	(findSourceProg(Prog,SourceFileName)
	 -> atom_codes(File,SourceFileName),
	    getPager(Pager),
	    appendAtoms([Pager,' ',File],Cmd),
	    shellCmdWithReport(Cmd)
	  ; write('No source program file available, generating source from FlatCurry...'),
            nl, nl,
            atom_codes(ProgA,Prog),
            checkCpmTool('curry-showflat','showflatcurry',ShowFlat),
	    appendAtoms([ShowFlat,' -mod ',ProgA],ShowProgCmd),
	    shellCmdWithCurryPathWithReport(ShowProgCmd)).

processCommand("show",ShTail) :- % show source of a module
	extractProgName(ShTail,EntityS),
	findSourceProg(EntityS,SourceFileName), !,
	atom_codes(File,SourceFileName),
	getPager(Pager),
	appendAtoms([Pager,' "',File,'"'],Cmd),
	shellCmdWithReport(Cmd).

processCommand("show",_) :- !,
	writeLnErr('ERROR: Source file not found').

processCommand("source",Arg) :-
	append(PModS,[46|FunS],Arg),
	append(_,[LM],PModS), isLetterDigitCode(LM),
	(\+ member(46,FunS) ; isOperatorName(FunS)),
	!,
	% show source code of function in module
	extractProgName(PModS,ModS),
	showSourceCodeOfFunction(ModS,FunS).

processCommand("source",ExprInput) :- !, % show source code of a function
	parseMainExpression(ExprInput,none,Term,_Type,_Vs,_,_,_),
	showSourceCode(Term).

processCommand("cd",DirString) :- !,
	(DirString="" -> writeLnErr('ERROR: missing argument'), fail
	               ; true),
	atom_codes(Dir,DirString),
	(existsDirectory(Dir)
	 -> (setWorkingDirectory(Dir) -> true
              ; writeLnErr('ERROR: cd command failed!'))
	  ; writeErr('ERROR: directory \''),
	    writeErr(Dir),
	    writeLnErr('\' does not exist!')).

processCommand("save",Exp) :- !,
	(Exp=[] -> MainGoal="main" ; MainGoal=Exp),
	currentprogram(Prog),
	atom_codes(ProgName,Prog),
	(Prog="Prelude" -> writeLnErr('ERROR: no program loaded'), fail
	                 ; true),
	appendAtom(ProgName,'.state',ProgStName),
	initializationsInProg(ProgInits),
	resetDynamicPreds,
	(retract(rtArgs(_)) -> true ; true),
	% start saved program in non-verbose mode if initial goal provided:
	verbosemode(QM), setVerboseMode(no),
	processExpression(MainGoal,ExecGoal),
	%write('Goal to execute:'), nl, writeq(ExecGoal), nl,
	(pakcsrc(smallstate,yes)
	 -> atom_codes(ProgA,Prog), prog2PrologFile(ProgA,ProgPl),
	    createSavedState(ProgPl,ProgStName,
			(ProgInits,evaluator:evaluateGoalAndExit(ExecGoal)))
	  ; saveprog_entry(ProgStName,
 		        (ProgInits,evaluator:evaluateGoalAndExit(ExecGoal)))),
	setVerboseMode(QM),
	installDir(PH),
	appendAtoms(['"',PH,'/scripts/makesavedstate" '],CMD1),
	((pakcsrc(standalone,yes), (prolog(swi) ; sicstus310orHigher))
	 -> appendAtom(CMD1,'-standalone ',CMD2)
	  ; CMD2=CMD1),
	appendAtoms([CMD2,ProgStName,' ',ProgName],Cmd),
	shellCmdWithReport(Cmd),
	(verbosityNotQuiet
	  -> write('Executable saved in: '), write(ProgName), nl
	   ; true),
	call(ProgInits).

processCommand("fork",STail) :- !, processFork(STail).

processCommand(_,_) :- !,
	write('ERROR: unknown command. Type :h for help'), nl, fail.

% Check for existence of a binary in the path, e.g., 'wish'.
% If the binary does not exist, print the error message (2nd argument) and fail.
checkProgram(Program,_,CProgram) :-
        appendAtoms(['which ',Program,' > /dev/null'],CheckCmd),
        shellCmd(CheckCmd,ECode),
        ECode=0, !, Program=CProgram.
checkProgram(Program,_,CProgram) :-
        getHomeDirectory(Home),
        appendAtoms([Home,'/.cpm/bin/',Program],CPMProg),
        appendAtoms(['which ',CPMProg,' > /dev/null'],CheckCmd),
        shellCmd(CheckCmd,ECode),
        ECode=0, !, CProgram=CPMProg.
checkProgram(_,ErrMsg,_) :-
        writeErr(ErrMsg), nlErr, !, fail.

checkWish :-
        checkProgram(wish,
          'Windowing shell "wish" not found. Please install package "tk"!',_).

checkCpmTool(CpmBin,Package,Prog) :-
        appendAtoms(['"',CpmBin,'" not found. Install it by: "cypm install ',
                     Package,'"!'],ErrMsg),
        checkProgram(CpmBin,ErrMsg,Prog).

% call "shellCmd" and report its execution if verbosityIntermediate:
shellCmdWithReport(Cmd) :-
	(verbosityIntermediate -> write('Executing: '), write(Cmd), nl ; true),
	flush_output(user_output),
	shellCmd(Cmd).


% add a module to be imported in addition to the main module:
addImportModule(Arg) :-
	extractProgName(Arg,Prog),
	isValidModuleName(Prog),
	findSourceProg(Prog,_), !,
	atomCodes(NewImp,Prog),
	retract(addImports(OldAddImps)),
	(member(NewImp,OldAddImps) -> NewAddImps = OldAddImps
	  ; NewAddImps = [NewImp|OldAddImps]),
	asserta(addImports(NewAddImps)).
addImportModule(Arg) :-
	writeErr('ERROR: Source file of module "'),
	atomCodes(ArgA,Arg), writeErr(ArgA),
	writeLnErr('" not found!').

% show the Curry programs in a given directory and its subdirectories:
% (where we assume that the subdirectories contain hierarchical modules)
showProgramsInDirectory(Dir) :-
	format('In directory "~w":~n',[Dir]),
        showProgramsInDirectory('',Dir),
	nl, nl.

showProgramsInDirectory(Prefix,Dir) :-
	(directoryFiles(Dir,Files)
         -> sort(Files,SFiles),
            map1partialM(user:showCurryProgramInDir(Prefix,Dir),SFiles)
          ; true).

showCurryProgramInDir(Prefix,_,File) :-
	atom_codes(File,FileS),
	(append(ProgS,".curry",FileS) ; append(ProgS,".lcurry",FileS)),
        ProgS = [_|_], !,
        format('~s~s ',[Prefix,ProgS]).
showCurryProgramInDir(_,_,SDir) :-
        atom_codes(SDir,[C|_]),
        % ignore dirs not starting with uppercase letter as hierarchical names
        (C<65 ; C>90), !.
showCurryProgramInDir(Prefix,Dir,SDir) :-
        appendAtoms([Dir,'/',SDir],SubDir),
        existsDirectory(SubDir), !,
        appendAtoms([Prefix,SDir,'.'],NewPrefix),
        showProgramsInDirectory(NewPrefix,SubDir).
showCurryProgramInDir(_,_,_).

% get the editor command (for editing files):
getEditor(Editor) :- pakcsrc(editcommand,Editor), \+ Editor='', !.
getEditor(Editor) :- getEnv('EDITOR',Editor), \+ Editor='', !.
getEditor('vi').

% get the pager command (for showing files):
getPager(Pager) :- pakcsrc(showcommand,Pager), \+ Pager='', !.
getPager(Pager) :- getEnv('PAGER',Pager), \+ Pager='', !.
getPager('cat').

% Wait for a file and delete it (used for synchronization with external tools).
% Second argument is the number of waiting attempts before failing.
waitForFile(FName,_) :-
	existsFile(FName), !,
	deleteFile(FName).
waitForFile(_,0) :- !, fail.
waitForFile(FName,N) :-
	sleepSeconds(1), N1 is N-1,
	waitForFile(FName,N1).

% write a pair of a module / file name:
writeModuleFile((M,F)) :-
	write(M),
	atom_codes(M,MS), length(MS,L), BL is 25-L, writeBlanks(BL),
	write(' (loaded from '), write(F), write(')'), nl.

% compile a source program into Prolog code:
processCompile(ProgS,PrologFile) :-
	verbosity(Verbosity),
	parser_warnings(PWarnings),
        (Verbosity=0 -> Warnings=no ; Warnings=PWarnings),
	parseProgram(ProgS,Verbosity,Warnings),
	atom_codes(Prog,ProgS),
	prog2PrologFile(Prog,LocalPrologFile),
	tryXml2Fcy(Prog),
	(findFlatProgFileInLoadPath(Prog,PathProgName)
	 -> true
	  ; writeErr('ERROR: FlatCurry file for program "'),
	    writeErr(Prog),
	    writeLnErr('" not found!'),
	    deletePrologTarget(LocalPrologFile),!, failWithExitCode),
	prog2PrologFile(PathProgName,PrologFile),
	checkProgramHeader(PrologFile),
	c2p(Prog,PrologFile),
	!.
processCompile(_,_) :- failWithExitCode.

% recompile Prolog code of main program:
reloadMainProgram :-
	lastload(LastLoad),
	(LastLoad="" -> ProgS="Prelude" ; ProgS=LastLoad),
	atom_codes(Prog,ProgS),
	(findFlatProgFileInLoadPath(Prog,PathProgName)
	 -> true
	  ; writeErr('ERROR: FlatCurry file for program "'),
	    writeErr(Prog),
	    writeLnErr('" not found!'),
	    !, fail),
	prog2PrologFile(PathProgName,PrologFile),
	loadMain(PrologFile),
	!.


% create a saved state for an already compiled Curry program:
createSavedState(ProgPl,ProgState,InitialGoal) :-
	writeLnErrNQ('>>> Creating saved state without interactive environment...'),
	findall(assertz(prologbasics:pakcsrc(Tag,Value)),pakcsrc(Tag,Value),Pakcsrcs),
	foldr(',',true,Pakcsrcs,AssertPakcsrcs),
	generateMainPlFile(ProgPl,MainPrologFile),
	appendAtom(ProgPl,'.save',TmpSavePl),
	tell(TmpSavePl),
	installDir(PH),
	appendAtom(PH,'/src/',PHCP),
	appendAtom(PHCP,'prologbasics.pl',PrologBasicsPl),
	appendAtom(PHCP,'basics.pl',BasicsPl),
	appendAtom(PHCP,'evaluator.pl',EvalPl),
	appendAtom(PHCP,'loader.pl',LoadPl),
	writeClause((:- multifile(prologbasics:pakcsrc/2))),
	writeClause((:- dynamic(prologbasics:pakcsrc/2))),
	writeClause((:- AssertPakcsrcs,
		        compile([PrologBasicsPl,BasicsPl,EvalPl,LoadPl]),
		        (retract(rtArgs(_)) -> true ; true),
		        loadAndCompile(ProgPl,[],load(MainPrologFile)),
		        saveprog_entry(ProgState,InitialGoal), halt)),
	told,
	appendAtoms([PH,'/bin/sicstus -l ',TmpSavePl],Cmd),
	shellCmd(Cmd),
	deleteMainPrologFile(MainPrologFile),
	deleteFile(TmpSavePl).

% process the various options of the ":set" command:
processSetOption("+error") :- !,
	writeLnErr('WARNING: option "error" no longer supported!').
processSetOption("-error") :- !,
	writeLnErr('WARNING: option "error" no longer supported!').
processSetOption("+interactive") :- !,
	retract(interactiveMode(_)),
	asserta(interactiveMode(yes)).
processSetOption("-interactive") :- !,
	retract(interactiveMode(_)),
	asserta(interactiveMode(no)).
processSetOption("+first") :- !,
	retract(firstSolutionMode(_)),
	asserta(firstSolutionMode(yes)).
processSetOption("-first") :- !,
	retract(firstSolutionMode(_)),
	asserta(firstSolutionMode(no)).
processSetOption("+plprofile") :- prolog(sicstus), !,
	retract(plprofiling(_)),
	prolog_flag(compiling,_,profiledcode),
	asserta(plprofiling(yes)),
	(lastload("") -> true ; process(":r")).
processSetOption("+plprofile") :- !, onlySICStusMessage('+plprofile').
processSetOption("-plprofile") :- prolog(sicstus), !,
	retract(plprofiling(_)),
	prolog_flag(compiling,_,compactcode),
	asserta(plprofiling(no)),
	(lastload("") -> true ; process(":r")).
processSetOption("-plprofile") :- !, onlySICStusMessage('-plprofile').
processSetOption("+profile") :- !,
	retract(profiling(_)),
	asserta(profiling(yes)).
processSetOption("-profile") :- !,
	retract(profiling(_)),
	asserta(profiling(no)).
processSetOption("+single") :- !, checkDebugMode, singleOn.
processSetOption("-single") :- !, checkDebugMode, singleOff.
processSetOption("+trace") :- !, checkDebugMode, traceOn.
processSetOption("-trace") :- !, checkDebugMode, traceOff.
processSetOption("+spy") :- !, checkDebugMode, spyOn.
processSetOption("-spy") :- !, checkDebugMode, spyOff.
processSetOption("+suspend") :- !,
	retract(suspendmode(_)),
	asserta(suspendmode(yes)).
processSetOption("-suspend") :- !,
	retract(suspendmode(_)),
	asserta(suspendmode(no)).
processSetOption("+time") :- !,
	retract(timemode(_)),
	asserta(timemode(yes)).
processSetOption("-time") :- !,
	retract(timemode(_)),
	asserta(timemode(no)).
processSetOption("+verbose") :- !, setVerboseMode(yes).
processSetOption("-verbose") :- !, setVerboseMode(no).
processSetOption("+warn") :- !,
	retract(parser_warnings(_)),
	asserta(parser_warnings(yes)).
processSetOption("-warn") :- !,
	retract(parser_warnings(_)),
	asserta(parser_warnings(no)).

processSetOption("+compact") :-
	retract(compileWithCompact(_)),
	asserta(compileWithCompact(" --compact")).
processSetOption("-compact") :-
	retract(compileWithCompact(_)),
	asserta(compileWithCompact([])).

processSetOption("+allfails") :-
	(printAllFailures -> true ; asserta(printAllFailures)), !.
processSetOption("-allfails") :-
	(printAllFailures -> retract(printAllFailures) ; true), !.

processSetOption("-consfail") :- printConsFailure(no), !.
processSetOption("-consfail") :- !,
	retract(printConsFailure(_)),
	asserta(printConsFailure(no)),
	reloadMainProgram.
processSetOption(Option) :-
	append("+consfail",OptTailBlanks,Option), !,
	removeBlanks(OptTailBlanks,OptTail),
	retract(printConsFailure(Old)),
	(OptTail=""     -> asserta(printConsFailure(yes)) ;
	 OptTail="all"  -> asserta(printConsFailure(all)) ;
	 OptTail="int"  -> asserta(printConsFailure(int)) ;
	 append("file:",FileS,OptTail)
	   -> atom_codes(File,FileS), asserta(printConsFailure(file(File)))
	    ; asserta(printConsFailure(Old)),
	      writeLnErr('ERROR: illegal option for +consfail!')),
	(Old=no -> reloadMainProgram ; true).

processSetOption("+debug") :- compileWithDebug, !.
processSetOption("+debug") :- !,
	asserta(compileWithDebug),
	(lastload("") -> true ; process(":r")).
processSetOption("-debug") :- compileWithDebug, !,
	retract(compileWithDebug),
	(lastload("") -> true ; process(":r")).
processSetOption("-debug") :- !.

processSetOption("-printfail") :-
	compileWithFailPrint, !,
	retract(compileWithFailPrint),
	(lastload("") -> true ; process(":r")).
processSetOption("-printfail") :- !.
processSetOption("+printfail") :-
	compileWithFailPrint, !.
processSetOption("+printfail") :-
	asserta(compileWithFailPrint), !,
	(lastload("") -> true ; process(":r")).
processSetOption("v0") :- !, setVerbosity(0).
processSetOption("v1") :- !, setVerbosity(1).
processSetOption("v2") :- !, setVerbosity(2).
processSetOption("v3") :- !, setVerbosity(3).

processSetOption("path") :- !,
	setCurryPath(''),
	printCurrentLoadPath.
processSetOption(Option) :-
	append("path ",OptTail,Option), !,
	removeBlanks(OptTail,P),
	pathString2loadPath(P,Dirs),
	map2M(basics:toAbsPath,Dirs,AbsDirs),
	path2String(AbsDirs,PathS),
	atom_codes(Path,PathS),
	setCurryPath(Path),
	printCurrentLoadPath.
processSetOption(Option) :-
	append("printdepth ",OptTail,Option), !,
	removeBlanks(OptTail,P),
	(codes2number(P,D) -> true
	       ; write('Illegal print depth number'), nl, fail),
	retract(printDepth(_)),	
	(D=0 -> D1=D ; D1 is D+1),
	asserta(printDepth(D1)).
processSetOption("safe") :- !,
	retract(forbiddenModules(_)),
	asserta(forbiddenModules(['Unsafe'])),
	retract(safeMode(_)), asserta(safeMode(yes)), !.
processSetOption("parser") :- !,
	retract(parserOptions(_)), asserta(parserOptions([])).
processSetOption(Option) :-
	append("parser ",OptTail,Option), !,
	removeBlanks(OptTail,StrippedOpt),
	atom_codes(POpts,StrippedOpt),
	retract(parserOptions(_)), asserta(parserOptions(POpts)).
processSetOption("args") :- !,
	retract(rtArgs(_)), asserta(rtArgs([])).
processSetOption(Option) :-
	append("args ",OptTail,Option), !,
	removeBlanks(OptTail,StrippedOpt),
	split2words(StrippedOpt,ArgsS),
	map2M(prologbasics:atomCodes,Args,ArgsS),
	retract(rtArgs(_)), asserta(rtArgs(Args)).
processSetOption(Option) :-
	append("spy ",OptTail,Option), !,
	checkDebugMode,
	removeBlanks(OptTail,P),
	atom_codes(PN,P),
	spypoint(PN).
processSetOption(_) :- !,
	writeLnErr('ERROR: unknown option. Type :set for help').

printCurrentLoadPath :- verbosityQuiet, !.
printCurrentLoadPath :-
	loadPath('.',LP),
	write('Current search path for loading modules: '), nl,
	path2String(LP,SP),
	atom_codes(ASP,SP), write(ASP), nl.

% fork an expression where arg1 is the expression (string):
processFork(ExprString) :-
	% check the type of the forked expression (must be "IO ()"):
	removeBlanks(ExprString,ExprInput),
	parseMainExpression(ExprInput,none,_Term,Type,_Vs,_,_,_),
	(Type = 'TCons'('Prelude.IO',['TCons'('Prelude.()',[])]) -> true
	  ; write('*** Type error: Forked expression must be of type "IO ()"!'), nl,
	    !, failWithExitCode),
	% start saved program in non-verbose mode:
	verbosemode(QM), setVerboseMode(no),
	processExpression(ExprString,ExecGoal),
	forkProcessForGoal(evaluator:evaluateGoalAndExit(ExecGoal)),
	setVerboseMode(QM),
	sleepSeconds(1). % wait a little bit for starting up the new process

% write free variables, if there are any:
writeFreeVars([]).
writeFreeVars([(V=_)|Bs]) :-
	write('Free variables in goal: '),
	writeVar(user_output,V), writeVars(user_output,Bs), nl.

writeVars(_,[]).
writeVars(Str,[(V=_)|Bs]) :-
	write(Str,', '), writeVar(Str,V),
	writeVars(Str,Bs).


checkDebugMode :-
	compileWithDebug, currentprogram(P), P\=="Prelude", !.
checkDebugMode :-
	write('ERROR: current program not loaded in debug mode, option not applicable!'),
	nl, !, fail.


% exists there a Curry source program with name Prog (of type string)?
findSourceProg(ProgS,SourceFileName) :-
	(Ext = '.curry' ; Ext = '.lcurry'),
	atom_codes(Prog,ProgS),
	findSourceFileInLoadPath(Prog,Ext,PathProgName),
	!,
	atom_codes(PathProgName,SourceFileName).

% Find the path (an atom) to a Curry source program with name Prog (a string)
% If the soure program has a hirarchical name, the path does not contain
% the hierarchy of the name.
findSourceProgPath(ProgS,ProgPath) :-
	(Ext = '.curry' ; Ext = '.lcurry'),
	atom_codes(Prog,ProgS),
	findSourceFileInLoadPath(Prog,Ext,PathProgName),
	!,
	atom_codes(PathProgName,PathProgNameS),
	split2dirbase(Prog,_,ProgName),
	prog2DirProg(ProgName,ProgDName),
	appendAtoms([ProgDName,Ext],LCurryProgName),
	atom_codes(LCurryProgName,LCurryProgNameS),
	(append(ProgPathS,[47|LCurryProgNameS],PathProgNameS)
	 -> atom_codes(ProgPath,ProgPathS)
	  ; LCurryProgNameS=PathProgNameS, % otherwise in current dir
	    ProgPath='.'),
	!.


% exists there a program with name Prog (of type atom)?
prog_exists(Prog) :-
	appendAtom(Prog,'.lcurry',LCurryProgName),
	existsFile(LCurryProgName), !.
prog_exists(Prog) :-
	appendAtom(Prog,'.curry',CurryProgName),
	existsFile(CurryProgName), !.
prog_exists(Prog) :-
	prog2FlatCurryFile(Prog,FLCProgName),
	existsFile(FLCProgName), !.
prog_exists(Prog) :-
	appendAtom(Prog,'_flat.xml',XMLProgName),
	existsFile(XMLProgName), !.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print failures:

failprint(Exp,E,E) :-
	\+ hasPrintedFailure,
	write('Failure due to irreducible expression: '),
	writeCurry(Exp), nl,
	(printAllFailures -> true ; asserta(hasPrintedFailure)),
	!,
	fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call the front-end to parse a Curry program (argument is a string):

parseProgram(ProgS,Verbosity,Warnings) :-
	findSourceProgPath(ProgS,ProgPath), !,
  	installDir(TCP),
	compilerMajorVersion(MajorVersion),
	versionAtom(MajorVersion, MajorVersionAtom),
	compilerMinorVersion(MinorVersion),
	(MinorVersion < 100 -> true
	  ; writeLnErr('ERROR minor version too large!'), fail),
	padVersionAtom(MinorVersion, PaddedMinorVersionAtom),
	appendAtoms(['"',TCP,'/bin/pakcs-frontend" --flat -D__PAKCS__=',
	  MajorVersionAtom,PaddedMinorVersionAtom],CM1),
	(Warnings=no -> appendAtom(CM1,' -W none',CM2)    ; CM2 = CM1 ),
	(Verbosity=0 -> appendAtom(CM2,' --no-verb',CM3)  ; CM3 = CM2 ),
	((Warnings=yes, pakcsrc(warnoverlapping,no))
           -> appendAtom(CM3,' --no-overlap-warn',CM4)    ; CM4 = CM3 ),
	(pakcsrc(curryextensions,yes)
           -> appendAtom(CM4,' --extended',CM5)           ; CM5 = CM4 ),
	getCurryPath(CP),
	getSysLibPath(SysLibPath),
	append(CP,SysLibPath,ImportPath),
	addImports(ImportPath,CM5,CM6),
	parserOptions(POpts),
	atom_codes(Prog,ProgS),
	split2dirbase(Prog,_,ProgName),
	workingDirectory(CurDir),
	setWorkingDirectory(ProgPath),
	appendAtoms([CM6,' ',POpts,' ',ProgName],LoadCmd),
	(shellCmdWithReport(LoadCmd) -> Parse=ok
	  ; writeLnErr('ERROR occurred during parsing!'), Parse=failed),
	setWorkingDirectory(CurDir),
	!, Parse=ok, % proceed only in case of successful parsing
	% finally, we apply the FlatCury preprocessor:
	findSourceProg(ProgS,ProgPathS), !,
	appendAtoms(['"',TCP,'/bin/pakcs-fcypp"'],PP1),
	(verbosity(0) -> appendAtom(PP1,' --quiet',PP2) ; PP2 = PP1 ),
	compileWithCompact(CWC), atom_codes(CWCA,CWC),
	% delete leading './' in ProgPathS:
	(append([46,47],PPS,ProgPathS) -> true ; PPS=ProgPathS),
	atom_codes(PPSA,PPS),
	stripSuffix(PPSA,ProgNameA),
	appendAtoms([PP2,CWCA,' ',ProgNameA],PPCmd),
	(shellCmdWithReport(PPCmd) -> true
	  ; writeLnErr('ERROR occurred during FlatCurry preprocessing!'),
	    fail).
parseProgram(_,_,_). % do not parse if source program does not exist

addImports([],CY,CY).
addImports([I|Is],CY1,CY3) :-
	appendAtoms([CY1,' -i',I],CY2),
	addImports(Is,CY2,CY3).

versionAtom(Version,Atom) :-
	number_chars(Version,Chars),
	atom_chars(Atom,Chars).

padVersionAtom(Version,PaddedAtom) :-
	number_chars(Version,Chars),
	padList(Chars,'0',2,PaddedChars),
	atom_chars(PaddedAtom,PaddedChars).

padList(List,_,Length,PaddedList) :-
	length(List,Length), !,
	PaddedList = List.
padList(List,Pad,Length,PaddedList) :-
	length(List, Length2), Length2 < Length,
	padList([Pad|List],Pad,Length,PaddedList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% small pretty printer for type expressions:

writeType(T) :- writeTypeWithClassContext(T).

% write standard type contexts in their source form, if possible:
writeTypeWithClassContext('FuncType'(C1,'FuncType'(C2,T))) :-
	classDict(C1,A1,Mod1Name,Cls1Name),
	classDict(C2,A2,Mod2Name,Cls2Name), !,
        write('('),
        writeClassContext(Mod1Name,Cls1Name,A1), write(', '),
        writeClassContext(Mod2Name,Cls2Name,A2),
        writeTypeWithRemainingClassContexts(T).
writeTypeWithClassContext('FuncType'(C,T)) :-
	classDict(C,A,ModName,ClsName), !,
        writeClassContext(ModName,ClsName,A), write(' => '),
        writeType(T,top).
writeTypeWithClassContext(T) :- writeType(T,top).

writeTypeWithRemainingClassContexts('FuncType'(C,T)) :-
	classDict(C,A,ModName,ClsName), !, write(', '),
        writeClassContext(ModName,ClsName,A),
        writeTypeWithRemainingClassContexts(T).
writeTypeWithRemainingClassContexts(T) :-
        write(') => '),
        writeType(T,top).

writeClassContext(ModName,ClsName,A) :-
        atom_codes(ModN,ModName),
        currentModuleFile(CurrMod,_),
        ((ModN='Prelude' ; ModN=CurrMod)
          -> true % don't print module prefix if prelude or current module
           ; write(ModN), write('.')),
	atom_codes(ClsN,ClsName), write(ClsN), write(' '), writeType(A,nested).

% the second argument is 'top' or 'nested':
% in case of 'nested', brackets are written around complex type expressions
writeType(A,_) :- var(A), !, write(A).
writeType(A,_) :- atom(A), !, write(A).
writeType('FuncType'(S,T),top) :-
	(S='FuncType'(_,_) -> S_Tag=nested ; S_Tag=top),
	writeType(S,S_Tag), write(' -> '), writeType(T,top).
writeType('FuncType'(S,T),nested) :-
	(S='FuncType'(_,_) -> S_Tag=nested ; S_Tag=top),
	write('('), writeType(S,S_Tag), write(' -> '), writeType(T,top),
	write(')').
writeType('TCons'('Prelude.Apply',[S,T]),top) :-
        !, % print type constructor "Prelude.Apply" as type application
	writeType(S,nested), write(' '), writeType(T,nested).
writeType('TCons'('Prelude.Apply',[S,T]),nested) :-
        !, % print type constructor "Prelude.Apply" as type application
	write('('), writeType(S,nested), write(' '), writeType(T,nested),
	write(')').
writeType('TCons'([],['TCons'('Prelude.Char',[])]),_) :-
        !, % print "[Char]" as "String"
	write('String').
writeType('TCons'([],[T]),_) :- !,  % list type
	write('['), writeType(T,top), write(']').
writeType('TCons'(TC,[Type|Types]),_) :-
	isTupleCons(TC), % tuple type constructor
	!,
	write('('), writeType(Type,top), writeTupleType(Types), write(')').
writeType('TCons'(TC,[]),_) :- writeTypeCons(TC), !.
writeType('TCons'(TC,[T1,T2]),top) :- isTypeApplyCons(TC), !,
	writeType(T1,nested), write(' '), writeType(T2,nested), !.
writeType('TCons'(TC,[T1,T2]),nested) :- isTypeApplyCons(TC), !,
	write('('),
        writeType(T1,nested), write(' '), writeType(T2,nested),
        write(')'), !.
writeType('TCons'(TC,Ts),top) :-
	writeTypeCons(TC), writeTypes(Ts), !.
writeType('TCons'(TC,Ts),nested) :-
	write('('), writeTypeCons(TC),
	writeTypes(Ts), write(')'), !.

isTypeApplyCons(TC) :-
        atom_codes(TC,TCS),
        append(_,[46,64],TCS), !. % 64 = @

writeTypeCons(TC) :-
        atomic2Codes(TC,TCS), % hack for SWI 7.x if TC == []
	append("Prelude.",NameS,TCS), !,
	atom_codes(Name,NameS), write(Name).
writeTypeCons(TC) :-
	currentModuleFile(Mod,_), atom_codes(Mod,ModS),
	atomic2Codes(TC,TCS), % hack for SWI 7.x if TC == []
	(append(ModS,[46|NameS],TCS)
	 -> atom_codes(Name,NameS), write(Name)
	  ; write(TC)), !.

writeTupleType([]).
writeTupleType([T|Ts]) :-
	write(','),
	writeType(T,top),
	writeTupleType(Ts).

writeTypes([]).
writeTypes([T|Ts]) :- write(' '), writeType(T,nested), writeTypes(Ts).


% provide a name for each variable
numbersmallvars(N1,N2,A) :-
	var(A), !,
	N2 is N1+1,
	atom_codes(A,[N1]).
numbersmallvars(N1,N2,T) :-
	T =.. [_|Ts],
	numbersmallvarsl(N1,N2,Ts).

numbersmallvarsl(N,N,[]).
numbersmallvarsl(N1,N3,[T|Ts]) :-
	numbersmallvars(N1,N2,T), numbersmallvarsl(N2,N3,Ts).

% replace each variable in a term by a unique integer
% (starting from the first arg):
% (this is similar to numbervars except that no special structure
% is put around the integer):
vars2integers(A,N1,N2) :-
	var(A), !,
	N2 is N1+1,
	A=N1.
vars2integers(T,N1,N2) :-
	T =.. [_|Ts],
	vars2integersl(Ts,N1,N2).

vars2integersl([],N,N).
vars2integersl([T|Ts],N1,N3) :-
	vars2integers(T,N1,N2), vars2integersl(Ts,N2,N3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates for debugging:

% assert new spy points:
spypoint(FName) :-
	nonvar(FName),
	transDefinedFunc(FName,IntName),
	retract(spypoints(SPs)), asserta(spypoints([IntName|SPs])),
	spyOn, traceOff, singleOff,
	write('Spy mode turned on'), nl, !.
spypoint(FName) :-
	write('ERROR: Cannot place spy point on '), write(FName), nl.

singleOn :- singlestep -> true ; assert(singlestep).
singleOff :- singlestep -> retract(singlestep) ; true.
traceOn :- tracemode -> true ; assert(tracemode).
traceOff :- tracemode -> retract(tracemode) ; true.
spyOn  :- spymode -> true ; assert(spymode).
spyOff :- spymode -> retract(spymode) ; true.

spythis(_) :- tracemode, !.
spythis(Call) :- 
	spymode,
	functor(Call,FName,_),
	spypoints(SPs),
	member(FName,SPs),
	singleOn, traceOn, spyOff,
	!.

traceCall(P,Skip) :-
	retract(numberOfCalls(NC)), NC1 is NC+1, asserta(numberOfCalls(NC1)),
	% include this to avoid infinite loops (e.g., in a WWW version):
	%(NC1>10000 -> write('Execution aborted: more than 10000 reduction steps'),
        %            nl, abort ; true),
	(profiling(yes) -> profileCall(P) ; true),
	(spythis(P) -> write('Call: '), writeCurry(P), nl,
	               (singlestep -> call_singlestepmenu(Answer) ; true)
                     ; true),
	(Answer==eval ->
           evalToken(Eval),
	   (on_exception(ErrorMsg,nf(P,NF,Eval,_Done),
		                  printError(ErrorMsg))
             -> writeCurry(NF), nl
              ; write('*** Evaluation failed.'), nl), singleOn, traceOn
	  ; Skip=Answer).
traceCall(P,_) :-
	(profiling(yes) -> profileFail(P) ; true),
	((spythis(P) ; spyFail)
	   ->  write('Fail: '), writeCurry(P), nl,
	       (singlestep -> call_singlestepmenu(_) ; true)
            ; true),
	!, fail.

traceExit(P,R,E,Skip) :-
	(nonvar(Skip) % have we reached the exit of the skipped call?
          -> singleOn, traceOn ; true),
	retract(numberOfExits(NE)), NE1 is NE+1, asserta(numberOfExits(NE1)),
	(profiling(yes) -> profileExit(P) ; true),
	(spythis(P) ->
           write('Exit: '), writeCurry(P),
	   (var(E) -> write(' (*suspended*)'), nl
            ; write(' (HNF: '), writeCurry(R),
              write(')'), nl),
           (singlestep -> exit_singlestepmenu ; true)
          ; true).
traceExit(P,_,_,_) :-
	(profiling(yes) -> profileRedo(P) ; true),
	(spythis(P) -> write('Redo: '), writeCurry(P), nl,
	              (singlestep -> exit_singlestepmenu ; true)
                    ; true),
	!, fail.


% single step menu for call ports:
call_singlestepmenu(Skip) :-
        write('(g)o (t)race (n)otrace (s)kip (l)eap (e)val (a)bort <return>(single step) >'),
        get_code(C),nl,
        call_debug_option(C,Skip),        % check input
	% if input trace/notrace -> show menu again
        (member(C,[110,116]) -> call_singlestepmenu(Skip) ; true), !.

call_debug_option(103,_) :- !, skip(10),   % g: turn off single step mode
        singleOff, write('Single step mode off.'), nl.
call_debug_option(116,_) :- !, skip(10),   % t: turn on trace mode
        traceOn, write('Trace mode on.'), nl.
call_debug_option(110,_) :- !, skip(10),   % n: turn off trace mode
        traceOff, write('Trace mode off.'), nl.
call_debug_option(115,skip) :- !, skip(10),   % s: skip over function call
        singleOff, traceOff.
call_debug_option(108,skip) :- !, skip(10),   % l: jump to next spy point
        singleOff, traceOff, spyOn.
call_debug_option(97,_) :- !, skip(10),   % a: abort
        raise_exception(debugger_abort). %abort.
call_debug_option(101,eval) :- !, skip(10),   % e: evaluate to normal form
	singleOff, traceOff.
call_debug_option(10,_) :- !. % <return>: do the next single step
call_debug_option(_,S) :- write('ERROR: wrong option!'), nl,
	skip(10), call_singlestepmenu(S).

% single step menu for exit ports:
exit_singlestepmenu :-
        write('(g)o (t)race (n)otrace (l)eap (a)bort <return>(single step) >'),
        get_code(C), nl,
        exit_debug_option(C),        % check input
	% if input trace/notrace -> show menu again
        (member(C,[110,116]) -> exit_singlestepmenu ; true), !.

exit_debug_option(103) :- !, skip(10),   % g: turn off single step mode
        singleOff, write('Single step mode off.'), nl.
exit_debug_option(116) :- !, skip(10),   % t: turn on trace mode
        traceOn, write('Trace mode on.'), nl.
exit_debug_option(110) :- !, skip(10),   % n: turn off trace mode
        traceOff, write('Trace mode off.'), nl.
exit_debug_option(108) :- !, skip(10),   % l: jump to next spy point
        singleOff, traceOff, spyOn.
exit_debug_option(97) :- !, skip(10),   % a: abort
        raise_exception(debugger_abort). %abort.
exit_debug_option(10) :- !. % do the next single step
exit_debug_option(_) :- write('ERROR: wrong option!'), nl,
	skip(10), exit_singlestepmenu.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of internal <-> external names:
%
% translate a functor (operation or constructor symbol) from
% external into internal representation (fails if functor unknown):

% unqualified access to entities of main module is always allowed:
transDefinedFunc(F,ModFAtom) :-
	currentModuleFile(Mod,_), atom_codes(Mod,ModS),
	atom_codes(F,Fs),
	append(ModS,[46|Fs],ModFs),
	%atom_codes(ModF,ModFs),
	flatName2Atom(ModFs,ModFAtom),
	constructorOrFunctionType(ModFAtom,_,_,_), !.
% allow qualified access to prelude names in the interpreter shell:
transDefinedFunc(F,ModFAtom) :-
	atom_codes(F,FS),
	(append("prelude.",FNS,FS) ; append("Prelude.",FNS,FS)),
	%atom_codes(FN,FNS),
	flatName2Atom(FNS,ModFAtom),
	constructorOrFunctionType(ModFAtom,_,_,_), !.
% allow unqualified access to other entities if it is unique:
transDefinedFunc(F,_) :-
	constructorOrFunctionType(FI,F,_,_), constructorOrFunctionType(FJ,F,_,_),
	\+ FI=FJ, !,
	writeErr('ERROR: Symbol "'), writeErr(F),
	writeLnErr('" not unique due to multiple imports.'), fail.
transDefinedFunc(F,FI) :-
	constructorOrFunctionType(FI,F,_,_).
% allow access to non-exported qualified names in the interpreter shell:
transDefinedFunc(ModF,ModFAtom) :-
	atom_codes(ModF,ModFS),
	flatName2Atom(ModFS,ModFAtom),
	constructorOrFunctionType(ModFAtom,_,_,_), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parser for expressions:
%
% current restrictions:
% - no lambda abstractions
% - no case expressions
% - no let expressions

hasfixity('.',infixr(5)) :- !. % hack for list cons operator
hasfixity('Prelude..',infixr(9)) :- !. % hack for function composition operator
hasfixity(O,Fixity) :-
	functiontype(O,_,_,_,OFix,_),
	(OFix=nofix -> Fixity=infixl(9) % non declared opids have fixity "infixl 9"
	             ; Fixity=OFix).

% parser for (define) bindings:
mainbinding(X,E) --> id(XS), skipblanks, "=", skipblanks,
	             expr(E), {atom_codes(X,XS)}.



min(X,Y,Z) :- X<Y -> Z=X ; Z=Y.

append3(X,Y,Z,L) :- append(XY,Z,L), append(X,Y,XY).


var2comb(var(F),comb(TF,[])) :- transDefinedFunc(F,TF), !.
var2comb(var(V),var(UV)) :- !,
	(atom_codes(V,[95|_])  % 95 = '_'
	 -> V=UV
	  ; appendAtom('_',V,UV)).
var2comb(E,E) :- \+ E=opid(_).

generateApply(T,[],T).
generateApply(T,[X|Xs],E) :- generateApply('Prelude.apply'(T,X),Xs,E).
		   
getArityFromType(T,0) :- var(T), !.
getArityFromType('FuncType'(_,T2),N) :-
	getArityFromType(T2,N2), N is N2+1.
getArityFromType('TCons'(TC,_),N) :- TC="IO" -> N=1 ; N=0.


% extract the program name from a given input, i.e., remove all blanks,
% replace leading "~" by current home directory,
% delete possible suffix ".curry" or ".lcurry", and delete absolute
% file path prefix if it is identical to the working directory:
extractProgName(S,ProgName) :-
	removeBlanks(S,S1),
	(append(P,".curry",S1) -> true ;
	 append(P,".lcurry",S1) -> true ; P=S1),
	((P=[126|P1], getHomeDirectory(HomeDir))
           -> atom_codes(HomeDir,HomeDirS), append(HomeDirS,P1,PH)
            ; PH=P),
	workingDirectory(Dir),
	atom_codes(Dir,DirS),
	((append(DirS,[47|ProgS],PH), \+ append(_,[47|_],ProgS))
	 -> ProgName=ProgS
	  ; ProgName=PH).

% check whether a program name (obtained by extractProgName) contains
% a valid module name:
isValidProgramName(ProgString) :-
	atom_codes(ProgAtom,ProgString),
	split2dirbase(ProgAtom,_,ModName),
	atom_codes(ModName,ModString),
	isValidModuleName(ModString).

% check whether a module name (a code list) is valid:
isValidModuleName(ModString) :-
	(isValidModuleString(ModString) -> true
	 ; writeErr('ERROR: Illegal module name: '),
	   atom_codes(ModName,ModString), writeLnErr(ModName),
	   fail).

isValidModuleString([]).
isValidModuleString([C|Cs]) :-
	(isLetterDigitCode(C) ; C=95 ; C=46), % letter|_|.
	isValidModuleString(Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% convert an expression (of FlatCurry) into a Prolog term and return
% list of assignments (variable -> Prolog variable):

exp2Term(var(V),Vs,V1,Vs1) :- !, addVar(V,Vs,V1,Vs1).
exp2Term(int(I),Vs,I,Vs) :- !.
exp2Term(float(I),Vs,I,Vs) :- !.
exp2Term(char(I),Vs,I_Atom,Vs) :- !, char_int(I_Atom,I).
exp2Term(comb(Name,Exprs),Vs,CombTerm,Vs1) :- !,
	exp2Terms(Exprs,Vs,Terms,Vs1),
	(constructorOrFunctionType(Name,_,Arity,_)
	  -> true
           ; write('ERROR: type of function "'), write(Name),
             write('" is unknown'), nl, !, fail),
	length(Terms,TArity),
	Missing is Arity-TArity,
	Term =.. [Name|Terms],
	term2partcall(Term,Missing,TermOrPartCall),
	(compileWithSharing(function)
	 -> CombTerm = makeShare(TermOrPartCall,_) % TODO: improve: not necessary for constr
	  ; CombTerm = TermOrPartCall).
exp2Term('Prelude.apply'(F,X),Vs,Term,Vs2) :- !,
	exp2Term(F,Vs,FT,Vs1), exp2Term(X,Vs1,XT,Vs2),
	(compileWithSharing(function)
	 -> Term = makeShare('Prelude.apply'(FT,XT),_)
	  ; Term = 'Prelude.apply'(FT,XT)).
exp2Term(_Expr,_,_,_) :- write('*** Syntax error'), nl,
	!, fail.

exp2Terms([],Vs,[],Vs).
exp2Terms([E|Es],Vs,[T|Ts],Vs2) :-
	exp2Term(E,Vs,T,Vs1),
	exp2Terms(Es,Vs1,Ts,Vs2).

% convert variables into Prolog variables:
addVar(V,[],NV,[(V=NV)]).
addVar(V,[(X=NX)|Vs],NX,[(X=NX)|Vs]) :- V==X, !.
addVar(V,[(X=NX)|Vs],NV,[(X=NX)|Vs1]) :- addVar(V,Vs,NV,Vs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% type check an expression (of FlatCurry):

typecheck(Exp,Type) :-	typecheck(Exp,[],Type,_).

typecheck(var(V),Vs,Type,Vs1) :- !, addVarType(V,Vs,Type,Vs1).
typecheck(int(_),Vs,'TCons'('Prelude.Int',[]),Vs) :- !.
typecheck(float(_),Vs,'TCons'('Prelude.Float',[]),Vs) :- !.
typecheck(char(_),Vs,'TCons'('Prelude.Char',[]),Vs) :- !.
typecheck(comb(Name,[]),Vs,FType,Vs) :- !,
	(constructorOrFunctionType(Name,_,_,FType)
	  -> true
           ; write('ERROR: type of function "'), write(Name),
             write('" is unknown'), nl, !, fail).
typecheck(comb(Name,Exprs),Vs,Type,Vs1) :-
	comb2apply([comb(Name,[])|Exprs],ApplyExpr),
	typecheck(ApplyExpr,Vs,Type,Vs1).
typecheck('Prelude.apply'(F,X),Vs,Type,Vs2) :-
	typecheck(F,Vs,TF,Vs1), !,
	typecheck(X,Vs1,TX,Vs2), !,
	(unify_type(TF,'FuncType'(TA,Type)) -> true ;
          write('ERROR: Type error in application: '),
 	  exp2Term('Prelude.apply'(F,X),[],'Prelude.apply'(FT,XT),_),
	  writeCurry('Prelude.apply'(FT,XT)), nl,
	  write('*** term           : '), writeCurry(FT), nl,
	  numbersmallvars(97,_,TF),
	  write('*** type           : '), writeType(TF), nl,
	  write('*** is not of functional type'), nl,
	  !, fail),
	(unify_type(TA,TX) -> true ;
          write('ERROR: Type error in application: '),
 	  exp2Term('Prelude.apply'(F,X),[],Term,_),
	  writeCurry(Term), nl,
	  numbersmallvars(97,_,(TA,TX)),
	  write('*** required argument type : '), writeType(TA), nl,
	  write('*** does not match         : '), writeType(TX), nl,
	  !, fail).


% translate an expression list into a nested apply:
comb2apply([Expr],Expr) :- !.
comb2apply(Exprs,'Prelude.apply'(AE,E)) :-
	append(Es,[E],Exprs),
	comb2apply(Es,AE).


% add type information for a variable:
addVarType(V,[],Type,[type(V,Type)]). % new type for first variable occurrence
addVarType(V,[type(X,TX)|VTs],TX,[type(X,TX)|VTs]) :- V==X, !.
addVarType(V,[type(X,TX)|VTs],TV,[type(X,TX)|VTs1]) :-
	addVarType(V,VTs,TV,VTs1).

% unify types: instantiate occurrences of type variables by the other type:
unify_type(T1,T2) :- var(T1), var(T2), !, T1=T2.
unify_type(T1,T2) :- var(T1), !, tvar_occurs_not(T1,T2), T1=T2.
unify_type(T1,T2) :- var(T2), !, tvar_occurs_not(T2,T1), T1=T2.
unify_type('FuncType'(T1,T2),'FuncType'(T3,T4)) :-
	unify_type(T1,T3), unify_type(T2,T4).
unify_type('TCons'(TC,TArgs1),'TCons'(TC,TArgs2)) :-
	unify_types(TArgs1,TArgs2).

unify_types([],[]) :- !.
unify_types([T1|Ts1],[T2|Ts2]) :- unify_type(T1,T2), unify_types(Ts1,Ts2).

tvar_occurs_not(TV,T) :- var(T), !, TV\==T.
tvar_occurs_not(TV,'FuncType'(T1,T2)) :-
	tvar_occurs_not(TV,T1), tvar_occurs_not(TV,T2).
tvar_occurs_not(TV,'TCons'(_,TArgs)) :-
	map1partialM(user:tvar_occurs_not(TV),TArgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read a configuration file:
% contains comment lines (start with #)
% or lines of the form "property=value"
readConfigFile(File,Props) :-
	open(File,read,Stream),
	readStreamLines(Stream,Lines),
	close(Stream),
	extractProperties(Lines,Props).

extractProperties([],[]).
extractProperties([[35|_]|Ls],Ps) :- % ignore comment lines starting with #
	!,
	extractProperties(Ls,Ps).
extractProperties([L|Ls],[prop(N,V)|Ps]) :-
	append(Ns,[61|Vs],L), % 61 = '='
	!,
	atom_codes(N,Ns),
	atom_codes(V,Vs),
	extractProperties(Ls,Ps).
extractProperties([_|Ls],Ps) :-
	extractProperties(Ls,Ps).

readStreamLines(Str,[]) :-
	atEndOfStream(Str), !.
readStreamLines(Str,[Line|Lines]) :-
	readStreamLine(Str,Line),
	readStreamLines(Str,Lines).


% Update a configuration file with a list of given properties
updateConfigFile(OrgFile,_,UpdFile) :-
	fileModTime(OrgFile,OrgModTime),
	fileModTime(UpdFile,UpdModTime),
	OrgModTime < UpdModTime, !.  % new file is really newer, nothing to do
updateConfigFile(OrgFile,Props,UpdFile) :-
	appendAtom(UpdFile,'.bak',UpdFileBak),
	renameFile(UpdFile,UpdFileBak),
	open(OrgFile,read,IStream),
	open(UpdFile,write,OStream),
	updateStreamLines(IStream,Props,OStream),
	close(IStream),
	close(OStream),
	writeNQ('>>> '), writeNQ(UpdFile),
	writeNQ(' updated (old version saved in '), writeNQ(UpdFileBak),
	writeNQ(').'), nlNQ.

updateStreamLines(IStr,_,_) :-
	atEndOfStream(IStr), !.
updateStreamLines(IStr,Props,OStr) :-
	readStreamLine(IStr,Line),
	updatePropertyLine(Line,Props,NLine),
	putChars(OStr,NLine),
	put_code(OStr,10),
	updateStreamLines(IStr,Props,OStr).

updatePropertyLine([35|Cs],_,[35|Cs]) :- % ignore comment lines starting with #
	!.
updatePropertyLine(L,Props,NL) :-
	append(Ns,[61|Vs],L), % 61 = '='
	!,
	atom_codes(N,Ns),
	updateProperty(N,Ns,Vs,Props,NL).
updatePropertyLine(L,_,L).

updateProperty(_,Ns,Vs,[],NL) :- append(Ns,[61|Vs],NL).
updateProperty(N,Ns,_,[prop(N,V)|_],NL) :-
	!,
	atom_codes(V,Vs),
	append(Ns,[61|Vs],NL).
updateProperty(N,Ns,Vs,[_|Ps],NL) :- updateProperty(N,Ns,Vs,Ps,NL).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% show source code of a function call:
showSourceCode(F) :- var(F),
	writeLnErr('Cannot show source code of a variable!').
showSourceCode(partcall(_,QF,_)) :- !,
	atom_codes(QF,QFS),
	append(ModS,[46|FS],QFS), !,
	showSourceCodeOfFunction(ModS,FS).
showSourceCode(FCall) :- FCall =.. [QF|_],
	atom_codes(QF,QFS),
	append(ModS,[46|FS],QFS), !,
	showSourceCodeOfFunction(ModS,FS).

% show source code of a function via the simple GUI for showing complete fun's:
showSourceCodeOfFunction(ModS,FunS) :-
        checkCpmTool('curry-showsource','sourceproggui',_),
        checkWish,
	writeNQ('Showing source code of function "'),
	concat([ModS,[46],FunS],QFS), atom_codes(QF,QFS), writeNQ(QF),
	writeNQ('" in separate window...'), nlNQ,
	(retract(lastShownSourceCode(LastModS,LastF)) ; LastModS=[]),
	(LastModS=[] -> true
          ; getModStream(LastModS,LastStr),
            (verbosityIntermediate -> write('SEND: -'), write(LastF), nl
	       ; true),
            on_exception(_ErrorMsg,
	                 (put_code(LastStr,45), write(LastStr,LastF),
			  nl(LastStr), flush_output(LastStr)),
			  retract(sourceCodeGUI(LastModS,_)))),
	!,
	atom_codes(F,FunS),
	(verbosityIntermediate -> write('SEND: +'), write(F), nl ; true),
	getModStream(ModS,Str),
	put_code(Str,43), write(Str,F), nl(Str),
	flush_output(Str),
	assertz(lastShownSourceCode(ModS,F)).

getModStream(ModS,Stream) :-
	sourceCodeGUI(ModS,Stream), !.
getModStream(ModS,InStream) :-
        checkCpmTool('curry-showsource','sourceproggui',ShowSource),
	atom_codes(ModA,ModS),
	appendAtoms([ShowSource,' ',ModA,' 2>/dev/null'],Cmd),
   	execCommand(Cmd,InStream,_,std),
	assertz(sourceCodeGUI(ModS,InStream)).

% terminate all open source code GUIs:
cleanSourceCodeGUIs :-
	retract(sourceCodeGUI(_,Stream)), terminateSourceCodeGUI(Stream), fail.
cleanSourceCodeGUIs :-
	retract(lastShownSourceCode(_,_)), assertz(lastShownSourceCode([],'')).

terminateSourceCodeGUI(Stream) :- 
	nl(Stream), flush_output(Stream), !. %close(Stream), !.

% facts to store the already opened source code GUIs
:- dynamic sourceCodeGUI/2.

% last shown source code:
:- dynamic lastShownSourceCode/2.
lastShownSourceCode([],'').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
