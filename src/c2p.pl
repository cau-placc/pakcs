%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Curry2Prolog interactive system.
%

:- use_module(prologbasics).
:- use_module(pakcsversion).
:- use_module(basics).
:- use_module(version).
:- use_module(loader).
:- use_module(evaluator).
:- use_module(readACY).
:- use_module(compiler). % compiler from FlatCurry into Prolog

:- (swi7orHigher -> set_prolog_flag(double_quotes, codes) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic compileWithCompact/1,
	   parserWarnings/1, parserOptions/1,
	   addImports/1, safeMode/1, echoMode/1, showMode/1.

compileWithCompact([]).  % parsecurry options for compactification
parserWarnings(yes). % no if the warnings of the parser should be suppressed
parserOptions(''). % additional options passed to front end
addImports([]). % additional imports defined by the ":add" command
safeMode(no). % safe execution without IO actions at top level?
echoMode(no). % echo the current REPL input?
showMode(no). % yes if results should be shown with `Prelude.show`

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RC handling:

% Read the PAKCS rc file to define some constants
readRcFile(ArgProps) :-
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
        getHomeDirectory(Home),
	appendAtom(Home,'/.pakcsrc',HomeConfigFile),
	% first, try to install local .pakcsrc file:
        (existsFile(HomeConfigFile)
	 -> readConfigFile(HomeConfigFile,HomeProps)
	  ; HomeProps=[] ),
	(existsFile(ConfigFile)
	 -> readConfigFile(ConfigFile,GlobalProps),
	    updateConfigFile(ConfigFile,HomeProps,HomeConfigFile),
            checkArgProps(ArgProps,GlobalProps)
	  ; GlobalProps=[]),
	concat([ArgProps,HomeProps,GlobalProps],AllProps),
	deletePropDups(AllProps,UniqueProps),
	map1M(basics:assertPakcsrc,UniqueProps), !.
readRcFile(ArgProps) :-
	% maybe the environment variable HOME is not set since the system
	% is not started by a regular user:
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
	(existsFile(ConfigFile)
	 -> readConfigFile(ConfigFile,GlobalProps),
            checkArgProps(ArgProps,GlobalProps)
	  ; GlobalProps=[]),
	concat([ArgProps,GlobalProps],AllProps),
	deletePropDups(AllProps,UniqueProps),
	map1M(basics:assertPakcsrc,UniqueProps).

% Check existence of all properties provided as an argument against the
% globally defined properties. Terminate with error message if some
% undefined property is found.
checkArgProps([],_GlobalProps).
checkArgProps([prop(N,_)|ArgProps],GlobalProps) :-
        (member(prop(N,_),GlobalProps) -> true
          ; writeErr('"'), writeErr(N),
            writeLnErr('" is an unknown property (see "~/.pakcsrc")!'),
            halt(1)),
        checkArgProps(ArgProps,GlobalProps).
        

% Try to install the PAKCS rc file in home dir if not already there:
installRcFileIfNotPresent :-
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
        getHomeDirectory(Home),
	appendAtom(Home,'/.pakcsrc',HomeConfigFile),
	!,
        (existsFile(HomeConfigFile)
	 -> true
	  ; appendAtoms(['cp ',ConfigFile,' ',HomeConfigFile],CpCmd),
	    shellCmd(CpCmd),
	    writeNQ('>>> '),
	    writeNQ(HomeConfigFile), writeNQ(' installed.'), nlNQ).
installRcFileIfNotPresent.

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
        readLineOn,
	getProgramArgs(DArgs),
	processDArgs(DArgs,Props,Args),
	readRcFile(Props),
        checkAndSetTmpDir,
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
        installRcFileIfNotPresent,
	(verbosityNotQuiet
         -> printPakcsHeader,
            writeNQ('Type ":h" for help (contact: pakcs@curry-lang.org)'),
            nlNQ
          ; true),
	flush_output,
	main.
pakcsMain :- halt(1).  % halt if failure (in parameters) occurred

% Checks the "tmpdir" value of the RC file and set the predicate tmpDir/1.
checkAndSetTmpDir :-
        pakcsrc(tmpdir,TmpDir),
        absolute_file_name(TmpDir,AbsTmpDir),
        %write('TMPDIR='), write(AbsTmpDir), nl,
        retract(tmpDir(_)), asserta(tmpDir(AbsTmpDir)), !,
        mainPrologFileName(MainPrologFile),
        (isWritableFile(MainPrologFile) -> true
         ; writeErr('Directory \''), writeErr(AbsTmpDir),
           writeLnErr('\' is not writable!'),
           writeLnErr('Redefine property "tmpdir" in \'~/.pakcsrc\' and start again.'),
           halt(1)).

% extract the initial arguments of the form "-Dprop=value" as a property list:
processDArgs([],[],[]).
processDArgs([Arg|DArgs],[prop(Name,Val)|Props],Args) :-
	atom_codes(Arg,[45,68|Def]), !, % [45,68] = "-D"
	append(NameS,[61|ValS],Def), % 61 = '='
	atom_codes(Name,NameS),
	atom_codes(Val,ValS),
	processDArgs(DArgs,Props,Args).
processDArgs([Arg|Args],[],[Arg|Args]) :-
	atom_codes(Arg,[58|_]), !. % 58 = ':', REPL command, stop processing
processDArgs([Arg|DArgs],Props,[Arg|Args]) :-
	!, % store other args
	processDArgs(DArgs,Props,Args).
processDArgs(Args,[],Args).

% process the remaining run-time arguments:
processArgs(Halt,[]) :- Halt=yes -> halt(0) ; true.
% ignore '--nocypm|-n' or '--noreadline'
% (since they already processed by separate script to invoke the REPL)
processArgs(Halt,[Arg|Args]) :-
        member(Arg,['--nocypm','-n','--noreadline']), !,
	processArgs(Halt,Args).
processArgs(Halt,['--cpm-version'|CArgs]) :-
        (CArgs = [CV|Args] -> retract(cpmVersion(_)), asserta(cpmVersion(CV))
                            ; Args = CArgs), % this case should not occur...
        !,
	processArgs(Halt,Args).
processArgs(Halt,['--nocolor'|Args]) :-
        retract(withColor(_)), asserta(withColor(no)), !,
	processArgs(Halt,Args).
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
	(Arg='--quiet' ; Arg='-quiet' ; Arg='-q'), !,
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
	writeLnErr('-n|--nocypm       : do not invoke "cypm" to compute package load path'),
	writeLnErr('--noreadline      : do not use input line editing via command "rlwrap"'),
	writeLnErr('--nocolor         : do not use colored output'),
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
	writeLnErr('cypm     : Curry package manager'),
	writeLnErr('frontend : Curry front end'),
        nlErr,
        writeLnErr('To get more help about the usage of a tool, type'),
	nlErr,
	writeLnErr('    pakcs <tool> --help').


% Compute the prompt of the interactive loop:
pakcsPrompt('') :- verbosityQuiet, !.
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
	repeat,
	pakcsPrompt(Prompt),
        prompt(_,Prompt), % set prompt for next top-level command
	flush_output(user_output), flush_output(user_error),
	readLine(Input),
        prompt(_,''),  % clear standard Prolog prompt for further getChar
	(Input = end_of_file
         -> true
          ; removeBlanks(Input,ShortInput),
            echoInput(ShortInput),
	    process(ShortInput)),
	cleanupAtEnd,
	exitCode(EC),
	halt(EC).

echoInput(_) :-
        echoMode(no), !.
echoInput(InputS) :-
        pakcsPrompt(Prompt),
        write(Prompt),
        atom_codes(Input,InputS),
        write(Input), nl,
        !.

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
             "edit","eval","fork","help", "info",
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
            "+echo","-echo",
            "+free","-free",
            "+interactive","-interactive",
            "+first","-first",
            "+plprofile","-plprofile",
            "+printfail","-printfail",
            "+profile","-profile",
            "+show","-show",
            "+suspend","-suspend",
            "+time","-time",
            "+verbose","-verbose",
            "+warn","-warn",
            "path","printdepth","v0","v1","v2","v3","v4","parser","safe","args",
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
        Input = [108,101,116,32|I1], % input starts with "let "
        \+ append(_,[32,105,110,32|_],I1), !, % input does not contain " in "
        processLetExpression(Input), !, fail.
process(Input) :-
	processExpression(Input,ExprGoal),
	call(ExprGoal).

% check whether arbitrary top-level IO actions are allowed:
ioAdmissible :- safeMode(yes), !,
	write('Only initial expressions of non I/O type are allowed!'), nl,
	setExitCode(3),
	fail.
ioAdmissible.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process let expression as input:

:- dynamic letExpInput/1.

letExpInput([]). % let expressions defined as REPL input

clearLetExpInput :-
        retract(letExpInput(LetInput)),
        (LetInput = [] -> true
          ; writeLnIntermediate('All previous let expressions deleted.')),
        asserta(letExpInput([])).

% add the current let bindings to an input string:
addCurrentLetBindings(Input,LetInput) :-
        InSep = [32,105,110,10,32,32], % ' in\n  '
        letExpInput(LetBindings),
        append(LetBindings,[Input],LetWithInput),
        intersperse(InSep,LetWithInput,LetInsWithInput),
        concat(LetInsWithInput,LetInput).

processLetExpression(Input) :-
        atom_codes('  in ()',InputSuffix),
        append(Input,[10|InputSuffix],ExprInput),
        parseExpression(no,ExprInput,none,_Term,_Type,_Vs),
        !, % let expression is valid
        retract(letExpInput(OldLets)),
        append(OldLets,[Input],NewLets),
        asserta(letExpInput(NewLets)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process a given main expression:

processExpression(Input,ExprGoal) :-
        createNewTmpDir(MainExprDir),
	(processExpressionInDir(MainExprDir,Input,ExprGoal)
        -> deleteMainExpFiles(MainExprDir)
         ; failProcessExpression(MainExprDir)).

failProcessExpression(MainExprDir) :-
        deleteMainExpFiles(MainExprDir),
        !,
        failWithExitCode.

% process an expression and gettings its type: in show mode and if
% it is neither a functional type nor an I/O, decorate it with Prelude.show
processExpressionInDir(MainExprDir,Input,ExprGoal) :-
        processExpIntoTermTypeVars(MainExprDir,Input,Term,Type,Vs),
        ((showMode(no) ; Type = 'FuncType'(_,_) ;
          Type = 'TCons'('Prelude.IO',['TCons'('Prelude.()',[])]))
         -> ExprGoal = evaluateMainExpression(Term,Type,Vs)
          ; (Type='TCons'('Prelude.IO',_)
             -> wrapExpWithPrint(Input,ShowInput)
              ; wrapExpWithShow(Input,ShowInput)),
            processExpIntoTermTypeVars(MainExprDir,ShowInput,TermS,TypeS,VsS),
	    ExprGoal = evaluateMainExpression(TermS,TypeS,VsS)).

% wrap initial expression with Prelude.show:
wrapExpWithShow(Input,ShowInput) :-
        (append(" where ",_,WherePart), append(InpExp,WherePart,Input))
        -> concat(["Prelude.show (",InpExp,")",WherePart],ShowInput)
         ; concat(["Prelude.show (",Input,")"],ShowInput).

% wrap initial expression with ">>= Prelude.print":
wrapExpWithPrint(Input,InputPrint) :-
        (append(" where ",_,WherePart), append(InpExp,WherePart,Input))
        -> concat(["(",InpExp,") >>= Prelude.print",WherePart],InputPrint)
         ; concat(["(",Input,") >>= Prelude.print"],InputPrint).

processExpIntoTermTypeVars(MainExprDir,Input,Term,Type,Vs) :-
        % read both acy and flat file of main expression:
        writeAndParseExpression(MainExprDir,yes,Input,none,'--acy --flat',
                                MainExprMod,LCP,NewLCP,(AcyProg,FlatProg),
                                FreeVars),
        AcyProg = 'CurryProg'(_,_,_,_,_,_,['CFunc'(_,_,_,AcyQualType,_)],[]),
        !,
        (verbosityCommands
         -> write('Type of expression: '), writeAcyQualType(AcyQualType), nl
          ; true),
        (defaultQualType(AcyQualType,AcyType)
         -> true
         ; write('Overloaded type: '), writeAcyQualType(AcyQualType), nl,
           writeErr('Cannot handle arbitrary overloaded top-level expressions'),
           nlErr,
           writeLnErr('Hint: add type annotation to specialize the type'),
           fail), !,
        (verbosityCommands
         -> showAcyQualType(AcyType,InitExpType,_),
            write('Defaulted type    : '), write(InitExpType), nl
          ; true),
	(AcyQualType = AcyType % no defaulting required
         -> postProcessMainFlatProgExp(MainExprMod,LCP,NewLCP,FlatProg,FreeVars,
                                       Term,Type,Vs)
          ; parseExpressionIn(MainExprDir,yes,Input,AcyType,Term,Type,Vs)),
	!,
	(isIoType(Type) -> ioAdmissible ; true),
	(verbosityCommands
	 -> write('Evaluating expression: '),
	    writeCurryTermWithFreeVarNames(Vs,Term), nl,
	    writeFreeVars(Vs) % print free goal variables if present
	 ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Process a given expression by writing it into a main module
% and calling the front end.
% If the first argument has value `yes`, free variables declared
% in a "where...free" are transformed into arguments of the main operation.
parseExpression(SplitFreeVars,Input,InitAcyType,MainExp,Type,Vs) :-
        createNewTmpDir(MainExprDir),
	(parseExpressionIn(MainExprDir,SplitFreeVars,Input,InitAcyType,
                           MainExp,Type,Vs)
        -> deleteMainExpFiles(MainExprDir)
         ; failProcessExpression(MainExprDir)).

parseExpressionIn(MainExprDir,SplitFreeVars,Input,InitAcyType,
                  MainExp,Type,Vs) :-
        writeAndParseExpression(MainExprDir,SplitFreeVars,Input,
                                InitAcyType,'--flat',MainExprMod,
                                LCP,NewLCP,FlatProg,FreeVars),
        postProcessMainFlatProgExp(MainExprMod,LCP,NewLCP,FlatProg,FreeVars,
                                   MainExp,Type,Vs),
	(verbosityDetailed
          -> write('Translated expression: '), writeq(MainExp), nl
           ; true).

% write a program containing the main expression and parse and load
% it (according to the Target format: '--flat', '--acy', '--acy --flat'):
writeAndParseExpression(MainExprDir,SplitFreeVars,Input,InitAcyType,
                        Target,MainExprMod,LCP,NewLCP,Prog,FreeVars) :-
        (InitAcyType = none
         -> InitMainFuncType = none, Mods = []
          ; showAcyQualType(InitAcyType,InitMainFuncType,Mods)),
        %write('MODULES: '), write(Mods), nl,
        getMainProgPath(MainProgName,MainPath),
	appendAtoms([MainExprDir,'/PAKCS_Main_Exp'],MainExprMod),
	appendAtoms([MainExprMod,'.curry'],MainExprModFile),
        addCurrentLetBindings(Input,LetInput),
        (SplitFreeVars=yes -> splitWhereFree(LetInput,InputExp,FreeVars)
                            ; InputExp=LetInput, FreeVars=[]),
	(MainProgName='Prelude' -> MainImps = Mods
                                 ; MainImps = [MainProgName|Mods]),
	writeMainExprFile(MainExprModFile,MainImps,InputExp,FreeVars,
                          InitMainFuncType),
	(verbosityIntermediate -> PVerb=2 ; PVerb=0),
	workingDirectory(CurDir),
	toAbsPath(MainPath,AbsMainPath),
	getCurryPath(CP), path2Atom(CP,LCP),
        extendPath(AbsMainPath,LCP,NewLCP),
	setCurryPath(NewLCP),
	setWorkingDirectory(MainExprDir),
	(parseProgram("PAKCS_Main_Exp",PVerb,no,Target) -> Parse=ok
                                                         ; Parse=failed),
	setCurryPath(LCP), % restore old settings
	setWorkingDirectory(CurDir),
	Parse=ok, % proceed only in case of successful parsing
	loadPath(AbsMainPath,LoadPath),
	setCurryPath(NewLCP),
	setWorkingDirectory(MainExprDir),
        (Target = '--flat'
	 -> readProg(['.'|LoadPath],'PAKCS_Main_Exp',Prog,_,_)
          ; (Target = '--acy'
             -> readAcy(['.'|LoadPath],'PAKCS_Main_Exp',Prog)
              ; readProg(['.'|LoadPath],'PAKCS_Main_Exp',FlatProg,_,_),
                readAcy(['.'|LoadPath],'PAKCS_Main_Exp',AcyProg),
                Prog = (AcyProg,FlatProg) )),
	setCurryPath(LCP), % restore old settings
	setWorkingDirectory(CurDir).

postProcessMainFlatProgExp(MainExprMod,LCP,NewLCP,FlatProg,FreeVars,
                           MainExp,Type,Vs) :-
	FlatProg = 'Prog'(_,_,_,FDecls,_),
	FDecls = ['Func'(_,_,_,FuncType,'Rule'(RuleArgs,RuleExp))|MoreFs],
	!,
        desugarNewTypesInExp(RuleExp,DesRuleExp),
	((MoreFs=[], simpleFlatExp(DesRuleExp))
          -> FlatExp = DesRuleExp
           ; setCurryPath(NewLCP),
	     compileMainExprProg(MainExprMod),
	     setCurryPath(LCP),
	     map2M(compiler:varIndex2VarExp,RuleArgs,RuleVars),
	     FlatExp = 'Comb'('FuncCall',
	                      "PAKCS_Main_Exp.pakcsMainExp",RuleVars)),
        flatType2MainType([],FuncType,_,MFuncType),
	length(FreeVars,NumFreeVars),
        stripFuncTypes(NumFreeVars,MFuncType,Type),
	flatExp2MainExp([],FlatExp,EVs,MainExp),
	replaceFreeVarInEnv(FreeVars,RuleArgs,EVs,Vs),
	length(FreeVars,FVL), length(RuleArgs,RAL),
	(FVL=RAL
         -> true
          ; writeLnErr('WARNING: Expression with overloaded type!')).

% Get the type of an epxression (via the front end) in AbstractCurry format.
% If the second argument has value `yes`, free variables declared
% in a "where...free" are transformed into arguments of the main operation.
acyTypeOfExpr(Expr,AcyQualType) :-
        createNewTmpDir(MainExprDir),
        (acyTypeOfExprIn(MainExprDir,Expr,no,'--acy',_,_,_,_,AcyQualType)
        -> deleteMainExpFiles(MainExprDir)
         ; failProcessExpression(MainExprDir)).

acyTypeOfExprIn(MainExprDir,Expr,SplitFreeVars,Target,
                MainExprMod,LCP,NewLCP,FreeVars,QualType) :-
        writeAndParseExpression(MainExprDir,SplitFreeVars,Expr,none,Target,
                                MainExprMod,LCP,NewLCP,AcyProg,FreeVars),
        AcyProg = 'CurryProg'(_,_,_,_,_,_,['CFunc'(_,_,_,QualType,_)],[]),
        !.


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
compileMainExprProg(MainExprMod) :-
	prog2PrologFile(MainExprMod,PrologFile),
	c2p(MainExprMod,PrologFile),
	currentModuleFile(CurrMod,_),
	on_exception(ErrorMsg,
                     (addImports(AddImps),
		      loadAndCompile(PrologFile,AddImps,create)),
	             printError(ErrorMsg) ),
	curryModule(CurrMod).

% strip the first n function types
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

% split an input string of the form "Exp where Vs free"
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

writeMainExprFile(ExprFile,MainImps,Input,FreeVars,InitMainFuncType) :-
	(verbosityIntermediate
          -> write('Writing Curry main expression file: '), write(ExprFile), nl
           ; true),
	fileOpenOptions(FOptions),
	open(ExprFile,write,S,FOptions),
	% suppress parser warnings:
	write(S,'{-# OPTIONS_CYMAKE -Wnone #-}'), nl(S),
	addImports(AddImps),
        union(MainImps,AddImps,Imps),
        writeImports(S,Imps),
        (InitMainFuncType = none -> true
         ; write(S,'pakcsMainExp :: '), write(S,InitMainFuncType), nl(S)),
	write(S,'pakcsMainExp'),
	writeFreeVarArgs(S,FreeVars),
	write(S,' = '),
	putChars(S,Input), nl(S),
	close(S),
        (verbosityIntermediate
         -> write('Contents of '), write(ExprFile), write(':'), nl,
            appendAtoms(['cat ',ExprFile],Cmd), shellCmd(Cmd,_), nl
          ; true).

writeFreeVarArgs(_,[]).
writeFreeVarArgs(S,[V|Vs]) :-
        write(S,' '), write(S,V), writeFreeVarArgs(S,Vs).

writeImports(_,[]).
writeImports(S,[Imp|Imps]) :-
	write(S,'import '), write(S,Imp), nl(S),
	writeImports(S,Imps).

% convert variables into Prolog variables:
addVar(V,[],NV,[(V=NV)]).
addVar(V,[(X=NX)|Vs],NX,[(X=NX)|Vs]) :- V==X, !.
addVar(V,[(X=NX)|Vs],NV,[(X=NX)|Vs1]) :- addVar(V,Vs,NV,Vs1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

processCommand("quit",[]) :- !.
processCommand("help",[]) :- !,
	write('Basic commands (commands can be abbreviated to a prefix if unique):'),
        nl, nl,
        write('<expr>              - evaluate expression <expr>'), nl,
        write('let <p> = <expr>    - add let binding for main expression'), nl,
	write(':load <prog>        - compile and load program "<prog>.curry" and all imports'),nl,
	write(':reload             - recompile currently loaded modules'), nl,
	write(':add <m1> .. <mn>   - add modules <m1>,...,<mn> to currently loaded modules'),nl,
	write(':eval <expr>        - evaluate expression <expr>'), nl,
	write(':save               - save executable with main expression "main"'), nl,
	write(':save <expr>        - save executable with main expression <expr>'), nl,
	write(':type <expr>        - show the type of <expression>'), nl,
	write(':quit               - leave the PAKCS environment'), nl, nl,
	write('Further commands:'), nl, nl,
	write(':!<command>         - execute <command> in shell'), nl,
	write(':browse             - browse program and its imported modules'),nl,
	write(':cd <dir>           - change current directory to <dir>'), nl,
	write(':compile <prog>     - alias for ":load <prog>"'),nl,
	write(':coosy              - start Curry Object Observation System'), nl,
	write(':edit               - load source of currently loaded module into editor'), nl,
	write(':edit <m>           - load source of module <m> into editor'), nl,
	write(':fork <expr>        - fork new process evaluating <expr> (of type "IO ()")'), nl,
	write(':help               - show this message'), nl,
	write(':info <f>           - show information of (visible!) function <f>'), nl,
	write(':info <m>.<f>       - show information of function <f> in module <m>'), nl,
	write(':info type  <m>.<t> - show information of type <t> in module <m>'), nl,
	write(':info class <m>.<c> - show information of type class <c> in module <m>'), nl,
	write(':info <opts> ...    - ...and pass options <opts> to command "cpm-query"'), nl,
	write(':interface          - show interface of current program'),nl,
	write(':interface <m>      - show interface of module <m>'),nl,
	write(':modules            - show list of currently loaded modules'), nl,
	write(':peval              - partially evaluate current program'), nl,
	write(':programs           - show names of all Curry programs available in load path'), nl,
	write(':set <option>       - set a command line option'), nl,
	write(':set                - help on :set command'), nl,
	write(':show               - show source of currently loaded Curry program'), nl,
	write(':show <m>           - show source code of module <m>'), nl,
	write(':source <f>         - show source code of (visible!) function <f>'), nl,
	write(':source <m>.<f>     - show source code of function <f> in module <m>'), nl,
	write(':usedimports        - show all used imported functions/constructors'),nl,
	nl, fail.

processCommand("set",[]) :- !, printSetHelp, nl, printCurrentSettings.
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
	extractProgName(Arg,PathProg),
	checkProgramNameAndCD(PathProg,Prog),
	retract(lastload(OldLL)), asserta(lastload(Prog)),
	retract(addImports(OldImps)), asserta(addImports([])),
	(verbosityCommands -> write('Loading program "'),
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
        clearLetExpInput,
	processCompile(Prog,PrologFile),
	!,
	existsFile(PrologFile),
	addImports(AddImps),
	map2M(loader:checkPrologTarget,AddImps,_),
	on_exception(ErrorMsg,
                     (loadAndCompile(PrologFile,AddImps,create),
		      atom_codes(PrologFile,PrologFileL),
                      tmpDir(TmpDir), atom_codes(TmpDir,TmpDirS),
		      (append(TmpDirS,[47|_],PrologFileL)
		       -> % remove temporary Prolog file:
			  deleteFile(PrologFile)
		        ; true),
		      retract(currentprogram(_)),
	              asserta(currentprogram(Prog)),
		      initializationsInProg(ProgInits), call(ProgInits)),
	             printError(ErrorMsg) ),
	(compileWithDebug -> retract(spypoints(_)),
	                     asserta(spypoints([])),
	                     singleOn, traceOn, spyOff
	                   ; true).

processCommand("eval",ExprInput) :- !,
	processExpression(ExprInput,ExprGoal),
	call(ExprGoal).

processCommand("type",ExprInput) :- !,
        acyTypeOfExpr(ExprInput,QualType),
        atom_codes(EI,ExprInput), write(EI), write(' :: '),
        writeAcyQualType(QualType), nl.

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
        shellCmd('which curry-showflat > /dev/null',SFCode),
        % if curry-showflat is not installed, try to use curry-showinterface:
        (SFCode>0
         -> checkCpmTool('curry-showinterface','curry-interface',ShowIntCmd)
          ; shellCmd('which curry-showinterface > /dev/null',SICode),
            (SICode=0
             -> ShowIntCmd = 'curry-showinterface'
              ; appendAtoms(['curry-showflat',' -int'],ShowIntCmd))),
	extractProgName(IFTail,Prog),
	isValidModuleName(Prog),
        atom_codes(ProgA,Prog),
	appendAtoms([ShowIntCmd,' ',ProgA],GenIntCmd),
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
	appendAtoms(['"',BrowseProg,'" "',RealProg,'" & '],BrowseCmd),
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
	         ; path2Atom([CoosySrc|SLP],Path),
	           setCurryPath(Path)),
	printCurrentLoadPath.

processCommand("peval",[]) :- !,
        checkCpmTool('curry-pevalns','peval-noshare',PevalExec),
	lastload(Prog),
	(Prog="" -> writeLnErr('ERROR: no program loaded for partial evaluation'),
	            !, fail
                  ; true),
        atom_codes(ProgA,Prog),
	appendAtoms([PevalExec,' ',ProgA],PevalCmd),
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

processCommand("show",ShTail) :- !,
	writeErr('ERROR: Source file "'),
        atom_codes(ShTailA,ShTail), writeErr(ShTailA),
        writeLnErr('" not found!').

processCommand("source",Arg) :-
	append(PModS,[46|FunS],Arg),
	append(_,[LM],PModS), isLetterDigitCode(LM),
	(\+ member(46,FunS) ; isOperatorName(FunS)),
	!,
	% show source code of function in module
	extractProgName(PModS,ModS),
	showSourceCodeOfFunction(ModS,FunS).

processCommand("source",ExprInput) :- !, % show source code of a function
	parseExpression(no,ExprInput,none,Term,_Type,_Vs),
	showSourceCode(Term).

processCommand("info",Arg) :- !,
        checkCpmTool('cpm-query','cpm-query',CpmQuery),
        split2words(Arg,Args),
        splitOptions(Args,OptWords,MArgs),
        map2M(user:atom_codes,OptAtoms,OptWords), !,
        (MArgs = [Entity]         -> EntityKind='operation' ;
         MArgs = ["type",Entity]  -> EntityKind='type' ;
         MArgs = ["class",Entity] -> EntityKind='class'
         ; writeLnErr('Illegal arguments! Type :h for help'), fail),
        (pakcsrc(infoparams,InfoParams) ; InfoParams=''), !,
        appendAtoms([CpmQuery,' ',InfoParams],InfoCmd),
        infoCommand(EntityKind,[InfoCmd|OptAtoms],Entity).

processCommand("cd",DirString) :- !,
	(DirString="" -> writeLnErr('ERROR: missing argument'), fail
	               ; true),
	atom_codes(Dir,DirString),
        ensureDirectoryExists(Dir), !,
	(setWorkingDirectory(Dir) -> true
                                   ; writeLnErr('ERROR: cd command failed!')).

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
	% start saved program with quiet verbosity if initial goal provided:
	processExpression(MainGoal,ExecGoal),
        SaveGoal = (ProgInits, basics:setVerbosity(0), evaluator:evaluateGoalAndExit(ExecGoal)),
	(verbosityDetailed
          -> write('Goal to execute in saved state:'), nl, writeq(SaveGoal), nl
           ; true),
	(pakcsrc(smallstate,yes)
	 -> atom_codes(ProgA,Prog), prog2PrologFile(ProgA,ProgPl),
	    createSavedState(ProgPl,ProgStName,SaveGoal)
	  ; saveprog_entry(ProgStName,SaveGoal)),
	installDir(PH),
	appendAtoms(['"',PH,'/scripts/makesavedstate" '],CMD1),
	((pakcsrc(standalone,yes), (prolog(swi) ; sicstus310orHigher))
	 -> appendAtom(CMD1,'--standalone ',CMD2)
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

% Split a list of words into options (i.e., starting with "-") and non-options.
splitOptions([],[],[]).
splitOptions([W|Ws],[W|Opts],Words) :-
        W=[45|_], !, % 45 = '-'
        splitOptions(Ws,Opts,Words).
splitOptions([W|Ws],Opts,[W|Words]) :-
        splitOptions(Ws,Opts,Words).

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
	(verbosityCommands -> write('Executing: '), write(Cmd), nl ; true),
	flush_output(user_output),
	shellCmd(Cmd).


% add a module to be imported in addition to the main module:
addImportModule(Arg) :-
	extractProgName(Arg,ProgS),
	isValidModuleName(ProgS),
	findSourceProg(ProgS,_), !,
	atomCodes(NewImp,ProgS),
	retract(addImports(OldAddImps)),
	(member(NewImp,OldAddImps) -> NewAddImps = OldAddImps
	  ; processCompile(ProgS,_), NewAddImps = [NewImp|OldAddImps]),
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
	atom_codes(Prog,ProgS),
        writeNQ('Compiling Curry program "'), writeNQ(Prog), writeLnNQ('"...'),
	verbosity(Verbosity),
	parserWarnings(PWarnings),
        (Verbosity=0 -> Warnings=no ; Warnings=PWarnings),
	parseProgram(ProgS,Verbosity,Warnings,'--flat'),
	prog2PrologFile(Prog,LocalPrologFile),
	tryXml2Fcy(Prog),
	(findFlatProgFileInLoadPath(Prog,PathProgName)
	 -> true
	  ; deletePrologTarget(LocalPrologFile), !, failWithExitCode),
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
	findFlatProgFileInLoadPath(Prog,PathProgName),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ":set" handling:

% process the various options of the ":set" command:
processSetOption("+echo") :- !,	retract(echoMode(_)), asserta(echoMode(yes)).
processSetOption("-echo") :- !,	retract(echoMode(_)), asserta(echoMode(no)).
processSetOption("+error") :- !,
	writeLnErr('WARNING: option "error" no longer supported!').
processSetOption("-error") :- !,
	writeLnErr('WARNING: option "error" no longer supported!').
processSetOption("+interactive") :- !,
	retract(interactiveMode(_)), asserta(interactiveMode(yes)).
processSetOption("-interactive") :- !,
	retract(interactiveMode(_)), asserta(interactiveMode(no)).
processSetOption("+first") :- !,
	retract(firstSolutionMode(_)),
	asserta(firstSolutionMode(yes)).
processSetOption("-first") :- !,
	retract(firstSolutionMode(_)),
	asserta(firstSolutionMode(no)).
processSetOption("+plprofile") :- prolog(sicstus), !,
	retract(plprofiling(_)),
	%set_prolog_flag(compiling,profiledcode),
	set_prolog_flag(profiling,on),
	asserta(plprofiling(yes)),
	(lastload("") -> true ; process(":r")).
processSetOption("+plprofile") :- !, onlySICStusMessage('+plprofile').
processSetOption("-plprofile") :- prolog(sicstus), !,
	retract(plprofiling(_)),
	%set_prolog_flag(compiling,compactcode),
	set_prolog_flag(profiling,off),
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
processSetOption("+show") :- !,
	retract(showMode(_)),
	asserta(showMode(yes)).
processSetOption("-show") :- !,
	retract(showMode(_)),
	asserta(showMode(no)).
processSetOption("+suspend") :- !,
	retract(suspendMode(_)),
	asserta(suspendMode(yes)).
processSetOption("-suspend") :- !,
	retract(suspendMode(_)),
	asserta(suspendMode(no)).
processSetOption("+time") :- !,
	retract(timeMode(_)),
	asserta(timeMode(yes)).
processSetOption("-time") :- !,
	retract(timeMode(_)),
	asserta(timeMode(no)).
processSetOption("+verbose") :- !, setVerbosity(2).
processSetOption("-verbose") :- !, setVerbosity(1).
processSetOption("+warn") :- !,
	retract(parserWarnings(_)),
	asserta(parserWarnings(yes)).
processSetOption("-warn") :- !,
	retract(parserWarnings(_)),
	asserta(parserWarnings(no)).

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
processSetOption("v4") :- !, setVerbosity(4).

processSetOption("path") :- !,
	setCurryPath(''),
	printCurrentLoadPath.
processSetOption(Option) :-
	append("path ",OptTail,Option), !,
	removeBlanks(OptTail,P),
	pathString2loadPath(P,Dirs),
	map2M(basics:toAbsPath,Dirs,AbsDirs),
	path2Atom(AbsDirs,Path),
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
	asserta(forbiddenModules(['System.IO.Unsafe','Unsafe'])),
	retract(safeMode(_)), asserta(safeMode(yes)), !.
processSetOption("parser") :- !,
	retract(parserOptions(_)), asserta(parserOptions('')).
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
	path2Atom(LP,ALP), write(ALP), nl.

printSetHelp :-
	write('Options for ":set" command:'), nl,
	write('+/-allfails     - show all failures if printfail is turned on'), nl,
	write('+/-compact      - reduce size of target program during compilation'), nl,
	write('+/-consfail     - show pattern matching/unification failures'), nl,
	write('                  ("+consfail int": interactive mode to show fail trace)'), nl,
	write('                  ("+consfail all": show complete fail trace)'), nl,
	write('                  ("+consfail file:F": store complete fail trace in file F)'), nl,
	write('+/-debug        - debug mode (compile with debugging information)'), nl,
	write('+/-echo         - turn on/off echoing of commands'), nl,
	write('+/-first        - turn on/off printing only first value'), nl,
	write('+/-interactive  - turn on/off interactive execution of initial expression'), nl,
	write('+/-plprofile    - use Prolog profiler'), nl,
	write('+/-printfail    - show failures in top-level evaluation'), nl,
	write('+/-profile      - show profile data in debug mode'), nl,
	write('+/-show         - use "Prelude.show" to show evaluation results'), nl,
	write('+/-suspend      - show suspended goals at end of suspended computation'), nl,
	write('+/-time         - show execution time'), nl,
	write('+/-warn         - show parser warnings'), nl,
	write('path <path>     - set additional search path for loading modules'), nl,
	write('printdepth <n>  - set print depth to <n> (0 = unlimited)'), nl,
	write('v<n>            - verbosity level'), nl,
	write('                   0: quiet (errors only)'), nl,
	write('                   1: show status messages (default)'), nl,
	write('                   2: show commands and parser output'), nl,
	write('                   3: show intermediate infos'), nl,
	write('                   4: show all details'), nl,
	write('safe            - safe execution mode without I/O actions'), nl,
	write('parser <opts>   - additional options passed to Curry front end'), nl,
	write('args   <args>   - run-time arguments passed to main program'), nl,
	nl,
	write('Options in debug mode:'), nl,
	write('+/-single         - single step mode'), nl,
	write('+/-spy            - spy mode'), nl,
	write('+/-trace          - trace mode'), nl,
	write('spy <function>    - set spy point on <function>'), nl.

printCurrentSettings :-
	write('Current settings: '), nl,
	(printAllFailures -> write('+') ; write('-')),
	write(allfails), write('   '),
	(compileWithCompact([]) -> write('-') ; write('+')),
	write(compact),	write('  '),
	printConsFailure(PrintConsFail),
	(PrintConsFail=no -> write('-') ; write('+')),
	write(consfail),
	(PrintConsFail=no -> write('   ')
	  ; write('('), write(PrintConsFail), write(') ')),
	(compileWithDebug -> write('+') ; write('-')),
	write(debug),	write('  '),
	(echoMode(yes) -> write('+') ; write('-')),
	write(echo), write('     '),
	(firstSolutionMode(yes) -> write('+') ; write('-')),
	write(first), write('   '),
	(interactiveMode(yes) -> write('+') ; write('-')),
	write(interactive), write('  '), nl,
	(compileWithFailPrint -> write('+') ; write('-')),
	write(printfail), write('  '),
	profiling(P), (P=yes -> write('+') ; write('-')),
	write(profile),	write('  '),
	plprofiling(PLP), (PLP=yes -> write('+') ; write('-')),
	write(plprofile), write('  '),
	showMode(SM), (SM=yes -> write('+') ; write('-')),
	write(show), write('   '),
	suspendMode(BM), (BM=yes -> write('+') ; write('-')),
	write(suspend),	write('  '),
	timeMode(T), (T=yes -> write('+') ; write('-')),
	write(time),	write('    '),
	parserWarnings(W), (W=yes -> write('+') ; write('-')),
	write(warn), write('  '),
	nl,
	loadPath('.',LP),
        path2Atom(LP,AP),      write('loadpath          : '), write(AP), nl,
	printDepth(PD),        write('printdepth        : '),
	(PD=0 -> write(PD) ; PD1 is PD-1, write(PD1)), nl,
	verbosity(VL),         write('verbosity         : '), write(VL), nl,
	parserOptions(POpts),  write('parser options    : '), write(POpts), nl,
	rtArgs(RTArgs),        write('run-time arguments: '),
	intersperse(' ',RTArgs,RTBArgs),
	appendAtoms(RTBArgs,AllArgs), write(AllArgs), nl,
	letExpInput(LetBinds), write('let bindings      : '),
        intersperse([10|"                    "],LetBinds,LetBindsLines),
        concat(LetBindsLines,LetBindsString),
        atom_codes(Lets,LetBindsString), write(Lets), nl,
	(compileWithDebug ->
	  (singlestep -> write('+') ; write('-')), write(single), write('  '),
	  (spymode    -> write('+') ; write('-')), write(spy), write('  '),
	  (tracemode  -> write('+') ; write('-')), write(trace), write('  '),
	  write('/ spy points: '), spypoints(SPs), write(SPs), nl
	 ; true),
        (VL>1 -> nl, write('Current "pakcsrc" properties:'), nl, writeRCvalues
               ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fork an expression where arg1 is the expression (string):
processFork(ExprString) :-
	% check the type of the forked expression (must be "IO ()"):
	removeBlanks(ExprString,ExprInput),
	parseExpression(yes,ExprInput,none,_Term,Type,_Vs),
	(Type = 'TCons'('Prelude.IO',['TCons'('Prelude.()',[])]) -> true
	  ; write('*** Type error: Forked expression must be of type "IO ()"!'), nl,
	    !, failWithExitCode),
	% start forked program in quiet verbosity mode:
	processExpression(ExprString,ExecGoal),
	forkProcessForGoal((basics:setVerbosity(0), evaluator:evaluateGoalAndExit(ExecGoal))),
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
% call the front-end to parse a Curry program:
% ProgS: program to be parsed (string)
% Verbosity: verbosity (number, 0 = quiet)
% Warnings: yes/no
% Target: --flat or --acy

parseProgram(ProgS,Verbosity,Warnings,Target) :-
  	installDir(TCP),
	compilerMajorVersion(MajorVersion),
	versionAtom(MajorVersion, MajorVersionAtom),
	compilerMinorVersion(MinorVersion),
	(MinorVersion < 100 -> true
	  ; writeLnErr('ERROR minor version too large!'), fail),
	padVersionAtom(MinorVersion, PaddedMinorVersionAtom),
	getOutDirectory(OutDir),
	appendAtoms(['"',TCP,'/bin/pakcs-frontend" ',Target,
                     ' -o ', OutDir,
                     ' -D__PAKCS__=',
                     MajorVersionAtom,PaddedMinorVersionAtom],CM1),
	(Warnings=no -> appendAtom(CM1,' -W none',CM2)    ; CM2 = CM1 ),
	(Verbosity<2 -> appendAtom(CM2,' --no-verb',CM3)  ; CM3 = CM2 ),
	((Warnings=yes, pakcsrc(warnoverlapping,no))
           -> appendAtom(CM3,' --no-overlap-warn',CM4)    ; CM4 = CM3 ),
	(pakcsrc(curryextensions,no)
           -> appendAtom(CM4,' -XNoFunctionalPatterns -XNoAnonFreeVars',CM5)
            ; CM5 = CM4 ),
	getCurryPath(CP),
	getSysLibPath(SysLibPath),
	append(CP,SysLibPath,ImportPath),
	addImports(ImportPath,CM5,CM6),
	parserOptions(POpts),
	atom_codes(Prog,ProgS),
	split2dirbase(Prog,_,ProgName),
	appendAtoms([CM6,' ',POpts,' ',ProgName],LoadCmd),
	(shellCmdWithReport(LoadCmd) -> Parse=ok
	  ; writeLnErr('ERROR occurred during parsing!'), Parse=failed),
	!, Parse=ok, % proceed only in case of successful parsing
	% finally, we apply the FlatCury preprocessor:
        (Target = '--flat' -> runFCYPP(ProgS) ; true).
parseProgram(_,_,_,_). % do not parse if source program does not exist

% apply the FlatCury preprocessor:
runFCYPP(ProgS) :-
	findSourceProg(ProgS,ProgPathS), !,
  	installDir(TCP),
	appendAtoms(['"',TCP,'/bin/pakcs-fcypp"'],PP1),
	(verbosity(0) -> appendAtom(PP1,' --quiet',PP2) ; PP2 = PP1 ),
	compileWithCompact(CWC), atom_codes(CWCA,CWC),
	% delete leading './' in ProgPathS:
	(append([46,47],PPS,ProgPathS) -> true ; PPS=ProgPathS),
	atom_codes(PPSA,PPS),
	stripSuffix(PPSA,ProgNameA),
	appendAtoms([PP2,CWCA,' "',ProgNameA,'"'],PPCmd),
	(shellCmdWithReport(PPCmd) -> true
	  ; writeLnErr('ERROR occurred during FlatCurry preprocessing!'),
	    fail).

addImports([],CY,CY).
addImports([I|Is],CY1,CY3) :-
	appendAtoms([CY1,' -i"',I,'"'],CY2),
	addImports(Is,CY2,CY3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Try to default a qualified type (in AbstractCurry format) by instantiating
% type variables in type contexts.
% Type variables with a numeric constraint like `Num`/`Integral` or
% `Fractional`/`Floating` are defaulted to `Int` or `Float`, respectively.
% Moreover, existing `Data`, `Eq`, `Ord`, `Read`, and `Show` constraints
% for the same type variable are removed.
% Finally, remaining type variables with `Data` and `Monad` constraints are
% defaulted to `Prelude.Bool` and `Prelude.IO`, respectively.

defaultQualType('CQualType'('CContext'(Ctxt),Type),
                'CQualType'('CContext'([]),DType)) :-
        defaultMap(Ctxt,[],RemCtxt,[],TSub),
        removeConstraints(RemCtxt,TSub,TSubR),
        applyTSub(TSubR,Type,DType).

defaultMap([],RemCtxt,RemCtxt,TSub,TSub).
defaultMap([TC|TCs],RemCtxtO,RemCtxtN,TSub1,TSub2) :-
        typeConstraintWithTVar(TC,QN,TV),
        acyQName2Atom(QN,QNA),
        defaultClass(QNA,DefTC), !,
        defaultMap(TCs,RemCtxtO,RemCtxtN,[map(TV,DefTC)|TSub1],TSub2).
defaultMap([TC|TCs],RemCtxtO,RemCtxtN,TSub1,TSub2) :-
        defaultMap(TCs,[TC|RemCtxtO],RemCtxtN,TSub1,TSub2).

% Is the first argument an ACY type constraint with class name and a single
% type variable?
% The first and second clause is for single and multi-parameter parameter
% type classes, respectively.
typeConstraintWithTVar('Prelude.(,)'(QN,'CTVar'(TV)),QN,TV).
typeConstraintWithTVar('Prelude.(,)'(QN,['CTVar'(TV)]),QN,TV).

defaultClass('Prelude.Num','Int').
defaultClass('Prelude.Integral','Int').
defaultClass('Prelude.Fractional','Float').
defaultClass('Prelude.Floating','Float').
defaultClass('Prelude.Monad','IO').
defaultClass('Prelude.MonadFail','IO').

removeConstraints([],TSub,TSub).
removeConstraints([TC|TCs],TSub1,TSub2) :-
        typeConstraintWithTVar(TC,QN,TV),
        member(map(TV,ST),TSub1), !,
        acyQName2Atom(QN,QNA),
        removeClass(ST,QNA),
        removeConstraints(TCs,TSub1,TSub2).
removeConstraints([TC|TCs],TSub1,TSub2) :-
        typeConstraintWithTVar(TC,QN,TV),
        acyQName2Atom(QN,'Prelude.Data'), !, % default Data to Bool
        writeLnNQ('Defaulting "Data" context to "Bool"...'),
        removeConstraints(TCs,[map(TV,'Bool')|TSub1],TSub2).

removeClass(DClass,Class) :-
        member(DClass,['Int','Float']),
        member(Class,['Prelude.Data','Prelude.Eq','Prelude.Ord',
                      'Prelude.Read','Prelude.Show']), !.
removeClass(DClass,'Prelude.Enum') :-
        member(DClass,['Char','Int','Bool','Ordering']), !.
removeClass('IO','Monad').

applyTSub(TSub,'CTVar'(TV),DefType) :-
        member(map(TV,DTC),TSub), !,
        atom2String('Prelude',MNS), atom2String(DTC,TNS),
        DefType = 'CTCons'('Prelude.(,)'(MNS,TNS)).
applyTSub(_,'CTVar'(TV),'CTVar'(TV)).
applyTSub(TSub,'CFuncType'(T1,T2),'CFuncType'(ST1,ST2)) :-
        applyTSub(TSub,T1,ST1),
        applyTSub(TSub,T2,ST2).
applyTSub(_,'CTCons'(QN),'CTCons'(QN)).
applyTSub(TSub,'CTApply'(T1,T2),'CTApply'(ST1,ST2)) :-
        applyTSub(TSub,T1,ST1),
        applyTSub(TSub,T2,ST2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pretty print a qualified type given as an AbstractCurry term:

writeAcyQualType(QualType) :-
        showAcyQualType(QualType,T,_),
        write(T).

% Transforms a ACY qualified type into a pretty-printed atom. In addition,
% the last argument contains the list of modules (except for the Prelude)
% occurring in the qualified type.
showAcyQualType(AcyQualType,T,Mods) :-
        AcyQualType = 'CQualType'('CContext'(Ctxt),Type),
        showAcyCtxt(Ctxt,TCtxt),
        showAcyType(Type,TType),
        appendAtoms([TCtxt,TType],T),
        modsOfAcyQualType(AcyQualType,Mods).

% Compute the list of all module names occurring in an ACY qualified type.
modsOfAcyQualType('CQualType'('CContext'(Ctxt),Type),Mods) :-
        modsOfAcyCtxt(Ctxt,Mods1),
        modsOfAcyType(Type,Mods2),
        union(Mods1,Mods2,Mods), !.
modsOfAcyQualType(AcyQualType,[]) :- % just to be sure:
        writeLnErr('WARNING: internal error when processing ACY type:'),
        writeLnErr(AcyQualType).

% Compute the list of all module names occurring in an ACY type context.
modsOfAcyCtxt([],[]).
modsOfAcyCtxt(['Prelude.(,)'(QN,CTs)|Cs],Ms) :-
        modsOfAcyTypes(['CTCons'(QN)|CTs],Ms1),
        modsOfAcyCtxt(Cs,Ms2),
        union(Ms1,Ms2,Ms).

% Compute the list of all module names occurring in an ACY type.
modsOfAcyType('CTVar'(_),[]).
modsOfAcyType('CFuncType'(Ty1,Ty2),Ms) :-
        modsOfAcyType(Ty1,Ms1), modsOfAcyType(Ty2,Ms2),
        union(Ms1,Ms2,Ms).
modsOfAcyType('CTCons'('Prelude.(,)'(MS,_)),Ms) :-
        string2Atom(MS,MA),
        (MA='Prelude' -> Ms=[] ; Ms=[MA]).
modsOfAcyType('CTApply'(Ty1,Ty2),Ms) :-
        modsOfAcyType(Ty1,Ms1), modsOfAcyType(Ty2,Ms2),
        union(Ms1,Ms2,Ms).

modsOfAcyTypes([],[]).
modsOfAcyTypes([Ty|Tys],Ms) :-
        modsOfAcyType(Ty,Ms1),
        modsOfAcyTypes(Tys,Ms2),
        union(Ms1,Ms2,Ms).

% Show an ACY type context as an atom:
showAcyCtxt([],'').
showAcyCtxt([C],T) :- !, showAcyTConstr(C,CA), appendAtoms([CA,' => '],T).
showAcyCtxt(Cs,T) :-
        showAcyTConstrs(Cs,TCs),
        appendAtoms(['(',TCs,') => '],T).

showAcyTConstrs([],'').
showAcyTConstrs([C],T) :- !, showAcyTConstr(C,T).
showAcyTConstrs([C|Cs],T) :-
        showAcyTConstr(C,TC), showAcyTConstrs(Cs,TCs),
        appendAtoms([TC,', ',TCs],T).

showAcyTConstr('Prelude.(,)'(QN,CTs),T) :-
        isList(CTs), !, % for multi-parameter type classes
        showAcyQName(QN,TQN),
        showAcyTypes(' ',nested,CTs,TCTs),
        appendAtoms([TQN,' ',TCTs],T).
showAcyTConstr('Prelude.(,)'(QN,CT),T) :-
        showAcyQName(QN,TQN),
        showAcyType(nested,CT,TCT),
        appendAtoms([TQN,' ',TCT],T).

showAcyType(Ty,T) :- showAcyType(top,Ty,T).

showAcyType(_,'CTVar'('Prelude.(,)'(_,Vs)),V) :-
        string2Atom(Vs,V).
showAcyType(P,'CFuncType'(Ty1,Ty2),T) :-
        (Ty1 = 'CFuncType'(_,_) -> T1P = nested ; T1P = top),
        showAcyType(T1P,Ty1,T1), showAcyType(top,Ty2,T2),
        appendAtoms([T1,' -> ',T2],TF),
        bracketAtom(P,TF,T).
showAcyType(_,'CTCons'(QN),T) :- showAcyQName(QN,T).
showAcyType(_,'CTApply'('CTCons'(QN),Ty2),T) :- % list type
        acyQName2Atom(QN,QNA), QNA='Prelude.[]', !,
        showAcyType(top,Ty2,T2),
        appendAtoms(['[',T2,']'],T).
showAcyType(P,Ty,T) :-
        acyApplyCons(Ty,QN), acyApplyArgs(Ty,TArgs), !,
        acyQName2Atom(QN,QNA),
        (atom_codes(QNA,[80,114,101,108,117,100,101,46,40,44|_]) % tuple type
         -> showAcyTypes(',',top,TArgs,Ts), bracketAtom(nested,Ts,T)
          ; showAcyQName(QN,TQ),
            showAcyTypes(' ',nested,TArgs,Ts),
            appendAtoms([TQ,' ',Ts],TA), bracketAtom(P,TA,T)).
showAcyType(P,'CTApply'(Ty1,Ty2),T) :-
        showAcyType(P,Ty1,T1), showAcyType(nested,Ty2,T2),
        appendAtoms([T1,' ',T2],TA),
        bracketAtom(P,TA,T).

showAcyTypes(_,_,[],'').
showAcyTypes(_,P,[Ty],T) :- !, showAcyType(P,Ty,T).
showAcyTypes(S,P,[Ty|Tys],T) :-
        showAcyType(P,Ty,TTy), showAcyTypes(S,P,Tys,TTys),
        appendAtoms([TTy,S,TTys],T).

acyApplyCons('CTApply'('CTCons'(QN),_),QN) :- !.
acyApplyCons('CTApply'(T1,_),QN) :- acyApplyCons(T1,QN).

acyApplyArgs('CTApply'('CTCons'(_),T),[T]) :- !.
acyApplyArgs('CTApply'(T1,T2),Args) :-
        acyApplyArgs(T1,Args1),
        append(Args1,[T2],Args).

% translate ACY qualified name into atom which is qualified if the module
% qualified is not the prelude or the current module.
showAcyQName('Prelude.(,)'(Mod,Name),T) :-
        string2Atom(Mod,FMod),
	string2Atom(Name,FName),
        ((FMod = 'Prelude' ; currentModuleFile(FMod,_))
         -> T=FName
          ; appendAtoms([FMod,'.',FName],T)).

bracketAtom(top,A,A).
bracketAtom(nested,A,T) :- appendAtoms(['(',A,')'],T).

% translate ACY qualified name into qualified atom
acyQName2Atom('Prelude.(,)'(Mod,Name),QN) :-
        string2Atom(Mod,FMod),
	string2Atom(Name,FName),
        appendAtoms([FMod,'.',FName],QN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	((spythis(P) ; spyFail(yes))
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

call_debug_option(103,_) :- !, skipEOL,   % g: turn off single step mode
        singleOff, write('Single step mode off.'), nl.
call_debug_option(116,_) :- !, skipEOL,   % t: turn on trace mode
        traceOn, write('Trace mode on.'), nl.
call_debug_option(110,_) :- !, skipEOL,   % n: turn off trace mode
        traceOff, write('Trace mode off.'), nl.
call_debug_option(115,skip) :- !, skipEOL,   % s: skip over function call
        singleOff, traceOff.
call_debug_option(108,skip) :- !, skipEOL,   % l: jump to next spy point
        singleOff, traceOff, spyOn.
call_debug_option(97,_) :- !, skipEOL,   % a: abort
        raise_exception(debugger_abort). %abort.
call_debug_option(101,eval) :- !, skipEOL,   % e: evaluate to normal form
	singleOff, traceOff.
call_debug_option(10,_) :- !. % <return>: do the next single step
call_debug_option(_,S) :- write('ERROR: wrong option!'), nl,
	skipEOL, call_singlestepmenu(S).

% single step menu for exit ports:
exit_singlestepmenu :-
        write('(g)o (t)race (n)otrace (l)eap (a)bort <return>(single step) >'),
        get_code(C), nl,
        exit_debug_option(C),        % check input
	% if input trace/notrace -> show menu again
        (member(C,[110,116]) -> exit_singlestepmenu ; true), !.

exit_debug_option(103) :- !, skipEOL,   % g: turn off single step mode
        singleOff, write('Single step mode off.'), nl.
exit_debug_option(116) :- !, skipEOL,   % t: turn on trace mode
        traceOn, write('Trace mode on.'), nl.
exit_debug_option(110) :- !, skipEOL,   % n: turn off trace mode
        traceOff, write('Trace mode off.'), nl.
exit_debug_option(108) :- !, skipEOL,   % l: jump to next spy point
        singleOff, traceOff, spyOn.
exit_debug_option(97) :- !, skipEOL,   % a: abort
        raise_exception(debugger_abort). %abort.
exit_debug_option(10) :- !. % do the next single step
exit_debug_option(_) :- write('ERROR: wrong option!'), nl,
	skipEOL, exit_singlestepmenu.


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
checkProgramNameAndCD(ProgString,ModString) :-
	atom_codes(ProgAtom,ProgString),
	split2dirbase(ProgAtom,DirName,ModName),
	atom_codes(ModName,ModString),
	isValidModuleName(ModString), !,
        (DirName = '.'
         -> true
          ; ensureDirectoryExists(DirName),
            writeNQ('Switching to directory "'),
            writeNQ(DirName),
            writeLnNQ('"...'),
            setWorkingDirectory(DirName)).

ensureDirectoryExists(Dir) :- existsDirectory(Dir), !.
ensureDirectoryExists(Dir) :-
        writeErr('ERROR: directory \''),
        writeErr(Dir),
        writeLnErr('\' does not exist!'),
        !, fail.

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
	removeBlanks(Ns,NsS), atom_codes(N,NsS),
	removeBlanks(Vs,VsS), atom_codes(V,VsS),
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
        \+ (existsFile(OrgFile), existsFile(UpdFile)), !. % nothing to do
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
	removeBlanks(Ns,NsS), atom_codes(N,NsS),
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
	append(ModS,[46|FS],QFS),
        (\+ member(46,FS) ; isOperatorName(FS)),
        !,
	showSourceCodeOfFunction(ModS,FS).
showSourceCode(FCall) :- FCall =.. [QF|_],
	atom_codes(QF,QFS),
	append(ModS,[46|FS],QFS),
        (\+ member(46,FS) ; isOperatorName(FS)),
        !,
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
	(verbosityCommands -> write('Executing: '), write(Cmd), nl ; true),
	flush_output(user_output),
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
% Implementation of the 'info' command:
infoCommand(EntityKind,InfoCmd,Arg) :- % show information of qualified entity
	append(PModS,[46|NameS],Arg),
	append(_,[LM],PModS), isLetterDigitCode(LM),
	(\+ member(46,NameS) ; isOperatorName(NameS)),
	!,
	% show information of entity in module
	extractProgName(PModS,ModS),
	showInfoOfModEntity(EntityKind,InfoCmd,ModS,NameS).
infoCommand(operation,InfoCmd,ExprInput) :- % show information of a function
        !,
	parseExpression(no,ExprInput,none,Term,_Type,_Vs),
	showInfoOfFunction(InfoCmd,Term).
infoCommand(_,_,_) :-
        writeLnErr('Entity name is not unqualified! Type :h for help').

% show information of a function:
showInfoOfFunction(_,F) :- var(F),
	writeLnErr('Cannot show information of a variable!').
showInfoOfFunction(InfoCmd,FCall) :-
        (FCall = partcall(_,QF,_) ; FCall =.. [QF|_]), !,
	atom_codes(QF,QFS),
	append(ModS,[46|FS],QFS),
        (\+ member(46,FS) ; isOperatorName(FS)),
        !,
        atom_codes(FSA,FS), decodePrologName(FSA,FSCA), atom_codes(FSCA,FSCS),
	showInfoOfModEntity(operation,InfoCmd,ModS,FSCS).

% show information of an entity via the 'curry-showinfo' tool:
showInfoOfModEntity(EntityKind,InfoCmd,ModS,NameS) :-
	writeNQ('Showing information of '), writeNQ(EntityKind), writeNQ(' "'),
	concat([ModS,[46],NameS],QFS), atom_codes(QF,QFS),
        writeNQ(QF), writeLnNQ('"...'),
	atom_codes(ModA,ModS),
        escapeBackslash(NameS,NameSE),
	atom_codes(NameA,NameSE),
	appendAtoms(['"',NameA,'"'],NameAQuoted),
        appendAtom('--',EntityKind,EKOption),
        append(InfoCmd,[EKOption,ModA,NameAQuoted],CmdWords),
        intersperse(' ',CmdWords,CmdWordsSep),
	appendAtoms(CmdWordsSep,ShowInfoCmd),
	shellCmdWithCurryPathWithReport(ShowInfoCmd).

escapeBackslash([],[]).
escapeBackslash([92|Cs],[92,92|ECs]) :- !, escapeBackslash(Cs,ECs).
escapeBackslash([N|Cs],[N|ECs]) :- escapeBackslash(Cs,ECs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
