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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic mainFunction/1, compileWithCompact/1,
	   parser_warnings/1, freeVarsUndeclared/1, letBindings/1,
	   addImports/1, noIoAtTopLevel/0.

mainFunction("main"). % the main function for options -r and -s
compileWithCompact([]).  % parsecurry options for compactification
parser_warnings(yes). % no if the warnings of the parser should be suppressed
freeVarsUndeclared(no). % yes if free variables need not be declared in initial goals
addImports([]). % additional imports defined by the ":add" command
letBindings([]). % list of top-level bindings
%noIoAtTopLevel. % uncomment this if IO actions are not allowed at top level

% Read the PAKCS rc file to define some constants
readRcFile(ArgProps) :-
	installDir(PH),
        appendAtom(PH,'/pakcsrc.default',ConfigFile),
        getEnv('HOME',Home),
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
	map1M(basics:assertPakcsrc,UniqueProps),
	pakcsrc(verboserc,yes), !,
	writeNQ('>>> Reading RC files:'),
	(existsFile(HomeConfigFile)
	 -> writeNQ(' '), writeNQ(HomeConfigFile) ; true),
	(existsFile(ConfigFile)
	 -> writeNQ(' '), writeNQ(ConfigFile) ; true),
	nlNQ,
	writeNQ('Current configurations: '), nlNQ,
	writeRCvalues.
readRcFile(_) :-
	getEnv('HOME',_), !.
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
	getProgramArgs(Args),
	processDArgs(Args,Props),
	readRcFile(Props),
	processArgs(Args),
	rtargs(RTArgs),
	(RTArgs=[] -> true
	  ; writeNQ('Run-time parameters passed to application: '),
	    writeNQ(RTArgs), nlNQ),
	((firstCmds([FstCmd]), append(":compile ",TailCmd,FstCmd))
	 -> % -c option given, i.e., compile only:
	    extractProgName(TailCmd,ProgString),
	    isValidModuleName(ProgString),
	    atom_codes(ProgAtom,ProgString),
	    split2dirbase(ProgAtom,DirName,ProgName),
	    (setWorkingDirectory(DirName) -> true ; halt(1)),
	    atom_codes(ProgName,ProgNameS),
	    (processCompile(ProgNameS,PrologFile) -> true ; halt(1)),
	    (existsFile(PrologFile)
	     -> loadAndCompile(PrologFile,[],create), halt(0)
	      ; halt(1))
	  ; printPakcsHeader,
	    main).
pakcsMain :- halt(1).  % halt if failure (in parameters) occurred


% extract all run-time arguments of the form "-Dname=value" as a property list:
processDArgs([],[]).
processDArgs([Arg|Args],[prop(Name,Val)|Props]) :-
	atom_codes(Arg,[45,68|Def]), !, % [45,68] = "-D"
	append(NameS,[61|ValS],Def), % 61 = '='
	atom_codes(Name,NameS),
	atom_codes(Val,ValS),
	processDArgs(Args,Props).
processDArgs([_|Args],Props) :-
	processDArgs(Args,Props).

% process the remaining run-time arguments:
processArgs([]).
processArgs([Arg|Args]) :-
	atom_codes(Arg,[45,68|_]), !, % [45,68] = "-D"
	processArgs(Args).            % ignore since already processed
processArgs(['--noreadline'|Args]) :-
	processArgs(Args).            % ignore since already processed
processArgs([Arg|_]) :-
	(Arg='--help' ; Arg='-help' ; Arg='-h' ; Arg='-?'),
	writeMainHelp,
	halt(0). % quit
processArgs([Arg|Args]) :-
	(Arg='--quiet' ; Arg='-quiet' ; Arg='-q'),
	retract(quietmode(_)),
	asserta(quietmode(yes)), !,
	setVerbosity(0),
	processArgs(Args).
processArgs(['--safe'|Args]) :- !, % safe execution mode
	retract(forbiddenModules(_)),
	asserta(forbiddenModules(['Unsafe'])),
	asserta(noIoAtTopLevel), !,
	processArgs(Args).
processArgs(['-set'|Args]) :- !, processArgs(['--set'|Args]). % backward compat.
processArgs(['--set',path,Path|Args]) :- !,
	atom_codes(Path,PathL),
	append("path ",PathL,OptionL),
	processSetOption(OptionL), !,
	processArgs(Args).
processArgs(['--set',printdepth,Num|Args]) :- !,
	atom_codes(Num,NumL),
	append("printdepth ",NumL,OptionL),
	processSetOption(OptionL), !,
	processArgs(Args).
processArgs(['--set','+consfail',ConsOpt|Args]) :- !,
	atom_codes(ConsOpt,ConsOptL),
	((ConsOptL="int" ; ConsOptL="all" ; append("file:",_,ConsOptL))
	 -> append("+consfail ",ConsOptL,OptionL),
	    processSetOption(OptionL),
	    processArgs(Args)
	  ; processSetOption("+consfail"),
	    processArgs([ConsOpt|Args])).
processArgs(['--set',Option|Args]) :-
	atom_codes(Option,OptionL),
	processSetOption(OptionL), !,
	processArgs(Args).
processArgs(['-l',Prog|Args]) :- !,
	atom_codes(Prog,ProgL),
	append(":load ",ProgL,Load),
	storeFirstCmds([Load]),
	processArgs(Args).
processArgs(['-m',Main|Args]) :-
	atom_codes(Main,MainS),
	retract(mainFunction(_)),
	asserta(mainFunction(MainS)), !,
	processArgs(Args).
processArgs(['-r',Prog|Args]) :- !,
	atom_codes(Prog,ProgL),
	append(":load ",ProgL,Load),
	storeFirstCmds([Load,"$MAIN",":quit"]),
	processArgs(Args).
processArgs(['-s',Prog|Args]) :- !,
	atom_codes(Prog,ProgL),
	append(":load ",ProgL,Load),
	storeFirstCmds([Load,":save $MAIN",":quit"]),
	processArgs(Args).
processArgs(['-i',Prog|Args]) :- !,
	atom_codes(Prog,ProgL),
	append(":load ",ProgL,Load),
	storeFirstCmds([Load,"$MAIN"]),
	processArgs(Args).
processArgs(['-c',Prog|Args]) :- !,
	atom_codes(Prog,ProgL),
	append(":compile ",ProgL,Compile),
	storeFirstCmds([Compile]),
	(Args=[] -> true
	  ; writeErr('ERROR: Illegal arguments after "-c": '),
	    writeErr(Args), nlErr, fail).
processArgs([Arg|Args]) :- % for compatibility with KiCS2
	atom_codes(Arg,[58|CmdS]), !, % 58=':'
	map2M(user:isLowerCaseOf,ShortCmd,CmdS),
	expandCommand(ShortCmd,FullCmd),
	extractReplCmdParameters(Args,Params,RArgs),
	processReplCmd(FullCmd,Params,RArgs).
processArgs([Arg|Args]) :-
	retract(rtargs(RTA)),
	append(RTA,[Arg],RTAs),
	assertz(rtargs(RTAs)),
	processArgs(Args).

% extract REPL command parameters (i.e., everything until next REPL command):
extractReplCmdParameters([],[],[]).
extractReplCmdParameters([Arg|Args],[],[Arg|Args]) :-
	atom_codes(Arg,[58|_]), !. % 58=':'
extractReplCmdParameters([Arg|Args],[ArgS|Params],RArgs) :-
	atom_codes(Arg,ArgS),
	extractReplCmdParameters(Args,Params,RArgs).

% process a REPL command parameter (i.e., store it for later processing)
processReplCmd("save",[],Args) :- !,
	addFirstCmds([":save $MAIN"]),
	processArgs(Args).
processReplCmd(Cmd,Params,Args) :- !,
	combine2cmd([Cmd|Params],CmdS),
	addFirstCmds([[58|CmdS]]),
	processArgs(Args).

combine2cmd([],[]).
combine2cmd([X],X).
combine2cmd([X1,X2|Xs],CmdS) :-
	combine2cmd([X2|Xs],X2s),
	append(X1,[32|X2s],CmdS).
	      

% Show the main options of calling pakcs:
writeMainHelp :-
	nlErr,
	writeErr('Usage: pakcs <options>'), nlErr,
	nlErr,
	writeErr('Options:'), nlErr,
	writeErr('-h|--help|-? : show this message and quit'), nlErr,
	writeErr('-q|--quiet   : work silently'), nlErr,
	writeErr('--noreadline : do not use input line editing via command "rlwrap"'), nlErr,
	writeErr('-Dname=val   : define pakcsrc property "name" as "val"'), nlErr,
	writeErr('--safe       : safe execution mode without I/O actions'), nlErr,
	writeErr('--set OPT    : set option OPT (equivalent to command ":set OPT")'), nlErr,
	writeErr('-m F         : define F as the main function (default: main) for -i|-r|-s'), nlErr,
	writeErr('-l PROG      : load PROG.curry as the initial program'), nlErr,
	writeErr('-i PROG      : load PROG.curry and evaluate <main function> after loading'), nlErr,
	writeErr('-r PROG      : load PROG.curry, execute <main function> and quit'), nlErr,
	writeErr('-s PROG      : load PROG.curry, execute ":save <main function>" and quit'), nlErr,
	writeErr('-c PROG      : compile PROG.curry into a Prolog program and quit'), nlErr,
	writeErr('ARGS         : further arguments passed to application (see System.getArgs)'), nlErr.


% Compute the prompt of the interactive loop:
pakcs_prompt('') :- quietmode(yes), !.
pakcs_prompt(Prompt) :-
        currentModule(MN), atom_codes(MN,MNS),
	currentprogram(CPS),
	atom_codes(CP,CPS),
	split2dirbase(CP,_,BaseProg),
	atom_codes(BaseProg,CPS0),
	(MN=BaseProg -> MCP=CPS0
	 ; append(CPS0,"(module: ",CPS1),
	   append(CPS1,MNS,CPS2), append(CPS2,[41],MCP)),
	append(MCP,"> ",PromptString),
	atom_codes(Prompt,PromptString),
	!.

% main read-eval-print loop of the environment:
main :-
	prompt(_,''), % clear standard Prolog prompt
	repeat,
	pakcs_prompt(Prompt), write(Prompt),
	flush_output(user_output),
	flush_output(user_error),
	(firstCmds([Input|NextCmds]) % is there already a command to process?
	 -> storeFirstCmds(NextCmds)
	    %write(Prompt), atom_codes(Inp,Input), write(Inp), nl
	  ; readLine(Input)),
	(Input = -1 -> true
            ; removeBlanks(Input,ShortInput),
	      process(ShortInput)),
	cleanupAtEnd,
	exitCode(EC),
	halt(EC).

% things to do when leaving PAKCS interactive environment:
cleanupAtEnd :- cleanSourceCodeGUIs.

% Expand a command that may be shortened to its full name:
expandCommand(ShortCmd,Cmd) :-
	allCommands(AllCmds),
	findall(FullCmd,prefixOf(ShortCmd,AllCmds,FullCmd),FullCmds),
	(FullCmds=[Cmd] -> true ;
	 (FullCmds=[]
	  -> writeErr('ERROR: unknown command. Type :h for help'),
	     nlErr, fail
	   ; writeErr('ERROR: command not unique. Type :h for help'),
	     nlErr, fail)).

prefixOf(Prefix,[Full|_],Full) :- append(Prefix,_,Full).
prefixOf(Prefix,[_|FullS],Full) :- prefixOf(Prefix,FullS,Full).

% all possible commands:
allCommands(["add","browse","cd","coosy","edit","eval","fork","help",
	     "interface","load","modules","peval","programs","quit","reload",
	     "save","set","show","type","usedimports","xml"]).

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
process(Input) :- % replace $MAIN by <main function>:
	append(I1,[36|I2],Input), append("MAIN",I3,I2), !,
	mainFunction(Main),
	append(Main,I3,MI3), append(I1,MI3,MainInput),
	process(MainInput).
process([58|Cs]) :- !, % 58=':'
	(append(ShortCmd,[32|Rest],Cs) -> true ; ShortCmd=Cs, Rest=[]),
	map2M(user:isLowerCaseOf,LShortCmd,ShortCmd),
	expandCommand(LShortCmd,Cmd),
	removeBlanks(Rest,Params),
	(Cmd="load" -> true ; ioAdmissible),
	processCommand(Cmd,Params).
process(S) :- append("let",STail,S),
	removeBlanks(STail,SStrip),
	mainbinding(Var,Exp,SStrip,[]),
	!, ioAdmissible,
	typecheck(Exp,_Type),
	exp2Term(Exp,[],_Term,Vs),
	(Vs=[(V=_)|Bs]
	 -> write('*** Error: free variables in top-level binding: '),
	    writeVar(user_output,V), writeVars(user_output,Bs), nl, fail
	  ; true),
	retract(letBindings(Lets)), asserta(letBindings([Var=Exp|Lets])),
	!, fail.
process(Input) :-
	processExpression(Input,ExecGoal),
	call(ExecGoal).

% check whether arbitrary top-level IO actions are allowed:
ioAdmissible :- noIoAtTopLevel, !,
	write('Only initial expressions of non I/O type are allowed!'), nl,
	setExitCode(3),
	fail.
ioAdmissible.

processExpression(Input,ExecGoal) :-
	(mainexpr(Exp,FreeVars,Input,[])
          -> true
           ; write('*** Syntax error'), nl, setExitCode(1), !, fail),
	%write('Type expression: '), writeq(Exp), nl,
	typecheck(Exp,Type),
	(isIoType(Type) -> ioAdmissible ; true),
	exp2Term(Exp,[],Term,Vs),
	%write('Translated expression: '), writeq(Term), nl,
	checkFreeVars(FreeVars,Vs),
	(verbosemode(yes) -> write('Evaluating expression: '),
	                     writeCurryTermWithFreeVarNames(Vs,Term),
			     write(' :: '),
	                     numbersmallvars(97,_,Type), writeType(Type), nl,
			     % print free goal variables if present:
			     writeFreeVars(Vs)
		           ; true),
	!,
	ExecGoal = evaluateMainExpression(Term,Type,Vs).


processCommand("quit",[]) :- !.
processCommand("help",[]) :- !,
	write('Commands (can be abbreviated to a prefix if unique):'), nl,
	write(':load <prog>      - compile and load program "<prog>.curry" and all imports'),nl,
	write(':add <prog>       - add module <prog> to currently loaded modules'),nl,
	write(':reload           - recompile currently loaded modules'),nl,
	write(':eval <expr>      - evaluate expression <expr>'), nl,
	write('<expr>            - evaluate expression <expr>'), nl,
	write('let <var>=<expr>  - define variable binding for subsequent goals'), nl,
	write(':type <expr>      - show the type of <expression>'),nl,
	write(':browse           - browse program and its imported modules'),nl,
	write(':interface        - show interface of current program'),nl,
	write(':interface <prog> - show interface of program "<prog>.curry"'),nl,
	write(':usedimports      - show all used imported functions/constructors'),nl,
	write(':edit             - edit current program'), nl,
	write(':edit <file>      - edit file "<file>"'), nl,
	write(':modules          - show list of currently loaded modules'), nl,
	write(':show             - show source of currently loaded Curry program'), nl,
	write(':show <m>         - show source of module <m>'), nl,
	write(':show <f>         - show source code of function <f> in separate window'), nl,
	write(':programs         - show names of all Curry programs available in load path'), nl,
	write(':cd <dir>         - change current directory to <dir>'), nl,
	write(':!<command>       - execute <command> in shell'), nl,
	write(':save             - save current state to <prog>'), nl,
	write(':save <expr>      - save current state to <prog> with initial <expr>'), nl,
	write(':fork <expr>      - fork new process evaluating <expr> (of type "IO ()")'), nl,
	write(':coosy            - start Curry Object Observation System'), nl,
	write(':xml              - translate current program into XML format'), nl,
	write(':peval            - partially evaluate current program'), nl,
	write(':set <option>     - set a command line option'), nl,
	write(':set              - help on :set command'), nl,
	write(':help             - show this message'), nl,
	write(':quit             - leave the PAKCS environment'), nl,
	nl, fail.

processCommand("set",[]) :- !,
	write('Options for ":set" command:'), nl,
	write('+/-allfails       - show all failures if printfail is turned on'), nl,
	write('+/-compact        - reduce size of target program during compilation'), nl,
	write('+/-consfail       - show pattern matching/unification failures'), nl,
	write('                    ("+consfail int": interactive mode to show fail trace)'), nl,
	write('                    ("+consfail all": show complete fail trace)'), nl,
	write('                    ("+consfail file:F": store complete fail trace in file F)'), nl,
	write('+/-debug          - debug mode (compile with debugging information)'), nl,
	write('+/-error          - show messages on standard error in saved program states'), nl,
	write('+/-free           - free variables need not be declared in initial goals'), nl,
	write('+/-plprofile      - use Prolog profiler'), nl,
	write('+/-printfail      - show failures in top-level evaluation'), nl,
	write('+/-profile        - show profile data in debug mode'), nl,
	write('+/-suspend        - show suspended goals at end of suspended computation'), nl,
	write('+/-time           - show execution time'), nl,
	write('+/-verbose        - verbose mode (printing initial goals)'), nl,
	write('+/-warn           - show parser warnings'), nl,
	write('path <path>       - set additional search path for loading modules'), nl,
	write('printdepth <n>    - set print depth to <n> (0 = unlimited)'), nl,
	write('v<n>              - verbosity level'), nl,
	write('                     0: quiet (errors and warnings only)'), nl,
	write('                     1: status messages (default)'), nl,
	write('                     2: intermediate messages and commands'), nl,
	write('                     3: all intermediate results'), nl,
	nl,
	write('Options in debug mode:'), nl,
	write('+/-single         - single step mode'), nl,
	write('+/-spy            - spy mode'), nl,
	write('+/-trace          - trace mode'), nl,
	write('spy <function>    - set spy point on <function>'), nl,
	nl,
	write('Current settings: '), nl,
	(printAllFailures -> write('+') ; write('-')),
	write(allfails), write('  '),
	(compileWithCompact([]) -> write('-') ; write('+')),
	write(compact),	write('    '),
	printConsFailure(PrintConsFail),
	(PrintConsFail=no -> write('-') ; write('+')),
	write(consfail),
	(PrintConsFail=yes -> write('  ')
	  ; write('('), write(PrintConsFail), write(') ')),
	(compileWithDebug -> write('+') ; write('-')),
	write(debug),	write('  '),
	freeVarsUndeclared(FV), (FV=yes -> write('+') ; write('-')),
	write(free),	write('  '),
	(compileWithFailPrint -> write('+') ; write('-')),
	write(printfail), write('  '),
	nl,
	profiling(P), (P=yes -> write('+') ; write('-')),
	write(profile),	write('   '),
	plprofiling(PLP), (PLP=yes -> write('+') ; write('-')),
	write(plprofile), write('  '),
	suspendmode(BM), (BM=yes -> write('+') ; write('-')),
	write(suspend),	write('   '),
	timemode(T), (T=yes -> write('+') ; write('-')),
	write(time),	write('   '),
	verbosemode(V), (V=yes -> write('+') ; write('-')),
	write(verbose),	write(' '),
	parser_warnings(W), (W=yes -> write('+') ; write('-')),
	write(warn), write('  '),
	nl,
	loadPath('.',LP), path2String(LP,SP),
	atom_codes(AP,SP), write('loadpath   = '), write(AP), nl,
	printDepth(PD), write('printdepth = '),
	(PD=0 -> write(PD) ; PD1 is PD-1, write(PD1)), nl,
	verbosity(VL),  write('verbosity  = '), write(VL), nl,
	(compileWithDebug ->
	  (singlestep -> write('+') ; write('-')), write(single), write('  '),
	  (spymode    -> write('+') ; write('-')), write(spy), write('  '),
	  (tracemode  -> write('+') ; write('-')), write(trace), write('  '),
	  write('/ spy points: '), spypoints(SPs), write(SPs), nl
	 ; true),
	!, fail.

processCommand("set",Option) :- !,
	processSetOption(Option),
	!, fail.

processCommand("add",Arg) :- !,
	extractProgName(Arg,Prog),
	isValidModuleName(Prog),
	atom_codes(Imp,Prog),
	retract(addImports(Imps)), asserta(addImports([Imp|Imps])),
        processCommand("reload",[]),
	!, fail.

processCommand("load",Arg) :- !,
	extractProgName(Arg,Prog),
	isValidModuleName(Prog),
	retract(lastload(_)), asserta(lastload(Prog)),
	retract(addImports(_)), asserta(addImports([])),
        processCommand("reload",[]).

processCommand("reload",[]) :- !,
	lastload(Prog),
	(Prog="" -> writeErr('ERROR: no load command to repeat'),
	            nlErr, !, fail
                  ; true),
	processCompile(Prog,PrologFile),
	!,
	existsFile(PrologFile),
	on_exception(ErrorMsg,
                     (addImports(AddImps),
		      loadAndCompile(PrologFile,AddImps,create),
		      atom_codes(PrologFile,PrologFileL),
		      (append("/tmp/",_,PrologFileL)
		       -> % remove temporary Prolog file:
			  deleteFile(PrologFile)
		        ; true),
		      retract(currentprogram(_)),
	              asserta(currentprogram(Prog)),
		      initializationsInProg(ProgInits), call(ProgInits)),
	             printError(ErrorMsg) ),
	retract(letBindings(_)), asserta(letBindings([])),
	(compileWithDebug -> retract(spypoints(_)),
	                     asserta(spypoints([])),
	                     singleOn, traceOn, spyOff
	                   ; true),
	!, fail.

processCommand("eval",ExprInput) :- !,
	processExpression(ExprInput,ExecGoal),
	call(ExecGoal).

processCommand("type",ExprInput) :- !,
	(mainexpr(Exp,FreeVars,ExprInput,[]) -> true
                       ; write('*** Syntax error'), nl, !, failWithExitCode),
	typecheck(Exp,Type),
	exp2Term(Exp,[],Term,Vs),
	checkFreeVars(FreeVars,Vs),
	writeCurryTermWithFreeVarNames(Vs,Term),
	write(' :: '),
	numbersmallvars(97,_,Type), writeType(Type), nl,
	!, fail.

processCommand("usedimports",[]) :- !,
	lastload(Prog),
	(Prog="" -> write('ERROR: no program loaded for analysis'), nl, !, fail
                  ; true),
        atom_codes(ProgA,Prog),
	installDir(PH),
	appendAtoms(['"',PH,'/currytools/importcalls/ImportCalls" ',ProgA],
		    AnaCmd),
        %write('Executing: '), write(AnaCmd), nl,
        shellCmdWithCurryPath(AnaCmd),
	!, fail.

processCommand("interface",[]) :- !,
	lastload(Prog),
	(Prog="" -> processCommand("interface","Prelude")
                  ; processCommand("interface",Prog)).
processCommand("interface",IFTail) :- !,
	extractProgName(IFTail,Prog),
	isValidModuleName(Prog),
        atom_codes(ProgA,Prog),
        installDir(PH),
	appendAtoms(['"',PH,'/currytools/genint/GenInt" -int ',ProgA],
		    GenIntCmd),
        %write('Executing: '), write(GenIntCmd), nl,
        shellCmdWithCurryPath(GenIntCmd),
	!, fail.

processCommand("browse",[]) :- !,
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
        installDir(PH),
	appendAtoms(['"',PH,'/bin/currybrowse" ',RealProg,' & '],BrowseCmd),
        %write('Executing: '), write(BrowseCmd), nl,
        shellCmdWithCurryPath(BrowseCmd),
	!, fail.

processCommand("coosy",[]) :- !,
        installDir(PH),
	appendAtom(PH,'/tools/coosy',CoosyHome),
	getCurryPath(SLP),
	(SLP=[] -> setCurryPath(CoosyHome)
	         ; path2String([CoosyHome|SLP],PathS), atom_codes(Path,PathS),
	           setCurryPath(Path)),
	appendAtoms(['"',CoosyHome,'/CoosyGUI" ',CoosyHome,' &'],GuiCmd),
        %write('Executing: '), write(GuiCmd), nl,
        shellCmdWithCurryPath(GuiCmd),
	(waitForFile('COOSYLOGS/READY',3) -> true
	 ; writeErr('ERROR: COOSy startup failed'), nlErr, fail),
	printCurrentLoadPath,
	!, fail.

processCommand("xml",[]) :- !,
	lastload(Prog),
	(Prog="" -> write('ERROR: no program loaded for XML translation'), nl,
	            !, fail
                  ; true),
        atom_codes(ProgA,Prog),
        installDir(PH),
	appendAtoms(['"',PH,'/tools/curry2xml" ',ProgA],XmlCmd),
        %write('Executing: '), write(XmlCmd), nl,
        shellCmdWithCurryPath(XmlCmd),
	!, fail.

processCommand("peval",[]) :- !,
	lastload(Prog),
	(Prog="" -> write('ERROR: no program loaded for partial evaluation'),
	            nl,
	            !, fail
                  ; true),
        atom_codes(ProgA,Prog),
        installDir(PH),
	appendAtoms(['"',PH,'/tools/Peval/peval" ',ProgA],PevalCmd),
        %write('Executing: '), write(PevalCmd), nl,
        shellCmdWithCurryPath(PevalCmd),
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

processCommand("edit",FileS) :-
	getEditor(Editor),
	atom_codes(Editor,EditorS),
	concat([EditorS," ",FileS," &"],EditCmdS),
	atom_codes(EditCmd,EditCmdS),
	shellCmd(EditCmd),
	!, fail.

% show a list of all Curry programs available in the load path:
processCommand("programs",[]) :- !,
	loadPath('.',LP),
	write('Curry programs available in the load path:'), nl,
	map1M(user:showProgramsInDirectory,LP),
	!, fail.

processCommand("modules",[]) :- !, % show list of currently loaded modules
	findall((M,P),loadedModule(M,P),Mods),
	write('Currently loaded modules:'), nl,
	map1M(user:writeModuleFile,Mods),
	!, fail.

processCommand("show",[]) :- !, % show source code of current module
	currentprogram(Prog),
	(findSourceProg(Prog,SourceFileName)
	 -> atom_codes(File,SourceFileName),
	    getPager(Pager),
	    appendAtoms([Pager,' ',File],Cmd),
	    shellCmd(Cmd)
	  ; write('No source program file available, generating source from FlatCurry...'),
	    nl, nl,
	    installDir(PH), atom_codes(ProgA,Prog),
	    appendAtoms(['"',PH,'/currytools/genint/GenInt" -mod ',ProgA],
			ShowProgCmd),
            %write('Executing: '), write(ShowProgCmd), nl,
	    shellCmdWithCurryPath(ShowProgCmd)),
	!, fail.

processCommand("show",EntityS) :-
	findSourceProg(EntityS,SourceFileName), !, % show source of a module
	atom_codes(File,SourceFileName),
	getPager(Pager),
	appendAtoms([Pager,' "',File,'"'],Cmd),
	shellCmd(Cmd),
	!, fail.

processCommand("show",ExprInput) :- !, % show source code of a function
	(mainexpr(Exp,FreeVars,ExprInput,[]) -> true
                       ; write('*** Syntax error'), nl, !, failWithExitCode),
	typecheck(Exp,_Type),
	exp2Term(Exp,[],Term,Vs),
	checkFreeVars(FreeVars,Vs),
	showSourceCode(Term),
	!, fail.

processCommand("cd",DirString) :- !,
	(DirString="" -> writeErr('ERROR: missing argument'), nlErr, fail
	               ; true),
	atom_codes(Dir,DirString),
	(existsDirectory(Dir)
	 -> (setWorkingDirectory(Dir) -> true
              ; writeErr('ERROR: cd command failed!'), nlErr)
	  ; writeErr('ERROR: directory \''),
	    writeErr(Dir),
	    writeErr('\' does not exist!'),
	    nlErr),
	!, fail.

processCommand("save",MainGoal) :- !,
	currentprogram(Prog),
	atom_codes(ProgName,Prog),
	(Prog="Prelude" -> writeErr('ERROR: no program loaded'),
	                   nlErr, fail
	                 ; true),
	appendAtom(ProgName,'.state',ProgStName),
	initializationsInProg(ProgInits),
	resetDynamicPreds,
	(retract(rtargs(_)) -> true ; true),
	(MainGoal=[]
	 -> saveprog_entry(ProgStName,(ProgInits,user:main))
	 ; % start saved program in non-verbose mode if initial goal provided:
	   verbosemode(QM), setVerboseMode(no),
	   processExpression(MainGoal,ExecGoal),
	   %write('Goal to execute:'), nl, writeq(ExecGoal), nl,
	   (pakcsrc(smallstate,yes)
	    -> atom_codes(ProgA,Prog), prog2PrologFile(ProgA,ProgPl),
	       createSavedState(ProgPl,ProgStName,
			(ProgInits,evaluator:evaluateGoalAndExit(ExecGoal)))
	     ; saveprog_entry(ProgStName,
 		        (ProgInits,evaluator:evaluateGoalAndExit(ExecGoal)))),
	   setVerboseMode(QM)),
	installDir(PH),
	appendAtoms(['"',PH,'/bin/.makesavedstate" '],CMD1),
	((pakcsrc(standalone,yes), (prolog(swi) ; sicstus310orHigher))
	 -> appendAtom(CMD1,'-standalone ',CMD2)
	  ; CMD2=CMD1),
	appendAtoms([CMD2,ProgStName,' ',ProgName],Cmd),
	%write('Executing: '), write(Cmd), nl,
	shellCmd(Cmd),
	write('Executable saved in: '), write(ProgName), nl,
	call(ProgInits),
	!, fail.

processCommand("fork",STail) :- !,
	processFork(STail),
	!,
	fail.

processCommand(_,_) :- !,
	write('ERROR: unknown command. Type :h for help'), nl, fail.


% show the Curry programs in a given directory:
showProgramsInDirectory(Dir) :-
	format('In directory "~w":~n',[Dir]),
	directoryFiles(Dir,Files), sort(Files,SFiles),
	map1M(user:showIfCurryProgram,SFiles),
	nl, nl.
showIfCurryProgram(File) :-
	atom_codes(File,FileS),
	((append(ProgS,".curry",FileS) ; append(ProgS,".lcurry",FileS))
         -> format('~s ',[ProgS])
          ; true).

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
	parseProgram(ProgS),
	atom_codes(Prog,ProgS),
	prog2PrologFile(Prog,LocalPrologFile),
	tryXml2Fcy(Prog),
	(findFlatProgFileInLoadPath(Prog,PathProgName)
	 -> true
	  ; writeErr('ERROR: FlatCurry file for program '),
	    writeErr(Prog),
	    writeErr(' not found!'),
	    nlErr,
	    deletePrologTarget(LocalPrologFile),!, fail),
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
	  ; writeErr('ERROR: FlatCurry file for program '),
	    writeErr(Prog),
	    writeErr(' not found!'),
	    nlErr, !, fail),
	prog2PrologFile(PathProgName,PrologFile),
	loadMain(PrologFile),
	!.


% create a saved state for an already compiled Curry program:
createSavedState(ProgPl,ProgState,InitialGoal) :-
	writeErrNQ('>>> Creating saved state without interactive environment...'),
	nlErrNQ,
	findall(assertz(prologbasics:pakcsrc(Tag,Value)),pakcsrc(Tag,Value),Pakcsrcs),
	foldr(',',true,Pakcsrcs,AssertPakcsrcs),
	generateMainPlFile(ProgPl,MainPrologFile),
	appendAtom(ProgPl,'.save',TmpSavePl),
	tell(TmpSavePl),
	installDir(PH),
	appendAtom(PH,'/curry2prolog/',PHCP),
	appendAtom(PHCP,'prologbasics.pl',PrologBasicsPl),
	appendAtom(PHCP,'basics.pl',BasicsPl),
	appendAtom(PHCP,'evaluator.pl',EvalPl),
	appendAtom(PHCP,'loader.pl',LoadPl),
	writeClause((:- multifile(prologbasics:pakcsrc/2))),
	writeClause((:- dynamic(prologbasics:pakcsrc/2))),
	writeClause((:- AssertPakcsrcs,
		        compile([PrologBasicsPl,BasicsPl,EvalPl,LoadPl]),
		        (retract(rtargs(_)) -> true ; true),
		        loadAndCompile(ProgPl,[],load(MainPrologFile)),
		        saveprog_entry(ProgState,InitialGoal), halt)),
	told,
	appendAtoms([PH,'/bin/sicstus -l ',TmpSavePl],Cmd),
	shellCmd(Cmd),
	deleteMainPrologFile(MainPrologFile),
	deleteFile(TmpSavePl).

% process the various options of the ":set" command:
processSetOption("+error") :- !,
	writeErr('WARNING: option "error" no longer supported!'), nlErr.
processSetOption("-error") :- !,
	writeErr('WARNING: option "error" no longer supported!'), nlErr.
processSetOption("+free") :- !,
	retract(freeVarsUndeclared(_)),
	asserta(freeVarsUndeclared(yes)).
processSetOption("-free") :- !,
	retract(freeVarsUndeclared(_)),
	asserta(freeVarsUndeclared(no)).
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
	asserta(compileWithCompact(" -compact")).
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
	      writeErr('ERROR: illegal option for +consfail!'), nlErr),
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
	atom_codes(AP,P),
	setCurryPath(AP),
	printCurrentLoadPath.
processSetOption(Option) :-
	append("printdepth ",OptTail,Option), !,
	removeBlanks(OptTail,P),
	(codes2number(P,D) -> true
	       ; write('Illegal print depth number'), nl, fail),
	retract(printDepth(_)),	
	(D=0 -> D1=D ; D1 is D+1),
	asserta(printDepth(D1)).
processSetOption(Option) :-
	append("spy ",OptTail,Option), !,
	checkDebugMode,
	removeBlanks(OptTail,P),
	atom_codes(PN,P),
	spypoint(PN).
processSetOption(_) :- !,
	write('ERROR: unknown option. Type :set for help'), nl.

printCurrentLoadPath :-
	loadPath('.',LP),
	write('Current search path for loading modules: '), nl,
	path2String(LP,SP),
	atom_codes(ASP,SP), write(ASP), nl.

% fork an expression (arg1 is the expression (string), arg2=verbose/quiet):
processFork(ExprString) :-
	% check the type of the forked expression (must be "IO ()"):
	removeBlanks(ExprString,ExprInput),
	(mainexpr(Exp,_,ExprInput,[]) -> true
                                 ; write('*** Syntax error'), nl, !, fail),
	typecheck(Exp,Type),
	(Type = 'TCons'('Prelude.IO',['TCons'('Prelude.()',[])]) -> true
	  ; write('*** Type error: Forked expression must be of type "IO ()"!'), nl,
	    !, failWithExitCode),
	% start saved program in non-verbose mode:
	verbosemode(QM), setVerboseMode(no),
	processExpression(ExprString,ExecGoal),
	forkProcessForGoal(evaluator:evaluateGoalAndExit(ExecGoal)),
	setVerboseMode(QM),
	sleepSeconds(2). % wait a little bit for starting up the new process

% check whether all free vars start with uppercase latter, if required:
checkFreeVars(FreeVars,Vs) :-
	freeVarsUndeclared(no),
	filterUndeclaredFreeVars(FreeVars,Vs,NUVs),
	NUVs = [(V=_)|RVs],
	!,
	writeErr('ERROR: Expression contains unknown symbols: '),
	writeVar(user_error,V), writeVars(user_error,RVs), nlErr,
	writeErr('(Note: free variables should be declared with "where...free" in initial goals)'),
	nlErr,
	failWithExitCode.
checkFreeVars(_,_).

filterUndeclaredFreeVars(_,[],[]).
filterUndeclaredFreeVars(FreeVars,[(V=_)|Bs],FBs) :-
	atom_codes(V,[_,C|Cs]), % first char always '_'
	atom_codes(RV,[C|Cs]),
	member(RV,FreeVars), !,
	filterUndeclaredFreeVars(FreeVars,Bs,FBs).
filterUndeclaredFreeVars(FreeVars,[(V=VB)|Bs],[(V=VB)|FBs]) :-
	filterUndeclaredFreeVars(FreeVars,Bs,FBs).


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
findSourceProg(Prog,SourceFileName) :-
	(Ext = ".lcurry" ; Ext = ".curry"),
	append(Prog,Ext,LCProgL), atom_codes(LCurryProgName,LCProgL),
	findFileInLoadPath(LCurryProgName,PathProgName),
	!,
	atom_codes(PathProgName,SourceFileName).

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

parseProgram(Prog) :-
	findSourceProg(Prog,_), !,
  	installDir(TCP),
        appendAtoms(['"',TCP,'/bin/parsecurry" --flat'],PC),
	atom_codes(PC,CL1),
	(parser_warnings(no) -> append(CL1," --nowarns",CL2)
	                      ; CL2 = CL1 ),
	(verbosity(0) -> append(CL2," --quiet",CL3)
	               ; CL3 = CL2 ),
	getCurryPath(LP),
	(LP=[] -> CL4 = CL3
	        ; path2String(LP,LPS), append(LPS,[34],LPSQ),
	          append(" --path ",[34|LPSQ],PLPS),
	          append(CL3,PLPS,CL4)),
	compileWithCompact(CWC), append(CL4,CWC,CL5),
	append(CL5,[32|Prog],LoadCmdL),
	atom_codes(LoadCmd,LoadCmdL),
	%write('Executing: '), write(LoadCmd), nl,
	(shellCmd(LoadCmd) -> true
	  ; writeErr('ERROR occurred during parsing!'), nlErr, fail).
parseProgram(_). % do not parse if source program does not exist


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% small pretty printer for type expressions:

writeType(T) :- writeType(T,top).
% the second argument is 'top' or 'nested':
% in case of 'nested', brackets are written around complex type expressions

writeType(A,_) :- atom(A), !, write(A).
writeType('FuncType'(S,T),top) :-
	(S='FuncType'(_,_) -> S_Tag=nested ; S_Tag=top),
	writeType(S,S_Tag), write(' -> '), writeType(T,top).
writeType('FuncType'(S,T),nested) :-
	(S='FuncType'(_,_) -> S_Tag=nested ; S_Tag=top),
	write('('), writeType(S,S_Tag), write(' -> '), writeType(T,top),
	write(')').
writeType('TCons'([],['TCons'('Prelude.Char',[])]),_) :- !,% print "[Char]" as "String"
	write('String').
writeType('TCons'([],[T]),_) :- !,  % list type
	write('['), writeType(T,top), write(']').
writeType('TCons'(TC,[Type|Types]),_) :-
	isTupleCons(TC), % tuple type constructor
	!,
	write('('), writeType(Type,top), writeTupleType(Types), write(')').
writeType('TCons'(TC,[]),_) :- writeTypeCons(TC), !.
writeType('TCons'(TC,Ts),top) :-
	writeTypeCons(TC), writeTypes(Ts), !.
writeType('TCons'(TC,Ts),nested) :-
	write('('), writeTypeCons(TC),
	writeTypes(Ts), write(')'), !.

writeTypeCons(TC) :-
	atom_codes(TC,TCS),
	append("Prelude.",NameS,TCS), !,
	atom_codes(Name,NameS), write(Name).
writeTypeCons(TC) :-
	currentModule(Mod), atom_codes(Mod,ModS),
	atom_codes(TC,TCS),
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
	currentModule(Mod), atom_codes(Mod,ModS),
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
	writeErr('" not unique due to multiple imports.'), nlErr, fail.
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

hasfixity('.',infixr(5)) :- !. % hack for list cons operator
hasfixity('Prelude..',infixr(9)) :- !. % hack for function composition operator
hasfixity(O,Fixity) :-
	functiontype(O,_,_,_,OFix,_),
	(OFix=nofix -> Fixity=infixl(9) % non declared opids have fixity "infixl 9"
	             ; Fixity=OFix).

% parser for (let) bindings:
mainbinding(X,E) --> id(XS), skipblanks, "=", skipblanks,
	             expr(E), {atom_codes(X,XS)}.

% parser for expressions:
mainexpr(E,Vars) --> skipblanks, letfree(LVars), expr(E), wherefree(WVars),
	             {append(LVars,WVars,Vars)}.

letfree(Vars) --> "let", !, skipblanks, varList(Vars), "free",
	          skipblanks, "in", skipblanks
	        ; {Vars=[]}.

wherefree(Vars) --> "where", !, skipblanks, varList(Vars), "free" ; {Vars=[]}.

varList([V|Vs]) --> id(Name), skipblanks,  {atom_codes(V,Name)},
	            (",", skipblanks, varList(Vs)
                     ; skipblanks, {Vs=[]}).

expr(comb('Prelude.if_then_else',[E1,E2,E3])) -->
	"if", skipblanks, expr(E1), "then", skipblanks, expr(E2),
	"else", skipblanks, expr(E3), !. 
expr(E) --> exprlist(EL), {resolvePrios(EL,E)}.

% resolve the priorities between operators:
resolvePrios([int(N)],int(N)) :- !.
resolvePrios([float(N)],float(N)) :- !.
resolvePrios([char(N)],char(N)) :- !.
resolvePrios(Es,E) :-
	append3(Es1,[opid(Op)],Es2,Es),
	minprio(Es1,P1),
	minprio(Es2,P2),
	transDefinedFunc(Op,TOp),
	hasfixity(TOp,Fix),
	(Fix=infix(PO) -> P1>PO, P2>PO
         ; (Fix=infixl(PO) -> P1>=PO, P2>PO
                            ; Fix=infixr(PO), P1>PO, P2>=PO)),
	E = comb(TOp,[E1,E2]),
	!,
	resolvePrios(Es1,E1),
	resolvePrios(Es2,E2).
resolvePrios([opid(-)|Es],E) :- % special case for unary minus
	minprio(Es,P),
	P>6,
	E = comb('Prelude.-',[int(0),E2]), !,
	resolvePrios(Es,E2).
resolvePrios([var(F)|Args],E) :-
	transDefinedFunc(F,TF), !,
	map2M(user:var2comb,Args,TArgs),
	fixOverApplications(TF,TArgs,E).
resolvePrios([var(V)],var(UV)) :- !,
	(atom_codes(V,[95|_])  % 95 = '_'
	 -> V=UV
	  ; appendAtom('_',V,UV)).
resolvePrios([E],E) :- \+ E=opid(_), !.
resolvePrios([E1,E2|Es],E) :- \+ E1=opid(_), !,
	var2comb(E1,TE1),
	var2comb(E2,TE2),
	resolvePrios(['Prelude.apply'(TE1,TE2)|Es],E).

% compute minimal priority of an expression list:
minprio([opid(Op)],PO) :- !,
	transDefinedFunc(Op,TOp), hasfixity(TOp,Fix), arg(1,Fix,PO).
minprio([_],10) :- !. % anything else has highest priority
minprio([E|Es],P) :- minprio([E],PE), minprio(Es,PEs), min(PE,PEs,P).

min(X,Y,Z) :- X<Y -> Z=X ; Z=Y.

append3(X,Y,Z,L) :- append(XY,Z,L), append(X,Y,XY).


var2comb(var(F),comb(TF,[])) :- transDefinedFunc(F,TF), !.
var2comb(var(V),var(UV)) :- !,
	(atom_codes(V,[95|_])  % 95 = '_'
	 -> V=UV
	  ; appendAtom('_',V,UV)).
var2comb(E,E) :- \+ E=opid(_).

fixOverApplications(F,Args,E) :-
	constructorOrFunctionType(F,_,_,FType),
	getArityFromType(FType,Arity),
	length(Args,N),
	(N>Arity -> splitAt(Arity,Args,FirstArgs,LastArgs),
	            generateApply(comb(F,FirstArgs),LastArgs,E)
	          ; E=comb(F,Args)).

generateApply(T,[],T).
generateApply(T,[X|Xs],E) :- generateApply('Prelude.apply'(T,X),Xs,E).
		   
getArityFromType(T,0) :- var(T), !.
getArityFromType('FuncType'(_,T2),N) :-
	getArityFromType(T2,N2), N is N2+1.
getArityFromType('TCons'(TC,_),N) :- TC="IO" -> N=1 ; N=0.


exprlist([BE|E]) --> bexpr(BE), !, (exprlist(E) ; {E=[]}).

getBinding(Var,[(Var=E)|_],E).
getBinding(Var,[_|Bs],E) :- getBinding(Var,Bs,E).

% basic expressions:
bexpr(Exp) --> id(S), {atom_codes(Name,S),
		       letBindings(Lets), getBinding(Name,Lets,Exp)}.
bexpr(var(Name)) --> id(S), {atom_codes(Name,S)}.
bexpr(var(Name)) --> "(", skipblanks, opid(S), ")", skipblanks,
                     {(S="=" ; S="|") -> !, fail ; atom_codes(Name,S)}.
bexpr(Num) --> numberconst(S), !,
	       {number_codes(N,S), (integer(N) -> Num=int(N) ; Num=float(N))}.
bexpr(opid(Name)) --> opid(S),
	              {(S="=" ; S="|") -> !, fail ; atom_codes(Name,S)}.
bexpr(opid(Name)) --> "`", id(S), "`", skipblanks, {atom_codes(Name,S)}.
bexpr(char(C)) --> "'", escape(C), "'", skipblanks. % escape characters
bexpr(char(C)) --> "'", [C], "'", skipblanks. % simple characters
bexpr(comb('Prelude.()',[])) --> "()", !, skipblanks.
bexpr(E) --> "(", skipblanks, exprlist(EL), ")", skipblanks,
             {transformOpSection(EL,E)}, !.
bexpr(E) --> "(", skipblanks, tupleElems(Es), {elems2tuple(Es,E)}.
bexpr(comb([],[])) --> "[]", !, skipblanks. % emptylist

% arithmetic sequences:
bexpr(comb('Prelude.enumFrom',[E])) --> "[", skipblanks,
             expr(E), "..", skipblanks, "]", skipblanks, !.
bexpr(comb('Prelude.enumFromTo',[E1,E2])) --> "[", skipblanks,
             expr(E1), "..", skipblanks, expr(E2), "]", skipblanks, !.
bexpr(comb('Prelude.enumFromThen',[E1,E2])) --> "[", skipblanks,
             expr(E1), ",", skipblanks, expr(E2), "..", skipblanks,
	     "]", skipblanks, !.
bexpr(comb('Prelude.enumFromThenTo',[E1,E2,E3])) --> "[", skipblanks,
             expr(E1), ",", skipblanks, expr(E2), "..", skipblanks,
	     expr(E3), "]", skipblanks, !.

bexpr(E) --> "[", skipblanks, listelems(E). % lists
bexpr(E) --> "\"", stringelems(E).  % strings

tupleElems([E|Es]) --> expr(E), (  ",", skipblanks, tupleElems(Es)
                                 ; ")", skipblanks, {Es=[]}).

transformOpSection(Section,comb(TOp,[E])) :-
	append(EL,[opid(Op)],Section), % left section
	!,
	transDefinedFunc(Op,TOp),
	resolvePrios(EL,E).
transformOpSection([opid(Op)|EL],
		   comb('Prelude.flip',[comb(TOp,[]),E])) :- % right section
	transDefinedFunc(Op,TOp),
	resolvePrios(EL,E).

elems2tuple([E],E) :- !.
elems2tuple(Exprs,comb(TupleCons,Exprs)) :-
	length(Exprs,N),
	Cons=")",
	prefixComma(Cons,N,ConsComma),
	append("Prelude.(",ConsComma,TupleConsString),
	atom_codes(TupleCons,TupleConsString).

listelems(comb((.),[E,Es])) --> expr(E), (",", skipblanks, listelems(Es)
                                        ; "]", skipblanks, {Es=comb([],[])}).

stringelems(comb((.),[char(C),Es])) --> escape(C), !, stringelems(Es).
stringelems(comb([],[])) --> "\"", skipblanks, !.
stringelems(comb((.),[char(C),Es])) --> [C], stringelems(Es).


id([C|Cs]) --> [C],
        { C >= "A", C =< "Z"
         ;C >= "a", C =< "z" },!,
        idrest(Cs), !, {\+ member([C|Cs],["where","free","let"])}.

idrest([C|Cs]) --> [C],
        { C >= "0", C =< "9"
         ;C >= "A", C =< "Z"
         ;C >= "a", C =< "z"
         ;[C] == "."  % here we accept dots to support module qualifications
         ;[C] == "_"
         ;[C] == "'" },!,
        idrest(Cs).
idrest([]) --> skipblanks.

opid([C|Cs]) --> specialchar([C]), opidrest(Cs).
opidrest([C|Cs]) --> specialchar([C]), !, opidrest(Cs).
opidrest([]) --> skipblanks.

specialchar("~") --> "~".
specialchar("!") --> "!".
specialchar("@") --> "@".
specialchar("#") --> "#".
specialchar("$") --> "$".
specialchar("%") --> "%".
specialchar("^") --> "^".
specialchar("&") --> "&".
specialchar("*") --> "*".
specialchar("+") --> "+".
specialchar("-") --> "-".
specialchar("=") --> "=".
specialchar("<") --> "<".
specialchar(">") --> ">".
specialchar("?") --> "?".
specialchar(".") --> ".".
specialchar("/") --> "/".
specialchar("|") --> "|".
specialchar("\\") --> "\\".
specialchar(":") --> ":".

escape(34) --> "\\\"".
escape(92) --> "\\\\".
escape(10) --> "\\n".
escape(8)  --> "\\b".
escape(9)  --> "\\t".
escape(13) --> "\\r".
escape(N)  --> "\\", [C1,C2,C3], 
        { isDigit(C1), isDigit(C2), isDigit(C3),
	  N is (C1-48)*100+(C2-48)*10+C3-48 }.

% extract the program name from a given input, i.e., remove all blanks,
% delete possible suffix ".curry" or ".lcurry", and delete absolute
% file path prefix if it is identical to the working directory:
extractProgName(S,ProgName) :-
	removeBlanks(S,S1),
	(append(P,".curry",S1) -> true ;
	 append(P,".lcurry",S1) -> true ; P=S),
	workingDirectory(Dir),
	atom_codes(Dir,DirS),
	((append(DirS,[47|ProgS],P), \+ append(_,[47|_],ProgS))
	 -> ProgName=ProgS
	  ; ProgName=P).

% check whether a program name (obtained by extractProgName) contains
% a valid module name:
isValidModuleName(ProgString) :-
	atom_codes(ProgAtom,ProgString),
	split2dirbase(ProgAtom,_,ModName),
	atom_codes(ModName,ModString),
	(isValidModuleString(ModString) -> true
	 ; writeErr('ERROR: Illegal module name: '), writeErr(ModName), nlErr,
	   fail).

isValidModuleString([]).
isValidModuleString([C|Cs]) :-
	(isLetterCharCode(C) ; C=95),
	isValidModuleString(Cs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% convert an expression (of Flat-Curry) into a Prolog term and return
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
% type check an expression (of Flat-Curry):

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
showSourceCode(partcall(_,F,_)) :- showSourceCodeOfFunction(F).
showSourceCode(FCall) :- FCall =.. [F|_], showSourceCodeOfFunction(F).

% show source code of a function via the simple GUI for showing complete fun's:
showSourceCodeOfFunction(QF) :-
	writeNQ('Showing source code of function '), writeNQ(QF),
	writeNQ('...'), nlNQ,
	(retract(lastShownSourceCode(LastModS,LastF)) ; LastModS=[]),
	(LastModS=[] -> true
          ; getModStream(LastModS,LastStr),
            %write('SEND: -'), write(LastF), nl,
            put_code(LastStr,45), write(LastStr,LastF), nl(LastStr),
            flush_output(LastStr)),
	atom_codes(QF,QFS),
	append(ModS,[46|FS],QFS), !,
	atom_codes(F,FS),
	%write('SEND: +'), write(F), nl,
	getModStream(ModS,Str),
	put_code(Str,43), write(Str,F), nl(Str),
	flush_output(Str),
	assertz(lastShownSourceCode(ModS,F)).

getModStream(ModS,Stream) :-
	sourceCodeGUI(ModS,Stream), !.
getModStream(ModS,InStream) :-
	installDir(PH),
	atom_codes(ModA,ModS),
	appendAtoms(['"',PH,'/currytools/browser/SourceProgGUI" ',ModA,
		     ' 2>/dev/null'],Cmd),
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
