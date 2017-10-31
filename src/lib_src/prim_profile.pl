%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Profile:
%
:- module(prim_profile,
	  [prim_getProcessInfos/1,
           prim_garbageCollectorOn/1, prim_garbageCollectorOff/1,
           prim_garbageCollect/1,
           prim_evalTime/4,prim_evalSpace/4]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).

% return statistics about the PAKCS process:
prim_getProcessInfos(Infos) :-
	(getCurrentGCs(GCs)
         -> I1=['Prelude.(,)'('Profile.GarbageCollections',GCs)] ; I1=[]),
	(getCurrentChoiceSize(Choice)
         -> I2=['Prelude.(,)'('Profile.Choices',Choice)|I1] ; I2=I1),
	(getCurrentHeapSize(Heap)
         -> I3=['Prelude.(,)'('Profile.Heap',Heap)|I2] ; I3=I2),
	(getCurrentStackSize(Stack)
         -> I4=['Prelude.(,)'('Profile.Stack',Stack)|I3] ; I4=I3),
	(getCurrentCodeSize(Code)
         -> I5=['Prelude.(,)'('Profile.Code',Code)|I4] ; I5=I4),
	(getCurrentMemorySize(Mem)
         -> I6=['Prelude.(,)'('Profile.Memory',Mem)|I5] ; I6=I5),
	(getElapsedTime(ETime)
         -> I7=['Prelude.(,)'('Profile.ElapsedTime',ETime)|I6] ; I7=I6),
	(getRunTime(RTime)
         -> I8=['Prelude.(,)'('Profile.RunTime',RTime)|I7] ; I8=I7),
	Infos = I8.

% turn on garbage collector:
prim_garbageCollectorOn('Prelude.()') :- garbageCollectorOn.

% turn off garbage collector:
prim_garbageCollectorOff('Prelude.()') :- garbageCollectorOff.

% turn off garbage collector:
prim_garbageCollect('Prelude.()') :- garbageCollect.

% evaluate the argument to normal form and print the evaluation time:
?- block prim_evalTime(?,?,-,?).
prim_evalTime(T,NF,E0,E) :-
	garbageCollect,
	getRunTime(RTime1),
	getElapsedTime(ETime1),
	getCurrentGCs(GC1),
	user:normalizeAndCheck(T,NF,E0,E1),
	getRunTime(RTime2),
	getElapsedTime(ETime2),
	getCurrentGCs(GC2),
	writeErr('EVALTIME: Runtime: '),
	RTime is RTime2-RTime1, writeErr(RTime),
	writeErr(' msec.'), nlErr,
	writeErr('EVALTIME: Elapsed time: '),
	ETime is ETime2-ETime1, writeErr(ETime),
	writeErr(' msec.'), nlErr,
	writeErr('EVALTIME: Number of garbage collections: '),
	GC is GC2-GC1, writeErr(GC), nlErr,
	garbageCollectorOn, E1=E.

% evaluate the argument to normal form (without garbage collection)
% and print the evaluation time and memory usage:
?- block prim_evalSpace(?,?,-,?).
prim_evalSpace(T,NF,E0,E) :-
	garbageCollect,
	garbageCollectorOff,
	getRunTime(RTime1),
	getElapsedTime(ETime1),
	getCurrentGCs(GC1),
	getCurrentHeapSize(GSU1),
	getCurrentStackSize(LSU1),
	user:normalizeAndCheck(T,NF,E0,E1),
	getRunTime(RTime2),
	getElapsedTime(ETime2),
	getCurrentGCs(GC2),
	getCurrentHeapSize(GSU2),
	getCurrentStackSize(LSU2),
	writeErr('EVALTIME: Runtime: '),
	RTime is RTime2-RTime1, writeErr(RTime),
	writeErr(' msec.'), nlErr,
	writeErr('EVALTIME: Elapsed time: '),
	ETime is ETime2-ETime1, writeErr(ETime),
	writeErr(' msec.'), nlErr,
	writeErr('EVALTIME: Number of garbage collections: '),
	GC is GC2-GC1, writeErr(GC), nlErr,
	writeErr('EVALTIME: Global stack usage: '),
	GS is GSU2-GSU1, writeErr(GS), nlErr,
	writeErr('EVALTIME: Local stack usage: '),
	LS is LSU2-LSU1, writeErr(LS), nlErr,
	garbageCollectorOn, E1=E.
