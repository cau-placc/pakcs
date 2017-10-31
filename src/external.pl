%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reading specification of primitive external functions.
%
% The specification primitive external functions of a module "Mod" must
% be contained in a file named "Mod.prim_c2p". This XML file must contain
% for each external function f with arity n an entry of the form
%
%  <primitive name="f" arity="n">
%   <library>lib</library>
%   <entry>pred</entry>
%  </primitive>
%
% where lib is the Prolog library containing the code implementing this
% function and pred is a predicate name (of arity n+3) in this library
% implementing this function.
%
% Furthermore, it can also contain entries of the form
%
%  <ignore name="f" arity="n" />
%
% for functions f with arity n that are declared in the Curry module
% but should be ignored for code generation, e.g., since they are
% never called w.r.t. to the current implementation of external functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(external,[readPrimitiveXmlSpecs/2]).

:- use_module(prologbasics).
:- use_module(basics).
:- use_module(readXml).

readPrimitiveXmlSpecs(PrimXmlFile,ExtSpecs) :-
	readXmlFile(PrimXmlFile,Contents),
	findXmlElement(Contents,element(primitives,[],XExts)),
	map2M(external:transXExt,XExts,ExtSpecs).

transXExt(element(primitive,[name=NameS,arity=ArityS],
		  [element(library,[],[pcdata(LibS)]),
		   element(entry,[],[pcdata(EntryS)])]),
	  primitive(Name,Arity,Lib,Entry)) :- !,
	flatName2Atom(NameS,Name),
	number_codes(Arity,ArityS),
	atom_codes(Lib,LibS), atom_codes(Entry,EntryS).
transXExt(element(ignore,[name=NameS,arity=ArityS],[]),ignore(Name,Arity)) :-
	!,
	flatName2Atom(NameS,Name),
	number_codes(Arity,ArityS).
transXExt(XElem,_) :-
	writeErr('ERROR: Primitive function specification has wrong format:'), nlErr,
	writeErr(XElem), nlErr,
	!, compiler:setFlcBug, fail.

findXmlElement([element(Tag,Attrs,Cont)|_],element(Tag,Attrs,Cont)) :- !.
findXmlElement([_|Cont],XElem) :- findXmlElement(Cont,XElem).

% read and parse an XML file:
readXmlFile(F,XDoc) :-
	readFileContents(F,FC),
	parseXml(FC,XDoc), !.
readXmlFile(F,_) :-
	write(user_error,'ERROR: Parse error in file '),
	write(user_error,F), nl(user_error), !, fail.

