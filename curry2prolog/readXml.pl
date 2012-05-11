%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A very simple XML parser used for reading primitive specification files:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(readXml,[parseXml/2]).

:- use_module(prologbasics).
:- use_module(basics).

% The XML parser:
% Argument 1: XML string
% Argument 2: XML documents, i.e., list with elements of the form
% - pcdata(String)
% - element(Tag,Attrs,XmlDocs) where Tag is an atom and each attribute
%   has the form Name=Value (Name is an atom and Value a string)
parseXml(S,Doc) :-
	scanXmlString(S,XTs),
	parseXmlTokens(XTs,nothing,Doc,[]).

parseXmlTokens([],nothing,[],[]).
parseXmlTokens([pcdata(S)|XTs],Stop,[pcdata(US)|XEs],Rem) :-
	parseXmlTokens(XTs,Stop,XEs,Rem),
	xmlUnquoteSpecials(S,US).
parseXmlTokens([element([60,47|EN],Atts,Cont)|XTs],Stop,XEs,Rem) :- !,
	(EN=Stop
	 -> % stop token reached
	    XEs=[], Rem=XTs
	  ; parseXmlTokens(XTs,Stop,NewXEs,Rem),
	    atom_codes(Tag,[47|EN]),
	    XEs=[element(Tag,Atts,Cont)|NewXEs]).
parseXmlTokens([element([60|EN],Atts,_)|XTs],Stop,
	       [element(Tag,Atts,XEs1)|XEs2],XTs2) :- !,
	parseXmlTokens(XTs,EN,XEs1,XTs1),
	parseXmlTokens(XTs1,Stop,XEs2,XTs2),
	atom_codes(Tag,EN).
parseXmlTokens([element(EN,Atts,Cont)|XTs],Stop,
	       [element(Tag,Atts,Cont)|XEs1],XTs1) :- !,
	parseXmlTokens(XTs,Stop,XEs1,XTs1),
	atom_codes(Tag,EN).


% scan an XML string into list of XML tokens:
scanXmlString(S,XTs) :- dropBlanks(S,SB), scanXml(SB,XTs).

scanXml([],[]) :- !.
scanXml([60|S],XTs) :- !, scanXmlElem(S,XTs).
scanXml(S,[pcdata(Txt)|XTs]) :- scanXmlText(S,S1,Txt), scanXml(S1,XTs).

scanXmlText([],[],[]).
scanXmlText([60|S],[60|S],[]) :- !.
scanXmlText([C|S],S2,Txt) :- isSpace(C), !,
	dropBlanks(S,S1),
	scanXmlText(S1,S2,RTxt),
	(RTxt=[] -> Txt=[] ; Txt=[32|RTxt]).
scanXmlText([C|S],S1,[C|Txt]) :- scanXmlText(S,S1,Txt).

% scan an XML element:
scanXmlElem([],[]).
scanXmlElem([33,45,45|S],XTs) :- !, % [33,45,45] = "!--"
	scanXmlComment(S,XTs).
scanXmlElem([33|S],XTs) :- !, % [33] = "!"
	scanXmlCData(S,XTs).
scanXmlElem([63|S],XTs) :- !, % [63] = "?"
	scanXmlProcInstr(S,XTs).
scanXmlElem([C|S],XTs) :- scanXmlElemName([C],S,XTs).

scanXmlElemName(EN,[],[element([60|EN],[],[])]).
scanXmlElemName(EN,[62|S],[element([60|EN],[],[])|XTs]) :- !,
	scanXmlString(S,XTs).
scanXmlElemName(EN,[47,62|S],[element(EN,[],[])|XTs]) :- !, % [47,62] = "/>"
	scanXmlString(S,XTs).
scanXmlElemName(EN,[C|S],[Elem|XTs]) :- isSpace(C), !,
	dropBlanks(S,S1),
	parseXmlAttrs(S1,S2,Attrs),
	(S2=[47,62|S3] -> scanXmlString(S3,XTs), Elem=element(EN,Attrs,[])
	  ; S2=[_|S3], scanXmlString(S3,XTs), Elem=element([60|EN],Attrs,[])).
scanXmlElemName(EN,[C|S],XTs) :-
	append(EN,[C],ENC),
	scanXmlElemName(ENC,S,XTs).

parseXmlAttrs([],[],[]).
parseXmlAttrs([C|Cs],Cs4,[(Tag=UVal)|Attrs]) :- isAlpha(C), !,
	append(TagS,[61,34|Cs1],Cs), !, % 61 = '='
	append(Val,[34|Cs2],Cs1), !, % 34 = '"'
	atom_codes(Tag,[C|TagS]),
	xmlUnquoteSpecials(Val,UVal),
	dropBlanks(Cs2,Cs3),
	parseXmlAttrs(Cs3,Cs4,Attrs).
parseXmlAttrs(Cs,Cs,[]).


% scan (and drop) an XML comment:
scanXmlComment([],[]).
scanXmlComment([45,45,62|S],XTs) :- !, % [45,45,62] = "-->"
	scanXmlString(S,XTs).
scanXmlComment([_|S],XTs) :- scanXmlComment(S,XTs).

% todo: improve
scanXmlCData([],[]).
scanXmlCData([62|S],XTs) :- !, scanXmlString(S,XTs).
scanXmlCData([_|S],XTs) :- scanXmlCData(S,XTs).

% scan (and drop) an XML processing instructions:
scanXmlProcInstr([],[]).
scanXmlProcInstr([63,62|S],XTs) :- !, % [63,62] = "?>"
	scanXmlString(S,XTs).
scanXmlProcInstr([_|S],XTs) :- scanXmlProcInstr(S,XTs).


dropBlanks([C|T],TB) :- isSpace(C), !, dropBlanks(T,TB).
dropBlanks(T,T).

isSpace(32).
isSpace(10).
isSpace(13).

isAlpha(C) :- C >= 65, C =< 90.
isAlpha(C) :- C >= 97, C =< 122.

% unquote special characters (<,>,&,',") in an XML string:
xmlUnquoteSpecials([],[]).
xmlUnquoteSpecials([38|Cs],UCs) :- !, % 38 = '&'
	append(Special,[59|Cs1],Cs), !, % 59 = ';'
	xmlUnquoteSpecial(Special,Cs1,UCs).
xmlUnquoteSpecials([C|Cs],[C|UCs]) :- xmlUnquoteSpecials(Cs,UCs).

xmlUnquoteSpecial("lt",Cs,[60|UCs]) :- !, xmlUnquoteSpecials(Cs,UCs).
xmlUnquoteSpecial("gt",Cs,[62|UCs]) :- !, xmlUnquoteSpecials(Cs,UCs).
xmlUnquoteSpecial("amp",Cs,[38|UCs]) :- !, xmlUnquoteSpecials(Cs,UCs).
xmlUnquoteSpecial("quot",Cs,[34|UCs]) :- !, xmlUnquoteSpecials(Cs,UCs).
xmlUnquoteSpecial("apos",Cs,[39|UCs]) :- !, xmlUnquoteSpecials(Cs,UCs).
xmlUnquoteSpecial(Special,Cs,AllUCs) :-
	append([38],Special,AmpSpecial),
	append(AmpSpecial,59,QuoteSpecial),
	xmlUnquoteSpecials(Cs,UCs),
	append(QuoteSpecial,UCs,AllUCs).
