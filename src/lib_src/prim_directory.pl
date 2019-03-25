%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definitions of builtins of module Directory:
%

:- module(prim_directory,
	  [prim_doesFileExist/2,prim_doesDirectoryExist/2,
	   prim_getModificationTime/2,prim_fileSize/2,
	   prim_getCurrentDirectory/1,prim_setCurrentDirectory/2,
	   prim_getDirectoryContents/2,
	   prim_createDirectory/2,
	   prim_removeFile/2,prim_removeDirectory/2,
	   prim_renameFile/3,prim_renameDirectory/3]).

:- (current_module(prologbasics) -> true ; use_module('../prologbasics')).
:- (current_module(basics)       -> true ; use_module('../basics')).


prim_doesFileExist(FileName,Exists) :-
	string2Atom(FileName,FName),
	(existsFile(FName) -> Exists='Prelude.True' ; Exists='Prelude.False').

prim_doesDirectoryExist(DirName,Exists) :-
	string2Atom(DirName,Dir),
	(existsDirectory(Dir) -> Exists='Prelude.True' ; Exists='Prelude.False').

prim_getModificationTime(FileName,'Data.Time.CTime'(Time)) :-
	string2Atom(FileName,FName),
	fileModTime(FName,Time).

prim_fileSize(FileName,Size) :-
	string2Atom(FileName,FName),
	fileSize(FName,Size).

prim_getCurrentDirectory(DirName) :-
	workingDirectory(Dir),
	atom2String(Dir,DirName).

prim_setCurrentDirectory(DirName,'Prelude.()') :-
	string2Atom(DirName,Dir),
	setWorkingDirectory(Dir).

prim_getDirectoryContents(DirName,EntryNames) :-
	string2Atom(DirName,Dir),
	directoryFiles(Dir,Entries),
	map2M(basics:atom2String,Entries,EntryNames).

prim_createDirectory(DirName,'Prelude.()') :-
	string2Atom(DirName,DName),
	makeDirectory(DName).

prim_removeFile(FileName,'Prelude.()') :-
	string2Atom(FileName,FName),
	deleteFile(FName).

prim_removeDirectory(DirName,'Prelude.()') :-
	string2Atom(DirName,DName),
	deleteDirectory(DName).

prim_renameFile(FileName1,FileName2,'Prelude.()') :-
	string2Atom(FileName1,FName1),
	string2Atom(FileName2,FName2),
	renameFile(FName1,FName2).

prim_renameDirectory(DirName1,DirName2,'Prelude.()') :-
	string2Atom(DirName1,DName1),
	string2Atom(DirName2,DName2),
	renameDirectory(DName1,DName2).
