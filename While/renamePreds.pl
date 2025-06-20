:- module(renamePreds,[main/1]).

:- use_module(library(lists)).
:- use_module(library(terms_vars)).

:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).
:- use_module(chclibs(canonical)).

% Create a renaming and signature table using the Logen memo table

main([InF,SigF,OutF]) :-
	renamePreds(InF,SigF,OutF).
main([InF,SigF]) :-
	renamePreds(InF,SigF,user_output).
	
renamePreds(InF,SigF,OutF) :-
	open(InF,read,S1),
	load_file(SigF),
	(OutF=user_output -> S2=user_output; open(OutF,write,S2)),
	translateFile(S1,S2),
	close(S1),
	close(S2).
	
translateFile(S1,S2) :-
	read(S1,C),
	(
	    C == end_of_file -> true
	;
		renameClauses(C,S2),
	    translateFile(S1,S2)
	).

renameClauses((:-B),S) :-
	!,
	numbervars(B,0,_),
	write(S,(:-B)),
	write(S,'.'),
	nl(S).
renameClauses((A :- B),S) :-
	tuple2list(B,BL),
	!,
	renameAtom(A,A1),
	renameBody(BL,Bs1),
	numbervars((A1,Bs1),0,_),
	writeClause(A1,[],Bs1,S).

renameClauses(A,S) :-
	renameAtom(A,A1),
	numbervars(A1,0,_),
	writeClause(A1,[],[],S),
	!.
renameClauses(D,S) :-
	numbervars(D,0,_),
	writeq(S,D),
	write(S,'.'),
	nl(S).

renameAtom(A,B) :-
	%functor(A,F,N),
	my_clause(rename(A,B,_),[],_),
	!.
renameAtom(A,A).
	
renameBody([],[]).
renameBody([B|Bs],[B|Bs1]) :-
	constraint(B,_),
	!,
	renameBody(Bs,Bs1).
renameBody([B|Bs],[B1|Bs1]) :-
	renameAtom(B,B1),
	renameBody(Bs,Bs1).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

writeClause(H,[],[],S) :-
	!,
	writeClause(H,[],[true],S).
writeClause(H,Cs,Bs,S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	append(Cs,Bs,Body),
	writeBody(Body,S).
	
writeBody([],S) :-
	write(S,'.'),
	nl(S).
writeBody([B],S) :-
	!,
	write(S,'    '),
	writeq(S,B),
	write(S,'.'),
	nl(S).
writeBody([B|Bs],S) :-
	write(S,'    '),
	writeq(S,B),
	write(S,','),
	nl(S),
	writeBody(Bs,S).
    
