:- module(clp2logen,[main/1]).

:- use_module(library(lists)).

main([InF,OutF]) :-
	open(InF,read,S1),
	open(OutF,write,S2),
	translateFile(S1,S2,0),
	close(S1),
	close(S2).
main([InF]) :-
	open(InF,read,S1),
	translateFile(S1,user_output,0),
	close(S1).	
	

translateFile(S1,S2,K) :-
	read(S1,C),
	(
	    C == end_of_file -> true
	;
	    numbervars(C,0,_),
		writeHornClause(C,S2,K),
		K1 is K+1,
	    translateFile(S1,S2,K1)
	).

writeHornClause((A :- B),S,K) :-
	!,
	tuple2list(B,BL),
	writeq(S,logen(givenRule/3,givenRule(K,A,BL))),
	write(S,'.'),
	nl(S).

writeHornClause(A,S,K) :-
	writeq(S,logen(givenRule/3,givenRule(K,A,[]))),
	write(S,'.'),
	nl(S),
	!.
writeHornClause((:- _),_,_).

tuple2list((A,As),[A|LAs]) :-
	!,
	tuple2list(As,LAs).
tuple2list(A,[A]).

	
constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_=\=_).
constraint(true).
constraint(fail).
