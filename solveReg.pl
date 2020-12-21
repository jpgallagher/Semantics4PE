:- module(solveReg,_).

:- use_module(regPathExpr).
:- use_module(chclibs(setops)).

% Compute path expression using algorithm from [Tarjan 1981], then solve.

go(Entry) :-
	vertices(Vs1),
	sort(Vs1,Vs),
	edges(Es),
	functor(Entry,P,N),
	regPathExpr(Vs,Es,P/N,Exprs),
	member(path(P/N,true/0,Expr),Exprs),
	pathsolve(Entry,true,Expr,[]).
	
% Solve using a given path expression

go2(Entry,Expr) :-
	pathsolve(Entry,true,Expr,[]).
	
% Regular expressions
% E ::= symb(Id) | E1:E2 | E1+E2 | star(E) | null | eps
	
pathsolve(A,Z,symb(C),_) :-
	clpClause(C,A,Cs,[Z]),
	solveConstraints(Cs).
pathsolve(A,Z,E1:E2,F) :-
	first(E2,F1),
	member(P/N,F1),
	functor(X,P,N),
	pathsolve(A,X,E1,F1),
	pathsolve(X,Z,E2,F).
pathsolve(A,Z,E1+_,F) :-
	pathsolve(A,Z,E1,F).
pathsolve(A,Z,_+E2,F) :-
	pathsolve(A,Z,E2,F).
pathsolve(A,A,star(_),_).
pathsolve(A,Z,star(E),F) :-
	first(E,FE),
	setunion(FE,F,F1),
	member(P/N,F1),
	functor(X,P,N),
	pathsolve(A,X,E,F1),
	pathsolve(X,Z,star(E),F).
	
solveConstraints([]).
solveConstraints([C|Cs]) :-
	call(C),
	solveConstraints(Cs).	
	
vertices(Vs) :-
	findall(P/N, 
		(clpClause(_,A,_,[B]),
		 (functor(A,P,N);functor(B,P,N))
		 ),
		Vs1),
		makeset(Vs1,Vs).

edges(Es) :-
	findall(edge(Id,P/N-Q/M), 
		(clpClause(Id,A,_,[B]),
		 functor(A,P,N),functor(B,Q,M)
		 ),
		Es).

first(eps,[]).
first(symb(Id),[P/N]) :-
	clpClause(Id,A,_,_),
	functor(A,P,N).
first(E1:E2,F) :-
	nullable(E1),
	!,
	first(E1,F1),
	first(E2,F2),
	setunion(F1,F2,F).
first(E1:_,F) :-
	first(E1,F).
first(E1+E2,F) :-
	first(E1,F1),
	first(E2,F2),
	setunion(F1,F2,F).
first(star(E),V) :-
	first(E,V).
	
	
nullable(eps).
nullable(star(_)).
nullable(E+_) :-
	nullable(E),
	!.
nullable(_+E) :-
	nullable(E).
nullable((E1:E2)) :-
	nullable(E1),
	nullable(E2).
 

clpClause(c1,wh(A,B),[A>=1,B>=1,C=B-1],[wh(A,C)]).
clpClause(c2,wh(A,B),[A>=1,B=<0,C=B+A,D=A-1],[wh(D,C)]).
clpClause(c3,wh(A,B),[A=<0],[true]).

/*

clpClause(c1,wh(A,B),[A>=1,B>=1,C=B-1],[wh___1(A,C)]).
clpClause(c2,wh(A,B),[A>=1,B=<0,C=B+A,D=A-1],[wh(D,C)]).
clpClause(c3,wh(A,B),[A=<0],[true]).
clpClause(c4,wh___1(A,B),[A>=1,B>=1,C=B-1],[wh___1(A,C)]).
clpClause(c5,wh___1(A,B),[A>=1,B=<0,C=B+A,D=A-1],[wh___2(D,C)]).
clpClause(c6,wh___2(A,B),[A>=1,B>=1,C=B-1],[wh___1(A,C)]).
clpClause(c7,wh___2(A,B),[A=<0],[true]).
*/


	