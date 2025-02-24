:- module(linearSolve,_).

% An interpreter for linear solving of Horn clauses

go(A) :-
	solve([A]).
solve([A|As]) :-
	clpClause(A,B),
	solveConstraints(B,B1),
	append(B1,As,As1),
	solve(As1).
solve([A|As]) :-
	constraint(A),
	call(A),
	solve(As).
solve([]).

solveConstraints([C|Gs],Gs1) :-
	constraint(C),
	call(C),
	solveConstraints(Gs,Gs1).
solveConstraints([],[]).
solveConstraints([G|Gs],[G|Gs]) :-
	\+ constraint(G).
	
%%%%%%%
 
constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_\=_).
constraint(true).
constraint(fail).

/*
clpClause(main__0,[A is 0,B is 0,C is 0,while__9(A,B,C,D,E,F)]).
clpClause(while__9(A,B,C,D,E,F),[ifthenelse__10(A,B,C,D,E,F)]).
clpClause(ifthenelse__10(A,B,C,D,E,F),[A<B,ifthenelse__13(A,B,C,G,H,I),while__9(G,H,I,D,E,F)]).
clpClause(ifthenelse__10(A,B,C,A,B,C),[A>=B,true]).
clpClause(ifthenelse__13(A,B,C,D,B,E),[C>0,D is 0,E is C-1]).
clpClause(ifthenelse__13(A,B,C,D,B,C),[C=<0,D is A+1]).
*/
