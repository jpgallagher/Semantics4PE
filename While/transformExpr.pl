:- module(transformExpr,_).
 	
% Regular expressions over alphabet 
% {asg(X,Expr),true(Expr), false(Expr), decl(X,E), release(X)}
% where X is a variable, E an expression
%
% E ::= asg(X,Expr) | 
%		true(Expr) | 
%		false(Expr) | 
%		decl(X,E) 	|
%		release(X)	|
%		E1:E2 | 
%		E1+E2 | 
%		star(E) | 
%		while(Expr,E) | 
%		null | 
%		eps
%
% Transform by replacing star(A+B) by star(B):star(A:star(B))
% Transform by replacing while(E,A+B) by while(E,B):while(E,A:while(E,B))
% Preserve local environments of form decl(--)  S  release(---)
	
transformRegExpr(E,E) :-
	literal(E).
transformRegExpr(decl(var(X),E):(E1:release(var(X))),decl(var(X),E):(E2:release(var(X)))) :-
	transformRegExpr(E1,E2).
transformRegExpr(E1:E2,E11:E21) :-
	functor(E1,F,N),
	F/N \== decl/2,
	transformRegExpr(E1,E11),
	transformRegExpr(E2,E21).
transformRegExpr(E1+E2,E11+E21) :-
	transformRegExpr(E1,E11),
	transformRegExpr(E2,E21).
transformRegExpr(star(E),E3) :-
	transformRegExpr(E,E1),
	dnfRegExpr(E1,E2),
	transformStar(star(E2),E3).
transformRegExpr(while(E,S),S4) :-
	dnfRegExpr(S,(true(E1):S1)+(false(E1):S2)),
	straightLineCode(S1),
	straightLineCode(S2),
	!,
	S3=while(logicaland(E,not(E1)),S2),
	S4=S3:while(logicaland(E,E1),S1:S3).
transformRegExpr(while(E,S),while(E,S)).

transformStar(star(E),star(E)) :-
	singlePath(E).
transformStar(star(E1+E2),E3:star(E1:E3)) :-
	transformStar(star(E2),E3).
	
straightLineCode(skip).
straightLineCode(call(assert,_)).
straightLineCode(asg(_,_)).
straightLineCode(S1:S2) :-
	straightLineCode(S1),
	straightLineCode(S2).
straightLineCode(let(_,_,S1)) :-
	straightLineCode(S1).

dnfRegExpr(E,E) :-
	pathLiteral(E).
dnfRegExpr(E1+E2, E1+E3) :-
	literal(E1),
	dnfRegExpr(E2,E3).
dnfRegExpr((E1+E2)+E3, DNF) :-
	dnfRegExpr(E1+(E2+E3), DNF).
dnfRegExpr((E1:E2)+E3, DNF) :-
	dnfRegExpr(E1:E2, DNF1),
	dnfRegExpr(E3, DNF2),
	appendDisj(DNF1,DNF2,DNF).
dnfRegExpr(decl(var(X),E):(E1:release(var(X))), E3) :-
	dnfRegExpr(E1,E2),
	makeDeclExprs(E2,X,E,E3).
dnfRegExpr(E1:E2, E4) :-
	functor(E1,F,N),
	F/N \== decl/2,
	pathLiteral(E1),
	dnfRegExpr(E2,E3),
	distribute(E1,E3,E4).
dnfRegExpr(((E1:E2):E3), DNF) :-
	dnfRegExpr((E1:(E2:E3)), DNF).
dnfRegExpr((E1+E2):E3, DNF) :-
	dnfRegExpr(E1:E3, DNF1),
	dnfRegExpr(E2:E3, DNF2),
	appendDisj(DNF1,DNF2,DNF).
	
distribute(E1,(E2+E3),(E1:E2)+E4) :-
	distribute(E1,E3,E4).
distribute(E1,E2,E1:E2) :-
	singlePath(E2).
	
appendDisj(E1+E2,E3,E1+E4) :-
	appendDisj(E2,E3,E4).
appendDisj(E1,E2,E1+E2) :-
	singlePath(E1).
	
makeDeclExprs(E1+E2,X,E,decl(var(X),E):(E1:release(var(X)))+E4) :-
	makeDeclExprs(E2,X,E,E4).
makeDeclExprs(E1,X,E,decl(var(X),E):(E1:release(var(X)))) :-
	singlePath(E1).
	
literal(eps).
literal(asg(_,_)).
literal(true(_)).
literal(false(_)).
literal(assert(_)).
literal(decl(_,_)).
literal(release(_)).

pathLiteral(E) :-
	literal(E).
pathLiteral(star(_)).		% star within a loop path treated as a literal
pathLiteral(while(_,_)).	% while within a loop path treated as a literal

singlePath(E) :-
	pathLiteral(E).
singlePath(_:_). 	% assuming the argument is in DNF
 