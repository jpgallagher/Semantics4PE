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
% E.g. while(E,if(E1,A,B)) by while(E,false(E1):B):while(E,true(E1):A:while(E,false(E1):B))
% Preserve local environments of form decl(--)  S  release(---)
	
transformRegExpr(E,_,E) :-
	literal(E).
transformRegExpr(decl(var(X),E):(E1:release(var(X))),T,decl(var(X),E):(E2:release(var(X)))) :-
	transformRegExpr(E1,T,E2).
transformRegExpr(E1:E2,T,E11:E21) :-
	functor(E1,F,N),
	F/N \== decl/2,
	transformRegExpr(E1,T,E11),
	transformRegExpr(E2,T,E21).
transformRegExpr(E1+E2,T,E11+E21) :-
	transformRegExpr(E1,T,E11),
	transformRegExpr(E2,T,E21).
%transformRegExpr(star(E),E3) :-
%	transformRegExpr(E,E1),
%	dnfRegExpr(E1,E2),
%	transformStar(star(E2),E3).
transformRegExpr(while(E,S),T,S4) :-
	dnfRegExpr(S,(true(E1):S1)+(false(E1):S2)),
	straightLineCode(S1),
	straightLineCode(S2),
	!,
	loopTransform(T,E,E1,S1,S2,S4).
transformRegExpr(while(E,S),_,while(E,S)).


% Transform loop and insert a marker 0=<1 for the ranking constraint.
loopTransform(t0,E,E1,S1,S2,S4) :-
	S3=while(logicaland(E,not(E1)),S2),
	S4=(S3:while(logicaland(E,E1),S1:S3)).
loopTransform(t1,E,E1,S1,S2,S4) :-
	S3=while(logicaland(E,not(E1)),S2),
	S4=(S3:while(logicaland(E,E1),S1:S3)):false(E).
loopTransform(t2,E,E1,S1,S2,S4) :-
	S3=while(logicaland(E,not(E1)),S2),	 
	S4=(true(E):((S3:while(logicaland(E,E1),S1:S3)):false(E)))+false(E).
loopTransform(star,E,E1,S1,S2,S4) :-
	S3=star(true(E):(false(E1):S2)),
	S4=S3:star(true(E):((true(E1):S1):S3)).
	
	
%transformStar(star(E),star(E)) :-
%	singlePath(E).
%transformStar(star(E1+E2),E3:star(E1:E3)) :-
%	transformStar(star(E2),E3).
	
straightLineCode(eps).
straightLineCode(null).
straightLineCode(assert(_)).
straightLineCode(asg(_,_)).
straightLineCode(S1:S2) :-
	straightLineCode(S1),
	straightLineCode(S2).
straightLineCode(decl(_,_)).
straightLineCode(release(_)).

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
 