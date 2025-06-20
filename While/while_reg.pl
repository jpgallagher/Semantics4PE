:- module(while_reg,_).

% Generate a regular expression from a single procedure program
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
%		null | 
%		eps


%%%%%%%%%%%%%%%%%%%%%%%%	
%----- statements -----
%%%%%%%%%%%%%%%%%%%%%%%%

regexp(skip,eps).
regexp(asg(var(X),E),asg(var(X),E)).
regexp(call(observe,[]),observe).
regexp(seq(S1,S2),E1:E2) :-
	regexp(S1,E1),
	regexp(S2,E2).
regexp(ifthenelse(E,S1,S2),(true(E):E1)+(false(E):E2)) :-
	regexp(S1,E1),
	regexp(S2,E2).
regexp(while(E,S1),star(true(E):E1):false(E)) :-
	regexp(S1,E1).
% Versions for if and while statements that evaluate their condition only once
%regexp(ifthenelse(E,S1,S2),decl(var(v),E):(((true(var(v)):E1)+(false(var(v)):E2)):release(var(v)))) :-
%	regexp(S1,E1),
%	regexp(S2,E2).
%regexp(while(E,S1),decl(var(b),E):((star(true(var(b)):(E1:asg(var(b),E))):false(var(b))):release(var(b)))) :-
%	regexp(S1,E1).
regexp(for(Init,Cond,Incr,S1),E1:E2) :-
	regexp(Init,E1),
	regexp(while(Cond,seq(S1,Incr)),E2).
regexp(let(X,E,S1),decl(X,E):(E1:release(X))) :-
	regexp(S1,E1).

	


