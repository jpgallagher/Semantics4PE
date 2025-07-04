:- module(reg2c,_).

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


prog(decl(var(cost),_):(S:release(var(cost)))) -->
	"extern int rand();","\n",
	"void assert(int e);",
	"\n","\n",
	"void main() {",
	"\n",
	ast2c(S,3),
	"}",
	"\n".

ast2c(eps,_) --> 
	"".
ast2c(true(Expr):E,I) --> 
	!,
	{I1 is I+3},
	indent(I),
	"if", " ","(",expr2c(Expr),")","{",
	"\n",
	ast2c(E,I1),
	indent(I),
	"}",
	"\n".
ast2c(false(Expr):E,I) --> 
	!,
	{I1 is I+3},
	indent(I),
	"if", " ","(","!",expr2c(Expr),")","{",
	"\n",
	ast2c(E,I1),
	indent(I),
	"}",
	"\n".
ast2c(assert(Expr),I) --> 
	indent(I),
	"assert","(",expr2c(Expr),")",
	";",
	"\n".
ast2c(asg(var(X),E),I) --> 
	indent(I),
	atomstr(X),"=",expr2c(E),";","\n".
ast2c(decl(var(X),null):(E1:release(var(X))),I) -->
	!,
	indent(I),
	"int"," ",atomstr(X),";","\n",
	ast2c(E1,I).
ast2c(decl(var(X),E):(E1:release(var(X))),I) -->
	!,
	indent(I),
	"int"," ",atomstr(X),"=",expr2c(E),";","\n",
	ast2c(E1,I).
ast2c(E1:E2,I) -->
	ast2c(E1,I),
	ast2c(E2,I).
ast2c((true(E):E1)+(false(E):E2),I) -->
	{I1 is I+3},
	indent(I),
	"if", " (",
	expr2c(E),
	")","{",
	"\n",
	ast2c(E1,I1),
	indent(I),
	"}","\n",
	indent(I),
	"else",
	" {","\n",
	ast2c(E2,I1),
	indent(I),
	"}",
	"\n".
ast2c(while(E,S1),I) -->
	{I1 is I+3},
	indent(I),
	"while"," (",
	expr2c(E),
	")","{","\n",
	ast2c(S1,I1),
	indent(I),
	"}",
	"\n".

indent(N,S,S0) :-
	blanks(N,Bs),
	append(Bs,S0,S).
	
atomstr(X,S,S0) :- 
	atom_codes(X,Chs),
	append(Chs,S0,S).
	
blanks(0,[]).
blanks(N,[32|Bs]) :-
	N>0,
	N1 is N-1,
	blanks(N1,Bs).
	
numstr(X,S,S0) :- 
	number_codes(X,Chs),
	append(Chs,S0,S).

expr2c(null) --> "0".	
expr2c(var(X)) -->
	atomstr(X).
expr2c(cns(nat(N))) -->
	numstr(N).
expr2c(add(E1,E2)) -->
	expr2c(E1),
	"+",
	expr2c(E2).
expr2c(sub(E1,E2)) -->
	expr2c(E1),
	"-",
	expr2c(E2).
expr2c(mul(E1,E2)) -->
	expr2c(E1),
	"*",
	expr2c(E2).
expr2c(div(E1,E2)) -->
	expr2c(E1),
	"/",
	expr2c(E2).
expr2c(E1>E2) -->
	expr2c(E1),
	">",
	expr2c(E2).
expr2c(E1<E2) -->
	expr2c(E1),
	"<",
	expr2c(E2).
expr2c(E1>=E2) -->
	expr2c(E1),
	">=",
	expr2c(E2).
expr2c(E1=<E2) -->
	expr2c(E1),
	"<=",
	expr2c(E2).
expr2c(E1==E2) -->
	expr2c(E1),
	"==",
	expr2c(E2).
expr2c(logicaland(E1,E2)) -->
	expr2c(E1),
	" ",
	"&&",
	" ",
	expr2c(E2).
expr2c(logicalor(E1,E2)) -->
	expr2c(E1),
	" ",
	"||",
	" ",
	expr2c(E2).
expr2c(not(E)) -->
	"!",expr2c(E).
expr2c(call(rand,[])) -->	
	"rand()".
	