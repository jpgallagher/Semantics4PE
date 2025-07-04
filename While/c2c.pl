:- module(c2c,_).

:- use_module(while_reg).
:- use_module(transformExpr).
:- use_module(reg2c).


%%%%%%%%%%%%%%%%%%%%%%%%%
% initial setup and entry
%%%%%%%%%%%%%%%%%%%%%%%%%

main([File,'-o',Outfile]) :-
    go(File,Outfile).
    
go(File,Outfile) :-
	open(File,read,S), 					% Read the AST of the parsed program
    read(S,P),
    close(S),
    P=logen(program/1,program(Prog)), 	% Get the main procedure
	member(function(main,[],Code),Prog),
	genvardecls(Prog,Code1,Code), 		% Global variable declarations
    regexp(let(var(cost),cns(nat(0)),Code1),Expr),
    transformRegExpr(Expr,TExpr),
    prog(TExpr,Str,[]),
    atom_codes(A,Str),
    open(Outfile,write,S1),
    write(S1,A),
    close(S1).
    
genvardecls([],Code0,Code0).
genvardecls([[vardecl(var(X),_,Init)]|P],let(var(X),Init,Code1),Code0) :-
	genvardecls(P,Code1,Code0).
genvardecls([T|P],Code1,Code0) :-
	T \= [vardecl(_,_,_)],
	genvardecls(P,Code1,Code0).