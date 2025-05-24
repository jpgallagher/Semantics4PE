:- module(while,_).

:- use_module(while_reg).
:- use_module(transformExpr).


%%%%%%%%%%%%%%%%%%%%%%%%%
% initial setup and entry
%%%%%%%%%%%%%%%%%%%%%%%%%

main(ArgV) :-
    get_options(ArgV,Options,_),
    set_options(Options,File,Style,LR,YN),
    go(File,Style,LR,YN).
    
go(File,Style,LR,YN) :-
    open(File,read,S), 				% Read the AST of the parsed program
    read(S,P),
    close(S),
    P=logen(program/1,program(Prog)),
    vardecls(Prog,St),				% Top level variable declarations
	member(function(main,[],Code),Prog),
    regexp(Code,Expr),
    exec(Expr,Style,LR,YN,St).
    
exec(Expr,big,right,no,St) :-
	copyStateSkeleton(St,St1),
	bigstep(Expr,right,St,St1),
	write(St1),
	nl.
exec(Expr,small,right,no,St) :-	
	run(Expr,St).
exec(Expr,big,left,no,St) :-
	copyStateSkeleton(St,St1),
	bigstep(Expr,left,St,St1),
	write(St1),
	nl.
exec(Expr,small,left,no,St) :-
	run(Expr,St).
exec(Expr,big,right,yes,St) :-
	transformRegExpr(Expr,TExpr),
	copyStateSkeleton(St,St1),
	bigstep(TExpr,right,St,St1),
	write(St1),
	nl.
exec(Expr,small,right,yes,St) :-
	transformRegExpr(Expr,TExpr),
	run(TExpr,St).
exec(Expr,big,left,yes,St) :-
	transformRegExpr(Expr,TExpr),
	copyStateSkeleton(St,St1),
	bigstep(TExpr,left,St,St1),
	write(St1),
	nl.
exec(Expr,small,left,yes,St) :-
	transformRegExpr(Expr,TExpr),
	run(TExpr,St).		
	

vardecls([],[]).
vardecls([[vardecl(var(X),_,Init)]|P],[(X,V)|St]) :-
	initDeclVal(Init,V),
	vardecls(P,St).
vardecls([T|P],St) :-
	T \= [vardecl(_,_,_)],
	vardecls(P,St).
	
initDeclVal(cns(nat(V)),V).
initDeclVal(null,_).

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

% Big-step interpretation wrt a regular expression and left/right recursion

bigstep(eps,_,St,St).
bigstep(asg(var(X),E),_,St0,St1) :-
	evalAndSave(X,E,St0,St1).
bigstep(true(E),_,St,St) :-
	evaltrue(E,St).
bigstep(false(E),_,St,St) :-
	evalfalse(E,St).
bigstep(decl(var(X),E):(E1:release(var(X))),LR,St0,St3) :-
	evalAndSave(X,E,[(X,_)|St0],St1),
	copyStateSkeleton(St1,St2),
	bigstep(E1,LR,St1,St2),
	removeVar(X,St2,St3).
bigstep(E1:E2,LR,St0,St2) :-
	functor(E1,F,N),
	F/N \== decl/2, 
	copyStateSkeleton(St0,St1),
	bigstep(E1,LR,St0,St1),
	bigstep(E2,LR,St1,St2).
bigstep(E1+_,LR,St0,St1) :-
	bigstep(E1,LR,St0,St1).
bigstep(_+E2,LR,St0,St1) :-
	bigstep(E2,LR,St0,St1).
bigstep(star(_),_,St,St).
bigstep(star(E),right,St0,St1) :-		% Right recursive interpretation
	bigstep(E:star(E),right,St0,St1).
bigstep(star(E),left,St0,St1) :-		% Left recursive interpretation
	bigstep(star(E):E,left,St0,St1).
	

% Small-step interpretation wrt a regular expression 
% Ignore left/right with small-step - unclear what it means

run(eps,St) :-
	haltState(St).
run(Expr,St) :-
	step(Expr,Expr1,St,St1),
	run(Expr1,St1).
	
step(asg(var(X),E),eps,St0,St1) :-
	evalAndSave(X,E,St0,St1).
step(true(E),eps,St0,St0) :-
	evaltrue(E,St0).
step(false(E),eps,St0,St0) :-
	evalfalse(E,St0).
step(decl(var(X),E),eps,St0,St1) :-
	evalAndSave(X,E,St0,St1).
step(release(var(X)),eps,St0,St1) :-
	removeVar(X,St0,St1).
step((eps:S),S,St0,St0).
step((S1:S2),S11:S2,St0,St1) :-
	S1\==eps,
	step(S1,S11,St0,St1).
step(Expr1+_Expr2,Expr11,St0,St1) :-
	step(Expr1,Expr11,St0,St1).
step(_Expr1+Expr2,Expr21,St0,St1) :-
	step(Expr2,Expr21,St0,St1).
step(star(_Expr),eps,St0,St0).
step(star(Expr),Expr1:star(Expr),St0,St1) :-		% Right recursive interpretation
	step(Expr,Expr1,St0,St1).

	
haltState(St) :-
	write(St),
	nl.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%
%----- expressions -----
%%%%%%%%%%%%%%%%%%%%%%%%

eval(var(X),St,V) :-
	find(St,X,V).
eval(cns(nat(N)),_,N).
eval(add(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1+V2.
eval(sub(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1-V2.
eval(mul(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1*V2.
eval(div(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1/V2.
eval(E1>E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	gt(V1,V2,V).
eval(E1<E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	lt(V1,V2,V).
eval(E1>=E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	gte(V1,V2,V).
eval(E1=<E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	lte(V1,V2,V).
eval(E1==E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	eq(V1,V2,V).
eval(logicaland(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	logicalAnd(V,V1,V2).
eval(not(E),St0,V) :-
	eval(E,St0,V1),
	negate(V1,V).

evaltrue(E,St) :-
	eval(E,St,1).
evalfalse(E,St) :-
	eval(E,St,0).
	
evalAndSave(X,E,St,St1) :-
	eval(E,St,V),
	save(X,V,St,St1).
	
%%%%%%%%%%%%%%%%%%%%%%%%
% Utility relations
%%%%%%%%%%%%%%%%%%%%%%%%

	
find([(X,N)|_],X,N).
find([(Y,_)|St],X,N) :-
	X \== Y,
	find(St,X,N).
	
save(X,V,[(X,_)|St],[(X,W)|St]) :-
	W is V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W is V.
	
removeVar(X,[(X,_)|St],St).
removeVar(X,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	removeVar(X,St,St1).

gt(X,Y,1) :-
	X > Y.
gt(X,Y,0) :-
	X =< Y.
	
lt(X,Y,1) :-
	X < Y.
lt(X,Y,0) :-
	X >= Y.
	
gte(X,Y,1) :-
	X >= Y.
gte(X,Y,0) :-
	X < Y.
	
lte(X,Y,1) :-
	X =< Y.
lte(X,Y,0) :-
	X > Y.
	
eq(X,Y,1) :-
	X == Y.
eq(X,Y,0) :-
	X \== Y.
	
logicalAnd(1,V1,V2) :-
	1 is V1/\V2.
logicalAnd(0,V1,V2) :-
	0 is V1/\V2.			
	
negate(1,0).
negate(0,1).

copyStateSkeleton([],[]).
copyStateSkeleton([(X,_)|St],[(X,_)|St1]) :-
	copyStateSkeleton(St,St1).

% Getting and setting options

% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
    ( recognised_option(X,Opt,Values) ->
        append(Values, Rest, T),
        RT = Rest,
        Options = [Opt|OT], Args = AT
    ; Options = OT, Args = [X|AT],
      RT = T
    ),
    get_options(RT,OT,AT).

recognised_option('-prg',file(R),[R]).
recognised_option('-big',style(big),[]).
recognised_option('-small',style(small),[]).
recognised_option('-transform',transform(yes),[]).
recognised_option('-left',recursion(left),[]).	% not yet implemented
recognised_option('-right',recursion(right),[]).

set_options(Options,File,Style,LR,YN) :-
    (member(file(File),Options) -> true
    ; write('No input file given'),nl,fail
    ),
    (member(style(Style),Options) -> true
    ; Style=small		% default to small step
    ),
    ( member(recursion(LR),Options) -> true
    ; LR=right
    ),
    ( member(transform(YN),Options) -> true
    ; YN=no
    ).

