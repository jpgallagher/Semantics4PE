:- module(while,_).

:- use_module(while_reg).
:- use_module(transformExpr).
:- use_module(reg2c).


%%%%%%%%%%%%%%%%%%%%%%%%%
% initial setup and entry
%%%%%%%%%%%%%%%%%%%%%%%%%

main(ArgV) :-
    get_options(ArgV,Options,_),
    set_options(Options,File,Style,LR,YN),
    go(File,Style,LR,YN).
    
go(File,Style,LR,YN) :-
    open(File,read,S), 					% Read the AST of the parsed program
    read(S,P),
    close(S),
    P=logen(program/1,program(Prog)), 	% Get the main procedure
	member(function(main,[],Code),Prog),
	genvardecls(Prog,Code1,Code), 		% Global variable declarations
    regexp(let(var(cost),cns(nat(0)),Code1),Expr),	% Initialise cost variable
    exec(Expr,Style,LR,YN,[]).
    
exec(Expr,big,right,no,St) :-
	copyStateSkeleton(St,St1),
	bigstep(Expr,right,St,St1).
exec(Expr,small,right,no,St) :-	
	run(Expr,St).
exec(Expr,big,left,no,St) :-
	copyStateSkeleton(St,St1),
	bigstep(Expr,left,St,St1).
exec(Expr,small,left,no,St) :-
	run(Expr,St).
exec(Expr,big,right,yes,St) :-
	transformRegExpr(Expr,TExpr),
	copyStateSkeleton(St,St1),
	bigstep(TExpr,right,St,St1).
exec(Expr,small,right,yes,St) :-
	transformRegExpr(Expr,TExpr),
	run(TExpr,St).
exec(Expr,big,left,yes,St) :-
	transformRegExpr(Expr,TExpr),
	copyStateSkeleton(St,St1),
	bigstep(TExpr,left,St,St1).
exec(Expr,small,left,yes,St) :-
	transformRegExpr(Expr,TExpr),
	run(TExpr,St).		
	
	
genvardecls([],Code0,Code0).
genvardecls([[vardecl(var(X),_,Init)]|P],let(var(X),Init,Code1),Code0) :-
	genvardecls(P,Code1,Code0).
genvardecls([T|P],Code1,Code0) :-
	T \= [vardecl(_,_,_)],
	genvardecls(P,Code1,Code0).
	
initDeclVal(cns(nat(V)),V).
initDeclVal(null,_).

% Regular expressions over alphabet 
% {asg(X,Expr),true(Expr), false(Expr), decl(X,E), release(X)}
% where X is a variable, E an expression
%
% E ::= asg(X,Expr) | 
%		true(Expr) 	| 
%		false(Expr) | 
%		assert(E)	|
%		decl(X,E) 	|
%		release(X)	|
%		E1:E2 		| 
%		E1+E2 		| 
%		star(E) 	| 
%		null 		| 
%		eps

% Big-step interpretation wrt a regular expression and left/right recursion

bigstep(eps,_,St,St).
bigstep(asg(var(X),E),_,St0,St2) :-
	evalAndSave(X,E,St0,St1),
	evalAndSave(cost,add(var(cost),cns(nat(1))),St1,St2).	% increment cost
bigstep(true(E),_,St,St) :-
	evaltrue(E,St).
bigstep(assert(E),_,St,St) :-
	evaltrue(E,St).
bigstep(false(E),_,St,St) :-
	evalfalse(E,St).
bigstep(decl(var(X),E):(E1:release(var(X))),LR,St0,St3) :-
	evalAndSave(X,E,[(X,_)|St0],St1),
	copyStateSkeleton(St1,St2),
	bigstep(E1,LR,St1,St2),
	observeStates(St1,St2),
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
%bigstep(star(_),_,St,St).
%bigstep(star(E),right,St0,St1) :-		% Right recursive interpretation
%	bigstep(E:star(E),right,St0,St1).
%bigstep(star(E),left,St0,St1) :-		% Left recursive interpretation
%	bigstep(star(E):E,left,St0,St1).
bigstep(while(E,_),_,St0,St0) :-
	evalfalse(E,St0).
bigstep(while(Expr,E),right,St0,St1) :-		% Right recursive interpretation
	evaltrue(Expr,St0),
	bigstep(E:while(Expr,E),right,St0,St1).
%bigstep(while(Expr,E),left,St0,St1) :-		% Left recursive interpretation omitted
%	evaltrue(Expr,St0),
%	bigstep(while(Expr,E):E,right,St0,St1).
	

% Small-step interpretation wrt a regular expression 
% Ignore left/right with small-step - unclear what it means

run(eps,_St).
run(Expr,St) :-
	step(Expr,Expr1,St,St1),
	run(Expr1,St1).
	
step(asg(var(X),E),eps,St0,St2) :-
	evalAndSave(X,E,St0,St1),
	evalAndSave(cost,add(var(cost),cns(nat(1))),St1,St2).	% increment cost
step(true(E),eps,St0,St0) :-
	evaltrue(E,St0).
step(assert(E),eps,St0,St0) :-
	evaltrue(E,St0).
step(false(E),eps,St0,St0) :-
	evalfalse(E,St0).
step(decl(var(X),E),eps,St0,St1) :-
	evalAndSave(X,E,[(X,_)|St0],St1).
step(release(var(X)),eps,St0,St1) :-
	observeState(St0),
	removeVar(X,St0,St1).
step((eps:S),S,St0,St0).
step((S1:S2),S11:S2,St0,St1) :-
	S1\==eps,
	step(S1,S11,St0,St1).
step(Expr1+_Expr2,Expr11,St0,St1) :-
	step(Expr1,Expr11,St0,St1).
step(_Expr1+Expr2,Expr21,St0,St1) :-
	step(Expr2,Expr21,St0,St1).
%step(star(_Expr),eps,St0,St0).
%step(star(Expr),Expr1:star(Expr),St0,St1) :-		% Right recursive interpretation
%	step(Expr,Expr1,St0,St1).
step(while(Expr,_E),eps,St0,St0) :-
	evalfalse(Expr,St0).
step(while(Expr,E),E1:while(Expr,E),St0,St1) :-		% Right recursive interpretation
	evaltrue(Expr,St0),
	step(E,E1,St0,St1).

	
	
%%%%%%%%%%%%%%%%%%%%%%%%
%----- expressions -----
%%%%%%%%%%%%%%%%%%%%%%%%

eval(null,_St,_V).	% null gives undefined value
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
eval(logicaland(E1,E2),St0,1) :-
	eval(E1,St0,1),
	eval(E2,St0,1).
eval(logicaland(E1,_E2),St0,0) :-
	eval(E1,St0,0).
eval(logicaland(_E1,E2),St0,0) :-
	eval(E2,St0,0).
eval(logicalor(E1,E2),St0,0) :-
	eval(E1,St0,0),
	eval(E2,St0,0).
eval(logicalor(E1,_E2),St0,1) :-
	eval(E1,St0,1).
eval(logicalor(_E1,E2),St0,1) :-
	eval(E2,St0,1).
eval(not(E),St0,V) :-
	eval(E,St0,V1),
	negate(V1,V).
eval(call(rand,[]),_St0,V) :-	% ad hoc handling of rand() function
	V>=0,
	V=<1.
	

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
	W = V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W = V.
	
removeVar(X,[(X,_)|St],St).
removeVar(X,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	removeVar(X,St,St1).

% Assume args are int
gt(X,Y,1) :-
	X >= Y+1.
gt(X,Y,0) :-
	X =< Y.
	
lt(X,Y,1) :-
	X =< Y-1.
lt(X,Y,0) :-
	X >= Y.
	
gte(X,Y,1) :-
	X >= Y.
gte(X,Y,0) :-
	X =< Y-1.
	
lte(X,Y,1) :-
	X =< Y.
lte(X,Y,0) :-
	X >= Y+1.
	
eq(X,Y,1) :-
	X == Y.
eq(X,Y,0) :-
	X \== Y.
				
	
negate(1,0).
negate(0,1).

copyStateSkeleton([],[]).
copyStateSkeleton([(X,_)|St],[(X,_)|St1]) :-
	copyStateSkeleton(St,St1).
	
observeState(St) :-	
	projectVars(St,_),
	write(St),
	nl.
	
observeStates(St0,St1) :-	
	append(IVars,[_],St0),	% Separate input vars and final cost
	append(_,[C],St1),
	observeCost(IVars,C),
	projectVars(St1,_),
	write((St0,St1)),
	nl.
	
projectVars([],0).
projectVars([X|Xs],N) :-
	projectVars(Xs,N1),
	N is N1+1,
	observeVar(N,X).
	
observeVar(_,_).	

observeCost(_,_).

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

