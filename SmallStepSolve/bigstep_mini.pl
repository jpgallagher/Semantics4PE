
%%%%%%%%%%%%%%%%%%%%%%%%
%----- expressions -----
%%%%%%%%%%%%%%%%%%%%%%%%

eval(var(X),St,St,V) :-
	find(St,X,V).
eval(cns(nat(N)),St,St,N).
eval(add(E1,E2),St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	V = V1+V2.
eval(sub(E1,E2),St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	V = V1-V2.
eval(mul(E1,E2),St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	V = V1*V2.
eval(div(E1,E2),St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	V = V1/V2.
eval(E1>E2,St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	gt(V1,V2,V).
eval(E1<E2,St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	lt(V1,V2,V).
eval(E1>=E2,St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	gte(V1,V2,V).
eval(E1=<E2,St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	lte(V1,V2,V).
eval(E1==E2,St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	eq(V1,V2,V).
eval(logicaland(E1,E2),St0,St2,V) :-
	eval(E1,St0,St1,V1),
	eval(E2,St1,St2,V2),
	V is V1/\V2.
eval(not(E),St0,St1,V) :-
	eval(E,St0,St1,V1),
	negate(V1,V).


%%%%%%%%%%%%%%%%%%%%%%%%	
%----- statements -----
%%%%%%%%%%%%%%%%%%%%%%%%

bigstep(skip,St,St).
bigstep(asg(var(X),E),St,St2) :-
	eval(E,St,St1,V),
	save(X,V,St1,St2).
bigstep(seq(S1,S2),St,St2) :-
	bigstep(S1,St,St1),
	bigstep(S2,St1,St2).
bigstep(ifthenelse(E,S1,_S2),St,St2) :-
	eval(E,St,St1,1),
	bigstep(S1,St1,St2).
bigstep(ifthenelse(E,_S1,S2),St,St2) :-
	eval(E,St,St1,0),
	bigstep(S2,St1,St2).
bigstep(while(E,S1),St,St1) :-
	bigstep(ifthenelse(E,seq(S1,while(E,S1)),skip),St,St1).
bigstep(for(Init,Cond,Incr,S1),St,St2) :-
	bigstep(Init,St,St1),
	bigstep(while(Cond,seq(S1,Incr)),St1,St2).

%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Utility relations --
%%%%%%%%%%%%%%%%%%%%%%%%%

	
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
	
negate(true,false).
negate(false,true).
