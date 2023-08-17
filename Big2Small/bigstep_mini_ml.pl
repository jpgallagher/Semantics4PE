% Mini-ML semantics
% From Kahn 1987

:-module(bigstep_mini_ml,_,[assertions]).

%:- entry testfac(_,_).

go(E,V) :-
	init(Rho),
	eval(E,Rho,V).
	
% initial environment
init([
		(id(eq),opaque(eq)),
		(id(times),opaque(times)),
		(id(sub),opaque(sub)),
		(id(plus),opaque(plus))
	]).

eval(number(N),_Rho,int(N)).
eval(true,_Rho,true).
eval(false,_Rho,false).
eval(lambda(P,E),Rho,closure(P,E,Rho)).
eval(id(I),Rho,Alpha) :-
	val_of(Rho,I,Alpha).
eval(if(E1,E2,E3),Rho,Alpha) :-
	eval(E1,Rho,V),
	eval(choice(V,E2,E3),Rho,Alpha).
eval(choice(true,E2,_E3),Rho,Alpha) :-
	eval(E2,Rho,Alpha).
eval(choice(false,_E2,E3),Rho,Alpha) :-
	eval(E3,Rho,Alpha).
eval(mlpair(E1,E2),Rho,Pair) :-
	eval(E1,Rho,Alpha),
	eval(E2,Rho,Beta),
	Pair = (Alpha,Beta).
eval(apply(E1,E2),Rho,Beta) :-
	eval(E1,Rho,Gamma),
	Gamma=closure(P,E,Rho1),
	eval(E2,Rho,Alpha),
	Rho2=[(P,Alpha)|Rho1],
	eval(E,Rho2,Beta).
eval(apply(E1,E2),Rho,Beta) :-
	eval(E1,Rho,Gamma),
	Gamma=opaque(Op),
	eval(E2,Rho,Alpha),
	opeval(Op,Alpha,Beta).
eval(let(P,E2,E1),Rho,Beta) :-
	eval(E2,Rho,Alpha),
	Rho1=[(P,Alpha)|Rho],
	eval(E1,Rho1,Beta).
eval(letrec(P,E2,E1),Rho,Beta) :-
	Rho1=[(P,Alpha)|Rho],
	eval(E2,Rho1,Alpha),
	eval(E1,Rho1,Beta).
	
val_of([(id(I),Alpha)|_Rho],I,Alpha).
val_of([(id(X),_Beta)|Rho],I,Alpha) :-
	X \== I,
	val_of(Rho,I,Alpha).
val_of([(pairpat(P1,P2),(Alpha,Beta))|Rho],I,N) :-
	val_of([(P1,Alpha),(P2,Beta)|Rho],I,N).
		
% predefined functions

opeval(eq,(X,Y),true) :-
	X==Y.
opeval(eq,(X,Y),false) :-
	X\==Y.
opeval(times,(int(X),int(Y)),int(Z)) :-
	Z is X*Y.
opeval(sub,(int(X),int(Y)),int(Z)) :-
	Z is X-Y.
opeval(plus,(int(X),int(Y)),int(Z)) :-
	Z is X+Y.
	


bigStepPred(eval(_X,_Y,_Z)).

otherPred(opeval(_X,_Y,_Z)).
otherPred(val_of(_X,_Y,_Z)).


test1(V) :-
	fac(E),
	go(E,V).
	
test2(V) :-
	block(E),
	go(E,V).
	
test3(V) :-
	simul(E),
	go(E,V).

test4(V) :-
	evenodd(E),
	go(E,V).

test5(V) :-
	init(Rho),
	eval(letrec(id(i),id(j),id(i)),Rho,V).
	
test6(V) :-
	if(E),
	go(E,V).

/* letrec fact= 𝜆x.if x = 0 then 1 else x * fact(x - 1) in fact 4 */

fac(
	letrec(id(fact),
		lambda(id(x),
		if(apply(id(eq),mlpair(id(x),number(0))),
			number(1),
			apply(id(times),
				mlpair(id(x),
					apply(id(fact),
						apply(id(sub),mlpair(id(x),number(1))))
			)
		))),
		let(id(n),number(5),apply(id(fact),id(n)))
		%id(fact)
		)
	).
		
/* let i=5 in let i=i+1 in i */

block(
	let(id(i),
		number(5),
		let(id(i),
			apply(id(plus),mlpair(id(i),number(1))),id(i))
	)).

/* let (x,y)=(2,3) in let (x,y)=(y,x) in x */

simul(
	let(pairpat(id(x),id(y)),
		mlpair(number(2),number(3)),
		let(pairpat(id(x),id(y)),
			mlpair(id(y),id(x)),
			id(x)))
	).

/* letrec(even,odd)=
	(𝜆x.if x=0 then true else odd(x-1), 
	 𝜆x.if x=0 then false else even(x-1))
    in even(3)
*/

evenodd(
	letrec(
		pairpat(id(even),id(odd)),
		mlpair(
			lambda(id(x),
				if(apply(id(eq),mlpair(id(x),number(0))),
				true,
				apply(id(odd),
					apply(id(sub),mlpair(id(x),number(1))))
			)),
			lambda(id(x),
				if(apply(id(eq),mlpair(id(x),number(0))),
				false,
				apply(id(even),
					apply(id(sub),mlpair(id(x),number(1))))
			))
			),
		apply(id(even),number(3))
		%mlpair(id(even),id(odd))
	)).
	
% factorial applied to N

fact(N,
	letrec(id(fact),
		lambda(id(x),
		if(apply(id(eq),mlpair(id(x),number(0))),
			number(1),
			apply(id(times),
				mlpair(id(x),
					apply(id(fact),
						apply(id(sub),mlpair(id(x),number(1))))
			)
		))),
		apply(id(fact),number(N))
		)
	).
	
if(let(id(x),number(3),
	if(apply(id(eq),mlpair(id(x),number(3))),
			number(1),
			number(0))
		)
	).
	
	
testfac(N,V) :-
	fact(N,E),
	go(E,V).
	
