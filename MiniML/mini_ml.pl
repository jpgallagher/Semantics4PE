% Mini-ML semantics
% From Kahn 1984

:-module(mini_ml,_,[assertions]).

:- entry testfac(_,_).

go(E,V) :-
	init(Rho),
	eval(E,V,Rho).
	
% initial environment
init([
		(id(eq),opaque(eq)),
		(id(times),opaque(times)),
		(id(sub),opaque(sub)),
		(id(plus),opaque(plus))
	]).

eval(number(N),int(N),_).
eval(true,true,_).
eval(false,false,_).
eval(lambda(P,E),closure(P,E,Rho),Rho).
eval(id(I),Alpha,Rho) :-
	val_of(Rho,I,Alpha).
eval(if(E1,E2,_),Alpha,Rho) :-
	eval(E1,true,Rho),
	eval(E2,Alpha,Rho).
eval(if(E1,_,E3),Alpha,Rho) :-
	eval(E1,false,Rho),
	eval(E3,Alpha,Rho).
eval(mlpair(E1,E2),(Alpha,Beta),Rho) :-
	eval(E1,Alpha,Rho),
	eval(E2,Beta,Rho).
eval(apply(E1,E2),Beta,Rho) :-
	eval(E1,closure(P,E,Rho1),Rho),
	eval(E2,Alpha,Rho),
	eval(E,Beta,[(P,Alpha)|Rho1]).
eval(apply(E1,E2),Beta,Rho) :-
	eval(E1,opaque(Op),Rho),
	eval(E2,Alpha,Rho),
	opeval(Op,Alpha,Beta).
eval(let(P,E2,E1),Beta,Rho) :-
	eval(E2,Alpha,Rho),
	eval(E1,Beta,[(P,Alpha)|Rho]).
eval(letrec(P,E2,E1),Beta,Rho) :-
	eval(E2,Alpha,[(P,Alpha)|Rho]),
	%unify_with_occurs_check(Alpha,Alpha1),
	eval(E1,Beta,[(P,Alpha)|Rho]).
	
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
	eval(letrec(id(i),id(j),id(i)),V,Rho).

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
		%apply(id(fact),number(4))
		id(fact)
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
		%apply(id(even),number(4))
		mlpair(id(even),id(odd))
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
	
	
testfac(N,V) :-
	fact(N,E),
	go(E,V).
	
