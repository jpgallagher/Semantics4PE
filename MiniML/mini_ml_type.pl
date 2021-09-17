% Mini-ML types
% From Kahn 1984

:-module(mini_ml_type,_).

:- use_module(chclibs(setops)).
:- use_module(library(terms_vars)).

type(number(_N),int,_).
type(true,bool,_).
type(false,bool,_).
type(lambda(P,E),(Tau1 -> Tau),Rho) :-
	declare(P,Tau1,Rho1),
	append(Rho1,Rho,Rho2),
	type(E,Tau,Rho2).
type(id(I),Tau,Rho) :-
	type_of(Rho,I,Sigma),
	generic_inst(Tau,Sigma).
type(if(E1,E2,E3),Tau,Rho) :-
	type(E1,bool,Rho),
	type(E2,Tau,Rho),
	type(E3,Tau,Rho).
type(mlpair(E1,E2),(Tau1 * Tau2),Rho) :-
	type(E1,Tau1,Rho),
	type(E2,Tau2,Rho).
type(apply(E1,E2),Tau,Rho) :-
	type(E1,(Tau1 -> Tau),Rho),
	type(E2,Tau1,Rho).
type(let(P,E2,E1),Tau1,Rho) :-
	declare(P,Tau2,Rho1),
	type(E2,Tau2,Rho),
	gen(Rho1,Rho,Rho2),
	append(Rho2,Rho,Rho3),
	type(E1,Tau1,Rho3).
type(letrec(P,E2,E1),Tau1,Rho) :-
	declare(P,Tau2,Rho1),
	append(Rho1,Rho,Rho2),
	type(E2,Tau2,Rho2),
	type(E1,Tau1,Rho2).
	
declare(id(X),Tau,[(id(X),Tau)]).
declare(pairpat(P1,P2),(Tau1 * Tau2),Rho3) :-
	declare(P1,Tau1,Rho1),
	declare(P2,Tau2,Rho2),
	disjoint(Rho1,Rho2),
	append(Rho1,Rho2,Rho3).
	
gen(Rho,Rho1,Rho2) :-
	freevars(Rho1,Ys),
	generalise(Rho,Ys,Rho2).
	
generalise([(id(X),Tau)|Rho],Ys,[(id(X),forall(Xs,Tau))|Rho2]) :-
	varset(Tau,Zs),
	setdiff(Zs,Ys,Xs),
	generalise(Rho,Ys,Rho2).
generalise([],_,[]).

freevars(X,[X]) :-
	var(X),
	!.
freevars(bool,[]).
freevars(int,[]).
freevars((Tau1 -> Tau2),Xs) :-
	freevars(Tau1,Xs1),
	freevars(Tau2,Xs2),
	setunion(Xs1,Xs2,Xs).
freevars((Tau1 * Tau2),Xs) :-
	freevars(Tau1,Xs1),
	freevars(Tau2,Xs2),
	setunion(Xs1,Xs2,Xs).
freevars(forall(Zs,Tau),Xs) :-
	freevars(Tau,Ys),
	setdiff(Ys,Zs,Xs).
freevars([(id(_X),Sigma)|Rho],Zs) :-
	freevars(Rho,Ys),
	freevars(Sigma,Xs),
	setunion(Xs,Ys,Zs).
freevars([],[]).

disjoint(Rho1,Rho2) :-
	\+ commonvar(Rho1,Rho2).
	
commonvar(Rho1,Rho2) :-
	member((id(X),_),Rho1),
	member((id(X),_),Rho2).
	
generic_inst(Tau,Tau1) :-
	var(Tau1),
	!,
	Tau=Tau1.
generic_inst(Tau,forall(_,Tau1)) :-
	!,
	Tau=Tau1.
generic_inst(Tau,Tau).
	
type_of([(id(X),Tau)|_Rho],X,Tau).
type_of([(id(Y),_Beta)|Rho],X,Tau) :-
	X \== Y,
	type_of(Rho,X,Tau).
	
% predefined functions

optypes([
	(id(eq),forall([X],(X*X->bool))),
	(id(times),(int*int->int)),
	(id(sub),(int*int->int)),
	(id(plus),(int*int->int))
	]).


test1(V) :-
	fac(E),
	optypes(Rho),
	type(E,V,Rho).

test2(V) :-
	block(E),
	optypes(Rho),
	type(E,V,Rho).

test3(V) :-
	simul(E),
	optypes(Rho),
	type(E,V,Rho).

test4(V) :-
	evenodd(E),
	optypes(Rho),
	type(E,V,Rho).

test5(V) :-
	optypes(Rho),
	type(letrec(id(i),id(j),id(i)),V,Rho).
	
test6(V) :-
	polyconst(E),
	optypes(Rho),
	type(E,V,Rho).
	
test7(V) :-
	monoconst(E),
	optypes(Rho),
	type(E,V,Rho).
	
% Example expressions

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
	
	
/* let k = 𝜆x. (let f = 𝜆y. x in f) in k */
 
polyconst(
	let(id(k), 
		lambda(id(x),
			let(id(f), lambda(id(y), id(x)), id(f))),
		id(k)
		)
	).

/* 𝜆x. 𝜆y. x */

monoconst(
	lambda(id(x),
		lambda(id(y),id(x))
	)).
	