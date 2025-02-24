:- module(bigstep_mini_ml,_).

/*  run(A) :- run__0(A). */
run__0([]).
run__0([eval(A,B,C)]) :-
    smallStep__1(A,B,C,D),
    run__0(D).

/*  smallStep(eval(B,C,D),A) :- smallStep__1(B,C,D,A). */
smallStep__1(number(A),_,int(A),[]).
smallStep__1(true,_,true,[]).
smallStep__1(false,_,false,[]).
smallStep__1(lambda(B,C),A,closure(B,C,A),[]).
smallStep__1(id(C),A,B,[]) :-
    eval__2(A,C,B).
smallStep__1(choice(true,C,_),A,B,[eval(C,A,B)]).
smallStep__1(choice(false,_,C),A,B,[eval(C,A,B)]).
smallStep__1(apply10_1(C,D),A,B,[eval(apply10_2(C,E,F,G),A,B)]) :-
    eval__3(D,closure(E,F,G)).
smallStep__1(apply10_3(C,D,E,F),A,B,[eval(D,A,B)]) :-
    eval__3(A,[(C,F)|E]).
smallStep__1(apply11_1(C,D),A,B,[eval(apply11_2(C,E),A,B)]) :-
    eval__3(D,opaque(E)).
smallStep__1(let12_1(C,D,E),A,B,[eval(D,F,B)]) :-
    eval__3(F,[(C,E)|A]).
smallStep__1(letrec(C,D,E),A,B,[eval(letrec13_1(D,E,G),F,B)]) :-
    eval__3(F,[(C,G)|A]).
smallStep__1(if(C,D,E),A,B,[eval(choice(F,D,E),A,B)]) :-
    smallStep__1(C,A,F,[]).
smallStep__1(if(C,D,E),A,B,[eval(if_aux_7(G,D,E,A),F,B)]) :-
    smallStep__1(C,A,H,[eval(G,F,H)]).
smallStep__1(mlpair(C,D),A,B,[eval(mlpair9_1(D,E),A,B)]) :-
    smallStep__1(C,A,E,[]).
smallStep__1(mlpair(C,D),A,B,[eval(mlpair_aux_10(F,D,A),E,B)]) :-
    smallStep__1(C,A,G,[eval(F,E,G)]).
smallStep__1(mlpair9_1(C,D),A,B,[]) :-
    smallStep__1(C,A,E,[]),
    eval__3(B,(D,E)).
smallStep__1(mlpair9_1(C,D),A,B,[eval(mlpair9_1(F,D),E,B)]) :-
    smallStep__1(C,A,G,[eval(F,E,G)]).
smallStep__1(apply(C,D),A,B,[eval(apply10_1(D,E),A,B)]) :-
    smallStep__1(C,A,E,[]).
smallStep__1(apply(C,D),A,B,[eval(apply_aux_12(F,D,A),E,B)]) :-
    smallStep__1(C,A,G,[eval(F,E,G)]).
smallStep__1(apply10_2(C,D,E,F),A,B,[eval(apply10_3(D,E,F,G),_,B)]) :-
    smallStep__1(C,A,G,[]).
smallStep__1(apply10_2(C,D,E,F),A,B,[eval(apply10_2(H,D,E,F),G,B)]) :-
    smallStep__1(C,A,I,[eval(H,G,I)]).
smallStep__1(apply(C,D),A,B,[eval(apply11_1(D,E),A,B)]) :-
    smallStep__1(C,A,E,[]).
smallStep__1(apply(C,D),A,B,[eval(apply_aux_16(F,D,A),E,B)]) :-
    smallStep__1(C,A,G,[eval(F,E,G)]).
smallStep__1(apply11_2(C,D),A,B,[]) :-
    smallStep__1(C,A,E,[]),
    eval__4(D,E,B).
smallStep__1(apply11_2(C,D),A,B,[eval(apply11_2(F,D),E,B)]) :-
    smallStep__1(C,A,G,[eval(F,E,G)]).
smallStep__1(let(C,D,E),A,B,[eval(let12_1(C,E,F),A,B)]) :-
    smallStep__1(D,A,F,[]).
smallStep__1(let(C,D,E),A,B,[eval(let_aux_19(C,G,E,A),F,B)]) :-
    smallStep__1(D,A,H,[eval(G,F,H)]).
smallStep__1(letrec13_1(C,D,E),A,B,[eval(D,A,B)]) :-
    smallStep__1(C,A,E,[]).
smallStep__1(letrec13_1(C,D,E),A,B,[eval(letrec13_1_aux_22(G,D,H,A),F,B)]) :-
    smallStep__1(C,A,E,[eval(G,F,H)]).
smallStep__1(if_aux_7(C,D,E,F),A,B,[eval(choice(G,D,E),F,B)]) :-
    smallStep__1(C,A,G,[]).
smallStep__1(if_aux_7(C,D,E,F),A,B,[eval(if_aux_7(H,D,E,F),G,B)]) :-
    smallStep__1(C,A,I,[eval(H,G,I)]).
smallStep__1(mlpair_aux_10(C,D,E),A,B,[eval(mlpair9_1(D,F),E,B)]) :-
    smallStep__1(C,A,F,[]).
smallStep__1(mlpair_aux_10(C,D,E),A,B,[eval(mlpair_aux_10(G,D,E),F,B)]) :-
    smallStep__1(C,A,H,[eval(G,F,H)]).
smallStep__1(apply_aux_12(C,D,E),A,B,[eval(apply10_1(D,F),E,B)]) :-
    smallStep__1(C,A,F,[]).
smallStep__1(apply_aux_12(C,D,E),A,B,[eval(apply_aux_12(G,D,E),F,B)]) :-
    smallStep__1(C,A,H,[eval(G,F,H)]).
smallStep__1(apply_aux_16(C,D,E),A,B,[eval(apply11_1(D,F),E,B)]) :-
    smallStep__1(C,A,F,[]).
smallStep__1(apply_aux_16(C,D,E),A,B,[eval(apply_aux_16(G,D,E),F,B)]) :-
    smallStep__1(C,A,H,[eval(G,F,H)]).
smallStep__1(let_aux_19(C,D,E,F),A,B,[eval(let12_1(C,E,G),F,B)]) :-
    smallStep__1(D,A,G,[]).
smallStep__1(let_aux_19(C,D,E,F),A,B,[eval(let_aux_19(C,H,E,F),G,B)]) :-
    smallStep__1(D,A,I,[eval(H,G,I)]).
smallStep__1(letrec13_1_aux_22(C,D,E,F),A,B,[eval(D,F,B)]) :-
    smallStep__1(C,A,E,[]).
smallStep__1(letrec13_1_aux_22(C,D,E,F),A,B,[eval(letrec13_1_aux_22(H,D,I,F),G,B)]) :-
    smallStep__1(C,A,E,[eval(H,G,I)]).

/*  eval(val_of(A,B,C)) :- eval__2(A,B,C). */
eval__2([(id(A),B)|_],A,B).
eval__2([(id(D),_)|C],A,B) :-
    eval__5(D,A),
    eval__2(C,A,B).
eval__2([(pairpat(D,E),F,G)|C],A,B) :-
    eval__2([(D,F),(E,G)|C],A,B).

/*  eval(A=B) :- eval__3(A,B). */
eval__3(A,B) :-
    A=B.

/*  eval(opeval(A,B,C)) :- eval__4(A,B,C). */
eval__4(eq,(A,B),true) :-
    eval__6(A,B).
eval__4(eq,(A,B),false) :-
    eval__5(A,B).
eval__4(times,(int(A),int(B)),int(C)) :-
    eval__7(C,A*B).
eval__4(sub,(int(A),int(B)),int(C)) :-
    eval__7(C,A-B).
eval__4(plus,(int(A),int(B)),int(C)) :-
    eval__7(C,A+B).

/*  eval(A\==B) :- eval__5(A,B). */
eval__5(A,B) :-
    A\==B.

/*  eval(A==B) :- eval__6(A,B). */
eval__6(A,B) :-
    A==B.

/*  eval(A is B) :- eval__7(A,B). */
eval__7(A,B) :-
    A is B.
/* Specialisation time 2.9709999999999894 ms (runtime) */

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
	go(letrec(id(i),id(j),id(i)),V).
	
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
%		apply(id(fact),number(4))
%		apply(id(fact),apply(id(fact),number(3)))
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
	
if(let(id(x),number(4),
	if(let(id(z),number(4),apply(id(eq),mlpair(id(x),id(z)))),
			let(id(y),number(1),id(y)),
			let(id(y),number(0),id(y)))
		)
	).
	
testfac(N,V) :-
	fact(N,E),
	go(E,V).
	
go(E,V) :-
	init(Rho),
	run__0([eval(E,Rho,V)]).
	
% initial environment
init([
		(id(eq),opaque(eq)),
		(id(times),opaque(times)),
		(id(sub),opaque(sub)),
		(id(plus),opaque(plus))
	]).

