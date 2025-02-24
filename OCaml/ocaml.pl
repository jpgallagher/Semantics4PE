% OCaml big-step semantics with substitution
% From lectures notes 
% https://courses.engr.illinois.edu/cs421/sp2009/lectures/ (Lecture 23)

:-module(ocaml,_,[assertions]).

eval(num(N),num(N)).
eval(true,true).
eval(false,false).
eval(fun(P,E),fun(P,E)).
eval(rec(P,E),E1) :-
	subst(E,P,E1,rec(P,E)).
eval(if(E1,E2,_),V) :-
	eval(E1,true),
	eval(E2,V).
eval(if(E1,_,E3),V) :-
	eval(E1,false),
	eval(E3,V).
eval(apply(E1,E2),V1) :-
	eval(E1,fun(P,E)),
	eval(E2,V),
	subst(E,P,E3,V),
	eval(E3,V1).

% translation rules for let and letrec

%eval(let(X,E,E1),V) :-
%	eval(apply(fun(X,E1),E),V).
%eval(letrec(F,E,E1),V) :-
%	eval(apply(fun(F,E1),rec(F,E)),V).
	
eval(let(X,E,E1),V) :-
	eval(E,W),
	subst(E1,X,D3,W),
	eval(D3,V).
eval(letrec(F,E,E1),V) :-
	subst(E,F,W,rec(F,E)),
	subst(E1,F,D3,W),
	eval(D3,V).
	
% predefined functions

eval(eq(E1,E2),V) :-
	eval(E1,V1),
	eval(E2,V2),
	opeval(eq,(V1,V2),V).
eval(le(E1,E2),V) :-
	eval(E1,V1),
	eval(E2,V2),
	opeval(le,(V1,V2),V).
eval(times(E1,E2),V) :-
	eval(E1,V1),
	eval(E2,V2),
	opeval(times,(V1,V2),V).
eval(sub(E1,E2),V) :-
	eval(E1,V1),
	eval(E2,V2),
	opeval(sub,(V1,V2),V).
eval(plus(E1,E2),V) :-
	eval(E1,V1),
	eval(E2,V2),
	opeval(plus,(V1,V2),V).

opeval(eq,(num(X),num(Y)),true) :-
	X==Y.
opeval(eq,(num(X),num(Y)),false) :-
	X\==Y.
opeval(le,(num(X),num(Y)),true) :-
	X=<Y.
opeval(le,(num(X),num(Y)),false) :-
	X>Y.
opeval(times,(num(X),num(Y)),num(Z)) :-
	Z is X*Y.
opeval(sub,(num(X),num(Y)),num(Z)) :-
	Z is X-Y.
opeval(plus,(num(X),num(Y)),num(Z)) :-
	Z is X+Y.
	
% subst(E,X,E1,Y): substitute occurrences of X in E by Y, yielding E1

subst(id(X),id(X),Y,Y).
subst(id(X),id(Z),id(X),_) :-
	X\==Z.
subst(num(X),_,num(X),_).
subst(true,_,true,_).
subst(false,_,false,_).
subst(fun(X,E),X,fun(X,E),_). 		% X is bound in E
subst(fun(Z,E),X,fun(Z,E1),Y) :- 	% X is free in E
	X\==Z,	
	subst(E,X,E1,Y).
subst(rec(X,E),X,rec(X,E),_). 		% X is bound in E
subst(rec(Z,E),X,rec(Z,E1),Y) :-	% X is free in E
	X\==Z,	
	subst(E,X,E1,Y).
subst(if(E1,E2,E3),X,if(E4,E5,E6),Y) :-
	subst(E1,X,E4,Y),
	subst(E2,X,E5,Y),
	subst(E3,X,E6,Y).
subst(apply(E1,E2),X,apply(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
subst(let(Z,E1,E2),X,let(Z,E3,E4),Y) :-		% apply translation rules for let and letrec
	subst(apply(fun(Z,E2),E1),X,apply(fun(Z,E4),E3),Y).
subst(letrec(F,E,E1),X,letrec(F,E3,E4),Y) :-
	subst(apply(fun(F,E1),rec(F,E)),X,apply(fun(F,E4),rec(F,E3)),Y).
subst(eq(E1,E2),X,eq(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
subst(sub(E1,E2),X,sub(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
subst(plus(E1,E2),X,plus(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
subst(times(E1,E2),X,times(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
subst(le(E1,E2),X,le(E3,E4),Y) :-
	subst(E1,X,E3,Y),
	subst(E2,X,E4,Y).
	
	

fact(N,V) :-
	fac(N,F),
	eval(F,num(V)).

fac(N,
	letrec(id(fac), 
	fun(id(n), 
		if(le(id(n),num(0)), 
			num(1), 
			times(id(n),apply(id(fac),sub(id(n),num(1)))))), 
	apply(id(fac),num(N)))).
	
block(
	let(id(i),
		num(5),
		let(id(i),
			plus(id(i),num(1)),id(i))
	)).

/* let (x,y)=(2,3) in let (x,y)=(y,x) in x */

simul(
	let(id(x),num(2),
		let(id(y),num(3),
			let(id(x),id(y),
				let(id(y),id(x),
				id(x)
			)
		)
	))).

/* letrec(even,odd)=
	(𝜆x.if x=0 then true else odd(x-1), 
	 𝜆x.if x=0 then false else even(x-1))
    in even(3)
*/

evenodd(N,
	letrec(id(even), 
		fun(id(x), 
			if(eq(id(x),num(0)),
				true,
				apply(id(odd),sub(id(x),num(1))))
			),
		letrec(id(odd),
		fun(id(x), 
			if(eq(id(x),num(0)),
				false,
				apply(id(even),sub(id(x),num(1))))
			),
		apply(id(even),num(N))
	))).
	
	
eo(N,V) :-
	evenodd(N,E),
	eval(E,V).
	
/* 

Factorial by fixpoint 

G = lambda f.lambda n.if n=0 then 1 else n*f(n-1)

G fact = lambda n.if n=0 then 1 else n*fact(n-1) = fact

Y G = fact

((Y G) n) = (fact n)

*/

ycomb(
	fun(id(f),apply(fun(id(x),apply(id(x),id(x))),fun(id(x),apply(id(x),id(x)))))
	).
	
	
g(fun(id(f),
	fun(id(n), 
		if(le(id(n),num(0)), 
			num(1), 
			times(id(n),apply(id(f),sub(id(n),num(1)))))))).
	
fixfact(N,M) :-
	ycomb(Y),
	g(G),
	eval(apply(apply(Y,G),num(N)),M).
	
fixg(F) :-
	ycomb(Y),
	g(G),
	eval(apply(Y,G),F).
	
y(V) :-
	ycomb(Y),
	eval(Y,V).
	
/* from Wikipedia article on fixpoint combinator

let rec fix f x = f (fix f) x (* note the extra x; here fix f = \x-> f
(fix f) x *)

let factabs fact = function   (* factabs has extra level of lambda
abstraction *) 0 -> 1
 | x -> x * fact (x-1)

let _ = (fix factabs) 5       (* evaluates to "120" *) 
*/