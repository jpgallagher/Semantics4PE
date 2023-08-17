:- module(smallStepSolve,_).

:- use_module(library(terms_vars)).
:- use_module(chclibs(setops)).

run([]).
run([A]) :-
	bigStepPred(A),
	smallStep(A,As1),
	run(As1).

smallStep(A,[]) :-			% axiom 
	givenRule(_,A,Bs),
	evalConditions(Bs,[]).
smallStep(A,[B]) :-			% rule with one big step premise
	givenRule(_,A,Bs),
	evalConditions(Bs,[B]).
smallStep(A,As) :-			% rule with 2 premises, first one is a big step
	rule(K,A,[B1,B2]),		
	bigStepPred(B1),
	smallStep(B1,D1),
	foldStack(D1,B2,K,As).
	
foldStack([],B2,_,As) :-	 % B1 terminates, B2 the remaining premise
	evalConditions([B2],As).
foldStack([D1],B2,K,[H]) :-  % Create new big step H for modified premises [D1,B2]
	newBigStep(D1,B2,K,H).
	
%======================== 

rule(K,A,Bs) :-
	givenRule(K,A,Bs).
rule(K,A,Bs) :-
	auxRule(K,A,Bs).
	
newBigStep(D1,B2,K,H) :-
	givenRule(K,_H1,[A1,A2]),
	A1=..[_P,S1,E1,_V1],
	varset(A2,Ys),
	setintersect(Ys,[S1,E1],Zs), % Check whether S1, E1 are used again
	newBigStepPred(Zs,D1,B2,K,H).
	
newBigStepPred([],D1,B2,K,H) :-	% fold with original clause, return clause head
	givenRule(K,H,[D1,B2]).
newBigStepPred([_|_],D1,B2,K,H) :-
	auxRule(K,H,[D1,B2]).
	
auxRule(K,H,[D1,B2]) :-
	givenRule(K,H1,[A1,A2]),
	A1=..[P,S1,E1,V1],
	varset(A2,Ys),
	setintersect(Ys,[S1,E1],[Z|Zs]), % S1 and/or E1 are reused in B2
	A3=..[P,S2,E2,V1],				 % Replace A1 by A3 in rule premise
	replace(S1,S2,H1,H2),
	replace(E1,E2,H2,H3),
	H3=..[P,S,E,V],
	S=..[F|Xs],
	newConstructor(F,K,FAuxK),
	append(Xs,[Z|Zs],Ws),			 % Remember the reused variables from B1
	SH=..[FAuxK|Ws],
	H=..[P,SH,E,V],
	[A3,A2]=[D1,B2].

newConstructor(F,K,FAuxK) :-
	atom_number(KAtom,K),
	atom_concat(F,'_aux_',FAux),
	atom_concat(FAux,KAtom,FAuxK).
	
%========================
		
evalConditions([],[]).
evalConditions([B|Bs],[B|Bs]) :-
	bigStepPred(B).
evalConditions([B|Bs],Bs1) :-
	otherPred(B),
	eval(B),
	evalConditions(Bs,Bs1).

eval(B) :-
	constraint(B),
	call(B).
eval(B) :-
	givenRule(_,B,B1),
	callPreds(B1).
	
callPreds([]).
callPreds([B|Bs]) :-
	eval(B),
	callPreds(Bs).
	
otherPred(A) :- 
	givenRule(_,otherPred(A),[]).
otherPred(B) :-
	constraint(B).
	
bigStepPred(A) :- 
	givenRule(_,bigStepPred(A),[]).

constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_==_).
constraint(_\=_).

replace(X,Y,X1,Y) :-
	var(X1),
	X1==X.
replace(X,_,X1,X1) :-
	var(X1),
	X1\==X.
replace(X,Y,T1,T2) :-
	nonvar(T1),
	T1=..[F|Xs],
	replaceArgs(X,Y,Xs,Ys),
	T2=..[F|Ys].

replaceArgs(_,_,[],[]).
replaceArgs(X,Y,[T1|Xs],[T2|Ys]) :-
	replace(X,Y,T1,T2),
	replaceArgs(X,Y,Xs,Ys).

init([
		(id(eq),opaque(eq)),
		(id(times),opaque(times)),
		(id(sub),opaque(sub)),
		(id(plus),opaque(plus))
	]).
	
	
test1(V) :-
	fac(E),
	init(Rho),
	run([eval(E,Rho,V)]).
	
test2(V) :-
	block(E),
	init(Rho),
	run([eval(E,Rho,V)]).
	
test3(V) :-
	simul(E),
	init(Rho),
	run([eval(E,Rho,V)]).

test4(V) :-
	evenodd(E),
	init(Rho),
	run([eval(E,Rho,V)]).

test5(V) :-
	init(Rho),
	run([eval(letrec(id(i),id(j),id(i)),Rho,V)]).
	
test6(V) :-
	if(E),
	init(Rho),
	run([eval(E,Rho,V)]).

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
	
givenRule(2,eval(number(A),B,int(A)),[]).
givenRule(3,eval(true,A,true),[]).
givenRule(4,eval(false,A,false),[]).
givenRule(5,eval(lambda(A,B),C,closure(A,B,C)),[]).
givenRule(6,eval(id(A),B,C),[val_of(B,A,C)]).
givenRule(7,eval(if(A,B,C),D,E),[eval(A,D,F),eval(choice(F,B,C),D,E)]).
givenRule(8,eval(choice(true,A,B),C,D),[eval(A,C,D)]).
givenRule(9,eval(choice(false,A,B),C,D),[eval(B,C,D)]).
givenRule(10,eval(mlpair(A,B),C,D),[eval(A,C,E),eval(mlpair9_1(B,E),C,D)]).
givenRule(11,eval(mlpair9_1(A,B),C,D),[eval(A,C,E),D=(B,E)]).
givenRule(12,eval(apply(A,B),C,D),[eval(A,C,E),eval(apply10_1(B,E),C,D)]).
givenRule(13,eval(apply10_1(A,B),C,D),[B=closure(E,F,G),eval(apply10_2(A,E,F,G),C,D)]).
givenRule(14,eval(apply10_2(A,B,C,D),E,F),[eval(A,E,G),eval(apply10_3(B,C,D,G),H,F)]).
givenRule(15,eval(apply10_3(A,B,C,D),E,F),[E=[(A,D)|C],eval(B,E,F)]).
givenRule(16,eval(apply(A,B),C,D),[eval(A,C,E),eval(apply11_1(B,E),C,D)]).
givenRule(17,eval(apply11_1(A,B),C,D),[B=opaque(E),eval(apply11_2(A,E),C,D)]).
givenRule(18,eval(apply11_2(A,B),C,D),[eval(A,C,E),opeval(B,E,D)]).
givenRule(19,eval(let(A,B,C),D,E),[eval(B,D,F),eval(let12_1(A,C,F),D,E)]).
givenRule(20,eval(let12_1(A,B,C),D,E),[F=[(A,C)|D],eval(B,F,E)]).
givenRule(21,eval(letrec(A,B,C),D,E),[F=[(A,G)|D],eval(letrec13_1(B,C,G),F,E)]).
givenRule(22,eval(letrec13_1(A,B,C),D,E),[eval(A,D,C),eval(B,D,E)]).
givenRule(23,val_of([(id(A),B)|C],A,B),[]).
givenRule(24,val_of([(id(A),B)|C],D,E),[A\==D,val_of(C,D,E)]).
givenRule(25,val_of([(pairpat(A,B),C,D)|E],F,G),[val_of([(A,C),(B,D)|E],F,G)]).
givenRule(26,opeval(eq,(A,B),true),[A==B]).
givenRule(27,opeval(eq,(A,B),false),[A\==B]).
givenRule(28,opeval(times,(int(A),int(B)),int(C)),[C is A*B]).
givenRule(29,opeval(sub,(int(A),int(B)),int(C)),[C is A-B]).
givenRule(30,opeval(plus,(int(A),int(B)),int(C)),[C is A+B]).
givenRule(31,bigStepPred(eval(A,B,C)),[]).
givenRule(32,otherPred(opeval(A,B,C)),[]).
givenRule(33,otherPred(val_of(A,B,C)),[]).

