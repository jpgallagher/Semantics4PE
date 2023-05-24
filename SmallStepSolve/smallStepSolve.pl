:- module(smallStepSolve,_).

run([]).
run([A]) :-
	bigStepPred(A),
	smallStep(A,As1),
	run(As1).

smallStep(A,[]) :-
	leaf(A),
	clpClause(_,A,Bs),
	callPreds(Bs).
smallStep(bigstep(A,St0,St1),As1) :-
	nonLeaf(bigstep(A,St0,St1)),
	clpClause(K,bigstep(A,St0,St1),[B|Bs]),
	nextStep(B,K,Bs,As1).
	
nextStep(A,_,Bs,Bs) :-
	leaf(A),
	smallStep(A,[]).
nextStep(bigstep(A,St0,St1),K,Bs,[H]) :-
	nonLeaf(bigstep(A,St0,St1)),
	smallStep(bigstep(A,St0,St1),[B1]),
	tryFold(K,H,[B1|Bs]).
	
tryFold(_,B1,[B1]).
tryFold(K,H,[B1|Bs]) :-
	Bs \== [],
	clpClause(K,H,[B1|Bs]).
	
evalConditions([],[]).
evalConditions([B|Bs],[B|Bs]) :-
	bigStepPred(B).
evalConditions([B|Bs],Bs1) :-
	otherPred(B),
	callPred(B),
	evalConditions(Bs,Bs1).

callPred(B) :-
	constraint(B),
	call(B).
callPred(B) :-
	clpClause(_,B,B1),
	callPreds(B1).
	
callPreds([]).
callPreds([B|Bs]) :-
	callPred(B),
	callPreds(Bs).
	
bigStepPred(bigstep(_,_,_)).
bigStepPred(controlExpr(_,_,_,_)).
	
otherPred(B) :-
	functor(B,P,N),
	member(P/N,[eval/4,find/3,gt/3,lt/3,gte/3,lte/3,eq/3,negate/2,save/4]).
otherPred(B) :-
	constraint(B).
	
leaf(bigstep(skip,_,_)).
leaf(bigstep(asg(_,_),_,_)).
leaf(controlExpr(_,_,_,_)).

nonLeaf(bigstep(seq(_,_),_,_)).
nonLeaf(bigstep(ifthenelse(_,_,_),_,_)).
nonLeaf(bigstep(while(_,_),_,_)).
nonLeaf(bigstep(for(_,_,_,_),_,_)).

constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_\=_).





%% big step semantics

clpClause(1,eval(var(A),B,B,C),[find(B,A,C)]).
clpClause(2,eval(cns(nat(A)),B,B,A),[]).
clpClause(3,eval(add(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G+H]).
clpClause(4,eval(sub(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G-H]).
clpClause(5,eval(mul(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G*H]).
clpClause(6,eval(div(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G/H]).
clpClause(7,controlExpr(A>B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gt(G,H,E)]).
clpClause(8,controlExpr(A<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lt(G,H,E)]).
clpClause(9,controlExpr(A>=B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gte(G,H,E)]).
clpClause(10,controlExpr(A=<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lte(G,H,E)]).
clpClause(11,controlExpr(A==B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),eq(G,H,E)]).
clpClause(12,controlExpr(logicaland(A,B),C,D,E),[controlExpr(A,C,F,G),controlExpr(B,F,D,H),E is G/\H]).
clpClause(13,controlExpr(not(A),B,C,D),[controlExpr(A,B,C,E),negate(E,D)]).
clpClause(14,bigstep(skip,A,A),[]).
clpClause(15,bigstep(asg(var(A),B),C,D),[eval(B,C,E,F),save(A,F,E,D)]).
clpClause(16,bigstep(seq(A,B),C,D),[bigstep(A,C,E),bigstep(B,E,D)]).
clpClause(17,bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,1),bigstep(B,F,E)]).
clpClause(18,bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,0),bigstep(C,F,E)]).
clpClause(19,bigstep(while(A,B),C,D),[bigstep(ifthenelse(A,seq(B,while(A,B)),skip),C,D)]).
clpClause(20,bigstep(for(A,B,C,D),E,F),[bigstep(A,E,G),bigstep(while(B,seq(D,C)),G,F)]).
clpClause(21,find([(A,B)|C],A,B),[]).
clpClause(22,find([(A,B)|C],D,E),[D\==A,find(C,D,E)]).
clpClause(23,save(A,B,[(A,C)|D],[(A,E)|D]),[E is B]).
clpClause(24,save(A,B,[(C,D)|E],[(C,D)|F]),[A\==C,save(A,B,E,F)]).
clpClause(25,save(A,B,[],[(A,C)]),[C is B]).
clpClause(26,gt(A,B,1),[A>B]).
clpClause(27,gt(A,B,0),[A=<B]).
clpClause(28,lt(A,B,1),[A<B]).
clpClause(29,lt(A,B,0),[A>=B]).
clpClause(30,gte(A,B,1),[A>=B]).
clpClause(31,gte(A,B,0),[A<B]).
clpClause(32,lte(A,B,1),[A=<B]).
clpClause(33,lte(A,B,0),[A>B]).
clpClause(34,eq(A,B,1),[A==B]).
clpClause(35,eq(A,B,0),[A\==B]).
clpClause(36,negate(true,false),[]).
clpClause(37,negate(false,true),[]).

test(S) :-
	exCode(C),
	run([bigstep(C,[(x,5),(y,2),(z,3)],S)]).
	
%exCode(seq(seq(asg(var(x),add(var(z),var(y))),asg(var(z),var(x))),asg(var(y),cns(nat(7))))).
exCode(while(var(x)>cns(nat(0)),asg(var(x),sub(var(x),cns(nat(1)))))).

