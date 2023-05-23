:- module(smallStepSolve,_).

run([]).
run([A|As]) :-
	smallStep(A,As,As1),
	run(As1).

smallStep(A,As,As) :-
	leaf(A),
	clpClause(A,Bs),
	callPreds(Bs).
smallStep(A,As,As1) :-
	nonLeaf(A),
	clpClause(A,Bs),
	evalConditions(Bs,Bs1),
	append(Bs1,As,[B|Bs2]),
	nextStep(B,Bs2,As1).
	
nextStep(A,As,As) :-
	leaf(A),
	smallStep(A,As,As).
nextStep(A,As,As1) :-
	nonLeaf(A),
	smallStep(A,As,As1).
	
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
	clpClause(B,B1),
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
constraint(true).
constraint(fail).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :-
	append(Xs,Ys,Zs).


%% big step semantics

clpClause(eval(var(A),B,B,C),[find(B,A,C)]).
clpClause(eval(cns(nat(A)),B,B,A),[]).
clpClause(eval(add(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G+H]).
clpClause(eval(sub(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G-H]).
clpClause(eval(mul(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G*H]).
clpClause(eval(div(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G/H]).
clpClause(controlExpr(A>B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gt(G,H,E)]).
clpClause(controlExpr(A<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lt(G,H,E)]).
clpClause(controlExpr(A>=B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gte(G,H,E)]).
clpClause(controlExpr(A=<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lte(G,H,E)]).
clpClause(controlExpr(A==B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),eq(G,H,E)]).
clpClause(controlExpr(logicaland(A,B),C,D,E),[controlExpr(A,C,F,G),controlExpr(B,F,D,H),E is G/\H]).
clpClause(controlExpr(not(A),B,C,D),[controlExpr(A,B,C,E),negate(E,D)]).
clpClause(bigstep(skip,A,A),[]).
clpClause(bigstep(asg(var(A),B),C,D),[eval(B,C,E,F),save(A,F,E,D)]).
clpClause(bigstep(seq(A,B),C,D),[bigstep(A,C,E),bigstep(B,E,D)]).
clpClause(bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,1),bigstep(B,F,E)]).
clpClause(bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,0),bigstep(C,F,E)]).
clpClause(bigstep(while(A,B),C,D),[bigstep(ifthenelse(A,seq(B,while(A,B)),skip),C,D)]).
clpClause(bigstep(for(A,B,C,D),E,F),[bigstep(A,E,G),bigstep(while(B,seq(D,C)),G,F)]).
clpClause(find([(A,B)|C],A,B),[]).
clpClause(find([(A,B)|C],D,E),[D\==A,find(C,D,E)]).
clpClause(save(A,B,[(A,C)|D],[(A,E)|D]),[E is B]).
clpClause(save(A,B,[(C,D)|E],[(C,D)|F]),[A\==C,save(A,B,E,F)]).
clpClause(save(A,B,[],[(A,C)]),[C is B]).
clpClause(gt(A,B,1),[A>B]).
clpClause(gt(A,B,0),[A=<B]).
clpClause(lt(A,B,1),[A<B]).
clpClause(lt(A,B,0),[A>=B]).
clpClause(gte(A,B,1),[A>=B]).
clpClause(gte(A,B,0),[A<B]).
clpClause(lte(A,B,1),[A=<B]).
clpClause(lte(A,B,0),[A>B]).
clpClause(eq(A,B,1),[A==B]).
clpClause(eq(A,B,0),[A\==B]).
clpClause(negate(true,false),[]).
clpClause(negate(false,true),[]).

test(S) :-
	exCode(C),
	run([bigstep(C,[(x,5),(y,2),(z,3)],S)]).
	
%exCode(seq(seq(asg(var(x),add(var(z),var(y))),asg(var(z),var(x))),asg(var(y),cns(nat(7))))).
exCode(while(var(x)>cns(nat(0)),asg(var(x),sub(var(x),cns(nat(1)))))).

