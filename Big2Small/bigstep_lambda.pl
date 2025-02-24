% Call-by-value lambda-calculus, big-step semantics
% Fig 2, Vesely and Fisher 2019

bigstep(val(V),_Rho,V).
bigstep(var(X),Rho,V) :-
	find(Rho,X,V).
bigstep(lam(X,E),Rho,clo(X,E,Rho)).
bigstep(app(E1,E2),Rho,V) :-
	bigstep(E1,Rho,clo(X,E,Rho1)),
	bigstep(E2,Rho,V2),
	save(X,V2,Rho1,Rho2),
	bigstep(E,Rho2,V).

find([(X,N)|_St],X,N).
find([(Y,_N)|St],X,N) :-
	X \== Y,
	find(St,X,N).
	
save(X,V,[(X,_)|St],[(X,W)|St]) :-
	W = V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W = V.
	
% meta information for solver

bigStepPred(bigstep(_,_,_)).

otherPred(find(_,_,_)).
otherPred(save(_,_,_,_)).
