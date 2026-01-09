:- module(candRFs,_).

:- use_module(library(ppl)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(common)).
:- use_module(chclibs(linearize)).


go(H,Cs,B,Rs) :-
    ppl_initialize,
	findRankingFunction(H,Cs,B,Rs),
	ppl_finalize.
	
findRankingFunction(H,Cs,B,RF) :-
	rankingFunctionSpace(H,Cs,B,H1,Point),
	ppl_Polyhedron_space_dimension(H1,N0),
	pickRankingFunction(H1,N0,Point,RF).
	
pickRankingFunction(H1,N0,_Point,RF) :-
	getConstraint(H1,Space),
	write('Space = '),write(Space),nl,
	candidateRFs(Space,N0,Point),
	instantiateRF(Point,N0,RF),
	!.
pickRankingFunction(_H1,N0,point(Q),RF) :-	% Use PPL soln if search does not find r.f.
	makeAffineFunction(Q,N0,RF).
	

% Find the space of linear ranking functions

rankingFunctionSpace(H,Cs,B,H1,RF0) :-
	functor(H,P,N),
	H=..[P|Ys0],
	B=..[P|Ys1],
	freshVars(Ys0,Ws0,ACs1,Cs),
	freshVars(Ys1,Ws1,ACs,ACs1),		% Ensure state vars are distinct
	numbervars((Ws0,Ws1,ACs),0,_),
	elimLocals(ACs,(Ws0,Ws1),Cs1),
	melt((Cs1,Ws0,Ws1),(Cs2,Zs,Zs1)),
	numbervars((Zs1,Zs,Cs2),0,_),		% Get vars in the order required by PPL
	makePolyhedron(Cs2,H0),
	N2 is 2*N,
	raiseDimension(H0,N2),
	ppl_all_affine_ranking_functions_MS_NNC_Polyhedron(H0,H1),
	ppl_one_affine_ranking_function_MS_NNC_Polyhedron(H0,RF0). 	% one r.f. backup soln.
	
freshVars([X|Xs],[Y|Ys],[X=Y|ACs],Cs) :-
	freshVars(Xs,Ys,ACs,Cs).
freshVars([],[],Cs,Cs).

elimLocals(B,(Xs,Xs1),B2) :-
	linearPart(B,BL),
	variables(BL,BVs),
	append(Xs,Xs1,Xs2),
	setdiff(BVs,Xs2,EVars),
	makePolyhedron(BL,H),
	project(H,EVars,H1), 	% note: eliminated variables have higher indexes 
	getConstraint(H1,B2).
	
raiseDimension(H,N) :-
	ppl_Polyhedron_space_dimension(H,Dim),
	J is N-Dim,
	ppl_Polyhedron_add_space_dimensions_and_embed(H,J).
	
linearPart(B,BL) :-
	melt(B,B1),
	linearize(B1,BL),
	B=B1.
	
variables('$VAR'(N),['$VAR'(N)]) :-
	!.
variables(T,Vs) :-
	T=..[_|Xs],
	argVars(Xs,Vs).
	
argVars([],[]).
argVars([X|Xs],Vs) :-
	variables(X,Vs1),
	argVars(Xs,Vs2),
	setunion(Vs1,Vs2,Vs).
	
candidateRFs(Space,N0,RFs) :-
	allocate(Space,0,N0,[],RFs).
	
% Successively pick a value for each dimension of the r.f. space
% Pick interior value near the boundary
% Pick non-zero values first
% Arbitrary choice of 3 values nearest to boundary, could be varied.
	
allocate(_,K,N0,RFs,RFs) :-
	K>=N0.
allocate(Space,K,N0,Js0,Js1) :-
	K<N0,
	K1 is K+1,
	bounds(Space,'$VAR'(K),L,U),
	pickVal(L,U,J),
	feasible(['$VAR'(K)=J|Space]),
	allocate(['$VAR'(K)=J|Space],K1,N0,['$VAR'(K)=J|Js0],Js1).

feasible(Cs) :-
	ppl_initialize,	% needed when backtracking past ppl_finalize
	makePolyhedron(Cs,H),
	\+ ppl_Polyhedron_is_empty(H),
	!.
	
pickVal(L,U,V) :-
	choiceList(L,U,Vs),
	member(V,Vs).
	
choiceList(false,false,[1,-1,0]) :-
	!.
choiceList(L,U,[L]) :-
	L==U,
	!.
choiceList(false,U,Vs) :-
	!,
	U1 is U-1,
	U2 is U-2,
	delayZero([U,U1,U2],Vs). 	% ensure 0 is the last choice.
choiceList(L,false,Vs) :-
	!,
	L1 is L+1,
	L2 is L+2,
	delayZero([L,L1,L2],Vs).	% ensure 0 is the last choice.
choiceList(L,U,Vs) :-
	choiceVals(L,U,3,Cs),		% Choose max 3 values
	delayZero(Cs,Vs). 			% ensure 0 is the last choice.
	
choiceVals(L,U,J,[L|Cs]) :-
	L=<U,
	J>0,
	L1 is L+1,
	J1 is J-1,
	choiceVals(L1,U,J1,Cs).
choiceVals(L,U,_,[]) :-
	L>U.
choiceVals(_,_,J,[]) :-			
	J=<0.
	
delayZero([0],[0]) :-
	!.
delayZero([X],[X]) :-
	!.
delayZero([0,X|Xs],[X|Vs]) :-
	!,
	delayZero([0|Xs],Vs).
delayZero([X|Xs],[X|Vs]) :-
	!,
	delayZero(Xs,Vs).

	
bounds(Space,V,L,U) :-
	makePolyhedron(Space,H0),
	(ppl_Polyhedron_maximize(H0,V,C1,C2,_TF1) ->
		U is floor(C1/C2); U=false),
	(ppl_Polyhedron_minimize(H0,V,C3,C4,_TF2) ->
		L is ceiling(C3/C4); L=false).
		
instantiateRF(Point,N,RF) :-
	rfTerms(Point,Ts),
	rfExpr(Ts,N,RF).
	
rfTerms(['$VAR'(_)=0|Ps],Es) :-
	!,
	rfTerms(Ps,Es).
rfTerms(['$VAR'(K)=V|Ps],[V*'$VAR'(K)|Es]) :-
	rfTerms(Ps,Es).
rfTerms([],[]).

rfExpr([V*'$VAR'(K),T|Es],N,RF+T1) :-
	(K is N-1 -> T1=V; T1=V*'$VAR'(K)),
	rfExpr([T|Es],N,RF).
rfExpr([V*'$VAR'(K)],N,T) :-
	(K is N-1 -> T=V; T=V*'$VAR'(K)).
	
		
% remove the k+1th variable if it is present since it represents the constant
makeAffineFunction(F1, K, F) :-
    VK = '$VAR'(K),
    subterm(F1, N*VK, F, N),
    !.
makeAffineFunction(F, _, F).

subterm(T,T,X,X) :-
	!.
subterm(T,R,T1,R1) :-
	T =.. [F|Xs],
	replace(Xs,R,Xs1,R1),
	T1 =.. [F|Xs1].

replace([],_,[],_).
replace([T|Xs],U,[T1|Ys],Y) :-
	subterm(T,U,T1,Y),
	replace(Xs,U,Ys,Y).
	
	
% Tests

test1(RF) :-
	go(p(A1,E,F,G),[1*A+ -1*B>=2,1*A+ -1*B+ -1*C>=1,1*B> -1,1*A+ -1*D>=2,1*C>0,1*D> -1,1*B+ -1*E= -1,1*C+ -1*F=1,1*D+ -1*G= -1,A1=A],p(A,B,C,D),RF).
	
test2(RF) :-
	go(p(A1,B1,C1,E),[1*A+ -1*D>=2,1*A+ -1*B>=1,1*A+ -1*B+ -1*C>=1,1*B> -1,1*C> -1,1*D> -1,1*D+ -1*E= -1,A1=A, B1=B, C1=C],p(A,B,C,D),RF).
	
test3(RF) :-
	go(p(A1,E,F,D1),[1*A+ -1*B+ -1*C+1*D>=0,1*A+ -1*B+ -1*C>=0,1*B> -1,1*C>=1,1*D> -1,1*B+ -1*E= -1,1*C+ -1*F=1, A1 = A, D1 = D],p(A,B,C,D),RF).
	
test4(RF) :-
	go(p(B),[B < 100, A = B+1],p(A),RF).

test5(RF) :-
	go(p(E,F,G,H),[-1*D+1*H>=1,1*D+ -1*H>= -2,1*C>=1,1*B>=1,1*D>=0,1*B+1*D+ -1*F+ -1*H= -1,1*C+ -2*D+ -1*G+2*H=3,1*A+ -1*E=0],p(A,B,C,D),RF).
	
test6(RF) :-
	go(p(A,B),[A>0,B>0,A1=A+1,B1=B-1],p(A1,B1),RF).	
