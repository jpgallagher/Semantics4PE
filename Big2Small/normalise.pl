:- module(normalise,_).

:- use_module(library(terms_vars)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(readprog)).
:- use_module(chclibs(common)).
:- use_module(chclibs(canonical)).


main([F,FOut]) :-
	normalise(F,Rs),
	open(FOut,write,S),
	writeRules(S,Rs),
	close(S).
main([F]) :-
	normalise(F,Rs),
	writeRules(user_output,Rs).
	
normalise(F,Rs) :-
	readprog(F,[predicates(Ps)|Cls]),
	sortClauses([predicates(Ps)|Cls],Ps,Procs),
	user_clauses(Procs,bigStepPred/1,BProc),
	user_clauses(Procs,otherPred/1,OProc),	
	transformRules(Cls,BProc,OProc,1,Rs,[]).
	
transformRules([clause((H:-B),Vs)|Cls],BProc,OProc,J,Rs0,Rs2) :-
	bigStepPred(H,BProc),
	!,
	conj2List(B,Bs),
	melt(clause((H:-Bs),Vs),clause((H1:-Bs1),Vs1)),
	constructor(H,F),
	transformRule((H1:-Bs1),J,1,F,BProc,OProc,Rs0,Rs1),
	varNames(Vs1),
	J1 is J+1,
	transformRules(Cls,BProc,OProc,J1,Rs1,Rs2).
transformRules([clause((H:-B),Vs)|Cls],BProc,OProc,J,[(H1:-Bs1)|Rs0],Rs1) :-
	conj2List(B,Bs),
	melt(clause((H:-Bs),Vs),clause((H1:-Bs1),Vs1)),
	varNames(Vs1),
	transformRules(Cls,BProc,OProc,J,Rs0,Rs1).
transformRules([],_,_,_,Rs,Rs).
	
transformRule((H:-Bs),_,_,_,BProc,OProc,[(H:-Bs)|Rs0],Rs0) :-
	noSplit(Bs,BProc,OProc).
transformRule((H:-Bs),J,K,F,BProc,OProc,[(H:-NewBody)|Rs0],Rs1) :-
	skipOtherPreds(Bs,BProc,OProc,Os,[D1|Ds]),
	envArgs(H,Bs,BProc,Es),	
	% Os=[]->length([D1|Ds])>2; length([D1|Ds])>1 
	newBody(Os,[D1|Ds],Es,H,J,K,F,B2,Rest,NewBody),
	K1 is K+1,
	transformRule((B2:-Rest),J,K1,F,BProc,OProc,Rs0,Rs1).
	
	
newBody([],[D1|Ds],Es,H,J,K,F,B2,Ds,[D1,B2]) :-
	!,
	newBigStep(Ds,D1,Es,H,J,K,F,B2).
newBody(Os,[D1|Ds],Es,H,J,K,F,B2,[D1|Ds],NewBody) :-
	newBigStep([D1|Ds],Os,Es,H,J,K,F,B2),
	append(Os,[B2],NewBody).
	
newBigStep(Ds,Pre,Es,H,J,K,F,B2) :-
	H=..[P,_,_,V],
	newConstructor(F,J,K,FK),
	inputEnvArg(Ds,Es,E1),
	syntaxArgs(H,Pre,Ds,E1,V,SXs),
	S1=..[FK|SXs],
	B2=..[P,S1,E1,V].
	
syntaxArgs(H,Pre,Ds,E1,V,SXs) :-
	varset(Ds,DXs),
	varset((H,Pre),HXs),
	setintersect(DXs,HXs,BXs),	% find values that are passed into Ds
	setdiff(BXs,[E1,V],SXs).
	
newConstructor(F,J,K,FJK) :-
	atom_number(KAtom,K),
	atom_number(JAtom,J),
	atom_concat(F,JAtom,FJ),
	atom_concat('_',KAtom,UK),
	atom_concat(FJ,UK,FJK).
	
inputEnvArg([D|_],Es,E1) :-
	varset(D,DXs),
	member(E1,Es),
	setintersect([E1],DXs,[_|_]),
	!.
inputEnvArg([_|Ds],Es,E1) :-
	inputEnvArg(Ds,Es,E1).
inputEnvArg([],[E|_],E).	% pick head env as default

envArgs(H,Bs,BProc,[E|Es]) :-
	arg(2,H,E), 	% 2nd arg is env
	bodyEnvArgs(Bs,BProc,Es).
	
bodyEnvArgs([B|Bs],BProc,[E|Es]) :-
	bigStepPred(B,BProc),
	!,
	arg(2,B,E),
	bodyEnvArgs(Bs,BProc,Es).
bodyEnvArgs([_|Bs],BProc,Es) :-
	bodyEnvArgs(Bs,BProc,Es).
bodyEnvArgs([],_,[]).
	
noSplit(Bs,BProc,OProc) :-
	skipOtherPreds(Bs,BProc,OProc,Os,Ds),
	length(Os,M),
	length(Ds,N),
	(M>0 -> N<2; N<3).
	
skipOtherPreds([],_,_,[],[]).
skipOtherPreds([B|Bs],BProc,_,[],[B|Bs]) :-
	bigStepPred(B,BProc).
skipOtherPreds([O|Bs],BProc,OProc,[O|Os],Ds) :-
	otherPred(O,OProc),
	skipOtherPreds(Bs,BProc,OProc,Os,Ds).
	
otherPred(A,OProc) :- 
	member((otherPred(A1):-true),OProc),
	melt(A1,A).
otherPred(B,_) :-
	constraint(B).
	
bigStepPred(A,BProc) :- 
	member((bigStepPred(A1):-true),BProc),
	melt(A1,A).
	
varNames([]).
varNames([X=X|Vs]) :-
	varNames(Vs).
	
constructor(H,F) :-
	arg(1,H,S),
	functor(S,F,_).

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
constraint(true).

writeRules(S,[(H:-B)|Rs]) :-
	write(S,H),
	writeBody(S,B),
	write(S,'.'),
	nl(S),
	writeRules(S,Rs).
writeRules(_,[]).


writeBody(_S,[]).
writeBody(_S,[true]) :-
	!.
writeBody(S,[B|Bs]) :-
	write(S,' :- '),
	nl(S),
	write(S,'    '),
	writeBodyAtoms(S,[B|Bs]).

writeBodyAtoms(S,[B]) :-
	!,
	write(S,B).
writeBodyAtoms(S,[B|Bs]) :-
	!,
	write(S,B),
	write(S,','),
	writeBodyAtoms(S,Bs).



