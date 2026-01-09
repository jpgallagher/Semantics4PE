%:- module(signatures,[main/1]).
:- module(signatures,_).


:- use_module(library(lists)).
:- use_module(library(terms_vars)).

:- use_module(chclibs(load_simple)).
:- use_module(chclibs(common)).
:- use_module(chclibs(canonical)).

% Create a renaming and signature table using the Logen memo table

main([MemoF,OutF]) :-
	signatures(MemoF,OutF).
main([MemoF]) :-
	signatures(MemoF,user_output).
	
signatures(MemoF,OutF) :-
	load_file(MemoF),
	renamingTable(Ps),
	(OutF=user_output -> S=user_output; open(OutF,write,S)),
	writeSignatures(Ps,S),
	close(S).

renamingTable(Ps) :-
	findall(rename(A,B,Sig), (
			renaming(A,B,N,Q),
			makeSignature(Q,N,Sig)), 
		Ps).
	
renaming(A,B,N,Q) :-
	memo_entry(A,Q,S,Ts),
	functor(A,F,N),
	getArgName(S,P),
	term_variables(Ts,Ys),
	atom_concat(Q,Suff,F),
	atom_concat(P,Suff,P1),
	B =.. [P1|Ys],
	numbervars(Ys,0,_).
	
% memo table entries for big and small-step semantics respectively

memo_entry(A,bigstep,S,(St0,St1)) :-
	my_clause(memo_table(_,bigstep(S,_LR,St0,St1),A,_),[],_).
memo_entry(A,run,S,St0) :-
	my_clause(memo_table(_,run(S,St0),A,_),[],_).
memo_entry(A,observeStates,observeStates,(St0,St1)) :-
	my_clause(memo_table(_,observeStates(St0,St1),A,_),[],_).
memo_entry(A,observeState,observeState,St0) :-
	my_clause(memo_table(_,observeState(St0),A,_),[],_).
memo_entry(A,observeCost,observeCost,(Vs,C)) :-
	my_clause(memo_table(_,observeCost(Vs,C),A,_),[],_).


getArgName(call(F,_),F) :-
	!.
getArgName(decl(_,_):_,decl) :-
	!.
getArgName(_:_,seq) :-
	!.
getArgName(E^_,Name) :-
	!,
	getArgName(E,Name).
getArgName(star(_),while) :-
	!.
getArgName(_+_,ifthenelse) :-
	!.
getArgName(S,P) :-
	functor(S,P,_).
    
writeSignatures([P|Ps],S) :-
	writeq(S,P),
	write(S,'.'),
	nl(S),
	writeSignatures(Ps,S).
writeSignatures([],S) :-
	writeMain(S),
	nl(S).
	
writeMain(S) :-
	my_clause(memo_table(_,_,B,[_,entry]),[],_),
	functor(B,_,N),
	makeSignature(bigstep,N,Sig),
	numbervars(B,0,_),
	writeq(S,rename(B,B,Sig)),
	write(S,'.'),
	nl(S).

% For big-step predicates, first N//2 arguments are input, declared 'num'.  Rest are 'var'.
% For small-step predicates, all arguments are input, declared 'num'.
	
makeSignature(bigstep,N,Sig) :-
	M is N//2,
	makeList(M,num,Is),
	M1 is N-M,
	makeList(M1,var,Vs),
	append(Vs,Is,Ss),
	makeSig(Ss,Sig).
makeSignature(run,N,Sig) :-
	makeList(N,num,Is),
	makeSig(Is,Sig).
makeSignature(observeStates,N,Sig) :-
	makeList(N,num,Is),
	makeSig(Is,Sig).
makeSignature(observeState,N,Sig) :-
	makeList(N,num,Is),
	makeSig(Is,Sig).
makeSignature(observeCost,N,Sig) :-
	makeList(N,num,Is),
	makeSig(Is,Sig).
	
makeList(0,_,[]).
makeList(N,X,[X|Xs]) :-
	N>0,
	N1 is N-1,
	makeList(N1,X,Xs).
	
makeSig([],void) :-
	!.
makeSig([X],X) :-
	!.
makeSig([X|Xs],Ys*X) :-
	makeSig(Xs,Ys).


	
	
