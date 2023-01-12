:- module(michelson,_).

run([],S,S).
run([Ins|Insns],S0,S) :- 
	ins(Ins,S0,S1), 
	run(Insns,S1,S).
				
ins(car,S0,S1) :-
	write('$car '),
	car(S0,S1).
ins((nil,_),S0,S1) :-
	write('$nil '),
	nil(S0,S1).
ins(pair,S0,S1) :-
	write('$pair '),
	pair(S0,S1).
ins(swap,S0,S1) :-
	write('$swap '),
	swap(S0,S1).
ins(cons,S0,S1) :-
	write('$cons '),
	cons(S0,S1).
ins((iter,Body),S0,S1) :-
	iter(Body,S0,S1).

car([(A,_)|S],[A|S]).

nil(S,[[]|S]).

swap([A,B|S],[B,A|S]).

pair([A,B|S],[(A,B)|S]).

cons([X,Xs|S],[[X|Xs]|S]).

iter(Body,[L|S0],S1) :-
	iter1(L,Body,S0,S1).
	
iter1([],_,S,S) :-
	write('$iffalse ').
iter1([X|Xs],Body,S0,S2) :-
	write('$iftrue '),
	run(Body,[X|S0],S1),
	iter1(Xs,Body,S1,S2).
	
	
	
test(S0,S1) :-
	run([car,(nil,int),swap,(iter,[cons]),(nil,operation),pair],[(S0,_)],S1).
	
