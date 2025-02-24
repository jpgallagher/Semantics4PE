:- module(michelson_big,_).

code((Ins;Insns),S0,S) :- 
	code(Ins,S0,S1), 
	code(Insns,S1,S).
code(car,S0,S1) :-
	write('$car '),
	car(S0,S1).
code((nil,T),S0,S1) :-
	write('$nil '),
	nil(T,S0,S1).
code(pair,S0,S1) :-
	write('$pair '),
	pair(S0,S1).
code(swap,S0,S1) :-
	write('$swap '),
	swap(S0,S1).
code(cons,S0,S1) :-
	write('$cons '),
	cons(S0,S1).
code((iter,_),[[]|S0],S0) :-
	write('$iffalse ').
code((iter,Body),[[X|Xs]|S0],S2) :-
	write('$iftrue '),
	code(Body,[X|S0],S1),
	code((iter,Body),[Xs|S1],S2).

car([(A,_)|S],[A|S]).

nil(_,S,[[]|S]).

swap([A,B|S],[B,A|S]).

pair([A,B|S],[(A,B)|S]).

cons([X,Xs|S],[[X|Xs]|S]).

% Test example
	
test(S0,S1) :-
	code((car;(nil,int);swap;(iter,cons);(nil,operation);pair),[(S0,_)],S1).
	
