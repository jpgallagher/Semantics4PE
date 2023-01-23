:- module(michelson_full,_).

:- use_module(ciao_tezos(micheline/micheline_lexer)).
:- use_module(ciao_tezos(micheline/micheline_parser)).
:- use_module(ciao_tezos(parser/ast)).

:- use_module(library(write), [writeq/1]).
:- use_module(engine(io_basic), [nl/0]).

% See https://tezos.gitlab.io/active/michelson.html for Michelson language definition.

% Control structures

code(failwith, [_|_],['FAILED']).
code(_,['FAILED'],['FAILED']).
code({}, S,S).
code((Ins;Insns),S0,S) :- 
	code(Ins,S0,S1), 
	code(Insns,S1,S).
code((if,BT,_BF),[true|S0],S1) :-
	code(BT,S0,S1).
code((if,_BT,BF),[false|S0],S1) :-
	code(BF,S0,S1).	
code((loop,Body),[true|S0],S1) :-
	code((Body;(loop,Body)),S0,S1).
code((loop,_Body),[false|S0],S0).
code((loop_left,Body),[left(A)|S0],S1) :-
	code((Body;(loop_left,Body)),[A|S0],S1).
code((loop_left,_Body),[right(B)|S0],[B|S0]).
code((dip,Code),[X|S0],[X|S1]) :-
	code(Code,S0,S1).
code((dip,N,Code),[X|S0],[X|S1]) :-
	number(N),
	N>0,
	N1 is N-1,
	code((dip,N1,Code),S0,S1).
code((dip,0,Code),S0,S1) :-
	code(Code,S0,S1).
code(exec,[A,F|S0],[R|S0]) :-
	%write([A,F|S0]),nl,
	code(F,[A],[R]).
code(apply,[A,F|S0],[((push,_TypeA,A);pair;F)|S0]).	% TypeA is the type of A
	
% Stack operations
	
code(drop,[_|S0],S0).
code((drop,N),[_|S0],S1) :-
	number(N),
	N>0,
	N1 is N-1,
	code((drop,N1),S0,S1).
code((drop,0),S0,S0).
code(dup,[X|S],[X,X|S]).
code((dup,N),[X|S],[Y,X|S]) :-
	number(N),
	N>1,
	N1 is N-1,
	code((dup,N1),S,[Y|S]).
code((dup,1),[X|S],[X,X|S]).
code(swap,[X,Y|S],[Y,X|S]).
code((dig,N),[X|S],[Y,X|S1]) :-
	number(N),
	N>0,
	N1 is N-1,
	code((dig,N1),S,[Y|S1]).
code((dig,0),S,S).
code((dug,N),[X,Y|S0],[Y|S1]) :-
	number(N),
	N>0,
	N1 is N-1,
	code((dug,N1),[X|S0],S1).
code((dug,0),S,S).
code((push,_TX,X),S,[X|S]).				% TX is the type of X
code((lambda,_TA,_TB,Code),S,[Code|S]).	% Code has type TA->TB
code((lambda_rec,TA,TB,Code),S,[((lambda_rec,TA,TB,Code);Code)|S]). % Code has type TA->TB

% Generic Comparison

code(eq,[0|S],[true|S]).
code(eq,[V|S],[false|S]) :-
	number(V),
	V\==0.
code(neq,[0|S],[false|S]).
code(neq,[V|S],[true|S]) :-
	number(V),
	V\==0.
code(lt,[V|S],[true|S]) :-
	number(V),
	V<0.
code(lt,[V|S],[false|S]) :-
	number(V),
	V>=0.
code(gt,[V|S],[true|S]) :-
	number(V),
	V>0.
code(gt,[V|S],[false|S]) :-
	number(V),
	V=<0.
code(le,[V|S],[true|S]) :-
	number(V),
	V=<0.
code(le,[V|S],[false|S]) :-
	number(V),
	V>0.
code(ge,[V|S],[true|S]) :-
	number(V),
	V>=0.
code(ge,[V|S],[false|S]) :-
	number(V),
	V<0.
	
% Operations on Unit

code(unit,S,[unit|S]).
code(compare,[unit,unit|S],[0|S]).

% Operations on type never

% type never is used in type inference 

% Operations on booleans

code(or,[X,Y|S],[Z|S]) :-
	or(X,Y,Z).
code(and,[X,Y|S],[Z|S]) :-
	and(X,Y,Z).
code(xor,[X,Y|S],[Z|S]) :-
	xor(X,Y,Z).
code(not,[X|S],[Y|S]) :-
	negate(X,Y).
code(compare,[false,false|S],[0|S]).
code(compare,[false,true|S],[-1|S]).
code(compare,[true,false|S],[1|S]).
code(compare,[true,true|S],[0|S]).

% Operations on integers and natural numbers

code(neg,[X|S],[Y|S]) :-
	number(X),
	Y is -X.
code(abs,[X|S],[Y|S]) :-
	number(X),
	Y is abs(X).
code(isnat,[X|S],[some(X)|S]) :-
	integer(X),
	X>=0.
code(isnat,[X|S],[none|S]) :-
	integer(X),
	X<0.
code(int,[X|S],[X|S]).		% Coerce nat to int
code(add,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X+Y.
code(sub,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X-Y.
code(mul,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X*Y.
code(ediv,[_X,0|S],[none|S]).
code(ediv,[X,Y|S],[some((U,V))|S]) :-
	number(X),
	number(Y),
	U is X // Y,
	V is mod(X,Y).
code(or,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X \/ Y.
code(and,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X /\ Y.
code(xor,[X,Y|S],[Z|S]) :-
	number(X),
	number(Y),
	Z is X # Y.
code(not,[X|S],[Y|S]) :-
	number(X),
	number(Y),
	Y is \X.
code(lsl,[X,Sh|S],[Z|S]) :-
	number(X),
	number(Sh),
	Sh =< 256,
	Z is X<<Sh.
code(lsl,[X,Sh|_S],['FAILED']) :-
	number(X),
	number(Sh),
	Sh > 256.
code(lsr,[X,Sh|S],[Z|S]) :-
	number(X),
	number(Sh),
	Sh =< 256,
	Z is X>>Sh.
code(lsr,[X,Sh|_S],['FAILED']) :-
	number(X),
	number(Sh),
	Sh > 256.
code(compare,[X,Y|S],[-1|S]) :-
	number(X),
	number(Y),
	X<Y.
code(compare,[X,Y|S],[0|S]) :-
	number(X),
	number(Y),
	X is Y.
code(compare,[X,Y|S],[1|S]) :-
	number(X),
	number(Y),
	X>Y.
	
% Operations on strings

code(concat,[X,Y|S],[Z|S]) :-
	atom(X),
	atom(Y),
	atom_concat(X,Y,Z).

% Operations on pairs and right combs

code(pair,[X,Y|S0],[(X,Y)|S0]).
code((pair,2),[X,Y|S0],[(X,Y)|S0]).
code((pair,N),[X|S0],[(X,Y)|S1]) :-
	N>2,
	N1 is N-1,
	code((pair,N1),S0,[Y|S1]).
code(unpair,[(X,Y)|S],[X,Y|S]).
code((unpair,2),[(X,Y)|S0],[X,Y|S0]).
code((unpair,N),[(X,Y)|S0],[X|S1]) :-
	N>2,
	N1 is N-1,
	code((unpair,N1),[Y|S0],S1).
code(car,[(X,_)|S0],[X|S0]).
code(cdr,[(_,Y)|S0],[Y|S0]).
code((get,0),[X|S],[X|S]).
code((get,1),[(X,_)|S],[X|S]).
code((get,K),[(_,Y)|S0],S1) :-
	K>1,
	K1 is K-2,
	code((get,K1),[Y|S0],S1).
code((update,0),[X,_|S],[X|S]).
code((update,1),[(X2,(_,Y))|S],[(X2,Y)|S]).
code((update,K),[Z,(X,Y1)|S0],[(X,Y2)|S0]) :-
	K>1,
	K1 is K-2,
	code((update,K1),[Z,Y1|S0],[Y2|S0]).
code(compare,[(SA,_SB),(TA,_TB)|S],[-1|S]) :-
	code(compare,[SA,TA|S],[-1|S]).
code(compare,[(SA,_SB),(TA,_TB)|S],[1|S]) :-
	code(compare,[SA,TA|S],[1|S]).
code(compare,[(SA,SB),(TA,TB)|S],[R|S]) :-
	code(compare,[SA,TA|S],[0|S]),
	code(compare,[SB,TB|S],[R|S]).
	
% Operations on sets

code((empty_set, _T),S,[[]|S]).
code(mem, [_X,[]|S], [false|S]).
code(mem, [X,[Y|Ys]|S], [R|S]) :-
	code(compare,[X,Y],[1]),
	code(mem,[X,Ys|S], [R|S]).
code(mem, [X,[Y|_Ys]|S], [true|S]) :-
	code(compare,[X,Y],[0]).
code(mem, [X,[Y|_Ys]|S], [false|S]) :-
	code(compare,[X,Y],[-1]).
code(update, [_X,false,[]|S], [[]|S]).
code(update, [X,true,[]|S], [[X]|S]).
code(update, [X,V,[Y|Ys]|S], [[Y|Ys1]|S]) :-
	code(compare,[X,Y],[1]),
	code(update, [X,V,Ys|S], [Ys1|S]).
code(update, [X,false,[Y|Ys]|S], [Ys|S]) :-
	code(compare,[X,Y],[0]).
code(update, [X,true,[Y|Ys]|S], [[Y|Ys]|S]) :-
	code(compare,[X,Y],[0]).
code(update, [X,false,[Y|Ys]|S], [[Y|Ys]|S]) :-
	code(compare,[X,Y],[-1]).
code(update, [X,true,[Y|Ys]|S], [[X,Y|Ys]|S]) :-
	code(compare,[X,Y],[-1]).
	
% Operations on maps

code((empty_map,_K,_V),S,[[]|S]).
code(get, [_X,[]|S], [none|S]).
code(get, [X,[element(K,_V)|Ys]|S], [R|S]) :-
	code(compare,[X,K],[1]),
	code(get,[X,Ys|S], [R|S]).
code(get, [X,[element(K,V)|_Ys]|S], [some(V)|S]) :-
	code(compare,[X,K],[0]).
code(get, [X,[element(K,_V)|_Ys]|S], [none|S]) :-
	code(compare,[X,K],[-1]).
code(mem, [_X,[]|S], [false|S]).
code(mem, [X,[element(K,_V)|Ys]|S], [R|S]) :-
	code(compare,[X,K],[1]),
	code(mem,[X,Ys|S], [R|S]).
code(mem, [X,[element(K,_V)|_Ys]|S], [true|S]) :-
	code(compare,[X,K],[0]).
code(mem, [X,[element(K,_V)|_Ys]|S], [false|S]) :-
	code(compare,[X,K],[-1]).
code(update, [_X,none,[]|S], [[]|S]).
code(update, [X,some(Y),[]|S], [[element(X,Y)]|S]).
code(update, [X,Opt_y,[element(K,V)|Tl]|S], [[element(K,V)|Tl1]|S]) :-
	code(compare,[X,K],[1]),
	code(update, [X,Opt_y,Tl|S], [Tl1|S]).
code(update, [X,none,[element(K,_V)|Ys]|S], [Ys|S]) :-
	code(compare,[X,K],[0]).
code(update, [X,some(V),[element(K,_V)|Ys]|S], [[element(K,V)|Ys]|S]) :-
	code(compare,[X,K],[0]).
code(update, [X,none,[element(K,V)|Ys]|S], [[element(K,V)|Ys]|S]) :-
	code(compare,[X,K],[-1]).
code(update, [X,some(Y),[element(K,V)|Ys]|S], [[element(X,Y),element(K,V),Y|Ys]|S]) :-
	code(compare,[X,K],[-1]).
code(get_and_update, [_X,none,[]|S], [none,[]|S]).
code(get_and_update, [X,some(Y),[]|S], [none,[element(X,Y)]|S]).
code(get_and_update, [X,Opt,[element(K,V)|Ys]|S], [R,[element(K,V)|Ys1]|S]) :-
	code(compare,[X,K],[1]),
	code(get_and_update, [X,Opt,Ys|S], [R,Ys1|S]).
code(get_and_update, [X,none,[element(K,V)|Ys]|S], [some(V),Ys|S]) :-
	code(compare,[X,K],[0]).
code(get_and_update, [X,some(V),[element(K,W)|Ys]|S], [some(W),[element(K,V)|Ys]|S]) :-
	code(get_and_update,[X,K],[0]).
code(get_and_update, [X,none,[element(K,V)|Ys]|S], [none,[element(K,V)|Ys]|S]) :-
	code(compare,[X,K],[-1]).
code(get_and_update, [X,some(Y),[element(K,V)|Ys]|S], [[none,element(X,Y),element(K,V),Y|Ys]|S]) :-
	code(compare,[X,K],[-1]).
code((map,_),[[]|S0],[[]|S0]).
code((map,Body),[[element(K,V)|Xs]|S0],[[element(K,W)|Ys]|S2]) :-
	code(Body,[(K,V)|S0],[W|S1]),
	code((map,Body),[Xs|S1],[Ys|S2]).

% Operations on big_maps

code((empty_big_map,_K,_V),S,[[]|S]).

% Operations on optional values

code(some,[V|S],[some(V)|S]).
code(none,S,[none|S]).
code((if_none,BT,_BF),[none|S],S1) :-
	code(BT,S,S1).
code((if_none,_BT,BF),[some(A)|S],S1) :-
	code(BF,[A|S],S1).
code(compare,[none,none|S],[0|S]).
code(compare,[none,some(_)|S],[-1|S]).
code(compare,[some(_),none|S],[1|S]).
code(compare,[some(A),some(B)|S],S1) :-
	code(compare,[A,B|S],S1).
code((map,_Body),[none|S], [none|S]).
code((map,Body),[some(A)|S0],[some(B)|S1]) :-
  code(Body,[A|S0],[B|S1]).

% Operations on unions

code(left,[V|S],[left(V)|S]).
code(right,[V|S],[right(V)|S]).
code((if_left,BT,_BF),[left(A)|S],S1) :-
	code(BT,[A|S],S1).
code((if_left,_BT,BF),[right(B)|S],S1) :-
	code(BF,[B|S],S1).
code(compare,[left(A),left(B)|S],S1) :-
	code(compare,[A,B|S],S1).
code(compare,[left(_),right(_)|S],[-1|S]).
code(compare,[right(_),left(_)|S],[1|S]).
code(compare,[right(A),right(B)|S],S1) :-
	code(compare,[A,B|S],S1).

% Operations on lists

code(cons,[X,Xs|S],[[X|Xs]|S]).
code((nil,_T),S,[[]|S]).
code((if_cons,BT,_BF),[[X|Xs]|S],S1) :-
	code(BT,[X,Xs|S],S1).
code((if_cons,_BT,BF),[[]|S],S1) :-
	code(BF,S,S1).

code((map,_),[[]|S0],[[]|S0]).
code((map,Body),[[X|Xs]|S0],[[Y|Ys]|S2]) :-
	code(Body,[X|S0],[Y|S1]),
	code((map,Body),[Xs|S1],[Ys|S2]).
code(size,[[]|S],[0|S]).
code(size,[[_|Xs]|S],[N|S]) :-
	code(size,[Xs|S],[N1|S]),
	N is N1+1.
code((iter,_),[[]|S0],S0).
code((iter,Body),[[X|Xs]|S0],S2) :-
	code(Body,[X|S0],S1),
	code((iter,Body),[Xs|S1],S2).
	
% Domain-specific operations.  Not yet implemented
% Operations on timestamps
% Operations on mutez
% Operations on contracts
% Special operations
% Operations on bytes
% Cryptographic primitives
% BLS12-381 primitives
% Sapling operations
% Operations on tickets
% Operations on timelock
% Events
% Operations on views

% Macros

% Compare

code(CMP_Op, S0,S1) :-
	atom(CMP_Op),
	atom_concat(cmp,Op,CMP_Op),
	member(Op,[eq,neq,lt,gt,le,ge]),
	code((compare;Op),S0,S1).
code((IF_Op,BT,BF), S0,S1) :-
	atom(IF_Op),
	atom_concat(if,Op,IF_Op),
	member(Op,[eq,neq,lt,gt,le,ge]),
	code((Op;(if,BT,BF)),S0,S1).
code((IFCMP_Op,BT,BF), S0,S1) :-
	atom(IFCMP_Op),
	atom_concat(ifcmp,Op,IFCMP_Op),
	member(Op,[eq,neq,lt,gt,le,ge]),
	code((compare;Op;(if,BT,BF)),S0,S1).
	
% Fail

code(fail,S0,S1) :-
	code((unit;failwith),S0,S1).
	
% Assertion macros

code(assert,S0,S1) :-
	code((if,{},fail),S0,S1).
code(Assert_Op, S0,S1) :-
	atom(Assert_Op),
	atom_concat('assert_',Op,Assert_Op),
	member(Op,[eq,neq,lt,gt,le,ge]),
	atom_concat(if,Op,IF_Op),
	code((IF_Op,{},fail),S0,S1).
code(Assert_Cmp_Op, S0,S1) :-
	atom(Assert_Cmp_Op),
	atom_concat(assert_cmp,Op,Assert_Cmp_Op),
	member(Op,[eq,neq,lt,gt,le,ge]),
	atom_concat(ifcmp,Op,IFCMP_Op),
	code((IFCMP_Op,{},fail),S0,S1).
code(assert_none,S0,S1) :-
	code((if_none,{},fail),S0,S1).
code((assert_some,'@x'),S0,S1) :-
	code((if_none,fail,(rename,'@x')),S0,S1).
code(assert_some,S0,S1) :-
	code((if_none,fail,{}),S0,S1).
code((assert_left,'@x'),S0,S1) :-
	code((if_left,(rename,'@x'),fail),S0,S1).
code(assert_left,S0,S1) :-
	code((if_left,{},fail),S0,S1).
code((assert_right,'@x'),S0,S1) :-
	code((if_left,fail,(rename,'@x')),S0,S1).
code(assert_right,S0,S1) :-
	code((if_left,fail,{}),S0,S1).
	
% Syntactic Conveniences

% Pair expressions.
% Grammar P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R

code(PairExp,S0,S1) :-
	pairMacro(PairExp,Struct),
	pairCode(Struct,PairCode),
	code(PairCode,S0,S1).
	
% Unpair expressions.
% Grammar UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R

code(UnpairExp,S0,S1) :-
	unpairMacro(UnpairExp,Struct),
	unpairCode(Struct,UnpairCode),
	code(UnpairCode,S0,S1).
	
% CAR/CDR expressions

code(CadrExpr,S0,S1) :-
	cadrMacro(CadrExpr,Struct),
	cadrCode(Struct,CadrCode),
	code(CadrCode,S0,S1).
code((car,K),S0,S1) :-
	N is 2*K+1,
	code((get,N),S0,S1).
code((cdr,K),S0,S1) :-
	N is 2*K,
	code((get,N),S0,S1).
	
code((if_some,BT,BF),S0,S1) :-
	code((if_none,BF,BT),S0,S1).
code((if_right,BT,BF),S0,S1) :-
	code((if_left,BF,BT),S0,S1).
	
code(set_car,S0,S1) :-
	code((cdr;swap;pair),S0,S1).
code(set_cdr,S0,S1) :-
	code((car;pair),S0,S1).
	
code(SetCadrExp,S0,S1) :-
	setCadrMacro(SetCadrExp,Struct),
	setCadrCode(Struct,SetCadrCode),
	code(SetCadrCode,S0,S1).
	
code((map_car,Code),S0,S1) :-
	code((dup;cdr;(dip,(car;Code));swap;pair),S0,S1).
code((map_cdr,Code),S0,S1) :-
	code((dup;cdr;Code;swap;car;pair),S0,S1).
	
code((MapCadrExp,Code),S0,S1) :-
	mapCadrMacro(MapCadrExp,Struct),
	mapCadrCode(Struct,Code,MapCadrCode),
	code(MapCadrCode,S0,S1).
		
% Macro expansions

pairMacro(PairExp,p+LCode+RCode+r) :-
	atom(PairExp),
	PairExp \== pair,
	atom_concat(p,LeftRight,PairExp),
	atom_concat(Left,RightR,LeftRight),
	left(Left,LCode),
	atom_concat(Right,r,RightR),
	right(Right,RCode).
	
unpairMacro(UnpairExp,un+(p+LCode+RCode+r)) :-
	atom(UnpairExp),
	UnpairExp \== unpair,
	atom_concat(unp,LeftRight,UnpairExp),
	atom_concat(Left,RightR,LeftRight),
	left(Left,LCode),
	atom_concat(Right,r,RightR),
	right(Right,RCode).
	
left(a,a).
left(LeftExp,p+LCode+RCode) :-
	atom(LeftExp),
	atom_concat(p,LeftRight,LeftExp),
	atom_concat(Left,Right,LeftRight),
	Left\=='',
	left(Left,LCode),
	right(Right,RCode).
	
right(i,i).
right(RightExp,p+LCode+RCode) :-
	atom(RightExp),
	atom_concat(p,LeftRight,RightExp),
	atom_concat(Left,Right,LeftRight),
	left(Left,LCode),
	Left\=='',
	right(Right,RCode).
	
cadrMacro(CadrExp,c+CadrCode+r) :-
	atom(CadrExp),
	CadrExp\==car, CadrExp\==cdr,
	atom_concat(c,E,CadrExp),
	atom_concat(ADExpr,r,E),
	unwind(ADExpr,CadrCode).
	
setCadrMacro(SetCadrExp,'set_'+(c+SetCadrCode+r)) :-
	atom(SetCadrExp),
	SetCadrExp\==set_car, SetCadrExp\==set_cdr,
	atom_concat(set_c,E,SetCadrExp),
	atom_concat(ADExpr,r,E),
	unwind(ADExpr,SetCadrCode).
	
mapCadrMacro(MapCadrExp,'map_'+(c+MapCadrCode+r)) :-
	atom(MapCadrExp),
	MapCadrExp\==map_car, MapCadrExp\==map_cdr,
	atom_concat(map_c,E,MapCadrExp),
	atom_concat(ADExpr,r,E),
	unwind(ADExpr,MapCadrCode).
	
unwind(a,a).
unwind(d,d).
unwind(ADExpr,a+CadrCode) :-
	atom_concat(a,Rest,ADExpr),
	unwind(Rest,CadrCode).
unwind(ADExpr,d+CadrCode) :-
	atom_concat(d,Rest,ADExpr),
	unwind(Rest,CadrCode).
	
% Generate code from parsed macro expressions

pairCode(p+a+i+r,pair).
pairCode(p+a+R+r, (dip,(RightR;pair))) :-
	R\==i,
	pairCode(R+r,RightR).
pairCode(p+L+i+r, (LeftR;pair)) :-
	L\==a,
	pairCode(L+r,LeftR).
pairCode(p+L+R+r, (LeftR;(dip,(RightR;pair))) ) :-
	L\==a,R\==i,
	pairCode(L+r,LeftR),
	pairCode(R+r,RightR).
	
unpairCode(un+(p+a+i+r),unpair).
unpairCode(un+(p+a+R+r), (unpair; dip,UnRightR)) :-
	R\==i,
	unpairCode(un+(R+r),UnRightR).
unpairCode(un+(p+L+i+r), (unpair; UnLeftR)) :-
	L\==a,
	unpairCode(un+(L+r),UnLeftR).
unpairCode(un+(p+L+R+r), (unpair;(dip,(UnRightR;UnLeftR))) ) :-
	L\==a,R\==i,
	unpairCode(un+(L+r),UnLeftR),
	unpairCode(un+(R+r),UnRightR).

cadrCode(c+a+r,car).
cadrCode(c+d+r,cdr).
cadrCode(c+(a+R)+r,(car;CRR)) :-
	cadrCode(c+R+r,CRR).
cadrCode(c+(d+R)+r,(cdr;CRR)) :-
	cadrCode(c+R+r,CRR).
	
setCadrCode('set_'+(c+a+r),set_car).
setCadrCode('set_'+(c+d+r),set_cdr).
setCadrCode('set_'+(c+(a+R)+r),(dup;(dip,(car;CRR));cdr;swap;pair)) :-
	setCadrCode('set_'+(c+R+r),CRR).
setCadrCode('set_'+(c+(d+R)+r),(dup;(dip,(cdr;CRR));car;pair)) :-
	setCadrCode('set_'+(c+R+r),CRR).
	
mapCadrCode('map_'+(c+a+r),Code,(map_car,Code)).
mapCadrCode('map_'+(c+d+r),Code,(map_cdr,Code)).
mapCadrCode('map_'+(c+(a+R)+r),Code,(dup;(dip,(car;MRR));cdr;swap;pair)) :-
	mapCadrCode('map_'+(c+R+r),Code,MRR).
mapCadrCode('map_'+(c+(d+R)+r),Code,(dup;(dip,(cdr;MRR));car;pair)) :-
	mapCadrCode('map_'+(c+R+r),Code,MRR).

% Boolean functions

and(true,true,true).
and(true,false,false).
and(false,true,false).
and(false,false,false).

or(true,true,true).
or(true,false,true).
or(false,true,true).
or(false,false,false).

xor(true,true,false).
xor(true,false,true).
xor(false,true,true).
xor(false,false,false).

negate(true,false).
negate(false,true).

% Execute Michelson source code file F with input Arg

run(F,Arg,Storage,Result) :-
	tz_tokenize(F,T1),
	parse_toplevel(T1,T2),
	ast(T2,AST),
	code(AST,[(Arg,Storage)],Result).

% Test example 1:  reverse a list.  test1([1,2,3],S)  --> S = [([],[3,2,1])]
	
test1(S0,S1) :-
	code((car;(nil,int);swap;(iter,cons);(nil,operation);pair),[(S0,_)],S1).
	
/* parameter (list string) ;
storage (list string) ;
code { CAR ;
       NIL string ;
       SWAP ;
       PUSH bool True ;
       LOOP { IF_CONS { CONS ; SWAP ; PUSH bool True } { NIL string ; PUSH bool False } } ;
       DROP ;
       NIL operation ;
       PAIR }
*/

% Test example 2:   test2(['hi', 'friend'],S)  --> [([],[hi,friend])]
test2(S0,S1) :-
	code(
	   (car;
       (nil,string);
       swap;
       (push, bool, true);
       (loop, (if_cons, (cons; swap; (push, bool, true)), ((nil,string); (push, bool,false)));
       drop;
       (nil, operation);
       pair)),[(S0,_)],S1).
       
/*
parameter (pair int (list int)) ; 
storage int ;
code {
CAR ;
UNPAIR ;
DUP ;
SUB ;
DIIP { PUSH int 0 } ;
IFNEQ { ITER { ADD } } { DROP } ; NIL operation ;  % should be IFEQ?
PAIR }
*/

% Example. test3((3,[5,6,7]),S) --> S=[([],18)]
% Modified slightly from Listing 1.2 in the SAS paper, 
% so that it now adds the elements of the list.
% The first input arg seems irrelevant since after dup; sub 
% the top of stack is 0, whatever the value of the parameter.

test3(S0,S1) :-
	code(
	   (car;
       unpair;
       dup;
       sub;
       (dip, 2, (push,int,0));
       eq; (if,(iter,add),drop); (nil, operation);
       pair),[(S0,_)],S1).
       
% factorial.  Slightly modified from https://tezos.gitlab.io/active/michelson.html

test4(S0,S1) :-
	code(
	   (car;
	   lambda_rec,int,int,
	   		(dup,2;
	   		eq;
	   		if,
	   			(push,int,1),
	   			(dup;push,int,1;dup,4;sub;exec;dup,3;mul);
	   		dip,drop,2);
	   swap;
	   exec;
	   nil,operation;
	   pair),
	   [(S0,_)],S1).
	   
	
