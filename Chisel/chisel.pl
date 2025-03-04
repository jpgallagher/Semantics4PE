% Chisel semantics
% From Rosendahl and Kirkeby 
% and https://github.com/madsrosendahl/ChiselInterval

:-module(chisel,_).

% Interpreter

chisel(File,Max) :-
	open(File,read,S),
	read(S,D),
	close(S),
	interpreter(D,Max,_Env1).
		

interpreter(Design,Max,Env1) :-
	init(Design,Env0),
	iterate(Design,0,Max,Env0,Env1).
	
writeenv(J,Env) :-
	write('===================='),nl,
	write(J),write(': '),nl,
	write(Env),nl.
	
iterate(_,J,Max,Env,Env) :-
	J>=Max.
iterate(Design,J,Max,Env0,Env3) :-
	J<Max,
	writeenv(J,Env0),
	tt(Design,Env0,Env1),
	addenv(Env1,Env0,Env2),
	J1 is J+1,
	iterate(Design,J1,Max,Env2,Env3).
	
	
% Initialisation

init(design(Mods,Conns,MDcls),Env) :-
	mm(Mods,MDcls,Env1),
	cc(Conns,Env2),
	addenv(Env1,Env2,Env).
	
cc(Cons,Env) :-
	cc_n(Cons,[],Env,1).
	
cc_n([],Env,Env,_).
cc_n([conn(var(M1),var(Out1),var(M2),var(In2))|Cs],Env0, Env,I) :-
	addenv(Env0,[(I:ready,0)],Env1),
	addenv(Env1,[(I:valid,0)],Env2),
	addenv(Env2,[(I:data,0)],Env3),
	addenv(Env3,[(M1:Out1,I)],Env4),
	addenv(Env4,[(M2:In2,I)],Env5),
	I1 is I+1,
	cc_n(Cs,Env5,Env,I1).
	
mm([DL|MDcls],Mods,Env) :-
	mm(MDcls,Mods,Env1),
	mm_dl(DL,Mods,Env2),
	addenv(Env1,Env2,Env).
mm([],_,[]).

mm_dl(val(var(MVal),var(M)),Mods,Env) :-
	getMod(M,Mods,Mod),
	dd(Mod,MVal,Env).


	
dd(module(_M,Ds,_Ss),MVal,Env) :-
	dd_d(Ds,MVal,Env1),
	addenv([(MVal:state,1)],Env1,Env).

	
dd_d([vardecl(_Type,var(V),num(N))|Ds],MVal,Env) :-
	dd_d(Ds,MVal,Env1),
	addenv(Env1,[(MVal:V,N)],Env).
dd_d([arraydecl(_Type,N,var(V))|Ds],MVal,Env) :-
	dd_d(Ds,MVal,Env1),
	dd_array(N,0,V,MVal,Env1,Env).
dd_d([indecl(_V)|Ds],MVal,Env) :-
	dd_d(Ds,MVal,Env).
dd_d([outdecl(_V)|Ds],MVal,Env) :-
	dd_d(Ds,MVal,Env).
dd_d([],_,[]).
	
dd_array(N,J,_,_,Env,Env) :-
	J>=N.
dd_array(N,J,V,MVal,Env1,Env) :-
	J<N,
	addenv(Env1,[(MVal:array(V,J),0)],Env2),
	J1 is J+1,
	dd_array(N,J1,V,MVal,Env2,Env).
	

% Channel reset

	
reset_chans([conn(var(M1),var(Out1),_M2,_In2)|Chans],Env,Env1,Env3) :-
	getv(Env,(M1:Out1),I),
	getv(Env,(I:ready),R),
	set_chan(R,I,Env1,Env2),
	reset_chans(Chans,Env,Env2,Env3).
reset_chans([],_,Env,Env).

	
set_chan(0,I,Env1,Env2) :-
	addenv(Env1,[(I:ready,1),(I:valid,0)],Env2).
set_chan(R,_,Env,Env) :-
	R\==0.

% State transitions

tt(design(Mods,Conns,MDcls),Env0,Env) :-
	reset_chans(Conns,Env0,[],Env1),
	td(Mods,MDcls,Env0,Env2),
	addenv(Env2,Env1,Env).
	
td([val(var(MVal),var(M))|MDcls],Mods,Env0,Env) :-
	td(MDcls,Mods,Env0,Env1),
	getMod(M,Mods,Mod),
	t_mod(Mod,MVal,Env0,Env2),
	addenv(Env2,Env1,Env).
td([],_,_Env,[]).

	
t_mod(module(_M,_Ds,Ss),MVal,Env0,Env) :-
	t_ss(Ss,MVal,Env0,Env).

t_ss(Ss,MVal,Env0,Env) :-
	getv(Env0,MVal:state,N),
	getstate(Ss,N,S),
	t_state(S,MVal,Env0,Env).
	
t_state(state(_N,Cmd,Ss,Goto),MVal,Env,Env1) :-
	ee(Cmd,MVal,Env,V),
	state_when(V,Ss,Goto,MVal,Env,Env1).
	
state_when(V,_Ss,_Goto,_M,_Env,[]) :-
	V\==1.
state_when(1,Ss,Goto,MVal,Env0,Env3) :-
	tt_stmts(Ss,MVal,Env0,Env1),
	eg(Goto,MVal,Env0,Env2),
	addenv(Env1,Env2,Env3).
	
tt_stmts([Stm|Stmts],MVal,Env0,Env3) :-
	tt_stmt(Stm,MVal,Env0,Env1),
	tt_stmts(Stmts,MVal,Env0,Env2),
	addenv(Env1,Env2,Env3).
tt_stmts([],_MVal,_Env0,[]).
	
tt_stmt(asg(var(Lhs),Rhs),MVal,Env0,Env1) :-
	ee(Rhs,MVal,Env0,V),
	Env1=[(MVal:Lhs,V)].
tt_stmt(asgmem(array(var(A),J),Rhs),MVal,Env0,Env1) :-
	ee(Rhs,MVal,Env0,V),
	ee(J,MVal,Env0,VJ),
	Env1=[(MVal:array(A,VJ),V)].
tt_stmt(readmem(Lhs,array(var(A),J)),MVal,Env0,Env1) :-
	ee(J,MVal,Env0,VJ),
	getv(Env0,MVal:array(A,VJ),V),
	Env1=[(MVal:Lhs,V)].
tt_stmt(write(Ch,Rhs),MVal,Env0,Env1) :-
	getv(Env0,MVal:Ch,J),
	ee(Rhs,MVal,Env0,V),
	Env1=[(J:data,V),(J:valid,1)].
tt_stmt(read(Lhs,Ch),MVal,Env0,Env1) :-
	getv(Env0,MVal:Ch,J),
	getv(Env0,J:data,V),
	Env1=[(MVal:Lhs,V),(J:ready,0)].
	

% Expression evaluation

ee(num(N),_,_,N).
ee(var(X),MVal,Env,V) :-
	getv(Env,MVal:X,V).
ee(mux(E0,E1,E2),MVal,Env,V) :-
	ee(E0,MVal,Env,V1),
	ee_choice(V1,E1,E2,MVal,Env,V).
ee(Term,MVal,Env,V) :-
	Term=..[Op,E1,E2],
	ee(E1,MVal,Env,V1),
	ee(E2,MVal,Env,V2),
	binop(Op,V1,V2,V).
ee(ready(S),MVal,Env,V) :-
	getv(Env,MVal:S,Ch),
	getv(Env,Ch:ready,R1),
	getv(Env,Ch:valid,R2),
	ret_ready(R1,R2,V).
ee(valid(S),MVal,Env,V) :-
	getv(Env,MVal:S,Ch),
	getv(Env,Ch:ready,R1),
	getv(Env,Ch:valid,R2),
	ret_valid(R1,R2,V).
	
ret_ready(1,0,1).
ret_ready(1,1,0).
ret_ready(0,0,0).
ret_ready(0,1,0).
	
ret_valid(1,1,1).
ret_valid(1,0,0).
ret_valid(0,0,0).
ret_valid(0,1,0).

binop('+',V1,V2,V) :-
	V is V1+V2.
binop('-',V1,V2,V) :-
	V is V1-V2.
binop('*',V1,V2,V) :-
	V is V1*V2.
binop('/',V1,V2,V) :-
	V is V1/V2.
binop('mod',V1,V2,V) :-
	V is V1 mod V2.
binop('>=',V1,V2,V) :-
	gte(V1,V2,V).
binop('>',V1,V2,V) :-
	gt(V1,V2,V).
binop('>=',V1,V2,V) :-
	gte(V1,V2,V).
binop('<',V1,V2,V) :-
	lt(V1,V2,V).
binop('=<',V1,V2,V) :-
	lte(V1,V2,V).
binop('==',V1,V2,V) :-
	eq(V1,V2,V).
binop('!=',V1,V2,V) :-
	neq(V1,V2,V).
binop('|',V1,V2,V) :-
	logicalor(V1,V2,V).
binop('&',V1,V2,V) :-
	logicaland(V1,V2,V).
	
gt(X,Y,1) :-
	X>Y.
gt(X,Y,0) :-
	X=<Y.

gte(X,Y,1) :-
	X>=Y.
gte(X,Y,0) :-
	X<Y.
	
lt(X,Y,1) :-
	X<Y.
lt(X,Y,0) :-
	X=<Y.
	
lte(X,Y,1) :-
	X<Y.
lte(X,Y,0) :-
	X>=Y.
	
eq(X,Y,1) :-
	X==Y.
eq(X,Y,0) :-
	X\==Y.
	
neq(X,Y,1) :-
	X\==Y.
neq(X,Y,0) :-
	X==Y.
	
logicalor(0,0,0).
logicalor(1,0,1).
logicalor(0,1,1).
logicalor(1,1,1).

logicaland(0,0,0).
logicaland(1,0,0).
logicaland(0,1,0).
logicaland(1,1,1).
	
ee_choice(1,E1,_E2,MVal,Env,V) :-
	ee(E1,MVal,Env,V).
ee_choice(V1,_E1,E2,MVal,Env,V) :-
	V1\==1,
	ee(E2,MVal,Env,V).

eg(next(N),MVal,_,Env) :-
	Env=[(MVal:state,N)].
eg(mux(Exp,G1,G2),MVal,Env,Env1) :-
	ee(Exp,MVal,Env,V),
	goto_choice(V,G1,G2,MVal,Env,Env1).
	
goto_choice(1,G1,_,M,Env,Env1) :-
	eg(G1,M,Env,Env1).
goto_choice(V,_,G2,M,Env,Env1) :-
	V\==1,
	eg(G2,M,Env,Env1).
	
% utilities

addenv(Env,[],Env).
addenv(Env1,[(X,V)|Env2],Env) :-
	addentry(X,V,Env1,Env3),
	addenv(Env3,Env2,Env).
	
addentry(X,V,[],[(X,V)]).
addentry(X,_,[(X,W)|Env],[(X,W)|Env]).
addentry(X,V,[(Y,W)|Env],[(Y,W)|Env1]) :-
	X @> Y,
	addentry(X,V,Env,Env1).
addentry(X,V,[(Y,W)|Env],[(X,V),(Y,W)|Env]) :-
	X @< Y.
	
getv([(X,V)|_],X,V).
getv([(Y,_)|Env],X,V) :-
	Y\==X,
	getv(Env,X,V).
	
getstate([State|_Ss],N,State) :-
	State=state(N,_,_,_).
getstate([state(M,_,_,_)|Ss],N,State) :-
	M\==N,
	getstate(Ss,N,State).
	
getMod(M,[Mod|_Mods],Mod) :-
	Mod=module(var(M),_,_).
getMod(M,[Mod1|Mods],Mod) :-
	Mod1=module(var(M1),_,_),
	M1 \== M,
	getMod(M,Mods,Mod).
	
env_skeleton([],[]).
env_skeleton([(X,_)|E1],[(X,_)|E2]) :-
	env_skeleton(E1,E2).

	
% Tests

test :-
	prog(D),
	interpreter(D,5,_Env1).
test1 :-
	prog1(D),
	interpreter(D,17,_Env1).
test6 :-
	prog6(D),
	interpreter(D,46,_Env1).
test7 :-
	prog7(D),
	interpreter(D,35,_Env1).
test8 :-
	prog8(D),
	interpreter(D,21,_Env1).
test9 :-
	prog9(D),
	interpreter(D,21,_Env1).
	
prog(
  design(
     [val(var('random'),var('Random'))],
    [],
    [module(var('Random'),
       [vardecl(int,var('x'),num(0)), vardecl(int,var('i'),num(0)), 
       	arraydecl(int,11,var('a'))],
    [state(1,num(1),[asg(var('i'),num(0))],next(2)), 
     state(2,num(1),[asg(var('x'),(num(83) * var('i')) + num(52))],next(3)), 
     state(3,num(1),[asg(var('x'),var('x')  mod  num(101))],next(4)), 
     state(4,num(1),[asgmem(array(var(a),var('i')), var('x')),
      				 asg(var('i'),var('i') + num(1))],
      				 mux(var('i') < num(10),next(2),next(5))), 
     state(5,num(1),[],next(5))])]
 )      			
).

prog9(design( 
	[val(var('range'),var('Range'))], 
	[],
	[module(var('Range'),
		[vardecl(int,var('x'),num(0)),
		vardecl(int,var('i'),num(0)), 
		vardecl(int,var('j'),num(0)),
		arraydecl(int,6,var('a'))],
	[state(1,num(1),
		[asg(var('i'),num(0)),
		asg(var('j'),num(0)), 
		asg(var('x'),num(0))],next(2)),
	state(2,num(1),
		[asgmem(array(var(a),var('i')), var('i')),
		asg(var('i'),var('i') + num(1))],
			mux(var('i') < num(5),next(2),next(3))), 
	state(3,num(1),
		[asg(var('i'),var('i') - num(1))],next(4)), 
	state(4,num(1),
		[readmem(x,array(var(a),var('i'))),
		asg(var('i'),var('i') - num(1))],mux(var('i') > num(0),next(5),next(6))), 
		state(5,num(1),[asg(var('j'),var('j') + var('x'))],next(4)),
		state(6,num(1),[],next(6))])] 
	) 
).


prog8(design( 
	[val(var('sender'),var('Sender')),
	val(var('receiver'),var('Receiver'))], 
	[conn(var(sender),var(out),var(receiver),var(in))],
	[module(var('Sender'),
		[vardecl(int,var('i'),num(0)),
		outdecl(out)],
		[state(1,num(1),
			[asg(var('i'),num(0))],next(2)),
		state(2,ready(out),
			[write(out,var('i')), 
			asg(var('i'),var('i') + num(1))],
				mux(var('i') < num(5),next(2),next(3))),
		state(3,num(1),[],next(3))]),
	module(var('Receiver'),
		[vardecl(int,var('x'),num(0)),
		vardecl(int,var('j'),num(0)),
		indecl(in)],
		
		[state(1,num(1),
			[asg(var('j'),num(0))],next(2)),
		state(2,valid(in),
			[read(x,in)],next(3)),
		state(3,num(1),[asg(var('j'),var('j') + var('x'))],
			mux(var('x') < num(5),next(2),next(4))), 
		state(4,num(1),[],next(4))])] 
	) 
).

prog7(design( 
	[val(var('random'),
	var('Random'))], 
	[],
	[module(var('Random'),
		[vardecl(int,var('x'),num(0)),
		vardecl(int,var('i'),num(0)),
		arraydecl(int,11,var('a'))],
		[state(1,num(1),[asg(var('i'),num(0))],next(2)), 
		state(2,num(1),[asg(var('x'),(num(83) * var('i')) + num(52))],next(3)), 
		state(3,num(1),[asg(var('x'),var('x')  mod num(101))],next(4)), 
		state(4,num(1),[asgmem(array(var(a),var('i')),var('x')), 
						asg(var('i'),var('i') + num(1))],
						mux(var('i') < num(10),next(2),next(5))), 
		state(5,num(1),[],next(5))])] )
).

prog6(design( 
	[val(var('random'),
	var('Random6'))] , 
	[] ,
	[module(var('Random6'),
		[vardecl(int,var('x'),num(0)),
		vardecl(int,var('i'),num(0)), 
		vardecl(int,var('k'),num(0)),
		vardecl(int,var('r'),num(0)),
		arraydecl(int,11,var('a'))],
	[state(1,num(1),[asg(var('i'),num(0))],next(2)), 
	state(2,num(1),[asg(var('x'),(num(83) * var('i')) + num(52))],next(3)), 
	state(3,num(1),[asg(var('x'),var('x')  mod num(101))],next(4)),
	state(4,num(1),[asgmem(array(var(a),var('i')),var('x')), 
					asg(var('i'),var('i') + num(1))],
					    mux(var('i')<num(10),next(2),next(5))), 
	state(5,num(1),[asg(var('i'),num(1)),
					readmem(r,array(var(a),num(0)))],next(6)),
	state(6,num(1),[asg(var('k'),var('k') + var('r')),
					readmem(r,array(var(a),var('i'))), asg(var('i'),var('i') +num(1))],
						mux(var('i') < num(10),next(6),next(7))),
	state(7,num(1),[],next(7))])] ) 
).

prog1(design( 
	[val(var('prog'),var('Prog'))] , 
	[] ,
	[module(var('Prog'),
		[vardecl(int,var('i'),num(0)),
		vardecl(int,var('j'),num(0)),
		vardecl(int,var('k'),num(0))],
	[state(1,num(1),[asg(var('i'),num(15)),
					asg(var('j'),num(1))],next(2)), 
	state(2,num(1),[asg(var('i'),var('i') - num(1)), 
					asg(var('j'),var('j') + num(1))],next(3)),
	state(3,num(1),[],mux(var('i') > var('j'),next(2),next(4))),
	state(4,num(1),[asg(var('k'),num(1))],next(4))])] ) 
).
	