% Chisel semantics
% From Rosendahl and Kirkeby 
% and https://github.com/madsrosendahl/ChiselInterval
% With structured environment (CEnv, [ME1,....,MEn])
% Treatment of channel reset slightly modified to assist specialisation

:-module(chisel,_).

% Interpreter

chisel(File,Max) :-
	open(File,read,S),
	read(S,D),
	close(S),
	interpreter(D,Max).
		

interpreter(Design,Max) :-
	init(Design,Env0),
	iterate(Design,0,Max,Env0).
	

	
iterate(_,J,Max,_Env) :-
	J>=Max.
iterate(design(Mods,Conns,MDcls),J,Max,(CEnv0,MEnvs0)) :-
	J<Max,
	writeenv(J,(CEnv0,MEnvs0)),
	env_skeleton((CEnv0,MEnvs0),(CEnv1,MEnvs1)),
	tt(design(Mods,Conns,MDcls),(CEnv0,MEnvs0),(CEnv1,MEnvs1)),
	J1 is J+1,
	iterate(design(Mods,Conns,MDcls),J1,Max,(CEnv1,MEnvs1)).
	
% Initialisation

init(design(Mods,Conns,MDcls),(CEnv,MEnv)) :-
	mm(Mods,MDcls,MEnv),
	cc(Conns,CEnv).
	
cc(Cons,Env) :-
	cc_n(Cons,[],Env,1).
	
cc_n([],Env,Env,_).
cc_n([conn(var(M1),var(Out1),var(M2),var(In2))|Cs],Env0, Env,I) :-
	addenv(Env0,[(I:ready,0),(I:valid,0),(I:data,0),(M1:Out1,I),(M2:In2,I)],Env5),
	I1 is I+1,
	cc_n(Cs,Env5,Env,I1).
	
mm([DL|MDcls],Mods,[MEnv|Env]) :-
	mm(MDcls,Mods,Env),
	mm_dl(DL,Mods,MEnv).
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

reset_chans([conn(var(M1),var(Out1),_M2,_In2)|Chans],Env,Env3) :-
	getv(Env,(M1:Out1),I),
	getv(Env,(I:ready),R),
	getv(Env,(I:valid),V),
	Env1=[(I:ready,_),(I:valid,_)],
	set_chan(R,I,V,Env1),
	reset_chans(Chans,Env,Env2),
	addenv(Env1,Env2,Env3).
reset_chans([],_,[]).

	
set_chan(0,I,_,[(I:ready,1),(I:valid,0)]).
set_chan(1,I,V,[(I:ready,1),(I:valid,V)]).

% State transitions

tt(design(Mods,Conns,MDcls),(CEnv0,Env0),(CEnv1,Env1)) :-
	storelist_skel(Env0,Env1),
	reset_chans(Conns,CEnv0,CEnv2),
	addenv(CEnv2,CEnv0,CEnvR),	% CEnvR = env with channels reset
	store_skel(CEnvR,CEnv1),
	td(Mods,MDcls,CEnvR,(CEnv0,Env0),(CEnv1,Env1)).
	
td([val(var(MVal),var(M))|MDcls],Mods,CEnvR,(CEnv0,[M0|Envs0]),(CEnv2,[M1|Envs1])) :-
	getMod(M,Mods,Mod),
	store_skel(M0,M1),
	store_skel(CEnv0,CEnv1),
	t_mod(Mod,MVal,CEnvR,(CEnv0,M0),(CEnv1,M1)),
	td(MDcls,Mods,CEnv1,(CEnv0,Envs0),(CEnv2,Envs1)).
td([],_,CEnv,_Env,(CEnv,[])).
	
t_mod(module(_M,_Ds,Ss),MVal,CEnvR,(CEnv0,MEnv0),(CEnv2,MEnv2)) :-
	t_ss(Ss,MVal,(CEnv0,MEnv0),(CEnv1,MEnv1)),
	addenv(MEnv1,MEnv0,MEnv2),
	addenv(CEnv1,CEnvR,CEnv2).

t_ss(Ss,MVal,(CEnv,Env0),Env) :-
	getv(Env0,MVal:state,N),
	getstate(Ss,N,S),
	t_state(S,MVal,(CEnv,Env0),Env).
	
t_state(state(_N,Cmd,Ss,Goto),MVal,Env,Env1) :-
	ee(Cmd,MVal,Env,V),
	state_when(V,Ss,Goto,MVal,Env,Env1).
	
state_when(V,_Ss,_Goto,_M,(_,_),([],[])) :-
	V\==1.
state_when(1,Ss,Goto,MVal,Env0,Env3) :-
	tt_stmts(Ss,MVal,Env0,Env1),
	eg(Goto,MVal,Env0,Env2),
	addenv2(Env1,Env2,Env3).
	
% Statements
	
tt_stmts([Stm|Stmts],MVal,Env0,Env3) :-
	tt_stmt(Stm,MVal,Env0,Env1),
	tt_stmts(Stmts,MVal,Env0,Env2),
	addenv2(Env1,Env2,Env3).
tt_stmts([],_MVal,_Env0,([],[])).
	
tt_stmt(asg(var(Lhs),Rhs),MVal,(CEnv,Env0),Env1) :-
	ee(Rhs,MVal,(CEnv,Env0),V),
	Env1=([],[(MVal:Lhs,V)]).
tt_stmt(asgmem(array(var(A),J),Rhs),MVal,(CEnv,Env0),Env1) :-
	ee(Rhs,MVal,(CEnv,Env0),V),
	ee(J,MVal,(CEnv,Env0),VJ),
	Env1=([],[(MVal:array(A,VJ),V)]).
tt_stmt(readmem(Lhs,array(var(A),J)),MVal,(CEnv,Env0),Env1) :-
	ee(J,MVal,(CEnv,Env0),VJ),
	getv(Env0,MVal:array(A,VJ),V),
	Env1=([],[(MVal:Lhs,V)]).
tt_stmt(write(Ch,Rhs),MVal,(CEnv,Env0),([(J:data,V),(J:valid,1)],[])) :-
	getv(CEnv,MVal:Ch,J),
	ee(Rhs,MVal,(CEnv,Env0),V).
tt_stmt(read(Lhs,Ch),MVal,(CEnv,_Env0),([(J:ready,0)],[(MVal:Lhs,V)])) :-
	getv(CEnv,MVal:Ch,J),
	getv(CEnv,J:data,V).
	
eg(next(N),MVal,_,([],[(MVal:state,N)])).
eg(mux(Exp,G1,G2),MVal,Env,Env1) :-
	ee(Exp,MVal,Env,V),
	goto_choice(V,G1,G2,MVal,Env,Env1).
	
goto_choice(1,G1,_,M,Env,Env1) :-
	eg(G1,M,Env,Env1).
goto_choice(V,_,G2,M,Env,Env1) :-
	V\==1,
	eg(G2,M,Env,Env1).
	

% Expression evaluation

ee(num(N),_,_,N).
ee(var(X),MVal,(_,Env),V) :-
	getv(Env,MVal:X,V).
ee(mux(E0,E1,E2),MVal,Env,V) :-
	ee(E0,MVal,Env,V1),
	ee_choice(V1,E1,E2,MVal,Env,V).
ee(Term,MVal,Env,V) :-
	Term=..[Op,E1,E2],
	ee(E1,MVal,Env,V1),
	ee(E2,MVal,Env,V2),
	binop(Op,V1,V2,V).
ee(ready(S),MVal,(CEnv,_),V) :-
	getv(CEnv,MVal:S,Ch),
	getv(CEnv,Ch:ready,R1),
	getv(CEnv,Ch:valid,R2),
	ret_ready(R1,R2,V).
ee(valid(S),MVal,(CEnv,_),V) :-
	getv(CEnv,MVal:S,Ch),
	getv(CEnv,Ch:ready,R1),
	getv(CEnv,Ch:valid,R2),
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
	X>=Y.
	
lte(X,Y,1) :-
	X=<Y.
lte(X,Y,0) :-
	X>Y.
	
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


	
% utilities

addenv(Env,[],Env1) :-
	addentries(Env,[],Env1). % to ensure sorted result
addenv(Env1,[(X,V)|Env2],Env) :-
	addentry(X,V,Env1,Env3),
	addenv(Env3,Env2,Env).
	
addenv2((C1,E1),(C2,E2),(C3,E3)) :-
	addenv(C1,C2,C3),
	addenv(E1,E2,E3).
	
addenvs((C1,E1),(C2,E2),(C3,E3)) :-
	addenv(C1,C2,C3),
	addenvlist(E1,E2,E3).
	
addenvlist([],[],[]). % to ensure sorted result
addenvlist([E1|Es1],[E2|Es2],[E3|Es3]) :-
	addenv(E1,E2,E3),
	addenvlist(Es1,Es2,Es3).
	
addentry(X,V,[],[(X,V)]).
addentry(X,_,[(X,W)|Env],[(X,W)|Env]).
addentry(X,V,[(Y,W)|Env],[(Y,W)|Env1]) :-
	X @> Y,
	addentry(X,V,Env,Env1).
addentry(X,V,[(Y,W)|Env],[(X,V),(Y,W)|Env]) :-
	X @< Y.
	
addentries([(X,V)|Env],Env0,Env2) :-
	addentry(X,V,Env0,Env1),
	addentries(Env,Env1,Env2).
addentries([],Env,Env).
	
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
	
env_skeleton((C,Es),(C1,Es1)) :-
	store_skel(C,C1),
	storelist_skel(Es,Es1).
	
storelist_skel([],[]).
storelist_skel([E|Es],[E1|Es1]) :-
	store_skel(E,E1),
	storelist_skel(Es,Es1).
		
store_skel([],[]).
store_skel([(X,_)|E1],[(X,_)|E2]) :-
	store_skel(E1,E2).
	
store_skel_undef([],[]).
store_skel_undef([(X,_)|E1],[(X,undef)|E2]) :-
	store_skel_undef(E1,E2).
	
menv_skel((C,M),(C1,M1)) :-
	store_skel(C,C1),
	store_skel(M,M1).
	
writeenv(J,(CEnv,MEnv)) :-
	write('===================='),nl,
	write(J),write(': '),nl,
	write(CEnv),nl,
	write(MEnv),nl.
	
% Tests

test :-
	prog(D),
	interpreter(D,5).
test1 :-
	prog1(D),
	interpreter(D,17).
test6 :-
	prog6(D),
	interpreter(D,46).
test7 :-
	prog7(D),
	interpreter(D,35).
test8 :-
	prog8(D),
	interpreter(D,20).
test9 :-
	prog9(D),
	interpreter(D,21).
	
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
	