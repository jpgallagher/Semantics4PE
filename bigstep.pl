:- module(bigstep,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%----- expressions -----
%%%%%%%%%%%%%%%%%%%%%%%%

eval(var(X),St,St,V,_) :-
	find(St,X,V).
eval(cns(nat(N)),St,St,N,_).
eval(add(E1,E2),St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	V = V1+V2.
eval(sub(E1,E2),St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	V = V1-V2.
eval(mul(E1,E2),St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	V = V1*V2.
eval(div(E1,E2),St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	V = V1/V2.
eval(E1>E2,St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	gt(V1,V2,V).
eval(E1<E2,St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	lt(V1,V2,V).
eval(E1>=E2,St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	gte(V1,V2,V).
eval(E1=<E2,St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	lte(V1,V2,V).
eval(E1==E2,St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	eq(V1,V2,V).
eval(logicaland(E1,E2),St0,St2,V,Env) :-
	eval(E1,St0,St1,V1,Env),
	eval(E2,St1,St2,V2,Env),
	V is V1/\V2.
eval(not(E),St0,St1,V,Env) :-
	eval(E,St0,St1,V1,Env),
	negate(V1,V).
eval(call(P,Args),St0,St1,Ret,Env) :-
	copyState(St0,St1),
	solve(call(P,Args),St0,St1,Ret,Env).

%%%%%%%%%%%%%%%%%%%%%%%%	
%----- statements -----
%%%%%%%%%%%%%%%%%%%%%%%%

solve(skip,St,St,_,_).
solve(ret(E),St,St1,V,Env) :-		% Assume that return is the last reached point in function
	eval(E,St,St1,V,Env).
solve(call(P,Args),St,St1,Ret,Env) :-
	def(P,Env,Params,Proc),
	bindParams(Args,Params,Decl),
	copyState(St,St1),
	solve(block(Decl,Proc),St,St1,Ret,Env).
solve(asg(var(X),E),St,St2,_Ret,Env) :-
	eval(E,St,St1,V,Env),
	save(X,V,St1,St2).
solve(seq(S1,S2),St,St2,Ret,Env) :-
	copyState(St,St1),
	solve(S1,St,St1,_,Env),
	solve(S2,St1,St2,Ret,Env).
solve(ifthenelse(E,S1,S2),St,St2,Ret,Env) :-
	eval(E,St,St1,V1,Env),
	branch(V1,S1,S2,St1,St2,Ret,Env).
solve(while(E,S1),St,St1,Ret,Env) :-
	solve(ifthenelse(E,seq(S1,while(E,S1)),skip),St,St1,Ret,Env).
solve(for(Init,Cond,Incr,S1),St,St2,Ret,Env) :-
	copyState(St,St1),
	solve(Init,St,St1,_,Env),
	solve(while(Cond,seq(S1,Incr)),St1,St2,Ret,Env).
solve(block(Decl,S),St,St3,Ret,Env) :-
	localState(Decl,St,St1,Env),
	copyState(St1,St2),
	solve(S,St1,St2,Ret,Env),
	restoreState(St2,Decl,St,St3).
solve(let(Var,E,S),St,St1,Ret,Env) :-
	solve(block([decl(Var,E)],S),St,St1,Ret,Env).
	
localState([decl(var(X),null)|Ds],St,St2,Env) :-
	savenull(X,St,St1),
	localState(Ds,St1,St2,Env).
localState([decl(var(X),E)|Ds],St,St3,Env) :-
	E \== null,
	eval(E,St,St1,V,Env),
	save(X,V,St1,St2),
	localState(Ds,St2,St3,Env).
localState([decl([vardecl(var(X),_,_)],E)|Ds],St,St3,Env) :-
	eval(E,St,St1,V,Env),
	save(X,V,St1,St2),
	localState(Ds,St2,St3,Env).
localState([],St,St,_).

branch(1,S1,_,St1,St2,Ret,Env) :-
	solve(S1,St1,St2,Ret,Env).
branch(0,_,S2,St1,St2,Ret,Env) :-
	solve(S2,St1,St2,Ret,Env).

%%%%%%%%%%%%%%%%%%%%%%%%
% Utility relations
%%%%%%%%%%%%%%%%%%%%%%%%

	
find([(X,N)|_],X,N).
find([(Y,_)|St],X,N) :-
	X \== Y,
	find(St,X,N).

definedVar([(X,N)|_],var(X),N,true).
definedVar([(Y,_)|St],var(X),N,TF) :-
	X \== Y,
	definedVar(St,var(X),N,TF).
definedVar([],_,null,false).
	
save(X,V,[(X,_)|St],[(X,W)|St]) :-
	W is V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W is V.
	
savenull(X,[(X,_)|St],[(X,_)|St]).
savenull(X,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	savenull(X,St,St1).
savenull(X,[],[(X,_)]).

def(P,Env,Params,S) :-
	member(function(P,Params,S),Env).

gt(X,Y,1) :-
	X > Y.
gt(X,Y,0) :-
	X =< Y.
	
lt(X,Y,1) :-
	X < Y.
lt(X,Y,0) :-
	X >= Y.
	
gte(X,Y,1) :-
	X >= Y.
gte(X,Y,0) :-
	X < Y.
	
lte(X,Y,1) :-
	X =< Y.
lte(X,Y,0) :-
	X > Y.
	
eq(X,Y,1) :-
	X == Y.
eq(X,Y,0) :-
	X \== Y.
	
negate(true,false).
negate(false,true).

copyState([],[]).
copyState([(X,_)|St],[(X,_)|St1]) :-
	copyState(St,St1).

localVar(X,[decl([vardecl(X,_,_)],_)|_],true).
localVar(X,[decl(X,_)|_],true).
localVar(X,[decl(var(Y),_)|Ds],TF) :-
	X \== var(Y),
	localVar(X,Ds,TF).
localVar(X,[decl([vardecl(Y,_,_)],_)|Ds],TF) :-
	X \== Y,
	localVar(X,Ds,TF).
localVar(_,[],false).
	
restoreState([(X,V)|St],Ds,St0,St1) :-
	localVar(var(X),Ds,TF),
	restoreStateCond1(TF,Ds,X,V,St,St0,St1).
restoreState([],_,_,[]).

restoreStateCond1(true,Ds,X,_,St,St0,St1) :-
	definedVar(St0,var(X),V0,TF),
	restoreStateCond2(TF,V0,Ds,X,St,St0,St1).
restoreStateCond1(false,Ds,X,V,St,St0,[(X,V)|St1]) :-
	restoreState(St,Ds,St0,St1).
	
restoreStateCond2(false,_,Ds,_,St,St0,St1) :-
	restoreState(St,Ds,St0,St1).
restoreStateCond2(true,V,Ds,X,St,St0,[(X,V)|St1]) :-
	restoreState(St,Ds,St0,St1).
	
bindParams([E|Es],[X|Xs],[decl(X,E)|Ds]) :-
	bindParams(Es,Xs,Ds).
bindParams([],[],[]).
	


%%%%%%%%%%%%%%%%%%%%%%%%%
% initial setup and entry
%%%%%%%%%%%%%%%%%%%%%%%%%

go(Ret) :-
		globalEnv(Env,St0,Args),
		copyState(St0,St1),
        solve(call(main,Args),St0,St1,Ret,Env).
	
globalEnv(P,A,Args) :-
	program(P),
	member(function(main,Args,_),P),
	vardecls(P,A).

vardecls([],[]).
vardecls([[vardecl(var(X),_,Init)]|P],[(X,V)|St]) :-
	initDeclVal(Init,V),
	vardecls(P,St).
vardecls([T|P],St) :-
	T \= [vardecl(_,_,_)],
	vardecls(P,St).
	
initDeclVal(cns(nat(V)),V).
initDeclVal(null,_).


%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%



/*
program(
[
 function(
   main,
   [],
   let(
     var(n),
     cns(nat(0)),
     let(
       var(x),
       cns(nat(0)),
       let(
         var(a),
         cns(nat(0)),
         seq(
           asg(var(x),var(n)),
           seq(
             asg(var(a),cns(nat(1))),
             while(
               var(x)>cns(nat(0)),
               seq(
                 asg(var(x),sub(var(x),cns(nat(1)))),
                 let(
                   var(y),
                   cns(nat(0)),
                   seq(
                     asg(var(y),var(a)),
                     while(
                       var(y)>cns(nat(0)),
                       seq(
                         asg(var(y),sub(var(y),cns(nat(1)))),
                         asg(var(a),add(var(a),cns(nat(1))))
                         )
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
   )
]).
*/
/*
program(
[
 function(
   fib,
   [
    [
     vardecl(var(n),int,cns(nat(0)))
    ]
   ],
   let(
     var(i),
     cns(nat(0)),
     let(
       var('Fnew'),
       cns(nat(0)),
       let(
         var('Fold'),
         cns(nat(0)),
         let(
           var(temp),
           cns(nat(0)),
           let(
             var(ans),
             cns(nat(0)),
             seq(
               asg(var('Fnew'),cns(nat(1))),
               seq(
                 asg(var('Fold'),cns(nat(0))),
                 seq(
                   for(
                     asg(var(i),cns(nat(2))),
                     var(i)=<var(n),
                     asg(var(i),add(var(i),cns(nat(1)))),
                     seq(
                       asg(var(temp),var('Fnew')),
                       seq(
                         asg(var('Fnew'),add(var('Fnew'),var('Fold'))),
                         asg(var('Fold'),var(temp))
                         )
                       )
                     ),
                   seq(
                     asg(var(ans),var('Fnew')),
                     ret(var(ans))
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
   ),
 function(
   main,
   [],
   let(
     var(a),
     cns(nat(0)),
     let(
       var(ans),
       cns(nat(0)),
       seq(
         asg(var(a),cns(nat(30))),
         seq(
           asg(var(ans),call(fib,[var(a)])),
           ret(cns(nat(0)))
           )
         )
       )
     )
   )
]).
*/

/*
program(
[
 function(
   fact,
   [
    [
     vardecl(var(i),int,cns(nat(0)))
    ]
   ],
   ifthenelse(
     var(i)=<cns(nat(0)),
     ret(cns(nat(1))),
     ret(mul(var(i),call(fact,[sub(var(i),cns(nat(1)))])))
     )
   ),
 function(
   main,
   [],
   call(fact,[cns(nat(10))])
   )
]).
*/
/*
program(
[
 [
  vardecl(var(m),int,cns(nat(5)))
 ],
 [
  vardecl(var(sum),int,null)
 ],
 function(
   sumup,
   [
    [
     vardecl(var(x),int,null)
    ]
   ],
   let(
     var(r),
     cns(nat(0)),
     seq(
       while(
         var(x)>cns(nat(0)),
         seq(
           asg(var(r),add(var(r),var(x))),
           asg(var(x),sub(var(x),cns(nat(1))))
           )
         ),
       ret(var(r))
       )
     )
   ),
 function(
   main,
   [],
   seq(
     asg(var(sum),call(sumup,[var(m)])),
     ret(var(sum))
     )
   )
]).
*/

program(
[
 [
  vardecl(var(n),int,null)
 ],
 function(
   f,
   [
    [
     vardecl(var(n),int,null)
    ]
   ],
   let(
     var(x),
     null,
     let(
       var(a),
       null,
       seq(
         asg(var(x),var(n)),
         seq(
           asg(var(a),cns(nat(1))),
           while(
             var(x)>cns(nat(0)),
             seq(
               asg(var(x),sub(var(x),cns(nat(1)))),
               let(
                 var(y),
                 null,
                 seq(
                   asg(var(y),var(a)),
                   while(
                     var(y)>cns(nat(0)),
                     seq(
                       asg(var(y),sub(var(y),cns(nat(1)))),
                       asg(var(a),add(var(a),cns(nat(1))))
                       )
                     )
                   )
                 )
               )
             )
           )
         )
       )
     )
   ),
 function(
   main,
   [],
   call(f,[var(n)])
   )
]).
