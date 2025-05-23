:- module(while_smallstep,_).

%%%%%%%%%%%%%%%%%%%%%%%%
%----- expressions -----
%%%%%%%%%%%%%%%%%%%%%%%%

eval(var(X),St,V) :-
	find(St,X,V).
eval(cns(nat(N)),_,N).
eval(add(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1+V2.
eval(sub(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1-V2.
eval(mul(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1*V2.
eval(div(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	V = V1/V2.
eval(E1>E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	gt(V1,V2,V).
eval(E1<E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	lt(V1,V2,V).
eval(E1>=E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	gte(V1,V2,V).
eval(E1=<E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	lte(V1,V2,V).
eval(E1==E2,St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	eq(V1,V2,V).
eval(logicaland(E1,E2),St0,V) :-
	eval(E1,St0,V1),
	eval(E2,St0,V2),
	logicalAnd(V,V1,V2).
eval(not(E),St0,V) :-
	eval(E,St0,V1),
	negate(V1,V).

evaltrue(E,St) :-
	eval(E,St,1).
evalfalse(E,St) :-
	eval(E,St,0).

%%%%%%%%%%%%%%%%%%%%%%%%	
%----- statements -----
%%%%%%%%%%%%%%%%%%%%%%%%

run(skip,St) :-
	halt(St).
run(S, St) :-
	S \== skip,
	smallstep(S,S1,St,St1),
	run(S1,St1).
	
halt(St) :-
	write(St),
	nl.
	
smallstep(asg(var(X),E),skip,St,St1) :-
	evalAndSave(X,E,St,St1).
smallstep(seq(skip,S2),S2,St,St).
smallstep(seq(S1,S2),seq(S11,S2),St,St1) :-
	S1\==skip,
	smallstep(S1,S11,St,St1).
smallstep(ifthenelse(E,S1,_),S1,St,St) :-
	evaltrue(E,St).
smallstep(ifthenelse(E,_,S2),S2,St,St) :-
	evalfalse(E,St).
smallstep(while(E,_),skip,St,St) :-
	evalfalse(E,St).
smallstep(while(E,S1),seq(S1,while(E,S1)),St,St) :-
	evaltrue(E,St).
smallstep(for(Init,Cond,Incr,S1),seq(Init,while(Cond,seq(S1,Incr))),St,St).
smallstep(let(var(X),null,S),S,St,St1) :-
	evalAndSave(X,cns(nat(0)),St,St1).
smallstep(let(var(X),E,S),S,St,St1) :-
	evalAndSave(X,E,St,St1).

evalAndSave(X,E,St,St1) :-
	eval(E,St,V),
	save(X,V,St,St1).
	
%%%%%%%%%%%%%%%%%%%%%%%%
% Utility relations
%%%%%%%%%%%%%%%%%%%%%%%%

	
find([(X,N)|_],X,N).
find([(Y,_)|St],X,N) :-
	X \== Y,
	find(St,X,N).
	
save(X,V,[(X,_)|St],[(X,W)|St]) :-
	W is V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W is V.

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
	
logicalAnd(1,V1,V2) :-
	1 is V1/\V2.
logicalAnd(0,V1,V2) :-
	0 is V1/\V2.			
	
negate(1,0).
negate(0,1).


%%%%%%%%%%%%%%%%%%%%%%%%%
% initial setup and entry
%%%%%%%%%%%%%%%%%%%%%%%%%

go :-
	program(P),
	vardecls(P,St),			% Top level variable declarations
	member(function(main,[],S),P),
	run(S,St).  % Run main in the global environment
	

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


program(
[
 function(
   main,
   [],
   let(
     var(n),
     cns(nat(30)),
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
                   
                     asg(var(ans),var('Fnew'))
                     
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

program(
[
 [
  vardecl(var(m),int,cns(nat(5)))
 ],
 [
  vardecl(var(sum),int,null)
 ],
 [
  vardecl(var(x),int,null)
 ],
 function(
   main,
   [],
   let(
     var(r),
     cns(nat(0)),
     seq(asg(var(x),var(m)),
     
       while(
         var(x)>cns(nat(0)),
         seq(
           asg(var(r),add(var(r),var(x))),
           asg(var(x),sub(var(x),cns(nat(1))))
           )
         )
       )
     )
   )
]).

/*
program(
[
 [
  vardecl(var(n),int,cns(nat(10)))
 ],
 function(
   main,
    [],
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
    )
]).


*/

