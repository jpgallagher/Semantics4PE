:- module(while_reg,_).

% Generate a regular expression from a single procedure program
% Regular expressions over alphabet {asg(X,Expr),true(Expr), false(Expr)}
% where X is a variable, E an expression
%
% E ::= asg(X,Expr) | true(Expr) | false(Expr) | E1:E2 | E1+E2 | star(E) | null | eps


%%%%%%%%%%%%%%%%%%%%%%%%	
%----- statements -----
%%%%%%%%%%%%%%%%%%%%%%%%

regexp(skip,eps).
regexp(asg(var(X),E),asg(var(X),E)).
regexp(seq(S1,S2),E1:E2) :-
	regexp(S1,E1),
	regexp(S2,E2).
regexp(ifthenelse(E,S1,S2),(true(E):E1)+(false(E):E2)) :-
	regexp(S1,E1),
	regexp(S2,E2).
regexp(while(E,S1),star(true(E):E1):false(E)) :-
	regexp(S1,E1).
regexp(for(Init,Cond,Incr,S1),E1:E2) :-
	regexp(Init,E1),
	regexp(while(Cond,seq(S1,Incr)),E2).
regexp(let(X,E,S1),asg(X,E):E1) :-
	regexp(S1,E1).
	


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

