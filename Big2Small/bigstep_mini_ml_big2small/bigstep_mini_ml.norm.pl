go(E,V) :- 
    init(Rho),eval(E,Rho,V).
init([(id(eq),opaque(eq)),(id(times),opaque(times)),(id(sub),opaque(sub)),(id(plus),opaque(plus))]).
eval(number(N),_Rho,int(N)).
eval(true,_Rho,true).
eval(false,_Rho,false).
eval(lambda(P,E),Rho,closure(P,E,Rho)).
eval(id(I),Rho,Alpha) :- 
    val_of(Rho,I,Alpha).
eval(if(E1,E2,E3),Rho,Alpha) :- 
    eval(E1,Rho,V),eval(choice(V,E2,E3),Rho,Alpha).
eval(choice(true,E2,E3),Rho,Alpha) :- 
    eval(E2,Rho,Alpha).
eval(choice(false,E2,E3),Rho,Alpha) :- 
    eval(E3,Rho,Alpha).
eval(mlpair(E1,E2),Rho,Pair) :- 
    eval(E1,Rho,Alpha),eval(mlpair9_1(E2,Alpha),Rho,Pair).
eval(mlpair9_1(E2,Alpha),Rho,Pair) :- 
    eval(E2,Rho,Beta),Pair=(Alpha,Beta).
eval(apply(E1,E2),Rho,Beta) :- 
    eval(E1,Rho,Gamma),eval(apply10_1(E2,Gamma),Rho,Beta).
eval(apply10_1(E2,Gamma),Rho,Beta) :- 
    Gamma=closure(P,E,Rho1),eval(apply10_2(E2,P,E,Rho1),Rho,Beta).
eval(apply10_2(E2,P,E,Rho1),Rho,Beta) :- 
    eval(E2,Rho,Alpha),eval(apply10_3(P,E,Rho1,Alpha),Rho2,Beta).
eval(apply10_3(P,E,Rho1,Alpha),Rho2,Beta) :- 
    Rho2=[(P,Alpha)|Rho1],eval(E,Rho2,Beta).
eval(apply(E1,E2),Rho,Beta) :- 
    eval(E1,Rho,Gamma),eval(apply11_1(E2,Gamma),Rho,Beta).
eval(apply11_1(E2,Gamma),Rho,Beta) :- 
    Gamma=opaque(Op),eval(apply11_2(E2,Op),Rho,Beta).
eval(apply11_2(E2,Op),Rho,Beta) :- 
    eval(E2,Rho,Alpha),opeval(Op,Alpha,Beta).
eval(let(P,E2,E1),Rho,Beta) :- 
    eval(E2,Rho,Alpha),eval(let12_1(P,E1,Alpha),Rho,Beta).
eval(let12_1(P,E1,Alpha),Rho,Beta) :- 
    Rho1=[(P,Alpha)|Rho],eval(E1,Rho1,Beta).
eval(letrec(P,E2,E1),Rho,Beta) :- 
    Rho1=[(P,Alpha)|Rho],eval(letrec13_1(E2,E1,Alpha),Rho1,Beta).
eval(letrec13_1(E2,E1,Alpha),Rho1,Beta) :- 
    eval(E2,Rho1,Alpha),eval(E1,Rho1,Beta).
val_of([(id(I),Alpha)|_Rho],I,Alpha).
val_of([(id(X),_Beta)|Rho],I,Alpha) :- 
    X\==I,val_of(Rho,I,Alpha).
val_of([(pairpat(P1,P2),Alpha,Beta)|Rho],I,N) :- 
    val_of([(P1,Alpha),(P2,Beta)|Rho],I,N).
opeval(eq,(X,Y),true) :- 
    X==Y.
opeval(eq,(X,Y),false) :- 
    X\==Y.
opeval(times,(int(X),int(Y)),int(Z)) :- 
    Z is X*Y.
opeval(sub,(int(X),int(Y)),int(Z)) :- 
    Z is X-Y.
opeval(plus,(int(X),int(Y)),int(Z)) :- 
    Z is X+Y.
bigStepPred(eval(_X,_Y,_Z)).
otherPred(opeval(_X,_Y,_Z)).
otherPred(val_of(_X,_Y,_Z)).
test1(V) :- 
    fac(E),go(E,V).
test2(V) :- 
    block(E),go(E,V).
test3(V) :- 
    simul(E),go(E,V).
test4(V) :- 
    evenodd(E),go(E,V).
test5(V) :- 
    init(Rho),eval(letrec(id(i),id(j),id(i)),Rho,V).
test6(V) :- 
    if(E),go(E,V).
fac(letrec(id(fact),lambda(id(x),if(apply(id(eq),mlpair(id(x),number(0))),number(1),apply(id(times),mlpair(id(x),apply(id(fact),apply(id(sub),mlpair(id(x),number(1)))))))),apply(id(fact),number(4)))).
block(let(id(i),number(5),let(id(i),apply(id(plus),mlpair(id(i),number(1))),id(i)))).
simul(let(pairpat(id(x),id(y)),mlpair(number(2),number(3)),let(pairpat(id(x),id(y)),mlpair(id(y),id(x)),id(x)))).
evenodd(letrec(pairpat(id(even),id(odd)),mlpair(lambda(id(x),if(apply(id(eq),mlpair(id(x),number(0))),true,apply(id(odd),apply(id(sub),mlpair(id(x),number(1)))))),lambda(id(x),if(apply(id(eq),mlpair(id(x),number(0))),false,apply(id(even),apply(id(sub),mlpair(id(x),number(1))))))),apply(id(even),number(3)))).
fact(N,letrec(id(fact),lambda(id(x),if(apply(id(eq),mlpair(id(x),number(0))),number(1),apply(id(times),mlpair(id(x),apply(id(fact),apply(id(sub),mlpair(id(x),number(1)))))))),apply(id(fact),number(N)))).
if(let(id(x),number(3),if(apply(id(eq),mlpair(id(x),number(3))),number(1),number(0)))).
testfac(N,V) :- 
    fact(N,E),go(E,V).
