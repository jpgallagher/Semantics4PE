bigstep(val(V),_Rho,V).
bigstep(var(X),Rho,V) :- 
    find(Rho,X,V).
bigstep(lam(X,E),Rho,clo(X,E,Rho)).
bigstep(app(E1,E2),Rho,V) :- 
    bigstep(E1,Rho,clo(X,E,Rho1)),bigstep(app4_1(E2,X,E,Rho1),Rho,V).
bigstep(app4_1(E2,X,E,Rho1),Rho,V) :- 
    bigstep(E2,Rho,V2),bigstep(app4_2(X,E,Rho1,V2),Rho2,V).
bigstep(app4_2(X,E,Rho1,V2),Rho2,V) :- 
    save(X,V2,Rho1,Rho2),bigstep(E,Rho2,V).
find([(X,N)|_St],X,N).
find([(Y,_N)|St],X,N) :- 
    X\==Y,find(St,X,N).
save(X,V,[(X,_6706)|St],[(X,W)|St]) :- 
    W=V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :- 
    X\==Y,save(X,V,St,St1).
save(X,V,[],[(X,W)]) :- 
    W=V.
bigStepPred(bigstep(_7062,_7063,_7064)).
otherPred(find(_7118,_7119,_7120)).
otherPred(save(_7174,_7175,_7176,_7177)).
