female(grace).
male(frank).
female(dede).
male(jay).
female(gloria).
male(javier).
female(barb).
male(merie).

male(phil).
female(claire).
male(mitchell).
male(joe).
male(manny).
male(cameron).
female(pameron).
male(bo).

male(dylan).
female(haley).
male(alex).
male(luke).
female(lily).
male(rexford).
male(calhoun).
male(george).
female(poppy).

parent(grace, phil).
parent(frank, phil).
parent(dede, claire).
parent(jay, claire).
parent(dede, mitchell).
parent(jay, mitchell).
parent(gloria, joe).
parent(jay, joe).

parent(javier, manny).
parent(gloria, manny).

parent(barb, cameron).
parent(merie, cameron).
parent(merie, pameron).
parent(barb, pameron).

parent(phil, haley).
parent(claire, haley).

parent(phil, alex).
parent(claire, alex).

parent(phil, luke).
parent(claire, luke).

parent(mitchell, lily).
parent(cameron, lily).
parent(mitchell, rexford).
parent(cameron, rexford).

parent(pameron, calhoun).
parent(bo, calhoun).

parent(dylan, george).
parent(haley, george).

parent(dylan, poppy).
parent(haley, poppy).

father(X, Y) :- parent(X , Y), male(X).
mother(X, Y) :- parent(X , Y), female(X).

grandparent(X,Y) :- parent(X, Z), parent(Z, Y).
grandfather(X,Y) :- father(X, Z), parent(Z, Y).
grandmother(X,Y) :- mother(X, Z), parent(Z, Y).

sibling(X,Y) :- father(F, X), mother(M, X), father(F, Y), mother(M, Y), X \= Y.

halfsiblings(X,Y) :- parent(P, X), parent(P, Y), parent(Q, X), parent(R, Y), Q \= R, X \= Y.

uncle(X, Y) :- parent(P, Y), male(X), sibling(P, X).
aunt(X,Y) :- parent(P, Y), female(X), sibling(P, X).

cousin(X, Y) :- parent(P1, X), parent(P2, Y), sibling(P1, P2), X \= Y.


married(jay, gloria, 2008).
married(jay, dede, 1968).

divorced(jay, dede, 2003).


teaches(adalberto, algorithms).
teaches(bernardete, databases).
teaches(capitolino, compilers).
teaches(diogenes, statistics).
teaches(ermelinda, networks).

attends(alberto, algorithms).
attends(bruna, algorithms).
attends(cristina, algorithms).
attends(diogo, algorithms).
attends(eduarda, algorithms).

attends(antonio, databases).
attends(bruno, databases).
attends(cristina, databases).
attends(duarte, databases).
attends(eduardo, databases).

attends(alberto, compilers).
attends(bernardo, compilers).
attends(clara, compilers).
attends(diana, compilers).
attends(eurico, compilers).

attends(antonio, statistics).
attends(bruna, statistics).
attends(claudio, statistics).
attends(duarte, statistics).
attends(eva, statistics).

attends(alvaro, networks).
attends(beatriz, networks).
attends(claudio, networks).
attends(diana, networks).
attends(eduardo, networks).

professor_of(X, Y) :- teaches(X, Class), attends(Y, Class).

colleagues(X, Y) :- attends(X, Class), attends(Y, Class), X \= Y.
colleagues(X, Y) :- teaches(X, _), teaches(Y, _), X \= Y.

multipleclasstakers(X) :- attends(X, Y), attends(X, Z), Y \= Z.

