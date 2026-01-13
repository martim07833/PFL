
female(grace).
female(dede).
female(claire).
female(gloria).
female(barb).
female(pameron).
female(bo).
female(haley).
female(lily).
female(poppy).


male(frank).
male(phil).
male(jay).
male(mitchell).
male(joe).
male(javier).
male(manny).
male(merle).
male(cameron).
male(dylan).
male(alex).
male(luke).
male(rexford).
male(calhoun).
male(george).

parent(grace, phil).
parent(frank, phil).

parent(dede, mitchell).
parent(jay, mitchell).
parent(dede, claire).
parent(jay, claire).

parent(gloria, joe).
parent(jay, joe).

parent(javier, manny).
parent(gloria, manny).

parent(barb, pameron).
parent(merle, pameron).

parent(claire, haley).
parent(phil, haley).

parent(claire, alex).
parent(phil, alex).

parent(claire, luke).
parent(phil, luke).

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

married(jay, gloria, 2008).
married(jay, dede, 1968).
divorced(jay, dede, 2003).

father(X, Y) :-
    male(X),
    parent(X, Y).

mother(X, Y) :-
    female(X),
    parent(X, Y).

grandparent(X, Y) :-
    parent(X, Z),
    parent(Z, Y).

grandmother(X, Y) :-
    female(X),
    grandparent(X, Y).

grandfather(X, Y) :-
    male(X),
    grandparent(X, Y).

sibling(X, Y) :-
    parent(P1, X),
    parent(P1, Y),
    X \= Y,
    parent(P2, X),
    parent(P2, Y),
    P1 \= P2.

halfsibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y,
    parent(Q, X),
    parent(R, Y),
    Q \= R.

aunt_uncle(X, Y) :-
    sibling(X, Z),
    parent(Z, Y).

cousin(X, Y) :-
    parent(Z, X),
    aunt_uncle(Z, Y).

uncle(X, Y) :-
    male(X),
    aunt_uncle(X, Y).

aunt(X, Y) :-
    female(X),
    aunt_uncle(X, Y).

