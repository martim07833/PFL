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


children(Person, Children) :-
    findall(Child, parent(Person, Child), Children).


children_of(ListOfPeople, ListOfPairs) :-
	children_of(ListOfPeople, ListOfPairs, []).

children_of([], ListOfPairs, ListOfPairs).

children_of([Head|Tail], ListOfPeople, Accumulator) :-
	children(Head, Children),
	children_of(Tail, ListOfPeople, [[Head, Children] | Accumulator]).


family(F) :-
	findall(Person, (male(Person); female(Person)), People),
	sort(People, F).



couple(X-Y) :-
	parent(X, Child),
	parent(Y, Child),
	X \= Y.


couples(List) :-
	findall(X-Y, couple(X-Y), Couples),
	sort(Couples, List).


spouse_children(Person, Spouse/Children) :-
	parent(Person1, Child),
	parent(Person2, Child),
	Person1 \= Person2,
	findall(CommonChild, (parent(Person, CommonChild), parent(Spouse, CommonChild)), Children),
	Children \= [].











