double(X, Y):- Y is X*2.

map(_, [], []).

map(Pred, [X | Xs], [Y | Ys]) :-
	call(Pred, X, Y),
	map(Pred, Xs, Ys).

sum(A, B, S):- S is A+B.

fold(_, StartValue, [], StartValue).


fold(Pred, StartValue, [X|Xs] , Final) :-
	call(Pred, StartValue, X, NewValue),
	fold(Pred, NewValue, Xs, Final).


even(X):- 0 =:= X mod 2.

separate([], _, [], []).

separate([X | Xs], Pred, Yes, No) :-
	(call(Pred,X)
	-> Yes = [X | YesTail],
	No = NoTail;
	Yes = YesTail,
	No = [X|NoTail]
	),
	separate(Xs, Pred, YesTail, NoTail).

take_while(_, [], [], []).

take_while(Pred, [X|Xs], Front, Back) :-
	(
		call(Pred, X) ->
		Front = [X | FrontTail],
		take_while(Pred, Xs, FrontTail, Back);
		Front = [],
		Back = [X | Xs]
	).

ask_execute :-
	write('Insert the goal to execute'),
	nl,
	write('|: '),
	read(Goal),
	call(Goal).


my_functor(Term, Name, Arity) :-
	nonvar(Term),
	!,
	Term =.. [Name|Args],
	length(Args, Arity).


my_functor(Term, Name, Arity) :-
	var(Term),
	integer(Arity), Arity >= 0,
	atom(Name),
	!,
	length(Args, Arity),
	Term =.. [Name | Args].

my_functor(_, Name, Arity) :-
	( \+ atom(Name) -> throw(error(type_error(atom, Name), _));
	  \+ integer(Arity) -> throw(error(type_error(integer, Arity), _));
	  Arity < 0 -> throw(error(domain_error(not_less_than_zero, Arity), _))
	).


my_arg(N, Term, Arg) :-
	integer(N), N > 0,
	!,
	Term =.. [_| Args],
	nth1(N, Args, Arg).


univ(Term, [Functor | Args]) :-
	functor(Term, Functor, Arity),
	get_all_args(1, Arity, Term, Args).


get_all_args(N, Arity, _, []) :-
	N > Arity, !.

get_all_args(N, Arity, Term, [Arg | Rest]) :-
	arg(N, Term, Arg),
	N1 is N + 1,
	get_all_args(N1, Arity, Term, Rest).


tree_size(nil, 0).

tree_size(t(Left, _, Right), Size) :-
	tree_size(Left, SizeLeft),
	tree_size(Right, SizeRight),
	Size is 1 + SizeLeft + SizeRight.


tree_map(_, nil, nil).

tree_map(Pred, t(Left1, Value1, Right1), t(Left2, Value2, Right2)) :-
	call(Pred, Value1, Value2),
	tree_map(Pred, Left1, Left2),
	tree_map(Pred, Right1, Right2).





