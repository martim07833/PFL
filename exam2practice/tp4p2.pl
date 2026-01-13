print_full_list(L) :-
	print_full_list(L, 0).

print_full_list([], _) :-
	write(']').

print_full_list([Head | Tail], Counter) :-
	Counter > 0,
	write(Head),
	(Tail = [] -> true ; write(', ')),
	Counter1 is Counter + 1,
	print_full_list(Tail, Counter1).
 
print_full_list([Head | Tail], 0) :-
	write('['),
	write(Head),
	(Tail = [] -> true ; write(', ')),
	print_full_list(Tail, 1).


print_list(L) :-
	length(L, ListSize),
	ListSize >= 11,
	print_list(L, 0, ListSize).

print_list(L) :-
	length(L, ListSize),
	ListSize < 11,
	print_full_list(L).

print_list([], _, _) :-
	write(']').

print_list([First, Second, Third | Tail], 0, ListSize) :-
	write('['),
	write(First),
	write(', '),
	write(Second),
	write(', '),
	write(Third),
	write(', ...,'),
	Counter1 is 3,
	print_list(Tail, Counter1, ListSize).

print_list([First, Second, Third | _], Counter, ListSize) :-
    Counter =:= ListSize - 3,
	write(' '),
	write(First),
	write(', '),
	write(Second),
	write(', '),
	write(Third),
	write(']').

print_list([First, Second, Third | Tail], Counter, ListSize) :-
	Counter =:= ListSize / 2 - 1,
	write(' '),
	write(First),
	write(', '),
	write(Second),
	write(', '),
	write(Third),
	write(', ...'),
	Counter1 is Counter + 3,
	print_list(Tail, Counter1, ListSize).

print_list([_ | Tail], Counter, ListSize) :-
	Counter > 0,
	print_list(Tail, Counter + 1, ListSize).


print_matrix([]) :-
	!.

print_matrix([Head | Tail]) :-
	print_list(Head),
	nl,
	print_matrix(Tail).
	