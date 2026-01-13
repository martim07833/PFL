list_size(List, Size) :-
    list_size(List, Size, 0).

list_size([], Size, Size).

list_size(List, Size, Accumulator) :-
    List \= [],
    List = [_|Tail],
    Acc1 is Accumulator + 1,
    list_size(Tail, Size, Acc1).

list_sum(List, Sum) :-
    list_sum(List, Sum, 0).

list_sum([], Sum, Sum).

list_sum(List, Sum, Accumulator) :-
    List \= [],
    List = [Item | Tail],
    Acc1 is Accumulator + Item,
    list_sum(Tail, Sum, Acc1).

list_prod(List, Prod) :- 
    list_prod(List, Prod, 1).

list_prod([], Prod, Prod).

list_prod(List, Prod, Accumulator) :-
    List \= [],
    List = [Item | Tail],
    Acc1 is Accumulator * Item,
    list_prod(Tail, Prod, Acc1).


inner_product(List, List2, Result) :-
    inner_product(List, List2, Result, 0).

inner_product([], [], Result, Result).

inner_product(List, List2, Result, Accumulator) :-
    List \= [],
    List2 \= [],
    List = [Item | Tail1],
    List2 = [Item2 | Tail2],
    Acc1 is Accumulator + Item*Item2,
    inner_product(Tail1, Tail2, Result, Acc1).



count(Elem, List, N) :-
    count(Elem, List, N, 0).

count(_, [], N, N).

count(Elem, [Elem | Tail], N, Accumulator) :-
    Acc1 is Accumulator + 1,
    count(Elem, Tail, N, Acc1).

count(Elem, [Head | Tail], N, Accumulator) :-
    Head \= Elem,
    count(Elem, Tail, N, Accumulator).

invert(List, List2) :-
    invert(List, List2, []).

invert([], List2, List2).

invert([Head | Tail], List2, Accumulator) :-
    invert(Tail, List2, [Head | Accumulator]).


del_one(Elem, List1, List2) :-
    del_one(Elem, List1, List2, false).

del_one(_, [], [], _).

del_one(Elem, [Elem | Tail], Tail, false).

del_one(Elem, [Head | Tail], [Head | Result], true) :-
    del_one(Elem, Tail, Result, true).

del_one(Elem, [Head | Tail], [Head | Result], false) :-
    Elem \= Head,
    del_one(Elem, Tail, Result, false).


del_all(Elem, List1, List2) :- 
    del_all(Elem, List1, List2, 0).

del_all(_, [], [], _).

del_all(Elem, [Elem | Tail], Result, Count) :-
    Count1 is Count + 1,
    del_all(Elem, Tail, Result, Count1).

del_all(Elem, [Head | Tail], [Head | Result], Count) :-
    Elem \= Head,
    del_all(Elem, Tail, Result, Count).

del_all_list(ListElems, List1, List2) :-
    del_all_list(ListElems, List1, List2, 0).

del_all_list(_, [], [], _).

del_all_list(ListElems, [Head | Tail], Result, Count) :-
    member(Head, ListElems),
    Count1 is Count + 1,
    del_all_list(ListElems, Tail, Result, Count1).

del_all_list(ListElems, [Head | Tail], [Head | Result], Count) :-
    \+ member(Head, ListElems),
    del_all_list(ListElems, Tail, Result, Count).   


del_dups(List1, List2) :-
    del_dups(List1, List2, []).

del_dups([], List2, List2).

del_dups([Head | Tail], List2, Accumulator) :-
    member(Head, Accumulator),
    del_dups(Tail, List2, Accumulator).

del_dups([Head | Tail], [Head | List2], Accumulator) :-
    \+ member(Head, Accumulator),
    del_dups(Tail, List2, [Head | Accumulator]).







