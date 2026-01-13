replicate(0, _, []).
replicate(Amount, Element, List) :-
    Amount > 0,
    Amount1 is Amount - 1,
    replicate(Amount1, Element, Tail),
    List = [Element | Tail].


intersperse(_, [X], [X]).
intersperse(Elem, List1, List2) :-
    List1 = [Head | Tail],
    intersperse(Elem, Tail, Tail2),
    List2 = [Head, Elem | Tail2].


insert_elem(0, List, Elem, [Elem | List]).
insert_elem(Index, List1, Elem, List2) :-
    Index > 0,
    List1 = [Head | Tail],
    Index1 is Index - 1,
    insert_elem(Index1, Tail, Elem, Tail2),
    List2 = [Head | Tail2].



delete_elem(0, [Elem | Tail], Elem, Tail).

delete_elem(Index, List, Elem, List2) :- 
    Index > 0,
    List = [Head | Tail],
    Index1 is Index - 1,
    delete_elem(Index1, Tail, Elem, Tail2),
    List2 = [Head | Tail2].

replace(List1, 0, Old, New, List2) :-
    List1 = [Old | Tail],
    List2 = [New | Tail].

replace(List1, Index, Old, New, List2) :-
    Index > 0,
    List1 = [Head | Tail],
    Index1 is Index - 1,
    replace(Tail, Index1, Old, New, Tail2),
    List2 = [Head | Tail2].



list_append([], List2, List2).
list_append(List1, List2, List3) :-
    List1 = [Head | Tail],
    list_append(Tail, List2, Tail2),
    List3 = [Head | Tail2].


list_member(Elem, [Elem | _]).

list_member(Elem, List) :-
    List = [_ | Tail],
    list_member(Elem, Tail).


list_last(List, Last) :-
    append(_, [Last], List).






list_nth(N, List, Elem) :-

    PrefixLength is N - 1,
    length(Prefix, PrefixLength),

    append(Prefix, [Elem | _], List).


list_append2(ListOfLists, List) :-
    list_append2(ListOfLists, List, []).

list_append2([], List, List).
list_append2([Head | Tail], List, Accumulator) :-
    list_append(Accumulator, Head, NewAccumulator),
    list_append2(Tail, List, NewAccumulator).


list_del(List, Elem, Result) :-
    append(Prefix, [Elem | Suffix], List),
    append(Prefix, Suffix, Res).


list_before(First, Second, List) :-
    append(_, [First|Rest], List), %# Procurar na lista o primeiro, i.e eu tive de adicionar a qualquer coisa uma lista que contém o primeiro elemento e o resto da lista para receber a lista em si
    append(_, [Second|_], Rest). %# Procurei no resto da lista se o segundo aparece em algum lugar, i.e eu tive de adicionar a alguma coisa uma lista que contém o segundo elemento e o fim da lista, para me dar o resto vindo da primeira pesquisa.



list_replace_one(X, Y, List1, List2) :-
    append(Prefix, [X|Suffix], List1), !,
    append(Prefix, [Y|Suffix], List2).


    
list_repeated(X,List) :-
    append(_, [X|Suffix], List),
    append(_, [X|_], Suffix).


list_slice(List1, Index, Size, List2) :-
    length(Prefix, Index), %# Defino que o prefixo tem tamanho index 
    append(Prefix, Rest, List1), %# Vou buscar o prefixo, assumindo que ele tem tamanho index.

    length(List2, Size), %# Defino que a lista 2 tem tamanho size
    append(List2, _, Rest). %# A lista 2 será o que resta da lista1 sem o sufixo (o rest) com o tamanho size.


addzeros(0, []).
addzeros(N, [0|Zeros]) :-
    N > 0,
    N1 is N - 1,
    addzeros(N1, Zeros).

list_shift_rotate(List, N, List2) :-
    length(Prefix, N),
    append(Prefix, Rest, List),
    append(Rest, Prefix, List2).


list_to(N, List) :-
    list_to(N, [], List).

list_to(0, Acc, Acc).
list_to(N, Acc, List) :-
    N > 0,
    N1 is N-1,
    list_to(N1, [N|Acc], List).


