s(1).
s(2) :- !.
s(3).

data(one).
data(two).
data(three).

cut_test_a(X) :- data(X).
cut_test_a('five').

cut_test_b(X) :- data(X), !.
cut_test_b('five').

cut_test_c(X, Y) :- data(X), !, data(Y).
cut_test_c('five', 'five').


immature(X) :- adult(X), !, fail.
immature(_X).
adult(X) :- person(X), !, age(X, N), N >= 18.
adult(X) :- turtle(X), !, age(X, N), N >= 50.
adult(X) :- spider(X), !, age(X, N), N >= 1.
adult(X) :- bat(X), !, age(X, N), N >= 5.

max(A, B, C, Max) :-
    A >= B,
    A >= C,
    Max = A,
    !.

max(A, B, C, Max) :-
    B >= A,
    B >= C, 
    Max = B,
    !.

max(A,B,C, Max) :-
    C >= A,
    C >= B,
    Max = C,
    !.

print_n(0, _) :-
    !.


print_n(N, S) :-
    N > 0,
    N1 is N - 1,
    print_n(N1, S),
    write(S).



print_space(0) :-
    !.

print_space(Padding) :-
    Padding > 0,
    Padding1 is Padding - 1,
    write(' '),
    print_space(Padding1).

print_text(Text, Symbol, Padding) :-
    atom_codes(Text, TextCodes),
    write(Symbol),
    print_space(Padding),
    write(Text),
    print_space(Padding),
    write(Symbol).



print_banner(Text, Symbol, Padding) :-
    atom_length(Text, StringLength),
    LineLength is StringLength + 2 + Padding*2,

    print_n(LineLength, Symbol),
    nl,

    write(Symbol),
    print_space(LineLength - 2),
    write(Symbol),
    nl,

    write(Symbol),
    print_space(Padding),
    write(Text),
    print_space(Padding),
    write(Symbol),
    nl,

    write(Symbol),
    print_space(LineLength - 2),
    write(Symbol),
    nl,

    print_n(LineLength, Symbol).
    



read_number(X) :-
    read_digits(Digits, []),
    number_codes(X, Digits).

read_digits(Digits, Accumulator) :-
    peek_code(Code),
    (is_digit(Code) -> get_code(Code),
    Acc1 = [Code|Accumulator],
    read_digits(Digits, Acc1);

    (Code == 10; Code == -1) -> reverse(Accumulator, Digits);

    get_code(_),

    read_digits(Digits, Accumulator)
    ).


is_digit(Code) :- 
    Code >= 48,
    Code =< 57.

reverse(List, Reversed) :-
    reverse(List, [], Reversed).

reverse([], Accumulator, Accumulator).
reverse([H|T], Accumulator, Reversed) :-
    reverse(T, [H|Accumulator], Reversed).
    

read_until_between(Min, Max, Value) :-
    read_number(Value),
    Value >= Min,
    Value =< Max.

read_string(Atom) :-
    read_chars(Chars, []),
    atom_codes(Atom, Chars).
    

read_chars(Chars, Accumulator) :-
    peek_code(Code),
    (
        Code == 10 -> get_code(10), 
        reverse(Accumulator, Chars);
        Code == -1 -> reverse(Accumulator, Chars);
        get_code(Code),
        Acc1 = [Code|Accumulator],
        read_chars(Chars, Acc1)
    ).


banner :-
    write('Enter your text here: '), nl,
    read_string(Text),
    write('Enter a symbol character: '), nl,
    read_string(SymbolCode),
    SymbolCode = [SymbolChar|_],
    write('Enter padding: '), nl,
    read_number(Padding),
    print_banner(Text, SymbolChar, Padding).