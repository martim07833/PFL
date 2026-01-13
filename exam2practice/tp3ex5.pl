list_from_to(Inf, Sup, List) :-
    list_from_to(Inf, Sup, List, []).

list_from_to(Inf,Inf,[Inf|List],List).

list_from_to(Inf, Sup, List, Accumulator) :-
    Sup >= Inf,
    Sup1 is Sup - 1,
    list_from_to(Inf, Sup1, List, [Sup | Accumulator]).

list_from_to_step(Inf, Sup, Step, List) :-
    list_from_to_step(Inf, Sup, Step, List, []).

list_from_to_step(Inf, Inf, _, [Inf| List], List).

list_from_to_step(Inf, Sup, Step, List, Accumulator) :-
    Sup >= Inf,
    Sup1 is Sup - Step,
    list_from_to_step(Inf, Sup1, Step, List, [Sup | Accumulator]).


isprime(2).
isprime(3).
isprime(X) :-
    X > 3,
    X mod 2 =\= 0,
    \+ hasfactor(X, 3).

hasfactor(X, Factor) :-
    Factor * Factor =< X,
    (X mod Factor =:= 0;
    NextFactor is Factor + 2,
    hasfactor(X, NextFactor)
    ).


primes(N, List) :-
    primes(N, List, []).

primes(1, List, List).


    
primes(N, List, Accumulator) :-
    N > 1,
    N1 is N - 1,
    \+ isprime(N1),
    primes(N1, List, Accumulator).

primes(N, List, Accumulator) :-
    N > 1,
    N1 is N - 1,
    isprime(N1),
    primes(N1, List, [N1 | Accumulator]).



fibonnaci(0,0).
fibonacci(1,1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.

fibs(N, List) :-
    integer(N), N >= 0,
    numlist(0, N, Orders),
    maplist(fib, Orders, List).


