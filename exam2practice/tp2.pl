r(a, b).
r(a, d).
r(b, a).
r(a, c).

%c(b, c).
%c(b, d).
%c(c, c).
%c(d, e).

% EX1
% i. x=a, y=b, z=c
%    x=a, y=d, z=e
%    x=a, y=c, z=c
% ii. y=c, x=a
%
% iii. x=a, y=c
%

% EX2

pairs(X, Y) :- d(X), q(Y).

pairs(X, Y) :- u(X).

u(1).
d(2).
d(4).
q(4).
q(16).

% EX3

a(a1, 1).
a(A2, 2).
a(a3, N).

b(1, b1).
b(2, B2).
b(N, b3).

c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).


% EX4


factorial(0,1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

sum_rec(0,0).

sum_rec(N, SUM) :-
    N > 0,
    N1 is N - 1,
    sum_rec(N1, SUM1),
    SUM is N + SUM1.

power_rec(_, 0, 1).
power_rec(X, 1, X).

power_rec(X, Y, Z) :-
    Y > 1,
    Y1 is Y - 1,
    power_rec(X, Y1, Z1),
    Z is X * Z1.



square_rec(0,0).

square_rec(N, S) :-
    N > 0,
    N1 is N - 1,
    square_rec(N1, S1),
    S is S1 + 2*N - 1.


fibonnaci(0,0).
fibonacci(1,1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N1, F2),
    F is F1 + F2.



collatz(1, 0).
collatz(N, S) :-
    N > 1,
    (N mod 2 =:= 0 -> Next is N // 2; Next is 3 * N + 1),
    collatz(Next, S1),
    S is S1 + 1.



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
    

tailfactorial(N, S) :- 
    tailfactorial(N, S, 1).

tailfactorial(0, S, S).
tailfactorial(N, S, Accumulator) :- 
    N > 0,
    N1 is N - 1,
    Acc1 is Accumulator * N,
    tailfactorial(N1, S, Acc1).


tailsumrec(N, S) :-
    tailsumrec(N, S, 0).

tailsumrec(0, S, S).

tailsumrec(N,S,Accumulator) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Accumulator + N,
    tailsumrec(N1, S, Acc1).


tailpowrec(N, S, P) :- 
    tailpowrec(N, S, P, 1).

tailpowrec(_,0,P,P).

tailpowrec(N,S,P,Accumulator) :-
    S > 0,
    S1 is S - 1,
    Acc1 is Accumulator * N,
    tailpowrec(N,S1,P,Acc1).

tailsquarerec(N,S) :- 
    tailsquarerec(N,S,0).

tailsquarerec(0,S,S).

tailsquarerec(N,S,Accumulator) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Accumulator + 2*N - 1,
    tailsquarerec(N1,S,Acc1).


tailfibonacci(N,F) :-
    tailfibonacci(N, 0, 1, F).

tailfibonacci(0, A, _, A).
tailfibonacci(1, _, B, B).

tailfibonacci(N, A, B, F) :-
    N > 1,
    N1 is N - 1,
    NextA is B,
    NextB is A + B,
    tailfibonacci(N1, NextA, NextB, F).










    