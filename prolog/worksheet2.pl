% i. (a,b,c) (a,b,d), (a,d,e), (a,c,c)
% ii. (c,c,a)
% iii. (a,c,c)
%
%
%
%
%
%
%
%
%
%
%
%

r(a,b).
r(a,d).
r(b,a).
r(a,c).

s(b,c).
s(b,d).
s(c,c).
s(d,e).

a(a1, 1).
a(A2, 2).
a(a3, N).
b(1, b1).
b(2, B2).
b(N, b3).
c(X, Y):- a(X, Z), b(Z, Y).
d(X, Y):- a(X, Z), b(Y, Z).
d(X, Y):- a(Z, X), b(Z, Y).

factorial(0, 1).
factorial(N, F) :- N > 0,
                N1 is N - 1,
                factorial(N1, F1),
                F is N * F1.

