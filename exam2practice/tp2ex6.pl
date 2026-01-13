gcd(X,0,X) :- X > 0.
gcd(X,Y,G) :- 
    Y > 0,
    Remainder is X mod Y,
    gcd(Y, Remainder, G).


lcm(X, Y, M) :- 
    integer(X), X > 0,
    integer(Y), Y > 0,
    gcd(X, Y, G),
    M is (X * Y) // G.
    