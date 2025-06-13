num(0..10).

1 ~ sum(X,Y,Z) :- num(X), num(Y), num(Z), (X + Y) == Z.
1 ~ sum(X,Y,Z) :- num(X), num(Y), num(Z+2), (X + Y) == (Z+2).
1 ~ mul(X,Y,Z) :- num(X), num(Y), num(Z), (X * Y) == Z.
1 ~ mul(X,Y,Z) :- num(X), num(Y), num(Z+2), (X * Y) == (Z+2).
1 ~ sub(X,Y,Z) :- num(X), num(Y), num(Z), (X - Y) == Z.
1 ~ sub(X,Y,Z) :- num(X), num(Y), num(Z+2), (X - Y) == (Z+2).
1 ~ div(X,Y,Z) :- num(X), num(Y), num(Z), (X / Y) == Z.
1 ~ div(X,Y,Z) :- num(X), num(Y), num(Z+2), (X / Y) == (Z+2).

#pos({sum(2,3,5), mul(2,3,6), sub(4,3,1)},{}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% sum(X, Y, Z) :- (X+Y) = Z; num(Z); num(Y); num(X).
%%% mul(X, Y, Z) :- (X*Y) = Z; num(Z); num(Y); num(X).
%%% sub(X, Y, Z) :- (X-Y) = Z; num(Z); num(Y); num(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%