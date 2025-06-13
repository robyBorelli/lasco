node(1..4).

1 ~ magic_pair((X,X)) :- node(X).
1 ~ magic_pair((X,X+1)) :- node(X).
1 ~ magic_triple((X,X,Z)) :- node(X), node(Z).
1 ~ magic_triple((X,Z,X+2)) :- node(X), node(Z).

#pos({magic_pair((2,2)), magic_triple((2,2,4))},{}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% magic_pair((X, X)) :- node(X).
%%% magic_triple((X, X, Z)) :- node(Z); node(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%