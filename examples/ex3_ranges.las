 edge(1..3).

1 ~ path(1) :- edge(1).
1 ~ path(2) :- edge(2).
1 ~ path(3) :- edge(3).
1 ~ path(5) :- edge(3).
1 ~ path(4) :- edge(4).
1 ~ edge(1..5).

#pos({path(5)},{}).
#neg({path(4)},{}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% path(5) :- edge(3).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%