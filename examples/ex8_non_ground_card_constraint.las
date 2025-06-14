num(0..3).
magic(3).

2 ~ :- magic(X), sad_num(X), num(X).
2 ~ 0{happy_num(X); sad_num(X)}2 :- num(X).

#pos({happy_num(2)}, {}).
#pos({happy_num(2), sad_num(2)}, {}).
#neg({sad_num(3)},{}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%%  :- num(X); sad_num(X); magic(X).
%%% 0 {happy_num(X);sad_num(X) } 2 :- num(X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%