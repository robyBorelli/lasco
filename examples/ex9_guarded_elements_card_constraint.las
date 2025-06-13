num(0..10).

color(red).
color(blue).
color(green).

2 ~ 1{ color_num(X,Y) : color(Y) }1 :- num(X).
2 ~ 1{ color_num(X,Y) : color(Y) }1 :- num(X), magic(X).
2 ~ 3{magic(1);  magic(2); magic(3)}3 .

#pos({color_num(1, red), color_num(2, blue), color_num(3, green)}, {}).
#pos({color_num(1, green), color_num(2, blue), color_num(3, red)}, {color_num(4, red), color_num(4, green), color_num(4, blue)}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% 1 {color_num(X, Y) : color(Y) } 1 :- magic(X); num(X).
%%% 3 {magic(1);magic(2);magic(3) } 3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%