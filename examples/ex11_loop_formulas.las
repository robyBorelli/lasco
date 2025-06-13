a :- not b.
c :- a, b.
e :- not a, not b.
b :- not a.
d :- b,c.

1 ~ c :- d.
1 ~ d :- a.

#pos({a,c,d},{b,e}).
#pos({b},{a,c,d,e}).
#neg({e},{}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% c :- d.
%%% d :- a.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%