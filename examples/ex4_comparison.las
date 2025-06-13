num(1..5).

1 ~ greater(2,1) :- num(1), num(2), 2 > 1. 
1 ~ smaller(1,2) :- num(1), num(2), 1 < 2. 
1 ~ greater(1,2) :- num(1), num(2), 2 > 1. 
1 ~ smaller(2,1) :- num(1), num(2), 1 < 2. 

1 ~ greater_eq(1,1) :- num(1), num(1), 1 >= 1. 
1 ~ smaller_eq(1,1) :- num(1), num(1), 1 <= 1. 
1 ~ greater_eq(1,2) :- num(1), num(2), 2 >= 1. 
1 ~ smaller_eq(2,1) :- num(2), num(1), 1 <= 2. 

1 ~ eq(1,1) :- num(1), num(1), 1 = 1. 
1 ~ eq(2,2) :- num(2), num(2), 2 == 2. 
1 ~ neq(2,1) :- num(2), num(1), 2 != 1. 
1 ~ neq(1,1) :- num(1), num(1), 1 = 1. 
1 ~ neq(2,2) :- num(2), num(2), 2 == 2. 
1 ~ eq(2,1) :- num(2), num(1), 2 != 1. 

#neg({},{greater(2,1)}).
#neg({},{smaller(1,2)}).
#neg({},{greater_eq(1,1)}).
#neg({},{smaller_eq(1,1)}).
#neg({},{eq(1,1)}).
#neg({},{eq(2,2)}).
#neg({},{neq(2,1)}).

#neg({greater(1,2)},{}).
#neg({smaller(2,1)},{}).
#neg({greater_eq(1,2)},{}).
#neg({smaller_eq(2,1)},{}).
#neg({neq(1,1)},{}).
#neg({neq(2,2)},{}).
#neg({eq(2,1)},{}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OPTIMUM SOLUTION %%%%%%%%
%%% greater(2, 1) :- 2 > 1; num(2); num(1).
%%% smaller(1, 2) :- 1 < 2; num(2); num(1).
%%% greater_eq(1, 1) :- 1 >= 1; num(1); num(1).
%%% smaller_eq(1, 1) :- 1 <= 1; num(1); num(1).
%%% eq(1, 1) :- 1 = 1; num(1); num(1).
%%% eq(2, 2) :- 2 = 2; num(2); num(2).
%%% neq(2, 1) :- 2 != 1; num(1); num(2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%