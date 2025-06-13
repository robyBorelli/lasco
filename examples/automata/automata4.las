%% run:
%%%% ILASP --version=2  --override-default-sm  ilasp.las
%% check hypothesis space: 
%%%% ILASP --override-default-sm -s ilasp.las

%% HYPOTHESIS SPACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% #bias("
%% states(0..9).      %% defines the maximum number of states
%% char(a). char(b).
%% 1 { head(state(S)) : states(S) ; head(final(S)) : states(S) ; head(delta(S,C,S1)) : states(S), states(S1), char(C) } 1.
%% ").

1 ~ delta(2,b,5).
1 ~ delta(9,b,3).
1 ~ delta(0,b,4).
1 ~ delta(1,b,4).
1 ~ delta(2,b,4).
1 ~ delta(3,b,4).
1 ~ delta(4,b,4).
1 ~ delta(5,b,4).
1 ~ delta(6,b,4).
1 ~ delta(7,b,4).
1 ~ delta(8,b,4).
1 ~ delta(9,b,4).
1 ~ delta(0,b,5).
1 ~ delta(1,b,5).
1 ~ delta(8,b,3).
1 ~ delta(3,b,5).
1 ~ delta(4,b,5).
1 ~ delta(5,b,5).
1 ~ delta(6,b,5).
1 ~ delta(7,b,5).
1 ~ delta(8,b,5).
1 ~ delta(9,b,5).
1 ~ delta(0,b,6).
1 ~ delta(1,b,6).
1 ~ delta(2,b,6).
1 ~ delta(3,b,6).
1 ~ delta(4,b,6).
1 ~ delta(5,b,6).
1 ~ delta(5,b,2).
1 ~ delta(2,b,1).
1 ~ delta(3,b,1).
1 ~ delta(4,b,1).
1 ~ delta(5,b,1).
1 ~ delta(6,b,1).
1 ~ delta(7,b,1).
1 ~ delta(8,b,1).
1 ~ delta(9,b,1).
1 ~ delta(0,b,2).
1 ~ delta(1,b,2).
1 ~ delta(2,b,2).
1 ~ delta(3,b,2).
1 ~ delta(4,b,2).
1 ~ delta(6,b,6).
1 ~ delta(6,b,2).
1 ~ delta(7,b,2).
1 ~ delta(8,b,2).
1 ~ delta(9,b,2).
1 ~ delta(0,b,3).
1 ~ delta(1,b,3).
1 ~ delta(2,b,3).
1 ~ delta(3,b,3).
1 ~ delta(4,b,3).
1 ~ delta(5,b,3).
1 ~ delta(6,b,3).
1 ~ delta(7,b,3).
1 ~ final(7).
1 ~ delta(4,b,9).
1 ~ delta(5,b,9).
1 ~ delta(6,b,9).
1 ~ delta(7,b,9).
1 ~ delta(8,b,9).
1 ~ delta(9,b,9).
1 ~ final(0).
1 ~ final(1).
1 ~ final(2).
1 ~ final(3).
1 ~ final(4).
1 ~ final(5).
1 ~ final(6).
1 ~ delta(3,b,9).
1 ~ final(8).
1 ~ final(9).
1 ~ state(0).
1 ~ state(1).
1 ~ state(2).
1 ~ state(3).
1 ~ state(4).
1 ~ state(5).
1 ~ state(6).
1 ~ state(7).
1 ~ state(9).
1 ~ state(8).
1 ~ delta(1,a,0).
1 ~ delta(0,b,8).
1 ~ delta(7,b,6).
1 ~ delta(8,b,6).
1 ~ delta(9,b,6).
1 ~ delta(0,b,7).
1 ~ delta(1,b,7).
1 ~ delta(2,b,7).
1 ~ delta(3,b,7).
1 ~ delta(4,b,7).
1 ~ delta(5,b,7).
1 ~ delta(6,b,7).
1 ~ delta(7,b,7).
1 ~ delta(8,b,7).
1 ~ delta(9,b,7).
1 ~ delta(1,b,1).
1 ~ delta(1,b,8).
1 ~ delta(2,b,8).
1 ~ delta(3,b,8).
1 ~ delta(4,b,8).
1 ~ delta(5,b,8).
1 ~ delta(6,b,8).
1 ~ delta(7,b,8).
1 ~ delta(8,b,8).
1 ~ delta(9,b,8).
1 ~ delta(0,b,9).
1 ~ delta(1,b,9).
1 ~ delta(2,b,9).
1 ~ delta(2,a,4).
1 ~ delta(9,a,2).
1 ~ delta(0,a,3).
1 ~ delta(1,a,3).
1 ~ delta(2,a,3).
1 ~ delta(3,a,3).
1 ~ delta(4,a,3).
1 ~ delta(5,a,3).
1 ~ delta(6,a,3).
1 ~ delta(7,a,3).
1 ~ delta(8,a,3).
1 ~ delta(9,a,3).
1 ~ delta(0,a,4).
1 ~ delta(1,a,4).
1 ~ delta(8,a,2).
1 ~ delta(3,a,4).
1 ~ delta(4,a,4).
1 ~ delta(5,a,4).
1 ~ delta(6,a,4).
1 ~ delta(7,a,4).
1 ~ delta(8,a,4).
1 ~ delta(9,a,4).
1 ~ delta(0,a,5).
1 ~ delta(1,a,5).
1 ~ delta(2,a,5).
1 ~ delta(3,a,5).
1 ~ delta(4,a,5).
1 ~ delta(5,a,5).
1 ~ delta(5,a,1).
1 ~ delta(2,a,0).
1 ~ delta(3,a,0).
1 ~ delta(4,a,0).
1 ~ delta(5,a,0).
1 ~ delta(6,a,0).
1 ~ delta(7,a,0).
1 ~ delta(8,a,0).
1 ~ delta(9,a,0).
1 ~ delta(0,a,1).
1 ~ delta(1,a,1).
1 ~ delta(2,a,1).
1 ~ delta(3,a,1).
1 ~ delta(4,a,1).
1 ~ delta(6,a,5).
1 ~ delta(6,a,1).
1 ~ delta(7,a,1).
1 ~ delta(8,a,1).
1 ~ delta(9,a,1).
1 ~ delta(0,a,2).
1 ~ delta(1,a,2).
1 ~ delta(2,a,2).
1 ~ delta(3,a,2).
1 ~ delta(4,a,2).
1 ~ delta(5,a,2).
1 ~ delta(6,a,2).
1 ~ delta(7,a,2).
1 ~ delta(7,a,9).
1 ~ delta(4,a,8).
1 ~ delta(5,a,8).
1 ~ delta(6,a,8).
1 ~ delta(7,a,8).
1 ~ delta(8,a,8).
1 ~ delta(9,a,8).
1 ~ delta(0,a,9).
1 ~ delta(1,a,9).
1 ~ delta(2,a,9).
1 ~ delta(3,a,9).
1 ~ delta(4,a,9).
1 ~ delta(5,a,9).
1 ~ delta(6,a,9).
1 ~ delta(3,a,8).
1 ~ delta(8,a,9).
1 ~ delta(9,a,9).
1 ~ delta(0,b,0).
1 ~ delta(1,b,0).
1 ~ delta(2,b,0).
1 ~ delta(3,b,0).
1 ~ delta(4,b,0).
1 ~ delta(5,b,0).
1 ~ delta(6,b,0).
1 ~ delta(7,b,0).
1 ~ delta(8,b,0).
1 ~ delta(9,b,0).
1 ~ delta(0,b,1).
1 ~ delta(0,a,7).
1 ~ delta(7,a,5).
1 ~ delta(8,a,5).
1 ~ delta(9,a,5).
1 ~ delta(0,a,6).
1 ~ delta(1,a,6).
1 ~ delta(2,a,6).
1 ~ delta(3,a,6).
1 ~ delta(4,a,6).
1 ~ delta(5,a,6).
1 ~ delta(6,a,6).
1 ~ delta(7,a,6).
1 ~ delta(8,a,6).
1 ~ delta(9,a,6).
1 ~ delta(0,a,0).
1 ~ delta(1,a,7).
1 ~ delta(2,a,7).
1 ~ delta(3,a,7).
1 ~ delta(4,a,7).
1 ~ delta(5,a,7).
1 ~ delta(6,a,7).
1 ~ delta(7,a,7).
1 ~ delta(8,a,7).
1 ~ delta(9,a,7).
1 ~ delta(0,a,8).
1 ~ delta(1,a,8).
1 ~ delta(2,a,8).

%% BACKGROUND KNOWLEDGE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% alphabet and initial state
states(0..9).
char(a). char(b).
initial(0).

%% run definition
run(0,S) :- initial(S), state(S).
run(T,S1) :- word(T,C), run(T-1,S), delta(S,C,S1),
             state(S), state(S1), time(T), char(C). 

%% run is a function
:- run(T,S), run(T,S2), S!=S2, state(S), state(S2), time(T).

%% delta is a function
:- delta(S,C,S1), delta(S,C,S2), S1!=S2, char(C), state(S), state(S1), state(S2).

%% reachability predicate
reachable(0).
reachable(S1) :- reachable(S), delta(S,C,S1), state(S), state(S1), char(C).

%% the automaton is complete on reachable states
complete(S,C) :- reachable(S), char(C), delta(S,C,S1), state(S1).
:- not complete(S,C), state(S), reachable(S), char(C).

%% accepting conditions
accepted :- run(L,S), state(S), final(S), length(L).
rejected :- not accepted.

%% search pruning
:- state(S), not state(S1), states(S), states(S1), S1 < S.

%% EXAMPLES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% example 1 (ab)^*
#pos({},{},{:- accepted. length(1). time(1). word(1,a).}).  % "a"
#pos({},{},{:- accepted. length(1). time(1). word(1,b).}).  % "b"
%% #pos({},{},{:- accepted. length(2). time(1..2). word(1,b). word(2,a).}).  % "ba"
%% #pos({},{},{:- accepted. length(2). time(1..2). word(1,a). word(2,a).}).  % "aa"
%% #pos({},{},{:- accepted. length(2). time(1..2). word(1,b). word(2,b).}).  % "bb"
%% #pos({},{},{:- rejected. length(2). time(1..2). word(1,a). word(2,b).}).  % "ab"

%% non può esserci alcuna parola lunga 10 in cui in quinta posizione ci sia la a e che venga rifiutata
#neg({},{},{:- accepted. length(2). time(1..2). word(2,a). }).