:- use_module(library(lists)).

% gsppg(+State,+Goals,-Plan): Plan is a sequence of operators
% that applied in State achieves Goals
gsppg(S,Gs,P) :-
    list_to_set(S,SS),
    list_to_set(Gs,GS),
    gsppg(SS,GS,[],P,_).

% gsppg(+State,+Goals,+ProtectedGoals,-Plan,-NewState): Plan is a
% sequence of operators that applied in State achieves Goals without
% clobbering any goal in ProtectedGoals and results in NewState
gsppg(S,Gs,_,[],S) :- holds(Gs,S).
gsppg(S,Gs,PGs,P,S3) :-
    select_goal(S,Gs,G),
    pop(O,Pre,A,D),
    member(G,A),
    intersection(D,PGs,[]),
    intersection(Pre,PGs,[]),	% avoid infinite regress
    gsppg(S,Pre,[G|PGs],P1,S1),
    apply(S1,A,D,S2),
    gsppg(S2,Gs,[G|PGs],P2,S3),
    append(P1,[O|P2],P).

% holds(+Goal,+State): Goal is achieved in State. As operators are
% ground, we can use member/2 rather than select/3.
holds([],_).
holds([G|Gs],S) :- member(G,S), holds(Gs,S).

% select_goal(State,+Goals,+Goal): Goal is goal in Goals not achieved in
% State
select_goal(S,Gs,G) :-
    member(G,Gs), \+ member(G,S).

apply(S,A,D,S1) :-
    subtract(S,D,S2), union(S2,A,S1).

%% pop(move_to_table(X),
%%     [clear(X),on(X,Y)],
%%     [on(X,table),clear(Y)],
%%     [on(X,Y)]) :-
%%     block(X), block(Y), X \= Y.
%% pop(move(X,Y),
%%     [clear(X),clear(Y),on(X,Z)],
%%     [on(X,Y),clear(Z)],
%%     [on(X,Z),clear(Y)]) :-
%%     block(X), block(Y), block_or_table(Z), X \= Y, X \= Z, Y \= Z.

%% block(a).
%% block(b).
%% block(c).

%% block_or_table(X) :- block(X).
%% block_or_table(table).
pop(grab(X, Y),
    [armClear, in(X,Y),over(Y)],
    [holding(X)],
    [armClear, in(X, Y)]):-bin(Y),object(X).
pop(drop(X,Y),
    [holding(X),over(Y)],
    [armClear, in(X,Y)],
    [holding(X)]):-bin(Y),object(X).
pop(move_to(X),
    [over(Y)],
    [over(X)],
    [over(Y)]):- bin(X), bin(Y), X \= Y.


%% pop(move_to(X),
%%     [bin(X),bin(Y),over(Y)],
%%     [over(X)],
%%     [over(Y)]).
%% pop(grab(X,Y),
%%     [armClear,in(X,Y),over(Y)],
%%     [holding(X)],
%%     [armClear,in(X,Y)]).
%% pop(drop(X,Y),
%%     [holding(X), over(Y)],
%%     [armClear, in(X,Y)],
%%     [holding(X)]).

bin(b1).
bin(b2).
object(o1).
object(o2).
object(o3).
object(o4).


initial_state([on(b,table),on(c,a),on(a,table),clear(b),clear(c)]).
goal_state([on(a,b),on(b,c),on(c,table)]).

