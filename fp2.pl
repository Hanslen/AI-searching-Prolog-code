:- use_module(library(lists)).
:- use_module(library(ordsets)).

% fp(+State,+Goals,-Plan): Plan is a sequence of operators that
% applied in State achieves Goals
fp(S,G,P) :-
    list_to_ord_set(S,OS),
    list_to_set(G,GS),
    fp(OS,GS,[OS],P).

fp(S,G,_,[]) :- holds(G,S).
fp(S,G,C,[O|P]) :-
    pop(O,Pre,A,D),
    holds(Pre,S),
    apply(S,A,D,S1),
    \+ member(S1,C),
    fp(S1,G,[S1|C],P).

% holds(+Goals,+State): the goals (or preconditions) Goals hold in State
holds([],_).
holds([Pre|Ps],S) :- select(Pre,S,S1), holds(Ps,S1).

% apply(+State,+AddList,+DeleteList,-NewState): NewState is the result
% of updating State with AddList and DeleteList. Note that all
% parameters must be ordered.
apply(S,A,D,S1) :-
    ord_subtract(S,D,S2),
    ord_union(A,S2,S1).

% Note: requires conditions to be ordered.
pop(move(X,Y),
    [block(X),block(Y),clear(X),clear(Y),on(X,Z)],
    [clear(Z),on(X,Y)],
    [clear(Y),on(X,Z)]).

pop(move_to_table(X),
    [block(X),block(Y),clear(X),on(X,Y)],
    [clear(Y),on(X,table)],
    [on(X,Y)]).

% Testing. Note that while the initial state doesn't need to be ordered,
% it should be 'complete', i.e., all fluents derivable from a sequence
% of operator applications that returns to the "same" initial state must
% be present.
initial_state([block(a),block(b),block(c),
	       clear(b),clear(c),clear(table),
	       on(a,table),on(b,table),on(c,a)]).
goal_state([on(a,b),on(b,c),on(c,table)]).
