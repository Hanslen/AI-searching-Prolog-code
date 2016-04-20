% bfs(+initial_state, ?goal_state, ?solution)
bfs(X,Y,P) :-
    bfs_b(Y, [n(X,[])],[],R),
    reverse(R,P).

% bfs_b(?goal_state, +open_list, +closed_list, -reversed_solution)
bfs_b(Y,[n(Y,P)|_],_,P).
bfs_b(Y,[n(S,P1)|Ns],C,P) :-
    findall(n(S1,[A|P1]), (s(S,S1,A), \+ member(S1,C)),Es), 
    append(Ns,Es,O),
    bfs_b(Y,O,[S|C],P).

%% s(currentPosition, nextPosition, s(ction), go(ction))
%% s([a|_], [b|_], go(a,b)).
%% s([a|_], [c|_], go(a,c)).
%% s([b|_], [d|_], go(b,d)).
%% s([b|_], [e|_], go(b,e)).

s(b1, b2, go(b1, b2)).
s(b1, t, go(b1, t)).
s(b2, b1, go(b2, b1)).
s(b2, b3, go(b2, b3)).
s(b2, t, go(b2, t)).
s(b3, b2, go(b3, b2)).
s(b3, t, go(b3, t)).
s(a1, a2, go(a1, a2)).
s(a1, t, go(a1, t)).
s(a2, a1, go(a2, a1)).
s(a2, a3, go(a2, a3)).
s(a2, t, go(a2, t)).
s(a3, a2, go(a3, a2)).
s(a3, t, go(a3, t)).
s(t, b1, go(t, b1)).
s(t, b2, go(t, b2)).
s(t, b3, go(t, b3)).
s(t, a1, go(t, a1)).
s(t, a2, go(t, a2)).
s(t, a3, go(t, a3)).