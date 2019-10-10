:- use_module(library(intersections)).

% shape(_, _) :- fail.

circ(Name, X, Y, R) :-
    asserta(shape(Name, circle((X, Y), R))).

quad(Name, X, Y, L) :-
    asserta(shape(Name, square((X, Y), L))).


intersection(A, B) :-
    shape(A, ShapeA), shape(B, ShapeB),
    ShapeA intersect_with ShapeB.

ordered_intersection(A, B) :-
    intersection(A, B), A @< B.

count(Goal, Count) :-
    aggregate_all(count, Goal, Count).


database_solver(Figures, Length, Intersections) :-
    maplist(call, Figures),
    count(ordered_intersection(X, Y), Length),
    findall(X-Y, ordered_intersection(X, Y), Intersections).
