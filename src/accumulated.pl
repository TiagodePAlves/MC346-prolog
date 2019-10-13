:- use_module([library(helpers), library(intersections)]).


%!  intersections(+NamedShapes, -Intersections, -Length) is det.
%
%   Encontra as intersecções entre todos os NamedShapes. Um
%   =NamedShape= é do tipo =Name:Shape= e um intersecção é
%   =Name-Name=.
%
%   ==
%   ?- intersections([a:circle((0, 0), 1), b:square((1, 1), 1), c:circle((2, -2), 1)], X, N).
%   X = [a-b],
%   N = 1.
%   ==
intersections(Shapes, Intersections, Length) :- !,
    intersections(Shapes, [], 0, Intersections, Length).

%!  intersections(+Shapes, +Acc, +AccLength, -Intersections, -IntersectionsLength) is det.
intersections([This|Shapes], Acc, N, AccF, NF) :- !,
    intersections(This, Shapes, Acc, N, Accx, Nx),
    intersections(Shapes, Accx, Nx, AccF, NF).
intersections([], Acc, N, Acc, N) :- !.

%!  intersections(+Shape, +Shapes, +Acc, +AccLength, -Intersections, -IntersectionsLength) is det.
intersections(This, [Other|Shapes], Acc, N, AccF, NF) :- !,
    intersections(This, Other, Acc, N, Accx, Nx),
    intersections(This, Shapes, Accx, Nx, AccF, NF).
intersections(_, [], Acc, N, Acc, N) :- !.

%!  intersections(+Shape, +Shape, +Acc, +AccLength, -Intersections, -IntersectionsLength) is det.
intersections(NameA: A, NameB: B, Acc, N, [NameA-NameB|Acc], NF) :-
    A intersect_with B, !, NF is N + 1.
intersections(_, _, Acc, N, Acc, N) :- !.



%!  accumulated_solver(+Figures, -Intersections, -Length) is det.
%
%   Resolve o problema para as figuras dadas, junto com
%   o tamanho. Wrapper sobre intersections/3.
%
%   ==
%   ?- accumulated_solver([circ(a, 0, 0, 1), quad(b, 1, 1, 1), circ(c, 2, -2, 1)], N, X).
%   N = 1,
%   X = [a-b].
%   ==
accumulated_solver(Figures, Intersections, Length) :-
    maplist(as_shape, Figures, Shapes),
    intersections(Shapes, Intersections, Length).

%!  accumulated_solver(+Figures, -Intersections) is det.
%
%   Resolve o problema para as figuras dadas.
%
%   @see accumulated_solver/3
accumulated_solver(Figures, Intersections) :-
    accumulated_solver(Figures, Intersections, _).
