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

intersections([This|Shapes], Acc, N, Accn, Nn) :- !,
    intersections(This, Shapes, Acc, N, Accx, Nx),
    intersections(Shapes, Accx, Nx, Accn, Nn).
intersections([], Acc, N, Acc, N) :- !.

intersections(NameA: A, [NameB: B|Shapes], Acc, N, Accn, Nn) :- !,
    (
        A intersect_with B -> Accx = [NameA-NameB|Acc], Nx is N + 1
        ; Accx = Acc, Nx is N
    ),
    intersections(NameA: A, Shapes, Accx, Nx, Accn, Nn).
intersections(_, [], Acc, N, Acc, N) :- !.



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
