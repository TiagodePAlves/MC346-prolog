:- use_module(library(helpers)).


%!  shape_intersections(+NamedShapes, -Intersections) is det.
%
%   Encontra as intersecções entre todos os NamedShapes. Um
%   =NamedShape= é do tipo =Name:Shape= e um intersecção é
%   =Name-Name=.
%
%   ==
%   ?- shape_intersections([a:circle((0, 0), 1), b:square((1, 1), 1), c:circle((2, -2), 1)], X).
%   X = [a-b].
%   ==

shape_intersections([This|Shapes], Intersections) :-
    convlist(named_intersection(This), Shapes, ThisIntersections),
    shape_intersections(Shapes, RestIntersections),
    append(ThisIntersections, RestIntersections, Intersections).
shape_intersections([], []).


%!  list_solver(+Figures, -Length, -Intersections) is det.
%
%   Resolve o problema para as figuras dadas. Wrapper sobre
%   shape_intersections/2.
%
%   ==
%   ?- list_solver([circ(a, 0, 0, 1), quad(b, 1, 1, 1), circ(c, 2, -2, 1)], N, X).
%   N = 1,
%   X = [a-b].
%   ==

list_solver(Figures, Length, Intersections) :-
    maplist(as_shape, Figures, Shapes),
    shape_intersections(Shapes, Intersections),
    length(Intersections, Length).

%!  topo is semidet.
%
%   Resolve o problema com a entrada e saída padrão.
topo :-
    read(Figures),
    list_solver(Figures, Length, Intersections),
    writeln(Length),
    maplist(writeln, Intersections).
