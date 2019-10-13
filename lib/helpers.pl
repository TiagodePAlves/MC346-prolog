:- module(helpers, [
    named_intersection/3,
    as_shape/2,
    op(1050, xfy, ==>),
    (==>)/2
]).
:- use_module(intersections).


/** <module> Operações auxiliares
*/


%!  named_intersection(+NamedShape, +NamedShape, -NamedIntersection) is semidet.
%
%   Verdade se os dois NamedShape têm intersecção e
%   NamedIntersection é composto pelos nomes das figuras.
%
%   ==
%   ?- named_intersection(a: circle((0, 0), 1), b: square((1, 1), 1), Intersection).
%   Intersection = a-b.
%   ==
named_intersection(NameA: ShapeA, NameB: ShapeB, NameA - NameB) :-
    ShapeA intersect_with ShapeB.


%!  as_shape(?Object, ?NamedShape) is det.
%
%   Transforma Object em uma forma com nome e
%   vice-versa.
%
%   As transformações são de =circ= para =circle=
%   e =quad= para =square=.
%
%   ==
%   ?- as_shape(circ(a, 0, 0, 1), Shape).
%   Shape = a:circle((0, 0), 1).
%   ?- as_shape(Object, b:square((0, 0), 1)).
%   Object = quad(b, 0, 0, 1).
%   ==
as_shape(circ(Name, X, Y, R), Name: circle((X, Y), R)).
as_shape(quad(Name, X, Y, L), Name: square((X, Y), L)).


%!  :Condition ==> :Action is det.
%
%   Implicação lógica. Funciona como (->)/2 mas
%   continua sendo verdade se a condição é falsa.
Condition ==> Action :-
    \+ Condition, !
    ; Action.
