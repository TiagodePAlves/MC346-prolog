:- use_module(library(intersections)).


%!  shape(+Name, +Shape)
%
%   Verdade se existe uma figura Shape associada
%   a Name.
%
%   Fato dinâmico.
:- dynamic(shape/2, [thread(local)]).

%!  circ(+Name, +X, +Y, +R) is det.
%
%   Registra um círculo no banco de dados do prolog.
%
%   ==
%   ?- shape(a, X).
%   ERROR: Undefined procedure: shape/2 (DWIM could not correct goal)
%   ?- circ(a, 0, 0, 1).
%   true.
%   ?- shape(a, X).
%   X = circle((0, 0), 1).
%   ==
%
%   @see asserta/1
circ(Name, X, Y, R) :-
    asserta(shape(Name, circle((X, Y), R))).

%!  quad(+Name, +X, +Y, +R) is det.
%
%   Registra um quadrado no banco de dados do prolog.
%
%   ==
%   ?- shape(a, X).
%   ERROR: Undefined procedure: shape/2 (DWIM could not correct goal)
%   ?- quad(a, 0, 0, 1).
%   true.
%   ?- shape(a, X).
%   X = aquare((0, 0), 1).
%   ==
%
%   @see asserta/1
quad(Name, X, Y, L) :-
    asserta(shape(Name, square((X, Y), L))).


%!  intersection(?NameA, ?NameB)
%
%   Verdade se existe uma figura com nome NameA
%   e outra com NameB e elas têm intersecção
%   não vazia.
intersection(A, B) :-
    shape(A, ShapeA), shape(B, ShapeB),
    ShapeA intersect_with ShapeB.

%!  ordered_intersection(?NameA, ?NameB)
%
%   Parecido com intersection/2, mas para ser verdade
%   também é preciso que NameA seja lexigraficamente
%   menor que NameB.
ordered_intersection(A, B) :-
    intersection(A, B), A @< B.


%!  database_solver(+Figures, -Intersections, -Length) is det.
%
%   Resolve o problema para as figuras dadas e
%   resolve o tamanho da solução também.
%
%   ==
%   ?- database_solver([circ(a, 0, 0, 1), quad(b, 1, 1, 1), circ(c, 2, -2, 1)], N, X).
%   N = 1,
%   X = [a-b].
%   ==

database_solver(Figures, Intersections, Length) :-
    database_solver(Figures, Intersections),
    aggregate_all(count, ordered_intersection(_X, _Y), Length).

%!  database_solver(+Figures, -Intersections) is det.
%
%   Resolve o problema para as figuras dadas, utilizando
%   o banco de fatos e regras de Prolog. Esta versão já
%   resolve o tamanho da solução também.
%
%   @see database_solver/2

database_solver(Figures, Intersections) :-
    maplist(call, Figures),
    findall(X-Y, ordered_intersection(X, Y), Intersections).
