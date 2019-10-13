:- use_module(library(intersections)).


%!  shape(+Name, +Shape)
%
%   Verdade se existe uma figura Shape associada
%   a Name.
:- dynamic(shape/2, [thread(local)]).

%!  insert_shape(+Name, +Shape)
%
%   Insere a figura no banco de dados e suas intersecções.
insert_shape(Name, Shape) :-
    asserta(shape(Name, Shape)).

%!  insert_shape(+NamedShape)
%
%   Funciona como insert_shape/2, mas quebra o NamedShape antes.
insert_shape(circ(Name, X, Y, R)) :-
    insert_shape(Name, circle((X, Y), R)).
insert_shape(quad(Name, X, Y, R)) :-
    insert_shape(Name, square((X, Y), R)).


%!  intersection(?NameA, ?NameB)
%
%   Verdade se existe uma figura com nome NameA
%   e outra com NameB e elas têm intersecção
%   não vazia.
intersection(A, B) :-
    shape(A, ShapeA), shape(B, ShapeB), A @< B,
    ShapeA intersect_with ShapeB.


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
    aggregate_all(count, intersection(_X, _Y), Length).

%!  database_solver(+Figures, -Intersections) is det.
%
%   Resolve o problema para as figuras dadas, utilizando
%   o banco de fatos e regras de Prolog. Esta versão já
%   resolve o tamanho da solução também.
%
%   @see database_solver/2
database_solver(Figures, Intersections) :-
    maplist(insert_shape, Figures),
    findall(X-Y, intersection(X, Y), Intersections).
