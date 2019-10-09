:- module(intersections, [
    op(800, xfx, intersect),
    intersect/2
]).
:- use_module(points).

/** <module> Intersecção de Figuras Geométricas

Neste módulo está definido algumas regras de
intersecção de círculos e quadrados.
*/


%!  intersect(+Shape, +Shape) is det.
%
%   Verdade se as formas geométricas
%   bidimensionais têm intersecção não vazia.
%
%   ==
%   ?- circle((0, 0), 1) intersect square((1, 1), 1).
%   true.
%   ==
%
%   As únicas formas implementadas são círculo,
%   definido por =|circle(Center, Radius)|=, e
%   quadrado, =|square(Center, SideLength)|=.

circle(C0, R0) intersect circle(C1, R1) :- !,
    % intersecção de círculos ocorre quando a
    % distância entre eles é menor ou igual a
    % soma dos raios
    Dist as distance(C0, C1),
    Dist =< R0 + R1.

square(C0, L0) intersect square(C1, L1) :- !,
    % quadrados são como círculos em norma infinita,
    % em que o raio é metade do lado, então a regra
    % de intersecção é parecida com a de círculos
    Dist as distance(inf, C0, C1),
    Dist =< (L0 + L1)/2.

circle(CC, R) intersect square(CQ, L) :-
    % intersecção círculo e quadrado pode ser
    % quando o círculo está dentro do quadrado
    % e distância uniforme entre eles é menor
    % ou igual a metado do lado
    Dist as distance(inf, CC, CQ),
    Dist =< L/2, !
    ; !,
    % ou quando a distância do centro do
    % círculo para qualquer um dos lados
    % do quadrado é menor que o raio
    square_side(L/2, CQ, Line),
    Dist as distance(CC, Line),
    Dist =< R, !.

square(CQ, L) intersect circle(CC, R) :- !,
    circle(CC, R) intersect square(CQ, L).


%!  square_side(+HalfSide, +Center, -Segment) is multi.
%
%   Verdade se Segment é um dos lados do quadrado
%   definido por Center e metade do lado, HalfSide.
%
%   Usado para descobrir os quatros lados de um
%   quadrado.

square_side(HL, (X, Y), vsegment(X-HL, Y-HL, Y+HL)).
square_side(HL, (X, Y), vsegment(X+HL, Y-HL, Y+HL)).
square_side(HL, (X, Y), hsegment(Y-HL, X-HL, X+HL)).
square_side(HL, (X, Y), hsegment(Y+HL, X-HL, X+HL)).
