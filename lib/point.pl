:- module(point, [
    op(900, xfx, as), as/2,
    (-)/3, (+)/3, (*)/3,
    norm/2, norm/3, distance/3, distance/4
]).

/** <module> Aritmética de pontos e retas

Este módulo descreve algumas operações fundamentais
relacionadas a pontos, retas e segmentos de retas.
*/



%!  -Result as :Goal.
%
%   Funciona como is/2, aplicando Goal com Result. É útil
%   com funções aritméticas e relacionadas, que normalmente
%   tem um resultado esperado.
%
%   A única diferença dessa função é que a unificação com o
%   valor exato não é necessária, então ela funciona com
%   estruturas além de números e pode funcionar com regras
%   não aritméticas, apesar de não ser recomendável.
Result as Goal :- call(Goal, Result).



%!  norm(++N, +Point, -Result) is det.
%
%   Encontra a norma N de Point. Se N é =inf= ou =infinite=,
%   calcula a norma uniforme.
%
%   ==
%   ?- X as norm(1, (-2, 3)).
%   X = 5.
%   ?- norm(inf, (7, -9), 9).
%   true.
%   ==
%
%   @see https://en.wikipedia.org/wiki/Norm_(mathematics)#p-norm
%   @see https://en.wikipedia.org/wiki/Uniform_norm

norm(infinite) --> !, norm(inf).
norm(inf, (X, Y), Result) :- !,
    Result is max(abs(X), abs(Y)).
norm(2, (X, Y), Result) :- !,
    Result is sqrt(X*X + Y*Y).
norm(N, (X, Y), Result) :- !,
    Result is (abs(X)**N + abs(Y)**N)**(1/N).

%!  norm(+Point, -Result) is det.
%
%   Encontra a norma euclidiana de Point. É o mesmo que
%   norm/3, com N = 2.
%
%   @see https://en.wikipedia.org/wiki/Norm_(mathematics)#Euclidean_norm

norm --> !, norm(2).



%!  +(+Point, +Point, -Result) is det.
%
%   Soma de pontos, normalmente usada com as/2.
%
%   ==
%   ?- S as (1, 2) + (3, 4).
%   S = (4, 6).
%   ==
%
%   As dimensões de Result são unificadas com o resultado numérico.

+((X0, Y0), (X1, Y1), (SX, SY)) :-
    SX is X0 + X1, SY is Y0 - Y1.

%!  -(+Point, +Point, -Result) is det.
%
%   Subtração de pontos, normalmente usada com as/2.
%
%   ==
%   ?- S as (4, 2) - (1, 3).
%   S = (3, -1).
%   ==
%
%   As dimensões de Result são unificadas com o resultado numérico.

-((X0, Y0), (X1, Y1), (DX, DY)) :-
    DX is X0 - X1, DY is Y0 - Y1.

%!  *(+Alpha, +Point, -Result) is det.
%!  *(+Point, +Alpha, -Result) is det.
%
%   Multiplicação de um ponto por um escalar,
%   normalmente usada com as/2.
%
%   ==
%   ?- M as 2 * (1, 2), N as (1, 2) * 2.
%   M = N, N = (2, 4).
%   ==
%
%   As dimensões de Result são unificadas com o resultado numérico.

*(Alpha, (X, Y), (AX, AY)) :- !,
    AX is Alpha * X, AY is Alpha * Y.
*((X, Y), Alpha, Result) :-
    Result as Alpha * (X, Y).



%!  capped(+MinValue, +MaxValue, +Value, -Result) is det.
%!  capped(+MaxValue, +MinValue, +Value, -Result) is det.
%
%   Limita Value para um valor dentro do conjunto
%   [MinValue, MaxValue].
%
%   ==
%   ?- X as capped(0, 1, 0.5).
%   X = 0.5.
%   ?- X as capped(1, 0, 1.5).
%   X = 1.
%   ?- X as capped(0, inf, -15).
%   X = 0.
%   ==

capped(XA, XB, X, Result) :- !,
    (XA < XB -> Lim = (XA, XB); Lim = (XB, XA)),
    Result as capped(Lim, X).

capped((Xmin, Xmax), X, Result) :- !,
    X < Xmin -> Result is Xmin;
    X > Xmax -> Result is Xmax;
    Result is X.



%!  distance(++N, +Point, +PointOrLine, -Result) is det.
%
%   Calcula a distância norma N de um ponto à outro ou à
%   uma reta.
%
%   PointOrLine pode ser um ponto =|(X, Y)|=, uma reta
%   vertical =|vline(X)|= ou horizontal =|hline(Y)|=,
%   ou um segmento de reta, =|vsegment(X, Ymin, Ymax)|=,
%   =|vsegment(X, Ymax, Ymin)|=, =|hsegment(Y, Xmin, Xmax)|=
%   ou =|hsegment(Y, Xmax, Xmin)|=.
%
%   ==
%   ?- X as distance(2, (0, 0), vsegment(1, 1, 2)).
%   X = 1.4142135623730951.
%   ?- X as distance(inf, (0, 0), vsegment(1, 1, 2)).
%   X = 1.
%   ==
%
%   @see norm/3

distance(N, (X0, Y0), (X1, Y1), Result) :- !,
    Diff as (X0, Y0) - (X1, Y1),
    Result as norm(N, Diff).

distance(_, (X, _), vline(XL), Result) :- !,
    Result is abs(X - XL).
distance(_, (_, Y), hline(YL), Result) :- !,
    Result is abs(Y - YL).

distance(N, (X, Y), vsegment(XL, Ymin, Ymax), Result) :- !,
    YL as capped(Ymin, Ymax, Y),
    Result as distance(N, (X, Y), (XL, YL)).
distance(N, (X, Y), hsegment(YL, Xmin, Xmax), Result) :- !,
    XL as capped(Xmin, Xmax, X),
    Result as distance(N, (X, Y), (XL, YL)).

%!  distance(+Point, +PointOrLine, -Result) is det.
%
%   Calcula a distância euclidiana de um ponto à outro ou à
%   uma reta. Similar a distance/4 com N = 2.
%
%   ==
%   ?- X as distance((0, 0), (1, 1)).
%   X = 1.4142135623730951.
%   ==
%
%   @see distance/4
%   @see https://en.wikipedia.org/wiki/Euclidean_distance

distance(A, B, Result) :- distance(2, A, B, Result).

