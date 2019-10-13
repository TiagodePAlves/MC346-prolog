:- load_files(run).
:- use_module(generate).


%!  ordered(?A-B, ?C-D) is det.
%
%   Verdade se C é o menor lexicograficamente
%   entre A e B e D é o maior deles.
ordered(A-B, A-B) :-
    A @< B, !.
ordered(A-B, B-A) :- !.

%!  ordered(+List, -Ordered) is det.
%
%   Ordena os elementos de List internamente
%   e a lista como um todo.
ordered(List, Ordered) :-
    maplist(ordered, List, UnSorted),
    sort(UnSorted, Ordered).



:- op(900, xfx, equiv).
%!  A equiv B is det.
%
%   Verdade se a solução A é equivalente a B.
A equiv B :-
    ordered(A, An),
    ordered(B, Bn),
    An == Bn.


%!  run_it(+Shapes, +Solution, +Solver)
%
%   Roda Solver com Shapes e compara com Solution.
%   O resultado é escrito na saída atual.
run_it(Shapes, Solution, Solver) :-
    format("  ~w: ", Solver),
    call(Solver, Shapes, Intersections),
    (
        Intersections equiv Solution -> writeln("PASSED")
        ; writeln("WRONG"),
        write('Shapes = '), writeln(Shapes),
        write('Solution = '), writeln(Solution),
        write('Intersections = '), writeln(Intersections)
    ),
    retractall(shape(_, _)),
    retractall(intersection(_, _)).

%!  run_all(+Options)
%
%   Gera um teste com generate_test/3 e Options. Aplica o teste
%   em todos os solvers de solver/2, escrevendo o resultado na
%   saída atual.
%
%   @see run_it/3
run_all(Options) :-
    write("Testing with:"), maplist(format(" ~w"), Options), nl,
    generate_test(Shapes, Solution, Options),
    forall(solver(_, Solver), run_it(Shapes, Solution, Solver)).


%!  time(:Goal, -Time)
%
%   Chama Goal marcando o tempo de execução e
%   retorna o tempo em Time, em segundos.
time(Goal, Time) :-
    Start is cputime,
    call(Goal),
    Time is cputime - Start.

%!  time_it(+Shapes, +Solver)
%
%   Roda Solver com Shapes marcando o tempo de execução.
%   O resultado é escrito na saída atual.
time_it(Shapes, Solver) :-
    Goal =.. [Solver, Shapes, _Intersections],
    format("~w:", Solver), nl,
    time(Goal, Time), retractall(shape(_, _)),
    format("  ~f s", Time), nl.

%!  time_all(+Options)
%
%   Gera um teste com generate_test/3 e Options. Aplica o teste
%   em todos os solvers de solver/2, escrevendo o tempo de
%   execução na saída atual.
%
%   @see time_it/3
time_all(Options) :-
    write("Timing with:"), maplist(format(" ~w"), Options), nl,
    generate_test(Shapes, Options),
    foreach(solver(_, Solver), time_it(Shapes, Solver)).


%!  options(?Action, ?Key, ?Val) is multi.
%
%   Base de fatos de opções para cada Action de teste.
% options(run_all, amount, X) :-
%     member(X, [1, 5, 10, 15, 50, 100]).
% options(run_all, max_dist, X) :-
%     member(X, [1, 10, 100]).
options(time_all, amount, 1000).
    % member(X, [100, 500, 1000, 2000, 5000, 10000]).
options(time_all, name_length, 12).

%!  options(?Action, ?Option) is multi.
%
%   Base de fatos de opções para cada Action de teste.
%   Como em options/3, mas as opções aqui são compostas
%   no formato Key(Val).
options(Action, Option) :-
    options(Action, Key, Val),
    Option =.. [Key, Val].

%!  action_options(?Action, ?Options) is multi.
%
%   Options é uma lista de opções encontradas com
%   options/2, com todas as chaves de options/3
%   relacionada com Action e cada chave é distinta.
action_options(Action, Options) :-
    action_options(Action, Options, []).

action_options(Action, [Option|Options], Keys) :-
    options(Action, Option),
    functor(Option, Key, _),
    \+ member(Key, Keys),
    action_options(Action, Options, [Key|Keys]).

action_options(Action, [], Keys) :-
    sort(Keys, Keys),
    forall(options(Action, Key, _), member(Key, Keys)).


%!  tests
%
%   Roda os testes com todas as combinações de
%   opções registradas em options/3.
tests :-
    foreach(action_options(Action, Options), (
        call(Action, Options),
        nl
    )).
