:- load_files([src/lists, src/accumulated]).


%!  solver(+Name, :Goal) is det.
%
%   Seletor da solução.
solver(acc, accumulated_solver) :- !.
solver(lst, lists_sover) :- !.



%!  topo is semidet.
%
%   Resolve o problema com a entrada e saída padrão.
topo :-
    read(Figures),
    solver(acc, Solver),
    call(Solver, Figures, Length, Intersections),
    writeln(Length),
    maplist(writeln, Intersections).
