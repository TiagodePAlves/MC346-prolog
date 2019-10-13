:- load_files([src/lists, src/accumulated, src/database]).


%!  solver(+Name, -Solver) is det.
%
%   Seletor da solução. As opções para Name são:
%
%   | Name  | Solver                |
%   | acc   | accumulated_solver/3  |
%   | lst   | list_solver/3        |
%   | dbs   | database_solver/3     |
solver(acc, accumulated_solver).
solver(lst, list_solver).
solver(dbs, database_solver).


run(Name) :-
    read(Figures),
    solver(Name, Solver),
    call(Solver, Figures, Intersections, Length),
    writeln(Length),
    maplist(writeln, Intersections).

acc :- run(acc).
lst :- run(lst).
dbs :- run(dbs).

%!  topo is semidet.
%
%   Resolve o problema com a entrada e saída padrão.
topo :- lst.
