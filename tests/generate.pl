:- module(generate, [
    generate_test/2,
    generate_test/3,
    generate_test/5,
    parse_option/2
]).


%!  join(+StringList, -JoinedString) is det.
%
%   Concatena strings com espaço entre cada uma.
%
%   ==
%   ?- join([a, b, c], S).
%   S = "a b c".
%   ==
join(Strings, Text) :-
    join_(Strings, Text, "").

join_([X, Y|L], String, Acc) :- !,
    string_concat(Acc, X, AccX),
    string_concat(AccX, " ", AccXS),
    join_([Y|L], String, AccXS).
join_([X], String, Acc) :- !,
    string_concat(Acc, X, String).
join_([], Acc, Acc).


%!  parse_key(+Key, -Text) is det.
%
%   Transforma a chave para a formatação do programa.
%
%   ==
%   ?- parse_key(a_b_c, X).
%   X = "--a-b-c".
%   ==
parse_key(Key, Text) :-
    atomic_list_concat(AtomList, '_', Key),
    atomic_list_concat(AtomList, -, ParsedKey),
    atom_concat('--', ParsedKey, Text).


%!  parse_option(+Option, -CmdArg) is det.
%
%   Option pode ser:
%
%   | Option            | CmdArg        |
%   | Key(Val, ...)     | --Key Val ... |
%   | Key               | --Key         |
%   | solution(File)    | File          |
%
%   * No caso, solution(File) é usada para indicar
%     que a solução para o problema deve ser escrita
%     no arquivo =File=.

parse_option(solution(Filename), Filename) :- !.

parse_option(Functor, [ArgName|Args]) :-
    Functor =.. [Key|Args],
    parse_key(Key, ArgName).


%!  generate_test_file(+FileName, +Options)
%
%   Chama o código em Python para gerar os
%   testes e gravar em FileName.
%
%   @see descrição de Options em parse_option/2
generate_test_file(File, Options) :-
    append(Options, [prolog], Opts),
    maplist(parse_option, Opts, OptionalArgs),
    flatten(['utils/generate.py', File | OptionalArgs], Args),
    process_create(path(python), Args, [process(PID)]),
    process_wait(PID, exit(0)).

%!  create_tmp_file(--FileName)
%
%   Cria um arquivo temporário e unifica o nome
%   do arquivo com FileName.
create_tmp_file(FileName) :-
    tmp_file_stream(text, FileName, Stream),
    close(Stream).

%!  read_file(+FileName, -Term)
%
%   Abre o arquivo FileName, lê os dados
%   e fecha novamente.
read_file(File, Term) :-
    open(File, read, Stream),
    read(Stream, Term),
    close(Stream).


%!  generate_test(+File, --Shapes, +Options)
%
%   Gera dados de teste e salva em File. Os dados são
%   unificados com Shapes.
%
%   @see descrição de Options em parse_option/2
generate_test(File, Shapes, Options) :-
    \+ var(File), !,
    generate_test_file(File, Options),
    read_file(File, Shapes).

%!  generate_test(--Shapes, --Solution, +Options)
%
%   Gera dados de teste e unifica com Shapes. A
%   solução é unificada com Solution.
%
%   @see descrição de Options em parse_option/2
generate_test(Shapes, Solution, Options) :-
    create_tmp_file(InFile), create_tmp_file(OutFile),
    generate_test(InFile, Shapes, OutFile, Solution, Options).


%!  generate_test(--Shapes, --Solution)
%
%   Mesmo que =|generate_test(Shapes, Solution, [])|=.
%
%   @see generate_test/3
generate_test(Shapes, Solution) :-
    var(Solution), !,
    generate_test(Shapes, Solution, []).

%!  generate_test(--Shapes, +Options)
%
%   @see generate_test/3
generate_test(Shapes, Options) :-
    create_tmp_file(File),
    generate_test(File, Shapes, Options).

%!  generate_test(+File, --Shapes, +SolFile, --Solution, +Options)
%
%   Além de gerar o teste e salvar em File, também salva
%   a solução em SolFile. O teste é unificado com Shapes
%   e Solution com a solução.
%
%   @see descrição de Options em parse_option/2
generate_test(InFile, Shapes, SolFile, Solution, Options) :-
    generate_test(InFile, Shapes, [solution(SolFile)|Options]),
    read_file(SolFile, Solution).
