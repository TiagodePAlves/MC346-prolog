%!  adapt_extension(?FullPath, ?FilePath) is semidet.
%
%   Remove as extensão =|.pl|= de FullPath ou
%   adiciona em FilePath.
adapt_extension(FullPath, FilePath) :-
    file_name_extension(FilePath, ".pl", FullPath).


%!  subdirs_prolog_files(-Files) is det.
%
%   Lista os arquivos com código-fonte de Prolog.
subdirs_prolog_files([run|Files]) :-
    expand_file_name("*/*.pl", SourceFiles),
    maplist(adapt_extension, SourceFiles, Files).


%   Carrega os arquivos para a documentação.
:- subdirs_prolog_files(Files), load_files(Files).



%!  css_body(-CSS) is det.
%
%   Texto com atributos CSS a serem incluídos
%   no estilo do documento.
css_body("
body {
  margin: auto;
  max-width: 50em;
  background-color: #ecebe3;
}
").


%!  edit_css(++Path) is semidet.
%
%   Edita o arquivo =pldoc.css= em Path com
%   os atributos de css_body/1.
edit_css(Path) :-
    string_concat(Path, "/pldoc.css", FullPath),
    open(FullPath, append, CSSFile),
    css_body(CSS), writeln(CSSFile, CSS),
    close(CSSFile).


%!  run is semidet.
%
%   Cria a documentação dos arquivos e edita o
%   CSS dela.
run :-
    doc_save(., [recursive(true), doc_root(docs)]),
    edit_css(docs).
