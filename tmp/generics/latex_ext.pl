:- module(
  latex_ext,
  [
    bibtex_convert_file/1, % +File:atom
    latex_clean_directory/1, % +Directory:atom
    latex_clean_file/1, % +File:atom
    latex_convert/1 % +FileOrDirectory:atom
  ]
).

/** <module> LaTeX extensions

Predicates for handling LaTeX files.

@author Wouter Beek
@version 2013/06, 2013/08, 2014/01, 2014/07, 2015/03
*/

:- use_module(library(apply)).

:- use_module(plc(generics/db_ext)).
:- use_module(plc(io/dir_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(prolog/pl_control)).
:- use_module(plc(process/process_ext)).

:- db_add_novel(user:prolog_file_type(aux, aux      )).
:- db_add_novel(user:prolog_file_type(aux, latex_out)).
:- db_add_novel(user:prolog_file_type(bib, bibtex   )).
:- db_add_novel(user:prolog_file_type(log, log      )).
:- db_add_novel(user:prolog_file_type(log, latex_out)).
:- db_add_novel(user:prolog_file_type(pdf, pdf      )).
:- db_add_novel(user:prolog_file_type(pdf, latex_out)).
:- db_add_novel(user:prolog_file_type(tex, tex      )).
:- db_add_novel(user:prolog_file_type(tex, latex_in )).





%! bibtex_convert_file(+File:atom) is det.

bibtex_convert_file(File):-
  file_component(File, base, Base),
  file_directory_name(File, Dir),
  handle_process(bibtex, [Base], [cwd(Dir),program('BibTeX')]).


%! latex_clean_directory(+Directory:atom) is det.

latex_clean_directory(Directory):-
  exists_directory(Directory), !,
  directory_files(
    [
      file_types([latex_out]),
      include_directories(false),
      include_self(false),
      recursive(true)
    ],
    Files
  ),
  maplist(delete_file, Files).


%! latex_clean_file(+File:atom) is det.
% Cleans the LaTeX output files in the given directory recursively.

latex_clean_file(File):-
  access_file(File, read), !,
  forall(
    (
      file_kind_alternative(File, latex_out, DeleteFile),
      access_file(DeleteFile, write)
    ),
    delete_file(DeleteFile)
  ).


%! latex_convert(FileOrDirectory:atom) is det.

latex_convert(Dir):-
  exists_directory(Dir), !,
  latex_convert_directory(Dir).
latex_convert(File):-
  file_directory_name(File, Dir),
  latex_convert_file(File, Dir).


%! latex_convert_directory(+Directory:atom) is det.

latex_convert_directory(Dir):-
  access_file(Dir, read),
  directory_files(
    [
      file_types([latex_in]),
      include_directories(false),
      order(lexicographic),
      recursive(true)
    ],
    Dir,
    Files
  ),
  maplist(latex_convert, Files).


%! latex_convert_file(+FromFile:atom, -ToDirectory:atom) is det.

latex_convert_file(FromFile, ToDir):-
  % Check arguments.
  is_absolute_file_name(FromFile),
  access_file(FromFile, read),
  access_file(ToDir, write),

  % Exit with an error code when an error is encountered.
  handle_process(pdflatex, ['-halt-on-error',FromFile], [cwd(ToDir)]).

