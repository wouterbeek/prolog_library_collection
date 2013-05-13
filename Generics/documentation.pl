:- module(
  documentation,
  [
    export_all/0,
    export_directory/1, % +Directory:atom
    export_file/1, % +File:atom
    export_files/1, % +Files:list(atom)
    export_module/1 % +Module:atom
  ]
).

/** <module> DOCUMENTATION

Methods for generating plDoc formatted code documentation in LaTeX format.

@author Wouter Beek
@version 2012/06
@tbd This should be tested.
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(file_ext)).
:- use_module(library(doc_latex)).



%! clear_for_export is det.
% Removes the latex files from previous exports.

clear_for_export:-
  absolute_file_name(project(documentation), ExportDirectory),
  delete_files(
    ExportDirectory,
    [
      auxiliary,
      latex,
      log,
      output,
      pdf,
      table_of_contents
    ]
  ).

%! export_all is det.
% Exports all plDoc to LaTeX files.
% Writes the include statement to each TeX file in file =|index.tex|=.

export_all:-
  file_search_path(project, ProjectDirectory),
  export_directory(ProjectDirectory).

export_directory(Directory):-
  path_walk_tree(Directory, '(.*.pl|.*.txt)$', Files),

  clear_for_export,

  absolute_file_name(documentation(index), IndexFile, [file_type(latex)]),
  open(IndexFile, write, IndexStream, [close_on_abort(true), type(text)]),
  format(IndexStream, '\\documentclass[a4paper,11pt,final]{book}\n\n', []),
  format(IndexStream, '\\usepackage{pldoc}\n\n', []),
  format(IndexStream, '\\begin{document}\n\n', []),
  format(IndexStream, '\\author{Wouter Beek}\n', []),
  format(IndexStream, '\\title{Truth-B-Told}\n\n', []),
  format(IndexStream, '\\maketitle\n\n', []),
  format(IndexStream, '\\tableofcontents\n\n', []),

  forall(
    member(File, Files),
    export_file(File, IndexStream, false, false)
  ),

  format(IndexStream, '\n\\end{document}\n', []),
  close(IndexStream).

  %absolute_file_name(project('Documentation'), ExportDirectory),
  %run_in_working_directory(run_script(compile, hide), ExportDirectory).

%! export_file(+File:atom) is det.
% Exports the given Prolog file to LaTeX.
%
% @arg File The atomic absolute file name of a Prolog file.

export_file(File):-
  % Do not generate a summary file, generate as a standalone file.
  export_file(File, fail, true).

%! export_file(+File:atom, +ShowSummary:boolean, +StandAlone:boolean) is det.
% Export the given Prolog file to LaTeX.
%
% @arg File The atomic absolute file name of a Prolog file.
% @arg ShowSummary Whether an additional LaTeX summary file is generated
%        or not.
% @arg StandAlone A boolean indicating whether this LaTeX file is part of
%        a larger document or not.

export_file(File, ShowSummary, StandAlone):-
  file_to_name(File, Name),
  absolute_file_name(documentation(Name), MainFile, [file_type(latex)]),
  open(MainFile, write, MainStream, [close_on_abort(true), type(text)]),

  % The LaTeX summary file.
  (
    ShowSummary == true
  ->
    atomic_concat(Name, '_summary', BaseName),
    absolute_file_name(documentation(BaseName), SummaryFile, [file_type(latex)]),
    open(
      SummaryFile,
      write,
      _SummaryStream,
      [close_on_abort(true), type(text)]
    ),
    ShowSummarySetting = [summary(SummaryFile)]
  ;
    ShowSummarySetting = []
  ),

  doc_latex(
    File,
    MainFile,
    [
      public_only(false),
      section_level(section),
      stand_alone(StandAlone) |
      ShowSummarySetting
    ]
  ),

  close(MainStream).

%! export_file(
%!   +File:atom,
%!   +IndexStream:stream,
%!   +ShowSummary:boolean
%!   +StandAlone:boolean
%! ) is det.
% Exports the given plDoc file to a TeX file and a summary TeX file.
%
% @arg File The file whose plDoc is exported to LaTeX.
% @arg IndexStream The file whose plDoc is exported to LaTeX.
% @arg ShowSummary Whether an additional LaTeX summary file is generated
%        or not.
% @arg StandAlone A boolean indicating whether this LaTeX file is part of
%        a larger document or not.

export_file(File, IndexStream, ShowSummary, StandAlone):-
  export_file(File, ShowSummary, StandAlone),
  file_to_name(File, Name),
  format(IndexStream, '\\input{~w}\n', [Name]),
  flush_output(IndexStream).

export_files(Files):-
  clear_for_export,
  maplist(export_file, Files).

%! file_to_name(File, Name) is det.
% Breaks the given file name down to its local name.
% @arg File
% @arg Name
% @tbd Remove the DynaLearn directory reference.

file_to_name(File, Name):-
  file_search_path(project, ProjectDirectory),
  atom_concat(ProjectDirectory, RelativeFile, File),
  atom_concat('/', Name_, RelativeFile),
  atom_replace(Name_, ['/'-'_', ' '-'_'], Name__),
  (
    file_name_type(Name, prolog, Name__)
  ;
    file_name_type(Name, text, Name__)
  ).

%! export_module(+Module:atom) is det.
% Export the module with the given name to LaTeX.
%
% @arg The atomic name of a module.

export_module(Module):-
  module_property(Module, file(File)),
  export_file(File).
