:- module(
  pdf,
  [
    open_pdf/1 % +File:atom
  ]
).

/** <module> PDF

Support for opening PDF files.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(os/external_program)).
:- use_module(library(os/process_ext)).

:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).

user:file_type_program(pdf, evince).
user:file_type_program(pdf, xpdf).





%! open_pdf(+File:atom) is det.
% Opens the given PDF file.

open_pdf(File):-
  once(find_program_by_file_type(pdf, Program)),
  run_process(Program, [File], [program(Program)]).
