:- module(
  pdf,
  [
    open_pdf/1, % +File
    open_pdf/2 % +File, +Opts
  ]
).

/** <module> PDF

Support for opening PDF files.

@author Wouter Beek
@license MIT License
@version 2015/10, 2016/03
*/

:- use_module(library(option)).
:- use_module(library(os/external_program)).
:- use_module(library(os/process_ext)).

:- predicate_options(open_pdf/2, 2, [
     pass_to(run_process/3, 3)
   ]).

:- dynamic
    user:file_type_program/2.

:- multifile
    user:file_type_program/2.

user:file_type_program(pdf, evince).
user:file_type_program(pdf, xpdf).





%! open_pdf(+File) is det.
% Wrapper around open_pdf/2 with default options.

open_pdf(File):-
  open_pdf(File, [detached(true)]).


%! open_pdf(+File, +Opts) is det.
% Opens the given PDF file.
% Options are passed to run_process/3.

open_pdf(File, Opts1):-
  merge_options(Opts1, [program(Program)], Opts2),
  once(find_program_by_file_type(pdf, Program)),
  run_process(Program, [File], Opts2).
