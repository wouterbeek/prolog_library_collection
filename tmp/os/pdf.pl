:- module(
  pdf,
  [
    open_pdf/1 % +File:atom
  ]
).

/** <module> PDF

Support for PDF files.

@author Wouter Beek
@version 2014/05, 2014/07, 2015/03
*/

:- use_module(plc(os/os_ext)).
:- use_module(plc(process/process_ext)).
:- use_module(plc(process/program_db)).

:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).
user:file_type_program(pdf, evince).
user:file_type_program(pdf, xpdf).





%! open_pdf(+File:atom) is det.
% Opens the given PDF file.

open_pdf(File):-
  once(find_program_by_file_type(pdf, Program)),
  handle_process(Program, [File], [program(Program)]).
