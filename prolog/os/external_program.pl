:- module(
  external_program,
  [
    exists_program/1,            % +Program
    find_program_by_file_type/2, % +FileType, -Program
    list_external_programs/0
  ]
).

/** <module> External program

Support for using external programs with SWI-Prolog.

@author Wouter Beek
@version 2015/08, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(os/os_ext)).

%! file_type_program(?FileType, ?Program) is nondet.
% Registration pointing out that files of type FileType
% are handled by an external Program.

:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).

%! user:module_uses(?Module, ?Resource:compound) is nondet.
% Registration pointing out that Module uses Resource.
%
% Resource can be one of the following:
%   * file_type(atom)
%   * program(atom)

:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).





%! exists_program(+Program) is semidet.
% Succeeds if the given program can be run from PATH.

exists_program(Program):-
  var(Program), !,
  instantiation_error(Program).
exists_program(Program):-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.


%! find_program_by_file_type(+FileType, -Program) is nondet.
% Succeeds if there is at least one existing program that is registered to
% the given file type.

find_program_by_file_type(FileType, Program):-
  user:file_type_program(FileType, Program),
  exists_program(Program), !.



%! list_external_programs is det.
% Writes a list of external programs that are registered
% with some file type to the console.
%
% The list indicates whether the external programs are available or not
% and whether a file type's external dependencies are met.

list_external_programs:-
  aggregate_all(
    set(module(M)),
    user:module_uses(M, _),
    Filters
  ),
  maplist(list_external_programs, Filters).



%! list_external_programs(+Filter:compound) is det.
% Writes a list of external programs that are registered
% for the given Filter, which is either `file_type(atom)` or `module(atom)`.
%
% The list indicates whether the external programs are available or not.
% A file type's external dependencies are met if at least one
% of the external programs is available.
% A module's external dependencies are met if all external programs
% are avaialble.

list_external_programs(file_type(FT)):-
  aggregate_all(set(P), user:file_type_program(FT, P), Ps),
  Ps \== [], !,
  list_external_programs0(Ps, FT, "File type").
list_external_programs(module(M)):-
  aggregate_all(set(P), module_uses_program0(M, P), Ps),
  list_external_programs0(Ps, M, "Module").

list_external_programs0(Ps, Content, Type):-
  include(write_program_support0, Ps, SupportedPs),
  (   SupportedPs == []
  ->  Color = red,
      SupportText = "UNSUPPORTED"
  ;   Color = green,
      SupportText = "SUPPORTED"
  ),
  format("~w ", [Type]),
  ansi_format([bold], "~w", [Content]),
  format(" is "),
  ansi_format([bold,fg(Color)], "~w", [SupportText]),
  format(".~n").

%! module_uses_program0(+Module, -Program) is nondet.

module_uses_program0(M, P):-
  user:module_uses(M, program(P)).
module_uses_program0(M, P):-
  user:module_uses(M, file_type(FT)),
  user:file_type_program(FT, P).

%! write_program_support0(+Program) is semidet.
% Succeeds if the program with the given name exists on PATH.
% Always writes a message to the standard user output as side effect.

write_program_support0(P):-
  exists_program(P), !,
  format("Program "),
  ansi_format([bold], "~w", [P]),
  format(" is "),
  ansi_format([fg(green)], "supported", []),
  format(".~n").
write_program_support0(P):-
  format("Program "),
  ansi_format([bold], "~w", P),
  format(" is "),
  ansi_format([fg(red)], "not supported", []),
  format(".~n"), !,
  fail.
