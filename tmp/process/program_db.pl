:- module(
  program_db,
  [
    exists_program/1, % +Program:atom
    find_program_by_file_type/2, % +FileType:atom
                                 % -Predicate:atom
    list_external_programs/0,
    list_external_programs/1 % +FileType:ato
  ]
).

/** <module> Program Database

Register external programs called from within Prolog code.

@author Wouter Beek
@version 2013/06-2013/07, 2013/11, 2014/01-2014/02, 2014/05, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).

:- use_module(plc(generics/db_ext)).
:- use_module(plc(generics/error_ext)).
:- use_module(plc(generics/print_ext)).
:- use_module(plc(os/ansi_ext)).
:- use_module(plc(process/process_ext)).

% This is used to relate programs to file types.
:- dynamic(user:file_type_program/2).
:- multifile(user:file_type_program/2).

% This is used to relate programs to modules.
:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).





%! exists_program(+Program:atom) is semidet.
% Succeeds if the given program can be run from PATH.

exists_program(Program):-
  catch(
    handle_process(
      Program,
      [],
      [detached(true),process(Pid),program(Program)]
    ),
    error(existence_error(_, _), _),
    fail
  ),
  process_kill(Pid).



%! find_program_by_file_type(+FileType:atom, -Program:atom) is nondet.
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
    set(Module),
    module_uses(Module, _),
    Modules
  ),
  maplist(list_external_programs, Modules).



%! list_external_programs(+Filter:atom) is det.
% Writes a list of external programs that are registered
% with the given file type or module to the console.
%
% The list indicates whether the external programs are available or not.
% A file type's external dependencies are met if at least one
% of the external programs is available.
%
% @param Filter Either a Prolog file type or a Prolog module.

list_external_programs(FileType):-
  findall(
    Program,
    file_type_program(FileType, Program),
    Programs
  ),
  Programs \== [], !,
  list_external_programs_label(Programs, FileType, 'File type').
list_external_programs(Module):-
  findall(
    Program,
    module_uses_program(Module, Program),
    Programs
  ),
  list_external_programs_label(Programs, Module, 'Module').
module_uses_program(Module, Program):-
  user:module_uses(Module, file_type(FileType)),
  user:file_type_program(FileType, Program).
module_uses_program(Module, Program):-
  user:module_uses(Module, program(Program)).

list_external_programs_label(Programs, Content, String):-
  include(write_program_support, Programs, SupportedPrograms),
  (   SupportedPrograms == []
  ->  Color = red,
      SupportText = 'UNSUPPORTED'
  ;   Color = green,
      SupportText = 'SUPPORTED'
  ),
  ansi_formatnl(
    [
      '~w '-[String],
      [bold]-'~w'-[Content],
      ' is ',
      [bold,fg(Color)]-'~w'-[SupportText],
      '.'
    ]
  ).

%! write_program_support(+Program:atom) is semidet.
% Succeeds if the program with the given name exists on PATH.
% Always writes a message to the standard user output as side effect.

write_program_support(Program):-
  exists_program(Program), !,
  indent(1),
  ansi_formatnl(
    [
      'Program ',
      [bold]-'~w'-[Program],
      ' is ',
      [fg(green)]-'supported',
      '.'
    ]
  ).
write_program_support(Program):-
  indent(1),
  ansi_formatnl(
    [
      'Program ',
      [bold]-'~w'-[Program],
      ' is not supported.'
    ]
  ), !, fail.

