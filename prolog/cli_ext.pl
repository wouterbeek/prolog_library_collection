:- module(
  cli_ext,
  [
    check_path/2, % +Mode:oneof([read,write])
                  % +Path:atom
    long_flag/3, % +Flag:atom
                 % +Value
                 % -Argument:atom
    show_help/1 % +OptionSpecification:list(compound)
  ]
).

/** <module> Command-line interface extensions

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(error)).
:- use_module(library(optparse)).





%! check_path(+Mode:oneof([read,write]), +Path:atom) is det.
% @throws existence_error
% @throws premission_error

check_path(Mode, Path):-
  file_directory_name(Path, Dir),
  (   \+ exists_directory(Dir)
  ->  existence_error(file, Dir)
  ;   \+ access_file(Dir, Mode)
  ->  permission_error(Mode, file, Dir)
  ;   true
  ).



%! long_flag(+Flag:atom, +Value, -Argument:atom) is det.

long_flag(Flag, Val, Arg):-
  format(atom(Arg), '--~w=~w', [Flag,Val]).



%! show_help(+OptionSpecification:list(compound)) is det.

show_help(OptSpec):-
  opt_help(OptSpec, Help),
  format(user_output, '~a\n', [Help]),
  halt.
