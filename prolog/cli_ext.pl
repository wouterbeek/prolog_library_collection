:- module(
  cli_ext,
  [
    check_input/2, % +Mode:oneof([read,write]), +Input:atom
    check_path/2,  % +Mode:oneof([read,write]), +Path:atom
    long_flag/2,   % +Flag:atom,         -Argument:atom
    long_flag/3,   % +Flag:atom, +Value, -Argument:atom
    show_help/1,   % +OptionSpecification:list(compound)
    user_input/1,  % +Msg
    user_input/2   % +Msg, :Dcg_0
  ]
).
:- reexport(library(optparse)).

/** <module> Command-line interface extensions

@author Wouter Beek
@version 2015/09-2015/10, 2016/04-2016/05
*/

:- use_module(library(error)).
:- use_module(library(uri)).

:- meta_predicate
    user_input(+,//).





%! check_input(+Mode:oneof([read,write]), +Path:atom) is det.

check_input(read, Iri):-
  uri_is_global(Iri), !.
check_input(Mode, Path):-
  check_path(Mode, Path).



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



%! long_flag(+Flag:atom, -Argument:atom) is det.
%! long_flag(+Flag:atom, +Value, -Argument:atom) is det.

long_flag(Flag, Arg) :-
  format(atom(Arg), '--~w', [Flag]).


long_flag(Flag, Val, Arg):-
  format(atom(Arg), '--~w=~w', [Flag,Val]).



%! show_help(+OptionSpecification:list(compound)) is det.

show_help(OptSpec):-
  opt_help(OptSpec, Help),
  format(user_output, '~a\n', [Help]),
  halt.



%! user_input(+Msg) is semidet.
%! user_input(+Msg, :Dcg_0) is det.

user_input(Msg) :-
  user_input(Msg, yn(true)).


user_input(Msg, Dcg_0):-
  repeat,
  format(user_output, "~s~n", [Msg]),
  read_line_to_codes(user_input, Cs),
  (   once(phrase(Dcg_0, Cs))
  ->  !
  ;   fail
  ).


yn(true) --> "y".
yn(false) --> "n".
