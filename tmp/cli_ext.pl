:- module(
  cli_ext,
  [
    check_input/2, % +Mode:oneof([read,write]), +Input:atom
    check_path/2,  % +Mode:oneof([read,write]), +Path:atom
    show_help/1,   % +OptionSpecification:list(compound)
    user_input/1,  % +Msg
    user_input/3   % +Msg, :Dcg_1, ?Success
  ]
).
:- reexport(library(optparse)).

/** <module> Command-line interface extensions

@author Wouter Beek
@version 2015/09-2015/10, 2016/04-2016/05, 2016/10
*/

:- use_module(library(dcg)).
:- use_module(library(error)).
:- use_module(library(uri)).

:- meta_predicate
    user_input(+, 3, ?).





%! check_input(+Mode:oneof([read,write]), +Path:atom) is det.

check_input(read, Uri) :-
  uri_is_global(Uri), !.
check_input(Mode, Path) :-
  check_path(Mode, Path).



%! check_path(+Mode:oneof([read,write]), +Path:atom) is det.
% @throws existence_error
% @throws premission_error

check_path(Mode, Path) :-
  file_directory_name(Path, Dir),
  (   \+ exists_directory(Dir)
  ->  existence_error(file, Dir)
  ;   \+ access_file(Dir, Mode)
  ->  permission_error(Mode, file, Dir)
  ;   true
  ).



%! show_help(+OptionSpecification:list(compound)) is det.

show_help(OptSpec) :-
  opt_help(OptSpec, Help),
  format(user_output, '~a\n', [Help]),
  halt.



%! user_input(+Msg) is semidet.
%! user_input(+Msg, :Dcg_1, ?Success) is det.

user_input(Msg) :-
  user_input(Msg, yn, true).


user_input(Msg, Dcg_1, Success) :-
  repeat,
  format(user_output, "~s~n", [Msg]),
  read_line_to_codes(user_input, Cs),
  (   once(phrase(quit, Cs))
  ->  throw(user_quits)
  ;   once(phrase(dcg_call(Dcg_1, Val), Cs))
  ->  !,
      Val = Success
  ;   fail
  ).

yn(true) --> str_ci("y").
yn(true) --> str_ci("yes").
yn(false) --> str_ci("n").
yn(false) --> str_ci("no").

quit --> str_ci("q").
quit --> str_ci("quit").
