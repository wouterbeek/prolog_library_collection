:- module(
  conf_ext,
  [
    conf_json/1 % -Conf:dict
  ]
).

/** <module> Configuration extension

@author Wouter Beek
@version 2017/06-2017/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(os_ext)).





%! conf_json(-Conf:dict) is semidet.

conf_json(Conf) :-
  cli_arguments(Args),
  option(conf(File), Args),
  setup_call_cleanup(
    open(File, read, In),
    json_read_dict(In, Conf, [value_string_as(atom)]),
    close(In)
  ).

cli_arguments(Args) :-
  current_prolog_flag(argv, Flags),
  convlist(parse_argument, Flags, Args).

parse_argument(Flag, Arg) :-
  atom_phrase(argument(Arg), Flag).

argument(Arg) -->
  "--",
  '...'(Codes),
  "=", !,
  {atom_codes(Key, Codes)},
  rest_as_atom(Value),
  {Arg =.. [Key,Value]}.
