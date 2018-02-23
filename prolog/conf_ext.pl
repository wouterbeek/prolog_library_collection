:- module(
  conf_ext,
  [
    conf_json/1 % -Conf:dict
  ]
).

/** <module> Configuration extension

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(dcg)).
:- use_module(library(http/json)).
:- use_module(library(option)).





%! conf_json(-Conf:dict) is semidet.

conf_json(Conf) :-
  cli_arguments(Args),
  option(conf(File0), Args),
  expand_file_name(File0, [File|_]),
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
  remainder_as_atom(Value),
  {Arg =.. [Key,Value]}.
