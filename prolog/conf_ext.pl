:- module(
  conf_ext,
  [
    conf_json/1 % -Dict:dict
  ]
).

/** <module> Configuration extension

@author Wouter Beek
@version 2017/06-2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(json_ext)).
:- use_module(library(option)).
:- use_module(library(os_ext)).





%! conf_json(-Dict:dict) is semidet.

conf_json(Dict) :-
  cli_arguments(Args),
  option(conf(FileSpec), Args),
  json_to_dict(FileSpec, Dict, [value_string_as(atom)]).

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
