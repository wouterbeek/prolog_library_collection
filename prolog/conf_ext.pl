:- module(
  conf_ext,
  [
    conf_json/1 % -Dict:dict
  ]
).

/** <module> Configuration extension

@author Wouter Beek
@version 2017/06-2017/07
*/

:- use_module(library(json_ext)).
:- use_module(library(option)).
:- use_module(library(os_ext)).





%! conf_json(-Dict:dict) is det.

conf_json(Dict) :-
  cli_arguments(Arguments),
  option(conf(FileSpec), Arguments),
  json_to_dict(FileSpec, Dict, [value_string_as(atom)]).
