:- module(
  conf,
  [
    cli_arguments/1, % -Args
    conf_json/1      % -Conf
  ]
).

/** <module> Configuration support

This module is typically used in the following way:

```prolog
:- initialization
   conf_json(Conf),
   configure_your_application(Conf).
```

The configuration file location must be supplied on the command line
(`--conf=$(FILE)').  Otherwise the default configuration file location
is used, i.e., `~/conf.json'.

---

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(option)).

:- use_module(library(dcg)).
:- use_module(library(json_ext)).





%! cli_arguments(-Args:list(opt)) is det.

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



%! conf_json(-Conf:dict) is semidet.
%
% Read a dictionary with configuration information from a file whose
% `FILE` name is supplied as a command-line argument of the form
% `--conf=FILE`.

conf_json(Conf) :-
  cli_arguments(Args),
  option(conf(Spec), Args, '~/conf.json'),
  expand_file_name(Spec, [File|_]),
  exists_file(File),
  json_load(File, Conf).
