:- encoding(utf8).
:- module(
  conf,
  [
    cli_argument/2,  % +Key, -Argument
    cli_argument/3,  % +Key, +Default, -Argument
    cli_arguments/1, % -Arguments
    conf_json/1,     % -Configuration
    conf_json/3      % +Key, -Configuration, -Directory
  ]
).

/** <module> Configuration support

This module is typically used in the following way:

```prolog
:- initialization
   conf_json(Conf),
   configure_your_application(Conf).
```

or

```prolog
:- initialization
   conf_json('my-application', Conf, Dir),
   configure_your_application(Conf, Dir).
```

The configuration file location can be supplied from the command line
by using the following flag.  If no location is supplied, the default
configuration file location is used, i.e., `~/conf.json'.


```sh
--conf=$(FILE)
```

---

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(json_ext)).





%! cli_argument(+Key:atom, -Value:term) is semidet.
%
% @throws existence_error/2 if CLI argument Key does not exist.

cli_argument(Key, Value) :-
  cli_argument_(Key, Value), !.
cli_argument(Key, _) :-
  existence_error(cli_argument, Key).

cli_argument_(Key, Value) :-
  cli_arguments(L),
  X =.. [Key,Value],
  memberchk(X, L), !.



%! cli_argument(+Key:atom, +Default:term, -Value:term) is det.

cli_argument(Key, _, Value) :-
  cli_argument_(Key, Value), !.
cli_argument(_, Value, Value).



%! cli_arguments(-Argumentss:list(compound)) is det.

cli_arguments(Args) :-
  current_prolog_flag(argv, Flags),
  convlist(parse_argument, Flags, Args).

parse_argument(Flag, Arg) :-
  atom_phrase(flag_argument(Arg), Flag), !.
parse_argument(Arg, Arg).

flag_argument(Arg) -->
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
%
% @throws existence_error/1 if no configuration file exists.

conf_json(Conf) :-
  conf_file(File), !,
  json_load(File, Conf).
conf_json(_) :-
  throw(error(existence_error(conf_file),conf_json/1)).

conf_file(File) :-
  conf_file_spec(Spec),
  expand_file_name(Spec, Files),
  member(File, Files),
  access_file(File, read), !.

conf_file_spec(Spec) :-
  cli_argument_(conf, Spec), !.
conf_file_spec('conf.json').
conf_file_spec('~/conf.json').


%! conf_json(+Key:atom, -Configuration:dict, -Directory:atom) is semidet.
%
% Like conf_json/1, but only returns that part of the configuration
% that resides underneath the given Key.
%
% This allows multiple libraries/applications to be configured within
% the same configuration file, using the first level keys as
% library/application identifiers that can be supplied as Key.
%
% This also supports the specification of optional outer and inner
% `directory' keys, which are appended and bound to Directory.
%
% @throws existence_error/1 if no configuration files exists.
%
% @throws existence_error/2 if Key does not occur in the configuration
% file.

conf_json(Key, Conf2, Dir) :-
  conf_json(Conf1),
  dict_get(directory, Conf1, '', Dir1),
  dict_get(Key, Conf1, Conf2), !,
  dict_get(directory, Conf2, '', Dir2),
  append_directories([Dir1,Dir2], Dir).
conf_json(Key, _, _) :-
  existence_error(conf_key, Key).





% MESSAGES %

:- multifile
    prolog:error_message//1.

prolog:error_message(cannot_find_cli_argument(Key)) -->
  ['Cannot find CLI argument ‘--~a="VALUE"’.'-[Key]].
prolog:error_message(cannot_find_configuration(Key)) -->
  ["Cannot find the configuration section for ‘~a’."-[Key]].
