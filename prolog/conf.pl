:- encoding(utf8).
:- module(
  conf,
  [
    cli_argument/2,   % +Key, -Value
    cli_argument/3,   % +Key, +Default, -Value
    cli_arguments/1,  % -Conf
    cli_arguments/2,  % +FlagMap, -Conf
    cli_arguments/3,  % +Argv, +FlagMap, -Conf
    conf_json/1,      % -Conf
    conf_json/2,      % +Key, -Value
    data_directory/1, % -Directory
    data_file/1,      % -AbsolutePath
    data_file/2       % +RelativePath, -AbsolutePath
  ]
).

/** <module> Configuration support

This module is typically used in the following way:

```pl
:- initialization
   conf_json(Conf),
   configure_your_application(Conf).
```

or

```pl
:- initialization
   conf_json('my-application', Conf),
   configure_your_application(Conf).
```

The configuration file location can be supplied from the command line
by using the following flag.  If no location is supplied, the default
configuration file location is used, i.e., `~/conf.json'.


```sh
--conf=$(FILE)
```

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(uuid)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(json_ext)).





%! cli_argument(+Key:atom, -Value:term) is semidet.

cli_argument(Key, Value) :-
  cli_arguments(Conf),
  dict_get(Key, Conf, Value).



%! cli_argument(+Key:atom, +Default:term, -Value:term) is det.

cli_argument(Key, Default, Value) :-
  cli_arguments(Conf),
  dict_get(Key, Conf, Default, Value).



%! cli_arguments(-Conf:dict) is det.
%! cli_arguments(+FlagMap:list(pair(atom,atom)), -Conf:dict) is det.
%! cli_arguments(+Argv:list(atom), +FlagMap:list(pair(atom,atom)), -Conf:dict) is det.

cli_arguments(Conf) :-
  current_prolog_flag(argv, Argv),
  cli_arguments(Argv, [], Conf).

cli_arguments(FlagMap, Conf) :-
  current_prolog_flag(argv, Argv),
  cli_arguments(Argv, FlagMap, Conf).

cli_arguments(Argv, FlagMap, Conf) :-
  maplist(cli_argument_(FlagMap), Argv, Pairs1),
  keysort(Pairs1, Pairs2),
  group_pairs_by_key(Pairs2, GroupedPairs1),
  maplist(remove_singleton_value_, GroupedPairs1, GroupedPairs2),
  dict_pairs(Conf, GroupedPairs2).

remove_singleton_value_(Key-[Value], Key-Value) :- !.
remove_singleton_value_(Pair, Pair).

cli_argument_(Map, Flag, Arg) :-
  atom_phrase(cli_flag_(Map, Arg), Flag), !.
cli_argument_(_, Arg, positional-Arg).

cli_flag_(Map, Key-Value) -->
  (   "--"
  ->  '...'(Codes),
      "=",
      {atom_codes(Key, Codes)}
  ;   "-"
  ->  '...'(Codes),
      "=",
      {
        atom_codes(Short, Codes),
        memberchk(Short-Key, Map)
      }
  ), !,
  remainder_as_atom(Value).
cli_flag_(Map, Key-true) -->
  (   "--"
  ->  remainder_as_atom(Key)
  ;   "-"
  ->  remainder_as_atom(Short),
      {memberchk(Short-Key, Map)}
  ).



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
  cli_argument(conf, Spec), !.
conf_file_spec('conf.json').
conf_file_spec('~/conf.json').


%! conf_json(+Key:atom, -Value:atom) is semidet.
%
% Like conf_json/1, but only returns that part of the configuration
% that resides underneath the given Key.
%
% @throws existence_error/1 if no configuration files exists.
%
% @throws existence_error/2 if Key does not occur in the configuration
% file.

conf_json(Key, Value) :-
  conf_json(Conf),
  dict_get(Key, Conf, Value), !.
conf_json(Key, _) :-
  existence_error(conf_key, Key).



%! data_directory(-Directory:atom) is semidet.

data_directory(Dir) :-
  conf_json('data-directory', Dir), !.
data_directory(_) :-
  existence_error(config, data_directory).



%! data_file(-AbsolutePath:atom) is multi.

data_file(File) :-
  uuid(Local),
  data_file(Local, File).



%! data_file(+RelativePath:atom, -AbsolutePath:atom) is det.

data_file(RelativePath, AbsolutePath) :-
  data_directory(Dir),
  absolute_file_name(RelativePath, AbsolutePath, [relative_to(Dir)]),
  create_file_directory(AbsolutePath).





% MESSAGES %

:- multifile
    prolog:error_message//1.

prolog:error_message(cannot_find_cli_argument(Key)) -->
  ['Cannot find CLI argument ‘--~a="VALUE"’.'-[Key]].
prolog:error_message(cannot_find_configuration(Key)) -->
  ["Cannot find the configuration section for ‘~a’."-[Key]].
