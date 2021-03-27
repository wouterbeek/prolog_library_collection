:- module(
  cli,
  [
    cli_main/3 % +Usages, +Flags, :Goal_2
  ]
).

/** <module> Generic support for CLI tools

This module can be used to build a CLI tool with minimal custom code.

The predicate cli_main/3 takes:

  1. an option specification, and
  2. a list of (command-line) arguments

and returns:

  1. an options dictionary containing the flag arguments whose values
     are of the specified types, and
  2. a list containing the positional arguments as atoms.

It also takes care of the common flags `-h`/`--help` and
`-v`/`--version`.

The following three types of command-line argument can be
distinguised:

  * Runtime arguments

    Should be handled by the Prolog runtime, and are therefore not
    handled by this module.  Runtime arguments must appear before
    other arguments, and not be separated from other arguments by
    `--`.

  * Flag arguments

    Key-value pairs (with a boolean value possibly implicit) intended
    to control your program in one way or another.

  * Positional arguments

    What remains after all runtime arguments and options have been
    removed (with implicit arguments ― true/false for booleans ―
    filled in).

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).

:- use_module(library(cli_help)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(string_ext)).

:- dynamic
    name/1,
    title/1,
    version/1.

:- meta_predicate
    cli_main(+, +, 2),
    main_(+, +, +, +, 2).



%! cli_main(+Usages:list(string),
%!          +Flags:list(list(compound)),
%!          :Goal_2) is det.

cli_main(Usages, Flags, Goal_2) :-
  cli_arguments(Flags, Options, Args),
  main_(Usages, Flags, Options, Args, Goal_2).

main_(Usages, Flags, Options, _, _) :-
  options{help: true} :< Options, !,
  cli_help(Usages, Flags, Options).
main_(_, _, Options, _, _) :-
  options{version: true} :< Options, !,
  cli_version.
main_(_, _, Options, Args, Goal_2) :-
  call(Goal_2, Args, Options).


%! cli_arguments(+Specs:flags,
%!               -Options:options,
%!               -PositionalArguments:list(atom)) is det
%
% Parse the arguments Arguments (as list of atoms) according to Specs.
% Any runtime arguments (typically terminated by '--') are assumed to
% be removed already.
%
% @param Options
%
%        An options dictionary containing the parsed flags.
%
% @param PositionalArguments
%
%        The remaining non-flag or ‘positional’ arguments.  Positional
%        arguments are allows to appear interspersed with flag
%        arguments (although it is good practice to put positional
%        arguments last).
%
% @error Dashed args not in Specs are not permitted and will raise an
%        error.

cli_arguments(Specs1, Options, PosArgs) :-
  Specs0 = optionSpecs{
    help: optionSpec{
      default: false,
      help: "Display help information for this tool and exit.",
      longflags: [help],
      shortflags: [h],
      type: boolean
    },
    version: optionSpec{
      default: false,
      help: "Display the version number of this tool and exit.",
      longflags: [version],
      shortflags: [v],
      type: boolean
    }
  },
  merge_dicts(Specs0, Specs1, Specs2),
  current_prolog_flag(argv, Atoms),
  parse_arguments(Specs2, Atoms, Pairs1, PosArgs),
  set_default_options(Specs2, Pairs1, Pairs2),
  dict_pairs(Options, Pairs2).

%! parse_arguments(+Specs:dict,
%!                 +Arguments:list(atom),
%!                 -Pairs:list(pair(atom,term)),
%!                 -PositionalArguments:list(atom)) is det.

% done
parse_arguments(_, [], [], []) :- !.
% flag argument
parse_arguments(Specs, [H1|T1], [H2|T2], L3) :-
  atom_phrase(parse_flag(Specs, H2), H1), !,
  parse_arguments(Specs, T1, T2, L3).
% positional argument
parse_arguments(Specs, [Arg|T1], L2, [Arg|T3]) :-
  parse_arguments(Specs, T1, L2, T3).

%! parse_flag(+Specs:dict, -Pair:pair(atom,term))// .

% Long negative flag: Boolean false.
parse_flag(Specs, Key-false) -->
  "--no-", !,
  {must_be_key_type_(Specs, Key, boolean)},
  remainder_as_atom(Key).
% Long explicit flag: the specified type.
parse_flag(Specs, Key-Value) -->
  "--",
  '...'(Codes),
  "=", !,
  {atom_codes(Key, Codes)},
  {key_type_(Specs, Key, Type)},
  parse_value(Type, Value).
% Long implicit flag: Boolean true.
parse_flag(Specs, Key-true) -->
  "--", !,
  remainder_as_atom(Key),
  {must_be_key_type_(Specs, Key, boolean)}.
% Short explicit flag: the specified type.
parse_flag(Specs, Key-Value) -->
  "-",
  dcg_char(Key),
  "=", !,
  {key_type_(Specs, Key, Type)},
  parse_value(Type, Value).
% Short implicit flag: Boolean true.
parse_flag(Specs, Key-true) -->
  "-", !,
  dcg_char(Key),
  {must_be_key_type_(Specs, Key, boolean)}.

key_type_(Specs, Key, Type) :-
  dict_get(Key, Specs, Spec), !,
  dict_get(type, Spec, Type).
key_type_(_, Key, _) :-
  existence_error(cli_key(Key), cli_unspecified_key).

must_be_key_type_(Specs, Key, SyntacticType) :-
  key_type_(Specs, Key, SemanticType),
  (   SemanticType = SyntacticType
  ->  true
  ;   syntax_error(cli_type_conflict(Key,SyntacticType,SemanticType))
  ).

%! parse_value(+Type:term, -Value:term)// is det.

parse_value(atom, Value) --> !,
  remainder_as_atom(Value).
parse_value(boolean, Value) --> !,
  dcg_boolean(Value).
parse_value(oneof(Values), Value) --> !,
  remainder_as_atom(Value),
  {must_be(oneof(Values), Value)}.
parse_value(string, Value) -->
  remainder_as_string(Value).

%! set_default_options(+Specs:dict,
%!                     +Pairs1:pair(atom,term),
%!                     -Pairs2:pair(atom,term)) is det.

set_default_options(Specs, L2, L3) :-
  dict_pairs(Specs, L1),
  set_default_options_(L1, L2, L3).

% done
set_default_options_([], _, []) :- !.
% explicit value
set_default_options_([Key-_|T1], L2, [Key-Value|T3]) :-
  memberchk(Key-Value, L2), !,
  set_default_options_(T1, L2, T3).
% default value
set_default_options_([Key-Spec|T1], L2, [Key-Default|T3]) :-
  optionSpec{default: Default} :< Spec, !,
  set_default_options_(T1, L2, T3).
% skip
set_default_options_([_|T1], L2, L3) :-
  set_default_options_(T1, L2, L3).


%! cli_version is det.

cli_version :-
  % Requires that the CLI tool was run from a directory containing a
  % pack.pl metadata file.
  exists_file('pack.pl'),
  consult(pack),
  name(Name),
  title(Title),
  version(Version),
  format(user_output, "~a: ~a (~a)\n", [Name,Title,Version]).
