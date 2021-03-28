:- module(
  cli_arguments,
  [
    cli_arguments/4 % +Usages, +Specs, -Options, -PositionalArguments
  ]
).

/** <module> Command-line argument parsing

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(string_ext)).



%! cli_arguments(+Usages:list(list(atom)),
%!               +Specs:dict,
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

cli_arguments(Usages, Specs, Options, PosArgs) :-
  current_prolog_flag(argv, Atoms),
  parse_arguments(Specs, Atoms, Pairs1, PosArgs),
  usage_arguments(Usages, PosArgs),
  set_default_options(Specs, Pairs1, Pairs2),
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


%! usage_arguments(+Usages:list(list(atom)), +PosArgs1:list(atom)) is det.

% A special case for zero arguments (e.g., `myTool --help` or `myTool
% --version`).
usage_arguments(_, []) :- !.
% A recognized usage.
usage_arguments(Usages, PosArgs) :-
  length(PosArgs, Len),
  length(Pattern, Len),
  memberchk(Pattern, Usages), !.
% Not a recognized usage.
usage_arguments(_, PosArgs) :-
  length(PosArgs, Arity),
  syntax_error(cli_args(Arity,PosArgs)).
