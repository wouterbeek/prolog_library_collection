:- module(
  cli,
  [
    cli_main/3, % +Name, +Usages, :Goal_2
    cli_main/4  % +Name, +Usages, +Specs, :Goal_2
  ]
).

/** <module> Generic support for CLI tools

This module can be used to build a CLI tool with minimal custom code.

The predicate cli_main/4 takes:

  1. the name of the tool,
  2. usage specifications (positional arguments),
  3. flag specifications (flag arguments), and
  4. the tool's main goal.

It takes care of the common flags `-h`/`--help` and `-v`/`--version`.

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

:- use_module(library(cli_arguments)).
:- use_module(library(cli_help)).
:- use_module(library(cli_version)).
:- use_module(library(dict)).

:- meta_predicate
    cli_main(+, +, 2),
    cli_main(+, +, +, 2),
    cli_main_(+, +, +, +, +, 2).



%! cli_main(+Name:atom, +Usages:list(list(atom)), :Goal_2) is det.

cli_main(Name, Usages, Goal_2) :-
  cli_main(Name, Usages, optionSpecs{}, Goal_2).


%! cli_main(+Name:atom, +Usages:list(list(atom)), +Specs:dict, :Goal_2) is det.

cli_main(Name, Usages, Specs1, Goal_2) :-
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
  catch(
    (
      cli_arguments(Usages, Specs2, Options, Args),
      cli_main_(Name, Usages, Specs2, Options, Args, Goal_2)
    ),
    Error,
    (
      cli_error(Error),
      cli_help(Name, Usages, Specs2)
    )
  ).

% Explicitly requested help message.
cli_main_(Name, Usages, Specs, Options, _, _) :-
  options{help: true} :< Options, !,
  cli_help(Name, Usages, Specs).
% Explicitly requested version message.
cli_main_(_, _, _, Options, _, _) :-
  options{version: true} :< Options, !,
  cli_version.
% Zero positional arguments: show help message.
cli_main_(Name, Usages, Specs, _, [], _) :- !,
  cli_help(Name, Usages, Specs).
% A recognized usage.
cli_main_(_, _, _, Options, Args, Goal_2) :-
  call(Goal_2, Args, Options).

cli_error(Error) :-
  print_message(error, Error).
