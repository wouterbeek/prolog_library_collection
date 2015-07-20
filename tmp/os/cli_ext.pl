:- module(
  cli_ext,
  [
    cli_long_flag/3 % +Flag:atom
                    % +Value
                    % -Argument:atom
  ]
).

/** <module> Command Line Interface extensions

Support for building and using command line interfaces.

@author Wouter Beek
@version 2015/01
*/




%! cli_long_flag(+Flag:atom, +Value, -Argument:atom) is det.

cli_long_flag(Flag, Value, Argument):-
  format(atom(Argument), '--~w=~w', [Flag,Value]).

