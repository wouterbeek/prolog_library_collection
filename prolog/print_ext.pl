:- module(
  print_ext,
  [
    formatln/1, % +Format:atom
    formatln/2, % +Format:atom
                % +Arguments:list
    formatln/3, % +Write:stream
                % +Format:atom
                % +Arguments:list
    tab/0
  ]
).

/** <module> Print extensions

Additional predicates for printing.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(settings)).

:- setting(
  screen_width,
  integer,
  80,
  'The default width of the screen in number of characters.'
).



%! formatln(+Format:atom) is det.
% Variant of format/1 with a newline appended.

formatln(Format):-
  format(Format),
  nl.

%! formatln(+Format:atom, +Arguments:list) is det.
% Variant of format/2 with a newline appended.

formatln(Format, Args):-
  format(Format, Args),
  nl.

%! formatln(+Write:stream, +Format:atom, +Arguments:list) is det.
% Variant of format/3 with a newline appended.

formatln(Write, Format, Args):-
  format(Write, Format, Args),
  nl(Write).


%! tab is det.
% The same as builtin tab(1).

tab:-
  tab(1).
