:- module(
  print_ext,
  [
    formatln/1,    % +Format
    formatln/2,    % +Format, +Args
    formatln/3,    % +Write, +Format, +Args
    indent/1,      % +NoSpaces
    print_table/1, % +Rows
    print_table/2, % +Rows, :Opts
    tab/0
  ]
).

/** <module> Print extensions

Additional predicates for printing.

@author Wouter Beek
@version 2015/08, 2015/11, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(settings)).

:- meta_predicate
    print_table(+, :).

:- predicate_options(print_table/2, 2, [
     pass_to(dcg_table/4, 2)
   ]).

:- setting(screen_width, integer, 80,
     'The default width of the screen in number of characters.'
   ).





%! formatln(+Format) is det.
%! formatln(+Format, +Args) is det.
%! formatln(+Write, +Format, +Args) is det.
% Variants of format/[1-3] with a newline appended.

formatln(Format):-
  format(Format), nl.

formatln(Format, Args):-
  format(Format, Args), nl.

formatln(Write, Format, Args):-
  format(Write, Format, Args),
  nl(Write).



%! indent(+NoSpaces) is det.

indent(N) :-
  N < 1, !.
indent(N1) :-
  write(" "),
  N2 is N1 - 1,
  indent(N2).


%! print_table(+Rows) is det.
%! print_table(+Rows, :Opts) is det.

print_table(L):-
  print_table(_, L).

print_table(Rows, Opts):-
  dcg_with_output_to(current_output, dcg_table(Rows, Opts)).



%! tab is det.
% Wrapper around tab/1 that prints a single horizontal tab character.

tab:-
  tab(1).
