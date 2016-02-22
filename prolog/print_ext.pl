:- module(
  print_ext,
  [
    formatln/1, % +Format:atom
    formatln/2, % +Format:atom
                % +Arguments:list
    formatln/3, % +Write:stream
                % +Format:atom
                % +Arguments:list
    print_table/1, % +Rows:list
    print_table/2, % +Rows:list
                   % :Options:list(compound)
    tab/0
  ]
).

/** <module> Print extensions

Additional predicates for printing.

@author Wouter Beek
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(settings)).

:- meta_predicate(print_table(+,:)).

:- predicate_options(print_table/2, 2, [
     pass_to(dcg_table/4, 2)
   ]).

:- setting(screen_width, integer, 80,
     'The default width of the screen in number of characters.'
   ).





%! formatln(+Format:atom) is det.
% Variant of format/1 with a newline appended.

formatln(Format):- format(Format), nl.


%! formatln(+Format:atom, +Arguments:list) is det.
% Variant of format/2 with a newline appended.

formatln(Format, Args):- format(Format, Args), nl.


%! formatln(+Write:stream, +Format:atom, +Arguments:list) is det.
% Variant of format/3 with a newline appended.

formatln(Write, Format, Args):- format(Write, Format, Args), nl(Write).



%! print_table(+Rows:list) is det.
% Wrapper around print_table/2 with default options.

print_table(L):-
  print_table(_, L).


%! print_table(+Rows:list, :Options:list(compound)) is det.

print_table(Rows, Opts):-
  dcg_with_output_to(current_output, dcg_table(Rows, Opts)).



%! tab is det.
% Wrapper around tab/1 that prints a single horizontal tab character.

tab:-
  tab(1).
