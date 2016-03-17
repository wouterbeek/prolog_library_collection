:- module(
  ansi_ext,
  [
    ansi_format/4 % +Output, +Attributes, +Format, +Args
  ]
).

:- reexport(library(ansi_term)).

/** <module> ANSI extensions

@author Wouter Beek
@version 2015/08, 2016/03
*/

:- use_module(library(ansi_term)).





ansi_format(Output, Attrs, Format, Args) :-
  with_output_to(Output, ansi_format(Attrs, Format, Args)).
