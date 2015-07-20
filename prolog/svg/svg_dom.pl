:- module(
  svg_dom,
  [
    svg_dom/2 % +File:atom
              % -Dom:compound
  ]
).

/** <module> SVG DOM

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(sgml)).





%! svg_dom(+File:atom, Dom:compound) is det.

svg_dom(File, Dom):-
  setup_call_cleanup(
    open(File, read, Read, [encoding(utf8),type(text)]),
    load_structure(
      stream(Read),
      Dom,
      [
        dialect(xmlns),
        max_errors(1),
        shorttag(false),
        space(default),
        syntax_errors(quiet)
      ]
    ),
    close(Read)
  ).
