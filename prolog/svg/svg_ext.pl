:- module(
  svg_ext,
  [
    svg_dom/2 % +File, -Dom
  ]
).

/** <module> SVG extensions

@author Wouter Beek
@version 2015/07, 2015/10, 2016/04
*/

:- use_module(library(sgml)).

:- dynamic
    user:prolog_file_type/2.

:- multifile
    user:prolog_file_type/2.

user:prolog_file_type(svg, svg).





%! svg_dom(+File, -Dom) is det.

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
