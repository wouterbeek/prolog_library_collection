:- module(
  html_symbol,
  [
    pipe//0
  ]
).

/** <module> HTML symbol

Symbols for inclusion into HTML.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(http/html_write)).





%! pipe// is det.

pipe -->
  html(span(class=pipe, '|')).
