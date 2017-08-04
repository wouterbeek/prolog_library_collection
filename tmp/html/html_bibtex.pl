:- module(
  html_bibtex,
  [
    bibtex_authors//1 % +Authors:list(pair(string))
  ]
).

/** <module> HTML BibTeX

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(char_ext)).
:- use_module(library(http/html_write)).





%! bibtex_author(+Author:pair(string))// is det.

bibtex_author(Given-Family) -->
  {
    first_char(Given, First),
    upcase_char(First, UpcaseFirst)
  },
  html([Family, ", ",UpcaseFirst,"."]).



%! bibtex_authors(+Authors:list(pair(string)))// is det.

bibtex_authors([]) --> !, [].
bibtex_authors([H]) --> !,
  bibtex_author(H).
bibtex_authors([H|T]) -->
  html([
    \bibtex_author(H),
    &(amp),
    \bibtex_authors(T)
  ]).
