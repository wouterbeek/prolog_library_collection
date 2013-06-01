:- module(
  dcg_page,
  [
    page//3 % ?Lang:atom
            % -Type:oneof([arabic,roman]).
            % -Page:integer
  ]
).

/** <module> DCG_PAGE

DCGs for parsing/generating page information.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(dcg/basics)).
:- use_module(math(roman_numerals)).



page(Lang, Type, Page) -->
  opening_bracket,
  page(Lang, Type, Page),
  closing_bracket.
page(Lang, Type, Page) -->
  page_word(Lang), dot, blank,
  (
    % Arabic numeral.
    integer(Page),
    {Type = arabic}
  ;
    % Roman numeral.
    roman_to_arabic(Page),
    {Type = roman}
  ).

page_word(en) --> "P".
page_word(en) --> "p".
page_word(en) --> "Page".
page_word(en) --> "page".
page_word(nl) --> "P".
page_word(nl) --> "p".
page_word(nl) --> "Pagina".
page_word(nl) --> "pagina".

