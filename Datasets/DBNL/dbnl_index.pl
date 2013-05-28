:- module(
  dbnl_index,
  [
    dbnl_scrape/2 % +Category:atom
                  % +Ordering:atom
  ]
).

/** <module> DBNL INDEX

Scrapes a DBNL index of titles.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_extract)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_title)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_category(+Category:atom, -SearchString:pair) is semidet.
% Returns the search string for the given category name.
%
% Supported categories:
%   * "alle titels"
%   * "middeleeuwen"
%   * "gouden eeuw"
%   * "achttiende eeuw"
%   * "negentiende eeuw"
%   * "twintigste eeuw"
%   * "eenentwintigste eeuw"

dbnl_category(Category1, c=Category2):-
  once(dbnl_category_translate(Category1, Category2)).

%! dbnl_category_translate(?CategoryName:atom, ?CategoryCode:atom) is nondet.
% Translate between category names (as used by the DBNL front-end)
% and category codes (as used by the DBNL back-end).

dbnl_category_translate('Alle titels',          a   ).
dbnl_category_translate('Middeleeuwen',         '15').
dbnl_category_translate('Gouden eeuw',          '17').
dbnl_category_translate('Achttiende eeuw',      '18').
dbnl_category_translate('Negentiende eeuw',     '19').
dbnl_category_translate('Twintigste eeuw',      '20').
dbnl_category_translate('Eenentwintigste eeuw', '21').

%! dbnl_ordering(+Ordering:atom, -SearchString:pair) is det.

dbnl_ordering(Ordering1, s=Ordering2):-
  once(dbnl_ordering_translate(Ordering1, Ordering2)).

%! dbnl_ordering_translate(?OrderingName:atom, ?OrderingCode:atom) is nondet.
% Translate between ordering names (as used by the DBNL front-end)
% and ordering codes (as used by the DBNL back-end).
%
% Supported orderings:
%   * "alfabetisch op auteur"
%   * "alfaberisch op titel"
%   * "chronologisch"
%   * "genre"

dbnl_ordering_translate('alfabetisch op auteur', a    ).
dbnl_ordering_translate('alfabetisch op titel',  t    ).
dbnl_ordering_translate(chronologisch,           c    ).
dbnl_ordering_translate(genre,                   genre).

%! dbnl_scrape(+Category:atom, +Ordering:atom) is det.
% Scrape the DBNL for the given category and using the given ordering.
%
% @arg Category The atomic name of a DBNL category. Supported categories:
%   * "alle titels"
%   * "middeleeuwen"
%   * "gouden eeuw"
%   * "achttiende eeuw"
%   * "negentiende eeuw"
%   * "twintigste eeuw"
%   * "eenentwintigste eeuw"
% @arg Ordering The atomic name of a DBNL ordering. Supported orderings:
%   * "alfabetisch op auteur"
%   * "alfaberisch op titel"
%   * "chronologisch"
%   * "genre"

dbnl_scrape(Category, Order):-
  Graph = dbnl,
  flag(deb, _OldID, 0),

  % Process predicate parameters.
  dbnl_category(Category, CategorySearchTerm),
  dbnl_ordering(Order, OrderSearchTerm),

  % Construct the URI for the index of titles.
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  Path = '/titels/index.php',
  uri_query_components(Search, [CategorySearchTerm, OrderSearchTerm]),
  Fragment = '',
  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, Search, Fragment)
  ),
  
  % First we assert all titles.
  dbnl_uri_to_html(URI, DOM),
  dbnl_index(Graph, DOM),

  % After all titles have been asserted we start scraping them.
  dbnl_titles(Graph).

%! dbnl_index(+Graph:atom, +DOM:list) is det.

dbnl_index(Graph, DOM):-
  dbnl_dom_center(DOM, Text),
  forall(
    (
      (
        xpath2(Text, //div(@class=even), Title)
      ;
        xpath2(Text, //div(@class=odd), Title)
      ),
      Title = element(div, _Attributes, Contents)
    ),
    dbnl_index_title(Graph, Contents)
  ).

%! dbnl_index_title(+Graph:atom, +Contents:dom) is det.

% Create a new title.
dbnl_index_title(Graph, Contents1):-
  selectchk(element(a, LinkAttributes, [TitleName]), Contents1, Contents2),
  memberchk(href=RelativeURI, LinkAttributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_assert_title(Graph, AbsoluteURI, TitleName, Title),
  dbnl_index_title(Graph, Title, Contents2).

%! dbnl_index_title(+Graph:atom, +Title:uri, +Contents:dom) is det.
% Scrapes a single title entry from a title index.
%
% URI example:
% ==
% http://www.dbnl.org/titels/titel.php?id=_abc002abco01
% ==
%
% These pages contain the following information in which we are interested:
%   1. Author name.
%   2. Publication title.
%   3. Genre.
%   4. Subgenres.
%   5. Available texts.
%   6. Link to Picarta / CBK information.
%
% @tbd Also extract intervals from strings like '16de eeuw'.

% Done!
dbnl_index_title(_Graph, _Title, []):-
  !.
% Skip notes on scans.
dbnl_index_title(
  Graph,
  Title,
  [element(i,[],['(alleen scans beschikbaar)']) | Contents]
):-
  dbnl_index_title(Graph, Title, Contents).
% Year.
dbnl_index_title(Graph, Title, [Year1 | Rest]):-
  % Assert the year only if it can be readily extracted.

  % Sometimes a comment occurs between the title and the year.
  % The year is always the last item in the content list.
  dbnl_extract_year(Year1, Year2),
  (
    Year2 = StartYear-EndYear
  ->
    rdf_assert_datatype(Title, dbnl:start_year, gYear, StartYear, Graph),
    rdf_assert_datatype(Title, dbnl:end_year, gYear, EndYear, Graph)
  ;
    rdf_assert_datatype(Title, dbnl:year, gYear, Year2, Graph)
  ),
  dbnl_index_title(Graph, Title, Rest).
% Debug.
dbnl_index_title(Graph, Title, [Content | Rest]):-
  %gtrace, %DEB
  format(user_output, '~w\n', [Content]),
  dbnl_index_title(Graph, Title, Rest).

dbnl_titles(Graph):-
  forall(
    rdfs_individual_of(Title, dbnl:'Title'),
    dbnl_title(Graph, Title)
  ).

