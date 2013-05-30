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
:- use_module(library(semweb/rdf_db)).
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
gtrace,
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

dbnl_index_author(Graph, Title, Author1):-
  strip([' ',','], Author1, Author2),
  rdf_assert_literal(Title, dbnl:author, Author2, Graph).

%! dbnl_index_title(+Graph:atom, +Contents:dom) is det.

% Create a new title.
dbnl_index_title(
  Graph,
  [Author, element(a, Attributes, [TitleName]) | Contents]
):-
  memberchk(href=RelativeURI, Attributes),
  
  % Skip the note on scans, if it is present.
  once((
    Contents = [element(i,[],['(alleen scans beschikbaar)']), YearEtc | _]
  ;
    Contents = [YearEtc | _]
  )),

  % Create the title resource.
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_assert_title(Graph, AbsoluteURI, TitleName, Title),
  rdf_assert(Title, dbnl:original_page, AbsoluteURI, Graph),
  rdfs_assert_label(Title, TitleName, Graph),

  % Author.
  dbnl_index_author(Graph, Title, Author),
  
  % Year
  atom_codes(YearEtc, C1),
  year(PointOrInterval, C1, C2),
  (
    PointOrInterval == Point1-Point2
  ->
    rdf_assert_datatype(Title, dbnl:start_year, gYear, Point1, Graph),
    rdf_assert_datatype(Title, dbnl:end_year, gYear, Point2, Graph)
  ;
    rdf_assert_datatype(Title, dbnl:year, gYear, PointOrInterval, Graph)
  ),

  % Handwritten.
  handwritten(Handwritten, C2, C3),
  rdf_assert_datatype(Title, dbnl:handwritten, boolean, Handwritten, Graph),

  % Print
  print(Print, C3, _C4),
  rdf_assert_datatype(Title, dbnl:print, int, Print, Graph).

dbnl_titles(Graph):-
  forall(
    rdfs_individual_of(Title, dbnl:'Title'),
    dbnl_title(Graph, Title)
  ).
