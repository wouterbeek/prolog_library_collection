:- module(
  dbnl,
  [
    dbnl_scrape/2 % +Category:atom
                  % +Order:atom
  ]
).

/** <module> DBNL

Digitale Bibliotheek der Nederlanden

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(skos(skos_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



% GENERICS: CONSIDER MOVING %

extract_year(Atom, StartYear-EndYear):-
  split_atom_exclusive('-', Atom, [StartAtom, EndAtom]),
  !,
  maplist(extract_year, [StartAtom, EndAtom], [StartYear, EndYear]).
extract_year(Atom, Year):-
  sub_atom(Atom, _Before, 4, _After, Temp),
  atom_number(Temp, Year).



% DBNL %

dbnl_assert_author(_Graph, AuthorName, Author):-
  rdfs_label(Author, AuthorName),
  !.
dbnl_assert_author(Graph, AuthorName, Author):-
  flag(author, AuthorFlag, AuthorFlag + 1),
  format(atom(AuthorID), 'author/~w', [AuthorFlag]),
  rdf_global_id(dbnl:AuthorID, Author),
  rdf_assert(Author, rdf:type, dbnl:'Author', Graph),
  rdfs_assert_label(Author, AuthorName, Graph).

dbnl_assert_genre(_Graph, GenreName, Genre):-
  rdfs_label(Genre, GenreName),
  !.
dbnl_assert_genre(Graph, GenreName, Genre):-
  format(atom(GenreID), 'genre/~w', [GenreName]),
  rdf_global_id(dbnl:GenreID, Genre),
  rdfs_assert_label(Genre, GenreName, Graph).

% The subgenre is truly hierarchic.
dbnl_assert_subgenre(Graph, SubgenreString, Subgenre):-
  split_atom_exclusive('/', SubgenreString, TempGenreNames),
  !,
  maplist(strip([' ']), TempGenreNames, GenreNames),
  maplist(dbnl_assert_genre(Graph), GenreNames, Genres),
  dbnl_assert_subgenre_hierarchy(Graph, Genres),
  last(Genres, Subgenre).
% The subgenre is like a genre.
dbnl_assert_subgenre(Graph, SubgenreName, Subgenre):-
  dbnl_assert_genre(Graph, SubgenreName, Subgenre).

dbnl_assert_subgenre(Graph, GenreName, SubgenreName):-
  maplist(
    dbnl_assert_genre(Graph),
    [GenreName, SubgenreName],
    [Genre, Subgenre]
  ),
  rdf_assert(Subgenre, rdfs:subClassOf, Genre, Graph).

dbnl_assert_subgenre_hierarchy(_Graph, []):-
  !.
dbnl_assert_subgenre_hierarchy(_Graph, [_Genre]):-
  !.
dbnl_assert_subgenre_hierarchy(Graph, [Genre1, Genre2 | Genres]):-
  skos_assert_broader(Genre1, Genre2, Graph),
  dbnl_assert_subgenre_hierarchy(Graph, [Genre2 | Genres]).

dbnl_authority('www.dbnl.org').

dbnl_base_uri(BaseURI):-
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  uri_components(BaseURI, uri_components(Scheme, Authority, '', '', '')).

dbnl_category(Category1, c=Category2):-
  once(dbnl_category_translate(Category1, Category2)).

dbnl_category_translate('Alle titels',          a ).
dbnl_category_translate('Middeleeuwen',         15).
dbnl_category_translate('Gouden eeuw',          17).
dbnl_category_translate('Achttiende eeuw',      18).
dbnl_category_translate('Negentiende eeuw',     19).
dbnl_category_translate('Twintigste eeuw',      20).
dbnl_category_translate('Eenentwintigste eeuw', 21).

dbnl_order(Order1, s=Order2):-
  once(dbnl_order_translate(Order1, Order2)).

dbnl_order_translate('alfabetisch op auteur', a    ).
dbnl_order_translate('alfabetisch op titel',  t    ).
dbnl_order_translate(chronologisch,           c    ).
dbnl_order_translate(genre,                   genre).

/*
dbnl_process_text(_Graph, _Title, URI):-
  uri_to_html(URI, DOM),
  gtrace,
  findall(
    RelativeURI-Title,
    (
      xpath(DOM, a(@class=head3), A),
      xpath(DOM, //div(@id=wrapper), DIV1),
      xpath(DIV1, div(self), DIV2),
      xpath(DIV2, table(self), TABLE),
      xpath(TABLE, tbody(self), TBODY),
      xpath(TBODY, tr(self), TR),
      xpath(TR, td(self), TD),
      xpath(TD, div(self), DIV3),
      xpath(DIV3, p, P),
      xpath(P, a(@class=head3), A),
      A = element(a, [href=RelativeURI | _], [Title])
    ),
    Contents
  ),
  write(Contents).
*/

dbnl_process_title(Title, URI, Graph):-
  uri_to_html(URI, DOM),

  % Assert the author's orginal page, if any.
  forall(
    xpath(DOM, //span(@class='titelpagina-auteur')/h1/a, A),
    (
      % In some cases there is no author information added.
      unless(
        A = element(a, _, []),
        (
          A = element(a, [href=RelativeAuthorURI | _], [AuthorName]),
          dbnl_assert_author(Graph, AuthorName, Author),
          rdf_assert(Title, dbnl:author, Author, Graph),
          dbnl_uri_resolve(RelativeAuthorURI, AbsoluteAuthorURI),
          rdf_assert(Author, dbnl:orignal_page, AbsoluteAuthorURI, Graph)
        )
      )
    )
  ),

  % Assert the genres.
  xpath(DOM, //span(@class='titelpagina-genres'), Genres_SPAN),
  Genres_SPAN = element(span, _, [_, TempGenres]),
  sub_atom(TempGenres, 2, _, 0, TempGenreNames),
  split_atom_exclusive(', ', TempGenreNames, GenreNames),
  maplist(dbnl_assert_genre(Graph), GenreNames, Genres),
  forall(
    member(Genre, Genres),
    rdf_assert(Title, dbnl:genre, Genre, Graph)
  ),

  % Assert the subgenres.
  xpath(DOM, //span(@class='titelpagina-subgenres'), Subgenres_SPAN),
  Subgenres_SPAN = element(span, _, [_, TempSubgenres]),
  atom_length(TempSubgenres, Length),
  if_then_else(
    % In some cases there are no subgenres.
    Length =< 2,
    TempSubgenreNames = [],
    sub_atom(TempSubgenres, 2, _, 0, TempSubgenreNames)
  ),
  split_atom_exclusive(', ', TempSubgenreNames, SubGenreNames),
  maplist(dbnl_assert_subgenre(Graph), SubGenreNames, Subgenres),
  forall(
    member(Subgenre, Subgenres),
    rdf_assert(Title, dbnl:subgenre, Subgenre, Graph)
  ),

  % Assert the text links.
  forall(
    (
      xpath(DOM, //div(@id=wrapper), DIV1),
      xpath(DIV1, div(self), DIV2),
      xpath(DIV2, table(self), TABLE),
      xpath(TABLE, tbody(self), TBODY),
      xpath(TBODY, tr(self), TR),
      xpath(TR, td(self), TD),
      xpath(TD, div(self), DIV3),
      xpath(DIV3, a, TextA)
    ),
    (
      TextA = element(a, [href=RelativeTextURI | _], [TitleName]),
      rdfs_label(Title, TitleName),
      dbnl_uri_resolve(RelativeTextURI, AbsoluteTextURI),
      rdf_assert(Title, dbnl:text, AbsoluteTextURI, Graph)
      %%%%dbnl_process_text(Graph, Title, AbsoluteTextURI)
    )
  ),
  !.
% Oops, debugging!
dbnl_process_title(Title, URI, _Graph):-
  gtrace, %DEB
  format(user_output, 'Title:\t~w\nURI:\t~w\n', [Title, URI]).

dbnl_register_title(Graph, element(div, _DIV_Attributes, DIV_Contents)):-
  % Sometimes the authors are left out.
  (
    DIV_Contents = [element(a, LinkAttributes, [TitleName]) | Rest]
  ;
    DIV_Contents = [_Authors, element(a, LinkAttributes, [TitleName]) | Rest]
  ),
  !,

  % Assert the title.
  flag(title, TitleFlag, TitleFlag + 1),
  format(atom(TitleID), 'title/~w', [TitleFlag]),
  rdf_global_id(dbnl:TitleID, Title),
  rdf_assert(Title, rdf:type, dbnl:'Title', Graph),
  rdfs_assert_label(Title, TitleName, Graph),

  % Assert the year only if it can be readily extracted.
  % @tbd Also extract intervals from strings like '16de eeuw'.
  if_then(
    (
      % Sometimes a comment occurs between the title and the year.
      % The year is always the last item in the content list.
      last(Rest, TempYear),
      extract_year(TempYear, Year)
    ),
    if_then_else(
      Year = StartYear-EndYear,
      (
        rdf_assert_datatype(Title, dbnl:start_year, gYear, StartYear, Graph),
        rdf_assert_datatype(Title, dbnl:end_year, gYear, EndYear, Graph)
      ),
      rdf_assert_datatype(Title, dbnl:year, gYear, Year, Graph)
    )
  ),

  % The original DBNL page where this title was described.
  member(href=RelativeURI, LinkAttributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  rdf_assert(Title, dbnl:original_page, AbsoluteURI, Graph),

  % Successful!
  !.
% Oops, we need to do some debugging now...
dbnl_register_title(_Graph, Element):-
  gtrace, %DEB
  format(user_output, '~w\n', [Element]).

dbnl_scheme(http).

dbnl_scrape(Category, Order):-
  Graph = dbnl,

  dbnl_category(Category, CategorySearchTerm),
  dbnl_order(Order, OrderSearchTerm),

  % URI parameters.
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  Path = '/titels/index.php',
  uri_query_components(Search, [CategorySearchTerm, OrderSearchTerm]),
  Fragment = '',

  uri_components(
    URI,
    uri_components(Scheme, Authority, Path, Search, Fragment)
  ),

  uri_to_html(URI, DOM),
  findall(
    Title,
    (
      xpath(DOM, //div(@class=even), Title)
    ;
      xpath(DOM, //div(@class=odd), Title)
    ),
    Titles
  ),
  maplist(dbnl_register_title(Graph), Titles),
  step2.

dbnl_uri_resolve(RelativeURI, AbsoluteURI):-
  dbnl_base_uri(BaseURI),
  uri_resolve(RelativeURI, BaseURI, AbsoluteURI).

step2:-
  Graph = dbnl,
  forall(
    rdfs_individual_of(Title, dbnl:'Title'),
    (
      rdf(Title, dbnl:original_page, URI, Graph),
      dbnl_process_title(Title, URI, Graph)
    )
  ).

/*
step3:-
  forall(
    rdf(Title, dbnl:text, AbsoluteTextURI, Graph),
    dbnl_process_text(Graph, Title, AbsoluteTextURI)
  ).
*/
