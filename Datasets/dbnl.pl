:- module(
  dbnl,
  [
    dbnl_scrape/2 % +Category:atom
                  % +Ordering:atom
  ]
).

/** <module> DBNL

Digitale Bibliotheek der Nederlanden

URI that currently fail:
  * http://www.dbnl.org/titels/titel.php?id=alph002jidn01
  * http://www.dbnl.org/titels/titel.php?id=bild002meng02
  * http://www.dbnl.org/titels/titel.php?id=busk001litt01
  * http://www.dbnl.org/titels/titel.php?id=crem001dokt01
  * http://www.dbnl.org/titels/titel.php?id=daal002janp01

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(uri_ext)).
:- use_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(www_browser)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(skos(skos_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



% GENERICS: CONSIDER MOVING %

%! extract_year(
%!   +Atom:atom,
%!   -PointOrInterval:oneof(integer,pair(integer))
%! ) is det.

extract_year(Atom, StartYear-EndYear):-
  split_atom_exclusive('-', Atom, [StartYearAtom, EndYearAtom]),
  maplist(extract_year, [StartYearAtom, EndYearAtom], [StartYear, EndYear]).
extract_year(Atom, Year):-
  sub_atom(Atom, _Before, 4, _After, Temp),
  atom_number(Temp, Year).
extract_year(Atom, Year):-
  sub_atom(Atom, 0, 4, _After, Temp),
  atom_number(Temp, Year).

% Publication year
process_rest(Graph, BNode, X1):-
  atom_concat('(', X2, X1),
  !,
  sub_atom(X2, 0, 4, X3, Year),
  rdf_assert_datatype(BNode, dbnl:year, gYear, Year, Graph),
  atom_concat(')', X4, X3),
  process_rest(Graph, BNode, X4).
% Publication venue
process_rest(Graph, BNode, X1):-
  atom_concat('In: ', X2, X1),
  atom_until(', ', X2, PublicationVenueName, X3),
  rdf_assert_datatype(BNode, dbnl:venue, string, PublicationVenueName, Graph),
  process_rest(Graph, BNode, X3).



% DBNL: ASSERT %

%! dbnl_assert_author(
%!   +Graph:atom,
%!   +AbsoluteAuthorURI:uri,
%!   +AuthorName:atom,
%!   -Author:uri
%! ) is det.

dbnl_assert_author(Graph, AbsoluteAuthorURI, AuthorName, Author):-
  rdfs_label(Author, AuthorName),
  rdf(Author, dbnl:original_page, AbsoluteAuthorURI, Graph),
  !.
dbnl_assert_author(Graph, AbsoluteAuthorURI, AuthorName, Author):-
  flag(author, AuthorFlag, AuthorFlag + 1),
  format(atom(AuthorID), 'author/~w', [AuthorFlag]),
  rdf_global_id(dbnl:AuthorID, Author),
  rdf_assert(Author, rdf:type, dbnl:'Author', Graph),
  rdfs_assert_label(Author, AuthorName, Graph),
  rdf_assert(Author, dbnl:orignal_page, AbsoluteAuthorURI, Graph).

%! dbnl_assert_genre(+Graph:atom, +GenreName:atom, -Genre:uri) is det.

dbnl_assert_genre(_Graph, GenreName, Genre):-
  rdfs_label(Genre, GenreName),
  !.
dbnl_assert_genre(Graph, GenreName, Genre):-
  format(atom(GenreID), 'genre/~w', [GenreName]),
  rdf_global_id(dbnl:GenreID, Genre),
  rdfs_assert_label(Genre, GenreName, Graph).

%! dbnl_assert_subgenre(
%!   +Graph:atom,
%!   +SubgenreString:atom,
%!   -Subgenre:uri
%! ) is det.
% Adds the given hierarchic string of genre and subgenre names to the graph.
% Returns the deepest subgenre resource.

% The subgenre is truly hierarchic.
dbnl_assert_subgenre(Graph, SubgenreString, Subgenre):-
  split_atom_exclusive('/', SubgenreString, TempGenreNames),
  !,
  maplist(strip([' ']), TempGenreNames, GenreNames),
  maplist(dbnl_assert_genre(Graph), GenreNames, Genres),
  dbnl_assert_subgenre_hierarchy(Graph, Genres),
  last(Genres, Subgenre).
% The subgenre is just a simple genre.
dbnl_assert_subgenre(Graph, SubgenreName, Subgenre):-
  dbnl_assert_genre(Graph, SubgenreName, Subgenre).
% @tbd See whether this clause can be removed.
dbnl_assert_subgenre(Graph, GenreName, SubgenreName):-
gtrace,
  maplist(
    dbnl_assert_genre(Graph),
    [GenreName, SubgenreName],
    [Genre, Subgenre]
  ),
  rdf_assert(Subgenre, rdfs:subClassOf, Genre, Graph).

%! dbnl_assert_subgenre_hierarchy(+Graph:atom, +Genres:list(uri)) is det.
% Assert the SKOS hierarchy for the given path of genres and subgenres.
%
% @tbd Complete the SKOS hierarchy that gets asserted here.

dbnl_assert_subgenre_hierarchy(_Graph, []):-
  !.
dbnl_assert_subgenre_hierarchy(_Graph, [_Genre]):-
  !.
dbnl_assert_subgenre_hierarchy(Graph, [Genre1, Genre2 | Genres]):-
  skos_assert_broader(Genre1, Genre2, Graph),
  dbnl_assert_subgenre_hierarchy(Graph, [Genre2 | Genres]).

%! dbnl_assert_title(+Graph:atom, +URI:uri, +Name:atom, -Title:uri) is det.

dbnl_assert_title(Graph, URI, Name, Title):-
  rdf(Title, dbnl:original_page, URI, Graph),
  rdfs_label(Title, Name),
  !.
dbnl_assert_title(Graph, URI, Name, Title):-
  flag(title, TitleFlag, TitleFlag + 1),
  format(atom(TitleID), 'title/~w', [TitleFlag]),
  rdf_global_id(dbnl:TitleID, Title),
  rdf_assert(Title, rdf:type, dbnl:'Title', Graph),
  rdfs_assert_label(Title, Name, Graph),
  % The original DBNL page where this title was described.
  rdf_assert(Title, dbnl:original_page, URI, Graph).



% DBNL: INFRASTRUCTURE %

%! dbnl_authority(-Authority:atom) is det.
% Returns the authority of the DBNL.

dbnl_authority('www.dbnl.org').

%! dbnl_base_uri(-BaseURI:uri) is det.
% Returns the base URI for the DBNL.

dbnl_base_uri(BaseURI):-
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  uri_components(BaseURI, uri_components(Scheme, Authority, '', '', '')).

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

%! dbnl_scheme(-Scheme:atom) is det.

dbnl_scheme(http).

%! dbnl_uri_resolve(+RelativeURI:uri, -AbsoluteURI:uri) is det.
% Resolve a relative URI into an absolute URI.

dbnl_uri_resolve(RelativeURI, AbsoluteURI):-
  dbnl_base_uri(BaseURI),
  uri_resolve(RelativeURI, BaseURI, AbsoluteURI).



% DBNL: DEBUG %

dbnl_debug(URI):-
  www_open_url(URI),
  gtrace. %DEB



% DBNL: DOM STRUCTURES %

%! dbnl_dom_to_contents(+DOM:dom, -Contents:dom) is det.

dbnl_dom_to_contents(DOM, Contents):-
  xpath(DOM, //table(@id=content), TABLE),
  xpath(TABLE, //td(@id=text), TD),
  xpath(TD, div(content), Contents),
  !.



% DBNL: MAIN PREDICATE %

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

  dbnl_category(Category, CategorySearchTerm),
  dbnl_ordering(Order, OrderSearchTerm),

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
  
  % First assert all titles.
  dbnl_scrape_titles(Graph, DOM).



% DBNL: VERY SPECIFIC, COSNIDER MAKING MORE GENERIC %

%! dbnl_process_secondary_literature(
%!   +Graph:atom,
%!   +Title:uri,
%!   +Contents:dom
%! ) is det.

dbnl_process_secondary_literature(Graph, Title, Contents):-
  Contents = [AuthorName, element(a, [href=RelativeURI], [TitleName]), Rest],
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  rdf_bnode(BNode),
  rdf_assert(Title, dbnl:secondary, BNode, Graph),
  rdf_assert_datatype(BNode, dbnl:author, string, AuthorName, Graph),
  rdf_assert_datatype(BNode, dbnl:title, string, TitleName, Graph),
  rdf_assert(BNode, dbnl:original_page, AbsoluteURI, Graph),
  process_rest(Graph, BNode, Rest).

%! dbnl_process_text(+Graph:atom, +Title:atom, +URI:uri) is det.
% Process the text that is located at the given URI.
%
% @tbd Could uri_resplve/3 be used instead of atom_concat/3?

dbnl_process_text(Graph, Title, URI1):-
  % There are several possibilities here:
  uri_components(
    URI1,
    uri_components(Scheme, Authority, Path, Search, Fragment)
  ),
  
  % Ensure the URI ends in '_01'.
  (
    atom_concat(_, '_01', URI1)
  ->
    URI2 = URI1
  ;
    atom_concat(_, '.php', URI1)
  ->
    URI2 = URI1
  ;
    atomic_concat(URI1, '_01', URI2)
  ),
  uri_to_html(URI2, DOM),
  term_to_atom(DOM, Atom),
  db_add_novel(user:prolog_file_type(txt, text)),
  absolute_file_name(project(temp), File, [file_type(text)]),
  atom_to_file(Atom, File),

/*
  % Retrieve the colofon DOM.
  atom_concat(URI2, '/colofon.php', ColofonURI),
  dbnl_process_text_colofon(Graph, Title, ColofonURI),
*/
  % Retrieve the downloads DOM.
  atom_concat(URI2, '/downloads.php', DownloadsURI),
  dbnl_process_text_downloads(Graph, Title, DownloadsURI),
/*
  % Retrieve the index DOM.
  atom_concat(URI2, '/index.php', IndexURI),
  dbnl_process_text_index(Graph, Title, IndexURI),
*/
  !.
dbnl_process_text(_Graph, _Title, URI):-
  gtrace, %DEB
  write(URI).

%! dbnl_process_text_colofon(+Graph:atom, +Title:uri, +URI:uri) is det.
% Asserts the contents that are found in the given colofon URI
% for the given title.
%
% @tbd Extract the various fields from the DOM.

dbnl_process_text_colofon(_Graph, _Title, URI):-
  uri_to_html(URI, DOM),
  dbnl_dom_to_contents(DOM, Contents),
  write(Contents).

%! dbnl_process_text_downloads(+Graph:atom, +Title:uri, +URI:uri) is det.
% Processed the downloads description for the given title.
%
% Example of a URI pointing to original scans:
% ==
% http://www.dbnl.org/tekst/saveas.php?
%   filename=_12m00112me01_01.pdf&
%   dir=/arch/_12m00112me01_01/pag&
%   type=pdf&
%   common=1
% ==

dbnl_process_text_downloads(Graph, Title, URI):-
  uri_to_html(URI, DOM),
  dbnl_dom_to_contents(DOM, Contents1),

  % ePUB format of the text.
  Contents1 = [_H3, _BR1, element(a, [name=epub_tekst | _], _) | Contents2],
  Contents2 =
    [element(p, _, ['Geen e-book van tekstbestand gevonden.']) | Contents3],

  % PDFs of the text.
  Contents3 = [_BR2, element(a, [name=pdf_tekst | _], _) | Contents4],
  (
    Contents4 =
      [element(p, _, ['Geen pdf van tekstbestand gevonden.']) | Contents5]
  ->
    true
  ;
    Contents4 =
      [
        element(p, _, [element(a, [href=PDFTextRelativeURI | _], _) | _])
      | Contents5
      ],
    dbnl_uri_resolve(PDFTextRelativeURI, PDFTextAbsoluteURI),
    rdf_assert(Title, dbnl:remote_pdftext, PDFTextAbsoluteURI, Graph),
    uri_query(PDFTextAbsoluteURI, filename, PDFTextFileName),
    absolute_file_name(file(PDFTextFileName), PDFTextFile, []),
    uri_to_file(PDFTextAbsoluteURI, PDFTextFile),
    rdf_assert_datatype(Title, dbnl:local_pdftext, file, PDFTextFile, Graph)
  ),

  % PDFs of the originals.
  Contents5 = [_BR3, element(a, [name=pdf_orig | _], _) | Contents6],
  Contents6 =
    [element(p, _, ['Geen pdf van originelen gevonden']) | Contents7],

  % Scans of the originals.
  (
    Contents7 = []
  ->
    true
  ;
    Contents7 = [_BR4, element(a, [name=orig | _], _) | Contents8],
    Contents8 = [_Text, element(a, [href=ScansRelativeURI | _], _) | _],
    dbnl_uri_resolve(ScansRelativeURI, ScansAbsoluteURI),
    rdf_assert(Title, dbnl:remote_scans, ScansAbsoluteURI, Graph),
    uri_query(ScansAbsoluteURI, filename, FileName),
    absolute_file_name(file(FileName), ScansFile, []),
    uri_to_file(ScansAbsoluteURI, ScansFile),
    rdf_assert_datatype(Title, dbnl:local_scans, file, ScansFile, Graph)
  ),
  !.
% For debugging.
dbnl_process_text_downloads(_Graph, _Title, URI):-
gtrace, %DEB
  write(URI).

dbnl_process_text_index(_Graph, _Title, URI):-
  uri_to_html(URI, DOM),
  dbnl_dom_to_content(DOM, Content),
  write(Content).



% DBNL: PROCESS THE CONTENTS OF A PAGE DESCRIBING A TITLE %

%! dbnl_process_title_contents(+Graph:atom, +Title:uri, +Contents:dom) is det.

dbnl_process_title_contents(_Graph, _Title, []):-
  !.
% Skip linebreaks and italized text.
dbnl_process_title_contents(
  Graph,
  Title,
  [element(Element, _, _) | Contents]
):-
  memberchk(Element, [br, i]),
  !,
  dbnl_process_title_contents(Graph, Title, Contents).
% Skip notes.
dbnl_process_title_contents(
  Graph,
  Title,
  [element(p, [class=note], _) | Contents]
):-
  !,
  dbnl_process_title_contents(Graph, Title, Contents).
dbnl_process_title_contents(Graph, Title, [Atom | Contents]):-
  atom(Atom),
  !,
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the author's orginal page, if any.
dbnl_process_title_contents(
  Graph,
  Title,
  [element(span, [class='titelpagina-auteur'], AuthorDOM) | Contents]
):-
  !,
  forall(
    (
      xpath(AuthorDOM, //a(@href), RelativeAuthorURI),
      xpath(AuthorDOM, //a(content), [AuthorName])
    ),
    (
      dbnl_uri_resolve(RelativeAuthorURI, AbsoluteAuthorURI),
      dbnl_assert_author(Graph, AbsoluteAuthorURI, AuthorName, Author),
      rdf_assert(Title, dbnl:author, Author, Graph)
    )
  ),
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the genres.
dbnl_process_title_contents(
  Graph,
  Title,
  [element(span, [class='titelpagina-genres'], [_B, GenresAtom1]) | Contents]
):-
  !,
  sub_atom(GenresAtom1, 2, _, 0, GenresAtom2),
  split_atom_exclusive(', ', GenresAtom2, GenreNames),
  maplist(dbnl_assert_genre(Graph), GenreNames, Genres),
  forall(
    member(Genre, Genres),
    rdf_assert(Title, dbnl:genre, Genre, Graph)
  ),
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the subgenres.
dbnl_process_title_contents(
  Graph,
  Title,
  [
    element(span, [class='titelpagina-subgenres'], [_B, SubgenresAtom1])
  | Contents
  ]
):-
  !,
  % In some cases there are no subgenres.
  atom_length(SubgenresAtom1, Length),
  if_then_else(
    Length =< 2,
    SubgenreNames = [],
    sub_atom(SubgenresAtom1, 2, _, 0, SubgenresAtom2)
  ),
  split_atom_exclusive(', ', SubgenresAtom2, SubgenreNames),
  maplist(dbnl_assert_subgenre(Graph), SubgenreNames, Subgenres),
  forall(
    member(Subgenre, Subgenres),
    rdf_assert(Title, dbnl:subgenre, Subgenre, Graph)
  ),
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the title.
dbnl_process_title_contents(
  Graph,
  Title,
  [element(span, [class='titelpagina-titel'], _TitleName) | Contents]
):-
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the pimary text links.
dbnl_process_title_contents(
  Graph,
  Title,
  [
    element(h4, [], ['Beschikbare tekst in de dbnl']),
    element(a, [href=RelativeTextURI | _], [TitleName])
  | Contents
  ]
):-
  !,
  % Just checking!
  (rdfs_label(Title, TitleName) -> true ; gtrace), %DEB

  dbnl_uri_resolve(RelativeTextURI, AbsoluteTextURI),
  rdf_assert(Title, dbnl:text, AbsoluteTextURI, Graph),
  dbnl_process_text(Graph, Title, AbsoluteTextURI),
  dbnl_process_title_contents(Graph, Title, Contents).
% Assert the secondary text links.
dbnl_process_title_contents(
  Graph,
  Title,
  [
    element(h4, [], ['Secundaire literatuur in de dbnl']),
    element(dl, [], DTs)
  | Contents
  ]
):-
  forall(
    member(element(dt, [], Contents), DTs),
    dbnl_process_secondary_literature(Graph, Title, Contents)
  ),
  !,
  dbnl_process_title_contents(Graph, Title, Contents).
% Unrecognized content.
dbnl_process_title_contents(_Graph, Title, Contents):-
  gtrace, %DEB
  format(user_output, '~w\n~w\n', [Title, Contents]).



% DBNL: SCRAPING TASKS %

%! dbnl_scrape_picarta_link(+Graph:atom, +Title:uri, +DOM:list) is det.

dbnl_scrape_picarta_link(Graph, Title, DOM):-
  forall(
    (
      xpath(DOM, //div(@id=meer), DIV),
      xpath(DIV, a(@href), PicartaURI)
    ),
    rdf_assert(Title, dbnl:picarta, PicartaURI, Graph)
  ).

%! dbnl_scrape_titles(+Graph:atom, +DOM:list) is det.

dbnl_scrape_titles(Graph, DOM):-
  forall(
    (
      (
        xpath(DOM, //div(@class=even), Title)
      ;
        xpath(DOM, //div(@class=odd), Title)
      ),
      Title = element(div, _Attributes, Contents)
    ),
    dbnl_scrape_title(Graph, Contents)
  ).

%! dbnl_scrape_title(+Graph:atom, +Contents:list) is det.

dbnl_scrape_title(Graph, Contents):-
  % Sometimes the authors are left out.
  (
    Contents = [element(a, LinkAttributes, [TitleName]) | Rest]
  ;
    Contents = [_Authors, element(a, LinkAttributes, [TitleName]) | Rest]
  ),
  !,
  member(href=RelativeURI, LinkAttributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  
  % Assert the title.
  dbnl_assert_title(Graph, AbsoluteURI, TitleName, Title),

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
  !,
  dbnl_scrape_title(Graph, Title, AbsoluteURI).
dbnl_scrape_title(_Graph, Contents):-
  gtrace, %DEB
  format(user_output, '~w\n', [Contents]).

%! dbnl_scrape_title(+Graph:atom, +Title:atom, +URI:uri) is det.

dbnl_scrape_title(Graph, Title, URI):-
  uri_to_html(URI, DOM),
  
  if_then(
    URI = 'http://www.dbnl.org/titels/titel.php?id=_abr001abra01',
    dbnl_debug(URI)
  ),

  % Process contents.
  dbnl_dom_to_contents(DOM, Contents),
  dbnl_process_title_contents(Graph, Title, Contents),

  % Picarta link.
  dbnl_scrape_picarta_link(Graph, Title, DOM),
  !.

% Non-existing URIs. Contact the DBNL about this.
dbnl_scrape_title(_Graph, _Title, URI):-
  member(
    URI,
    [
      'http://www.dbnl.org/titels/titel.php?id=_ikst001ikst0'
    ]
  ),
  !.
dbnl_scrape_title(_Graph, _Title, URI):-
  dbnl_debug(URI).
