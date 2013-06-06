:- module(
  dbnl_generic,
  [
% DEBUG
    dbnl_debug/0,
    dbnl_debug/1, % +URI:uri
    dbnl_set_current_uri/1, % +URI:uri

% DOM
    dbnl_dom_center/2, % +Page:dom
                       % -Center:dom
    dbnl_dom_left/2, % +DOM:dom
                     % -Left:dom
    dbnl_dom_notes/2, % +Page:dom
                      % -Notes:pair(atom,dom))
    dbnl_dom_right/2, % +DOM:dom
                      % -Right:dom

% DCG
    dbnl_author//1, % -AuthorName:atom
    dbnl_copyright//2, % +Graph:atom
                       % +Text:uri
    dbnl_editor//1, % -EditorName:atom
    dbnl_genres//2, % +Graph:atom
                    % +Text:uri
    dbnl_handwritten//2, % ?Language:atom
                         % ?Handwritten:boolean
    dbnl_journal//2, % +Graph:atom
                     % +Text:uri
    dbnl_logo//0,
    dbnl_page//2, % -Type:oneof([page,prepage])
                  % -Page:integer
    dbnl_publication_print//3, % ?Lang:atom
                               % ?Number:integer
                               % ?Changes:boolean
%    dbnl_source//2, % +Graph:atom
%                    % +Text:uri
    dbnl_title//2, % +Graph:atom
                   % +Text:uri
    dbnl_volume//2, % +Graph:atom
                    % +Text:uri
    dbnl_year//2, % ?Language:atom
                  % -Year:oneof([integer,pair(integer)])

% URI
    dbnl_authority/1, % -Authority:atom
    dbnl_base_uri/1, % -Base:uri
    dbnl_save_image/3, % +Graph:atom
                       % +ImageURI:uri
                       % -Image:bnode
    dbnl_save_image/4, % +Graph:atom
                       % +ImageURI:uri
                       % +Caption:dom
                       % -Image:bnode
    dbnl_scheme/1, % -Scheme:atom
    dbnl_uri_resolve/2, % +Relative:uri
                        % -Absolute:uri
    dbnl_uri_resolve/3, % +Relative:uri
                        % +Base:uri
                        % -Absolute:uri
    dbnl_uri_to_html/2 % +URI:uri
                       % -HTML:dom
  ]
).

/** <module> DBNL GENERIC

Generic predicates for scraping the DBNL.

Predicates for extracting information from atoms in DBNL.

---+ Parsing problems

---++ Problem 1

The use of the question mark for expressing the uncertainty of publication
years cannot be disambiguated. The following two examples show this.

==
Mentor, Raadgever voor de Nederlandsche jeugd., 1867-1873?
Volkszangdag, 1922-19??
==

In the former the question mark should be interpreted as expressing
uncertainty of an *expressed* digit (the 3 in this case). What is meant
is the interval 1870-1879 (not 1870-18799).

In the latter the question mark should be interpreted as expressing
uncertainty of an *unexpressed* digit. What is means is the interval
1922-1999.

@author Wouter Beek
@version 2013/05-2013/06
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_copyright)).
:- use_module(dcg(dcg_dict)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_page)).
:- use_module(dcg(dcg_print)).
:- use_module(dcg(dcg_volume)).
:- use_module(dcg(dcg_year)).
:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(html(html)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- dynamic(dbnl_current_uri(_URI)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



% DEBUG %

dbnl_debug:-
  dbnl_current_uri(URI),
  dbnl_debug(URI).

dbnl_debug(URI):-
  flag(deb, ID, ID + 1),
  (
    ID > 0
  ->
    once(www_open_url(URI))
  ;
    true
  ),
  gtrace.

dbnl_set_current_uri(URI):-
  flag(number_of_uris, ID, ID + 1),
  db_replace_novel(dbnl_current_uri(URI)).



% DOM %

dbnl_dom_center(DOM, Content):-
  findall(
    Content,
    xpath2(DOM, //td(@id=text,content), Content),
    Contents
  ),
  append(Contents, Content).

dbnl_dom_left(DOM, Content):-
  findall(
    Content,
    (
      xpath2(DOM, //td(@id=left), Left),
      xpath2(Left, div(content), Content)
    ),
    Contents
  ),
  append(Contents, Content).

dbnl_dom_notes(DOM, Notes):-
  findall(
    NoteIndex-Contents,
    (
      xpath2(DOM, //div(@class=note), DIV),
      DIV = element(div, _, [element(a, Attrs, _) | Contents]),
      memberchk(name=NoteIndex, Attrs)
    ),
    Notes
  ).

dbnl_dom_right(DOM, Content):-
  findall(
    Content,
    (
      xpath2(DOM, //td(@id=right), Right),
      xpath2(Right, div(content), Content)
    ),
    Contents
  ),
  append(Contents, Content).



% DCGS %

% Ended by a separator.
dbnl_author(AuthorName) -->
  dcg_atom_until(comma, AuthorName).
% The entiry input.
dbnl_author(AuthorName) -->
  dcg_atom_all(AuthorName).

dbnl_copyright(Graph, Text) -->
  [element(div, [class=copyright|_], [element(a, _, ['©']), Atom])],
  !,
  {
    dcg_phrase(copyright(Holders, Year), Atom),
    dbnl_copyright0(Graph, Text, Holders, Year)
  }.
dbnl_copyright(Graph, Text) -->
  [Atom],
  {
    atom(Atom),
    dcg_phrase(copyright(Holders, Year), Atom),
    dbnl_copyright0(Graph, Text, Holders, Year)
  }.

dbnl_copyright0(Graph, Text, Holders, Year):-
  dbnl_assert_copyright(Graph, Holders, Year, Copyright),
  rdf_assert(Text, dbnl:copyright, Copyright, Graph).

dbnl_editor(EditorName) -->
  ("editie" ; "hoofdredactie"),
  blank,
  dcg_atom_all(EditorName).

dbnl_genres(_Graph, _Text) --> [].
dbnl_genres(Graph, Text) -->
  (colon, blank ; ""),
  (
    dcg_atom_until(comma, GenreAtom), comma, blank
  ;
    dcg_atom_all(GenreAtom), {GenreAtom \== ''}
  ),
  {
    dbnl_assert_genre(Graph, GenreAtom, Genre),
    rdf_assert(Text, dbnl:genre, Genre, Graph)
  },
  dbnl_genres(Graph, Text).

dbnl_handwritten(nl, true) --> "(handschrift)".

dbnl_journal(Graph, Text) -->
  journal(Lang, Title, Volume),
  {
    if_then_else(
      var(Lang),
      rdf_assert_literal(Text, dbnl:journal, Title, Graph),
      rdf_assert_literal(Text, dbnl:journal, Lang, Title, Graph)
    ),
    unless(
      var(Volume),
      rdf_assert_datatype(Text, dbnl:volume, int, Volume, Graph)
    )
  }.

dbnl_logo -->
  [element(img, Attrs, [])],
  {memberchk(alt='DBNL vignet', Attrs)}.

dbnl_page(Type2, Page) -->
  page(_Lang, Type1, Page),
  {if_then(Type1 == roman, Type2 = prepage)},
  {if_then(Type1 == arabic, Type2 = page)}.

dbnl_publication_print(Lang, Number, Changes) -->
  publication_print(Lang, Number, Changes).
dbnl_publication_print(nl, _Number, _Changes) -->
  "(volksuitgave, 1ste vijfduizendtal)".
dbnl_publication_print(nl, _Number, _Changes) -->
  "(20ste-30ste duizendtal)".

/* THIS IS VERY DIFFICULT
dbnl_source(Graph, Text) -->
  "bron", colon, blank,
  dcg_atom_all(Source),
  !,
  dbnl_author(Author), comma, blank,
  % Book title. Publisher. Cities.
  dcg_atom_until(dot, Title), dot, blank,
  dcg_atom_until(comma, Publisher), comma, blank,
  dcg_separated_list(forward_slash, Cities1), blank,
  year(_Lang2, _Year), blank,
  !,
  {
    rdf_assert_literal(Text, dbnl:author, Author, Graph),
    rdf_assert_literal(Text, dbnl:title, Title, Graph),
    rdf_assert_literal(Text, dbnl:publisher, Publisher, Graph),
    maplist(atom_codes, Cities2, Cities1),
    forall(
      member(City, Cities2),
      rdf_assert_literal(Text, dbnl:city, City, Graph)
    )
  },
  dcg_codes_all(_).
*/

dbnl_title(Graph, Text) -->
  % Notice that cannot give the DCG body
  % =|((dot, blank) ; (space, opening_bracket))|= to dcg_atom_until//2.
  (
    dcg_atom_until((dot, space), TitleName),
    dot, space
  ;
    dcg_atom_until((space, opening_bracket), TitleName),
    space
  ),
  % Volume is optional. There are cases in which it is not
  % followed by a blank.
  (dbnl_volume(Graph, Text), blanks ; ""),
  {rdfs_assert_label(Text, TitleName, Graph)}.
dbnl_title(Graph, Text) -->
  dcg_atom_all(TitleName),
  {rdfs_assert_label(Text, TitleName, Graph)}.

dbnl_volume(Graph, Text) -->
  volume(_Lang, Volume),
  {rdf_assert_datatype(Text, dbnl:volume, int, Volume, Graph)}.

dbnl_year(Graph, Text) -->
  (dot, blank ; ""),
  dbnl_year0(_Lang, Year),
  {dbnl_assert_year(Graph, Text, Year)}.

dbnl_year0(Lang, Year) -->
  year(Lang, Year),
  (blanks, uncertainty(Lang) ; "").
% Hacked year interval.
dbnl_year0(Lang, Year1-Year2) -->
  year_interval(Lang, Year1-Year2),
  % This is an arbitrary disambiguation criterion for problem 1 (see header).
  (
    ""
  ;
    {atom_number(Atom, Year2)},
    {atom_length(Atom, 4)},
    uncertainty(Lang)
  ).
% Context-dependent indicator. Not resolved and asserted yet.
dbnl_year0(Lang, _Year) -->
  "z.j.",
  (blanks, uncertainty(Lang) ; "").
dbnl_year0(_Lang, _Year) -->
  question_mark.
dbnl_year0(_Lang, Year1-Year2) -->
  year(Lang, Year1), comma, blank,
  year(Lang, Year2),
  opening_square_bracket, integer(_), closing_square_bracket.
dbnl_year0(Lang, Year1-Year2) -->
  integer(Year1), blanks, conj(Lang), blanks, integer(Year2).
dbnl_year0(Lang, Year1-Year3) -->
  integer(Year1), hyphen_minus, integer(_Year2),
  blanks, conj(Lang), blanks,
  integer(Year3).
% @tbd Not yet parsed. What is the implication of status 'written' for the
%      print information?
dbnl_year0(nl, 1919) --> "ná aug. 1919 geschreven".
dbnl_year0(nl, 1928) --> "? [1928] (?)".
dbnl_year0(nl, 1626) --> "1626, herdr. 1638".



% URI %

%! dbnl_authority(-Authority:atom) is det.
% Returns the authority of the DBNL.

dbnl_authority('www.dbnl.org').

%! dbnl_base_uri(-BaseURI:uri) is det.
% Returns the base URI for the DBNL.

dbnl_base_uri(BaseURI):-
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  uri_components(BaseURI, uri_components(Scheme, Authority, '/', '', '')).

dbnl_save_image(Graph, ImageURI, BNode):-
  dbnl_save_image(Graph, ImageURI, [], BNode).

dbnl_save_image(Graph, ImageURI, Caption, BNode):-
  uri_to_file_name(ImageURI, ImageFileName),
  absolute_file_name(file(ImageFileName), ImageFile, []),
  uri_to_file(ImageURI, ImageFile),
  rdf_bnode(BNode),
  rdfs_assert_individual(BNode, dbnl:'Image', Graph),
  rdf_assert_datatype(BNode, dbnl:file, file, ImageFile, Graph),
  (
    Caption \== []
  ->
    dom_to_xml(dbnl, Caption, XML_Caption),
    rdf_assert_xml_literal(BNode, dbnl:caption, XML_Caption, Graph)
  ;
    true
  ).

%! dbnl_scheme(-Scheme:atom) is det.

dbnl_scheme(http).

%! dbnl_uri_resolve(+Relative:uri, -Absolute:uri) is det.
% Resolve a relative URI into an absolute URI.

dbnl_uri_resolve(Absolute, Absolute):-
  uri_is_global(Absolute),
  !.
dbnl_uri_resolve(Relative, Absolute):-
  dbnl_base_uri(Base),
  uri_resolve(Relative, Base, Absolute).

dbnl_uri_resolve(Relative, Base, Absolute):-
  uri_resolve(Relative, Base, Absolute),
  catch(
    http_open(Absolute, _Stream, []),
    error(existence_error(url, _URL), _Context),
    fail
  ),
  !.
dbnl_uri_resolve(Relative, Base1, Absolute):-
  \+ last_char(Base1, '/'),
  (atom_concat(Base1, '/', Base2) ; atom_concat(Base1, '_01/', Base2)),
  dbnl_uri_resolve(Relative, Base2, Absolute).
dbnl_uri_resolve(Relative, Base1, Absolute):-
  last_char(Base1, '/'),
  atom_concat(Base2, '/', Base1),
  atom_concat(Base2, '_01/', Base3),
  dbnl_uri_resolve(Relative, Base3, Absolute).
dbnl_uri_resolve(Relative, Base, Absolute):-
  gtrace, %DEB
  format(user_output, '~w\n~w\n~w\n', [Relative, Base, Absolute]).

dbnl_uri_to_html(URI1, DOM):-
  dbnl_uri_resolve(URI1, URI2),
  uri_to_html(URI2, DOM).

journal(Lang, Title, Volume) -->
  % Example: "In: ...".
  (pre(Lang), colon, blank ; ""),

  (
    % Volume information after the comma.
    dcg_atom_until(comma, Title), comma, blank
  ;
    % Volume information after the dot.
    dcg_atom_until(dot, Title), dot, blank
  ;
    % No volume information.
    dcg_codes_all(_)
  ),
  % With or without a volume.
  (volume(Lang, Volume) ; "").

