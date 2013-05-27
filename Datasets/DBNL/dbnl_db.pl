:- module(
  dbnl_db,
  [
    dbnl_assert_author/4, % +Graph:atom
                          % +Absolute:uri
                          % +Name:atom
                          % -Author:uri
    dbnl_assert_copyright/4, % +Graph:atom
                             % +Organization:oneof([atom,uri])
                             % +Year:integer
                             % -Copyright:bnode
    dbnl_assert_editor/3, % +Graph:atom
                          % +Name:atom
                          % -Editor:uri
    dbnl_assert_genre/3, % +Graph:atom
                         % +Name:atom
                         % -Genre:uri
    dbnl_assert_organization/3, % +Graph:atom
                                % +Name:atom
                                % +Organization:uri
    dbnl_assert_subgenre/3, % +Graph:atom
                            % +Hierarchy:atom
                            % -Subgenre:uri
    dbnl_assert_title/4 % +Graph:atom
                        % +Absolute:uri
                        % +Name:atom
                        % -Title:uri
  ]
).

/** <module> DBNL DB

Database predicates for scraping the DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(list_ext)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(skos(skos_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



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
  rdfs_assert_individual(Author, dbnl:'Author', Graph),
  rdfs_assert_label(Author, AuthorName, Graph),
  rdf_assert(Author, dbnl:orignal_page, AbsoluteAuthorURI, Graph).

dbnl_assert_copyright(Graph, OrganizationName, Year, Copyright):-
  atom(OrganizationName),
  !,
  dbnl_assert_organization(Graph, OrganizationName, Organization),
  dbnl_assert_copyright(Graph, Organization, Year, Copyright).
dbnl_assert_copyright(Graph, Organization, Year, Copyright):-
  rdf_bnode(Copyright),
  rdfs_assert_individual(Copyright, dbnl:'Copyright', Graph),
  rdf_assert(Copyright, dbnl:organization, Organization, Graph),
  rdf_assert_datatype(Copyright, dbnl:year, gYear, Year, Graph).

%! dbnl_assert_editor(+Graph:atom, +EditorName:atom, -Editor:uri) is det.

dbnl_assert_editor(_Graph, EditorName, Editor):-
  rdfs_label(Editor, EditorName),
  !.
dbnl_assert_editor(Graph, EditorName, Editor):-
  flag(editor, EditorFlag, EditorFlag + 1),
  format(atom(EditorID), 'editor/~w', [EditorFlag]),
  rdf_global_id(dbnl:EditorID, Editor),
  rdfs_assert_individual(Editor, dbnl:'Editor', Graph),
  rdfs_assert_label(Editor, EditorName, Graph).

%! dbnl_assert_genre(+Graph:atom, +GenreName:atom, -Genre:uri) is det.

dbnl_assert_genre(_Graph, GenreName, Genre):-
  rdfs_label(Genre, GenreName),
  !.
dbnl_assert_genre(Graph, GenreName, Genre):-
  format(atom(GenreID), 'genre/~w', [GenreName]),
  rdf_global_id(dbnl:GenreID, Genre),
  rdfs_assert_individual(Genre, dbnl:'Genre', Graph),
  rdfs_assert_label(Genre, GenreName, Graph).

dbnl_assert_organization(_Graph, OrganizationName, Organization):-
  rdfs_label(Organization, OrganizationName),
  !.
dbnl_assert_organization(Graph, OrganizationName, Organization):-
  format(atom(OrganizationID), 'organization/~w', [OrganizationName]),
  rdf_global_id(dbnl:OrganizationID, Organization),
  rdfs_assert_individual(Organization, dbnl:'Organization', Graph),
  rdfs_assert_label(Organization, OrganizationName, Graph).

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
  rdfs_assert_individual(Title, dbnl:'Title', Graph),
  rdfs_assert_label(Title, Name, Graph),
  % The original DBNL page where this title was described.
  rdf_assert(Title, dbnl:original_page, URI, Graph).

