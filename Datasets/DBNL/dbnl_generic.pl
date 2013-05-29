:- module(
  dbnl_generic,
  [
% DEBUG
    dbnl_debug/1, % +URI:uri

% DOM
    dbnl_dom_center/2, % +Page:dom
                       % -Center:dom
    dbnl_dom_left/2, % +DOM:dom
                     % -Left:dom
    dbnl_dom_notes/2, % +Page:dom
                      % -Notes:pair(atom,dom))
    dbnl_dom_right/2, % +DOM:dom
                      % -Right:dom

% ELEMENTS
    dbnl_copyright/3, % +Graph:atom
                      % +Text:uri
                      % +Element
    dbnl_logo/1, % +Element

% URI
    dbnl_authority/1, % -Authority:atom
    dbnl_base_uri/1, % -Base:uri
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

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_extract)).
:- use_module(generics(atom_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(library(www_browser)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



% DEBUG %

dbnl_debug(URI):-
  flag(deb, ID, ID + 1),
  (
    ID > 0
  ->
    once(www_open_url(URI))
  ;
    true
  ).



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
      DIV = element(div, _, [element(a, Attributes, _) | Contents]),
      memberchk(name=NoteIndex, Attributes)
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



% ELEMENTS %

dbnl_copyright(Graph, Text, Atom):-
  atom(Atom),
  dbnl_extract_copyright(Atom, Organization, Year),
  dbnl_assert_copyright(Graph, Organization, Year, Copyright),
  rdf_assert(Text, dbnl:copyright, Copyright, Graph).

dbnl_logo(element(img, Attributes, [])):-
  memberchk(alt='DBNL vignet', Attributes),
  memberchk(src='../dbnllogo.gif', Attributes),
  !.



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
  atom_concat(Base1, '/', Base2),
  dbnl_uri_resolve(Relative, Base2, Absolute).

dbnl_uri_to_html(URI1, DOM):-
  dbnl_uri_resolve(URI1, URI2),
  dbnl_debug(URI2),
  catch(
    setup_call_cleanup(
      % First perform this setup once/1.
      (
        http_open(URI2, Stream, []),
        set_stream(Stream, encoding(utf8))
      ),
      % The try to make this goal succeed.
      (
        dtd(html, DTD),
        load_structure(
          stream(Stream),
          DOM,
          [
            dtd(DTD),
            dialect(sgml),
            shorttag(false),
            space(remove)
          ]
        )
      ),
      % If goal succeeds, then perform this cleanup.
      close(Stream)
    ),
    error(limit_exceeded(max_errors, Max), _Context),
    (
      format(
        user_output,
        'Encountered ~w error(s) while parsing <~w>.',
        [Max, URI2]
      )
    )
  ).

