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

% URI
    dbnl_authority/1, % -Authority:atom
    dbnl_base_uri/1, % -Base:uri
    dbnl_scheme/1, % -Scheme:atom
    dbnl_uri_resolve/2, % +Relative:uri
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

:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(www_browser)).



% DEBUG %

dbnl_debug(URI1):-
  dbnl_uri_resolve(URI1, URI2),
  flag(deb, ID, ID + 1),
  (
    ID > 3
  ->
    once(www_open_url(URI2)),
    gtrace
  ;
    true
  ).



% DOM %

dbnl_dom_center(DOM, CenterDIVs):-
  dbnl_dom_content(DOM, Content),
  findall(
    CenterDIV,
    (
      xpath(Content, //td(@id=text), Text),
      xpath(Text, div(content), CenterDIV)
    ),
    CenterDIVss
  ),
  append(CenterDIVss, CenterDIVs).

dbnl_dom_content(DOM, Content):-
  xpath(DOM, //div(@id=dbnl), DBNL),
  xpath(DBNL, div(@id=wrapper), Wrapper),
  xpath(Wrapper, div(@id=scrollable), Scrollable),
  xpath(Scrollable, table(@id=content), TableContent),
  xpath(TableContent, tbody, TBODY),
  xpath(TBODY, tr(self), Content).

dbnl_dom_left(DOM, LeftDIV):-
  dbnl_dom_content(DOM, Content),
  xpath(Content, td(@id=left), Left),
  xpath(Left, div(content), LeftDIV).

dbnl_dom_notes(DOM, Notes):-
gtrace,
  findall(
    NoteIndex-Contents,
    (
      xpath(DOM, //div(@class=note), DIV),
      DIV = element(div, _, [element(a, Attributes, _) | Contents]),
      memberchk(name=NoteIndex, Attributes)
    ),
    Notes
  ).

dbnl_dom_right(DOM, RightDIVs):-
  dbnl_dom_content(DOM, Content),
  findall(
    RightDIV,
    (
      xpath(Content, //td(@id=right), Right),
      xpath(Right, div(content), RightDIV)
    ),
    RightDIVss
  ),
  append(RightDIVss, RightDIVs).



% URI %

%! dbnl_authority(-Authority:atom) is det.
% Returns the authority of the DBNL.

dbnl_authority('www.dbnl.org').

%! dbnl_base_uri(-BaseURI:uri) is det.
% Returns the base URI for the DBNL.

dbnl_base_uri(BaseURI):-
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  uri_components(BaseURI, uri_components(Scheme, Authority, '', '', '')).

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

