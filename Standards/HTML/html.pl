:- module(
  html,
  [
    reply_html_file/2, % +Style:atom
                       % +File:atom

% FETCHING
    file_to_html/2, % +File:atom
                    % -HTML:list
    uri_to_html/2, % +URI:uri
                   % -HTML:list

% GENERATING
    list_to_table/3, % +Options:list(nvpair)
                     % +List:list(list(term))
                     % -Markup:element

% PARSING
    html_char//2, % +Options:list(nvpair)
                  % ?Results
    html_convert/2, % +Atom:atom
                    % -HTML_Atom:atom
    parse_attributes_html/3 % +Context:oneof([table])
                            % +Attributes:list(nvpair)
                            % -ParsedAttributes:list(nvassignment)
  ]
).

/** <module> HTML

Support for HTML.

---+ Generation

From Prolog list to HTML table.

---+ Parsing

HTML characters, escaping the '<' and '>' characters.

HTML atom conversion, using HTML characters.

HTML attribute parsing, used in HTML table generation.

@author Wouter Beek
@version 2012/09-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(parse_ext)).
:- use_module(generics(type_checking), [type_check/2 as type_check_generic]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).

:- assert_novel(user:prolog_file_type(htm, html)).
:- assert_novel(user:prolog_file_type(html, html)).



% This attributes specifies the width (in pixels only) of the frame around a
% table (see the Note below for more information about this attribute).
% @tbd Deprecated, use CSS2 instead.
attribute(border, pixels, [table]).



reply_html_file(Style, File):-
  absolute_file_name(html(File), HTML, [access(read), file_type(html)]),
  load_html_file(HTML, DOM),
  contains_term(element(body, _, Body), DOM),
  reply_html_page(Style, [], Body).



% FETCHING %

file_to_html(File, DOM):-
  open(File, read, Stream),
  stream_to_html(Stream, DOM).

stream_to_html(Stream, DOM):-
  dtd(html, DTD),
  catch(
    load_structure(
      stream(Stream),
      DOM,
      [
        dtd(DTD),
        dialect(sgml),
        shorttag(false),
        max_errors(-1),
        space(remove),
        syntax_errors(quiet)
      ]
    ),
    error(limit_exceeded(max_errors, Max), _),
    debug(picarta, 'HTML could not be loaded due to ~w max errors.\n', [Max])
  ).

%% uri_to_html(+URI:resource, -HTML:list) is det.
% Returns the HTML Document Object Model (DOM) for the website with the given
% URI.
%
% @param URI
% @param HTML
% @throws existence_error(url, Id)

uri_to_html(URI, DOM):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, []),
    % The try to make this goal succeed.
    stream_to_html(Stream, DOM),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).



% GENERATING %

%% list_to_table(
%%   +Options:list(nvpair),
%%   +Rows:list(list(term)),
%%   -Markup
%% ) is det.
% Returns the HTML markup for a table.
%
% @apram Options A list of name-value pairs. The following options are
%        supported:
%        1. =|header(boolean)|= Whether or not the first sublist should be
%           displayed as the table header row.
% @param Rows A 2D table of terms.
% @param Markup An HTML table element.

list_to_table(
  Options,
  [Header | Rows],
  element(table, [border=1], [MarkupHeader | MarkupRows])
):-
  option(header(true), Options, true),
  !,
  table_header(Header, MarkupHeader),
  maplist(table_row, Rows, MarkupRows).
list_to_table(_Options, Rows, element(table, [border=1], MarkupRows)):-
  maplist(table_row, Rows, MarkupRows).

%% table_header(+Elements:list(term), -Markup) is det.
% Returns the header row of an HTML table containing the given elements.
%
% @param Elements A list of terms.
% @param Markup An HTML entity.

table_header(Elements, element(tr, [], MarkupCells)):-
  table_row0(Elements, th, MarkupCells).

%% table_row(+Elements:list(term), -Markup) is det.
% Returns the row of an HTML table containing the given elements.
%
% @param Elements A list of terms.
% @param Markup An HTML entity.

table_row(Elements, element(tr, [], MarkupCells)):-
  table_row0(Elements, td, MarkupCells).

table_row0([], _HTML_Entity, []).
table_row0(
  [H | T],
  HTML_Entity,
  [element(HTML_Entity, [], [Atom]) | Markup]
):-
  % If we use term_to_atom/2 for atom terms, extra single quotes are added
  % in front and at the end of the atom. Therefore, we first check whether
  % the term is an atom.
  (atom(H) -> Atom = H ; term_to_atom(H, Atom)),
  table_row0(T, HTML_Entity, Markup).



% PARSING %

%% html_char(+Options:list(nvpair), ?Results)//
% Returns the HTML atom representing the character given.

html_char(_Options, ['>' | Rest]-Rest) --> "&#62;".
html_char(_Options, ['<' | Rest]-Rest) --> "&#60;".
html_char(_Options, [X | Rest]-Rest) --> [X].

html_convert(Atom, HTML_Atom):-
  atom_codes(Atom, C),
  % Read the codes that constitute the given atom. This may include codes for
  % escape characters that are not legitimate C-characters.
  re(
    [case(sensitive), out(atom), q(+)],
    html_char,
    HTML_Atom,
    C-[]
  ).

parse_attribute(Context, Attribute, Name=Value):-
  Attribute =.. [Name, Value],
  attribute(Name, Type, Contexts),
  memberchk(Context, Contexts),
  !,
  type_check(Type, Value).

parse_attributes_html(Context, Attributes, ParsedAttributes):-
  maplist(parse_attribute(Context), Attributes, ParsedAttributes).

type_check(pixels, Value):-
  type_check(integer, Value),
  !.
type_check(Type, Value):-
  type_check_generic(Type, Value).

