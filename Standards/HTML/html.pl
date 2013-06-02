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
    html_image/3, % +Description:atom
                  % +Base:atom
                  % -DIV:element
    list_to_table/3, % +Options:list(nvpair)
                     % +List:list(list(term))
                     % -Markup:element

% PARSING
    html_attribute/2, % +Attributes:list(nvpair)
                      % +Attribute:nvpair
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

# Generation

From Prolog list to HTML table.

# Parsing

HTML characters, escaping the '<' and '>' characters.

HTML atom conversion, using HTML characters.

HTML attribute parsing, used in HTML table generation.

@author Wouter Beek
@version 2012/09-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(parse_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(option)).

% Assert DTD file locations.
:- db_add_novel(user:file_search_path(dtd, html(.))).

% Assert the HTML file types.
:- db_add_novel(user:prolog_file_type(htm, html)).
:- db_add_novel(user:prolog_file_type(html, html)).

% Register the supported image file types.
% These are shared with module RDF_DATATYPE.
:- dynamic(user:image_file_type/1).
:- multifile(user:image_file_type/1).
:- db_add_novel(user:prolog_file_type(jpeg, jpeg)).
:- db_add_novel(user:prolog_file_type(jpg, jpeg)).
:- db_add_novel(user:image_file_type(jpg)).
:- db_add_novel(user:prolog_file_type(png, png)).
:- db_add_novel(user:image_file_type(png)).

:- debug(html).



%! reply_html_file(+Style:atom, +File:atom) is det.
% Serve the given HTML file using the given styling.
%
% @arg Style The atomic name of the HTML style of the page served.
%        This style has to be defined using the multifile
%        preficates user:body//2 and user:head//2.
% @arg File The atomic base name of the HTML file that is served.

reply_html_file(Style, File):-
  absolute_file_name(html(File), HTML, [access(read), file_type(html)]),
  load_html_file(HTML, DOM),
  contains_term(element(body, _, Body), DOM),
  reply_html_page(Style, [], Body).



% FETCHING %

%! file_to_html(+File:atom, -HTML:dom) is det.
% Retrieves the HTML DOM from the file described with the given
% absolute file name.

file_to_html(File, HTML):-
  open(File, read, Stream),
  stream_to_html(Stream, HTML).

% stream_to_html(+Stream:stream, -HTML:dom) is det.
% Retrieves the HTML DOM from the given stream.
%
% @throws Limit exceeded exception due to >50 errors.
%         =|error(limit_exceeded(max_errors, Max), _)|=

stream_to_html(Stream, DOM):-
  dtd(html, DTD),
  load_structure(
    stream(Stream),
    DOM,
    [
      dtd(DTD),
      dialect(xmlns),
      shorttag(true),
      space(remove)
    ]
  ).

%! uri_to_html(+URI:resource, -HTML:list) is det.
% Returns the HTML Document Object Model (DOM) for the website with the given
% URI.
%
% @arg URI
% @arg HTML
% @throws existence_error(url, Id)

uri_to_html(URI, DOM):-
  setup_call_cleanup(
    % First perform this setup once/1.
    (
      http_open(URI, Stream, []),
      set_stream(Stream, encoding(utf8))
    ),
    % The try to make this goal succeed.
    stream_to_html(Stream, DOM),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).



% GENERATING %

%! html_image(+Description:atom, +Base:atom, -DIV:element) is det.
% Constructs an IMG element.
%
% @arg Description An atomic description of the image.
% @arg Base The atomic base name of the image file.
% @arg DIV The HTML image element.

html_image(Description, File, DIV):-
  % Make sure the file has a supported image file type.
  file_name_type(_Base, Type, File),
  user:image_file_type(Type),
  http_absolute_location(img(File), RelativeURI, []),
  % The DIV containing the image description.
  Description_DIV = element(div, [class=image_description], [Description]),
  % The image itself, using the description as alternative text.
  ImageElement =
    element(
      img,
      [alt=Description, class=image_thumbnail, src=RelativeURI],
      []
    ),
  % Make the image clickable, linking to the image file.
  LinkElement =
    element(a, [href=RelativeURI, target='_blank'], [ImageElement]),
  % Construe the DIV containing the image, the link, and the description.
  DIV = element(div, [class=image], [LinkElement, Description_DIV]).

%! list_to_table(
%!   +Options:list(nvpair),
%!   +Rows:list(list(term)),
%!   -Markup
%! ) is det.
% Returns the HTML markup for a table.
%
% @apram Options A list of name-value pairs. The following options are
%        supported:
%        1. =|caption(atom)|= The caption of the header.
%        2. =|header(boolean)|= Whether or not the first sublist should be
%           displayed as the table header row.
% @arg Rows A 2D table of terms.
% @arg Markup An HTML table element.

list_to_table(Options, Rows1, element(table, [border=1], TableContents)):-
  list_to_table_caption(Options, CaptionMarkup),
  list_to_table_header(Options, Rows1, HeaderMarkup, Rows2),
  maplist(table_row, Rows2, RowsMarkup),
  append([CaptionMarkup, HeaderMarkup, RowsMarkup], TableContents).

list_to_table_caption(Options, [element(caption, [], [Caption])]):-
  option(caption(Caption), Options),
  !.
list_to_table_caption(_Options, []).

%! list_to_table_header(
%!   +Options:list(nvpair),
%!   +AllRows:list(list),
%!   -Markup:list,
%!   -NonHeaderRows:list(list)
%! ) is det.
% Returns the header row of an HTML table, if the header option is present.

list_to_table_header(
  Options,
  [Header | Rows],
  [element(tr, [], MarkupCells)],
  Rows
):-
  option(header(true), Options),
  !,
  table_row0(Header, th, MarkupCells).
list_to_table_header(_Options, Rows, [], Rows).

%! table_row(+Elements:list(term), -Markup) is det.
% Returns the row of an HTML table containing the given elements.
%
% @arg Elements A list of terms.
% @arg Markup An HTML entity.

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
  (
    atom(H)
  ->
    Atom = H
  ;
    term_to_atom(H, Atom)
  ),
  table_row0(T, HTML_Entity, Markup).



% PARSING %

% This attribute specifies the width (in pixels only) of the frame around a
% table (see the Note below for more information about this attribute).
% @tbd Deprecated, use CSS2 instead.
attribute(border, pixels, [table]).

%! html_attribute(+Attributes:list(nvpair), +Attribute:nvpair) is nondet.
% Succeeds (semidet) or instantiates (nondet) the given attribute within
% the given attributes list.
%
% This predicate is typically used to extract the value belonging to a
% certain attribute name from a given set of attribute-value pairs that
% occurs in a DOM element/3 term.
%
% This predicate uses the swipl options library.
% In accordance with this, =Attribute= can be either of the form
% =|Name(Value)|= or =|Name=Value|=.

html_attribute(Attributes, Attribute):-
  option(Attribute, Attributes).

%! html_char(+Options:list(nvpair), ?Results)//
% Returns the HTML atom representing the character given.

html_char(_Options, ['>' | Rest]-Rest) --> "&#62;".
html_char(_Options, ['<' | Rest]-Rest) --> "&#60;".
html_char(_Options, [X | Rest]-Rest) --> [X].

html_convert(Atom, HTML_Atom):-
  atom_codes(Atom, C),
  % Read the codes that constitute the given atom. This may include codes for
  % escape characters that are not legitimate C-characters.
  parse_re(
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
  html_typecheck(Type, Value).

parse_attributes_html(Context, Attributes, ParsedAttributes):-
  maplist(parse_attribute(Context), Attributes, ParsedAttributes).

html_typecheck(pixels, Value):-
  html_typecheck(integer, Value),
  !.
html_typecheck(Type, Value):-
  typecheck(Type, Value).
