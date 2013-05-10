:- module(
  xml,
  [
    dom_to_xml/4, % +DTD_File:atom
                  % +StyleName:atom
                  % +DOM
                  % -XML:atom
    file_to_xml/2, % +File:atom
                   % -XML:dom
    stream_to_xml/2, % +Stream:stream
                     % -XML:dom
    stylesheet_pi/2, % +CSS_FileSpecification
                     % -PI:atom
    stylesheet_pi/3, % +Type:oneof(['text/css'])
                     % +CSS_FileSpecification
                     % -PI:atom
    uri_to_xml/2, % +URI:uri
                  % -XML:dom
    xml_declaration//2, % +Version:versionm
                        % +Standalone:atom
    xml_doctype/2 % +Stream:stream
                  % -DocType
  ]
).

/** <module> XML

XML, Extendable Markup Language specification.

@author Wouter Beek
@version 2012/10, 2013/02-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(standards(sgml_parse)).
:- use_module(standards(standards), [charset/1]).

:- db_add_novel(user:prolog_file_type(css, css)).
:- db_add_novel(user:prolog_file_type(dtd, dtd)).
:- db_add_novel(user:prolog_file_type(xml, xml)).

:- multifile(http:location/3).
:- dynamic(http:location/3).

% Serve CSS files.
http:location(css, root(css),  []).
:- assert(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix, priority(10)]).



%% dom_to_xml(+DTD_Name, +Style_Name, +DOM, -XML) is det.
% Translates DOM to XML, applying DTD checks and Style decoration.
%
% @param DTD_Name The atomic name of a DTD file. File locations that
%	 contain DTD files must be asserted using
%	 =|file_search_path/3|=.

dom_to_xml(DTD_Name, Style_Name, DOM, XML):-
  new_dtd(DTD_Name, DTD),
  % Retrieve the first DTD file with the given name.
  dtd_file(DTD_Name, DTD_File),
  load_dtd(DTD, DTD_File),
  tmp_file_stream(text, TemporaryFile, Out),
  file_name_type(Style_Name, css, Style),
  stylesheet_pi(css(Style), PI),
  % Set the header to false, since this XML content will be inserted inside
  % a Web page.
  % We do add the stylesheet parsing instruction, since this is allowed by
  % Firefox.
  xml_write(Out, [PI | DOM], [dtd(DTD), header(false)]),
  close(Out),
  open(TemporaryFile, read, In, [type(text)]),
  stream_to_atom(In, XML),
  close(In),
  delete_file(TemporaryFile),
  free_dtd(DTD).

%% dtd_file(+Name:atom, -File:atom) is det.
% Returns the first DTD file with the given name or throws an
% existence error.
%
% @param Name The atomic name of a DTD file.
% @param File The atomic name of the path of a DTD file.

dtd_file(Name, File):-
  % By setting option =solutions= to value =all= we backtrack over
  % the various file search paths that are defined for =dtd=.
  once(
    absolute_file_name(
      dtd(Name),
      File,
      [access(read), file_type(dtd), solutions(all)]
    )
  ).

%% file_to_xml(+File:atom, -XML:dom) is det.
% Reads the XML from the given file and return the DOM.

file_to_xml(File, XML):-
  setup_call_cleanup(
    open(File, read, Stream),
    stream_to_xml(Stream, XML),
    close(Stream)
  ).

%% stream_to_xml(+Stream:stream, -XML:dom) is det.
% Reads the XML DOM from the given stream.

stream_to_xml(Stream, XML):-
  load_structure(
    stream(Stream),
    XML,
    [
      dialect(xml),
      max_errors(-1),
      shorttag(false),
      space(remove),
      syntax_errors(quiet)
    ]
  ).

stylesheet_pi(CSS_FileSpecification, PI):-
  stylesheet_pi('text/css', CSS_FileSpecification, PI).

stylesheet_pi(Type, CSS_FileSpecification, pi(PI)):-
  http_absolute_location(CSS_FileSpecification, CSS_File, []),
  format(atom(PI), 'xml-stylesheet type="~w" href="~w"', [Type, CSS_File]).

%% uri_to_xml(+URI:uri, -XML:dom) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

uri_to_xml(URI, XML):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    stream_to_xml(Stream, XML),
    % If goal succeeds, then perform this cleanup.
    close(Stream)
  ).

%% xml_declaration(+Version:version, +Standalone:yes_no)//
% DCG for XML declarations.
% Based on the XML version and whether the XML is a standalone file.
%
% @param Version A version of XML.
% @param Standalone Whether the XML file is standalone or not.
%        Possible values:
%        1. =yes=
%        2. =no=

xml_declaration(version(Major/Minor/_Variant, _Year), Standalone) -->
  ['<?xml'],
  {format(atom(VersionInfo), 'version="~w.~w"', [Major, Minor])},
  [VersionInfo],
  {
    charset(Encoding),
    format(atom(EncodingDeclaration), 'encoding="~w"', [Encoding])
  },
  [EncodingDeclaration],
  {format(atom(StandaloneDeclaration), 'standalone="~w"', [Standalone])},
  [StandaloneDeclaration],
  ['?>'].

%% xml_doctype(+Stream, -DocType) is semidet.
% Parse a _repositional_ stream and get the  name of the first XML
% element *and* demand that this   element defines XML namespaces.
% Fails if the document is illegal XML before the first element.
%
% Note that it is not  possible   to  define valid RDF/XML without
% namespaces, while it is not possible  to define a valid absolute
% Turtle URI (using <URI>) with a valid xmlns declaration.
%
% @author Jan Wielemaker
% @version 2011

xml_doctype(Stream, DocType):-
  catch(
    setup_call_cleanup(
      make_parser(Stream, Parser, State),
      sgml_parse(
        Parser,
        [
          call(begin, on_begin),
          call(cdata, on_cdata),
          max_errors(1),
          source(Stream),
          syntax_errors(quiet)
        ]
      ),
      cleanup_parser(Stream, Parser, State)
    ),
    Exception,
    true
  ),
  nonvar(Exception),
  Exception = tag(DocType).

on_begin(Tag, Attributes, _Parser):-
  memberchk(xmlns:_=_, Attributes),
  throw(tag(Tag)).

on_cdata(_CDATA, _Parser):-
  throw(error(cdata)).

