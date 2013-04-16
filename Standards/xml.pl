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
    xml_declaration//2 % +Version:versionm
                       % +Standalone:atom
  ]
).

/** <module> XML

XML, Extendable Markup Language specification.

@author Wouter Beek
@version 2012/10, 2013/02-2013/03
*/

:- use_module(generic(file_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_path)).
:- use_module(library(sgml_write)).
:- use_module(standards(standards), [charset/1]).

:- assert(prolog_file_type(xml, xml)).

:- multifile(http:location/3).
:- dynamic(http:location/3).

http:location(http_www,     http_root(www), []).
http:location(http_www_css, http_www(css),  []).

user:file_search_path(www,     project(www)).
user:file_search_path(www_css, www(css)    ).
user:file_search_path(www_dtd, www(dtd)    ).



dom_to_xml(DTD_Name, Style_Name, DOM, XML):-
  new_dtd(DTD_Name, DTD),
  absolute_file_name(www_dtd(DTD_Name), DTD_File, [access(read), file_type(dtd)]),
  load_dtd(DTD, DTD_File),
  tmp_file_stream(text, TemporaryFile, Out),
  format(atom(Style), '~w.css', [Style_Name]),
  stylesheet_pi(http_www_css(Style), PI),
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

file_to_xml(File, XML):-
  setup_call_cleanup(
    open(File, read, Stream),
    stream_to_xml(Stream, XML),
    close(Stream)
  ).

stream_to_xml(Stream, DOM):-
  load_structure(
    stream(Stream),
    DOM,
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

%% uri_to_xml(+URI:resource, -DOM:list) is det.
% Returns the HTML Document Object Model (DOM) for the website with the given
% URI.
%
% @param URI
% @param HTML

uri_to_xml(URI, DOM):-
  setup_call_cleanup(
    % First perform this setup once/1.
    http_open(URI, Stream, [timeout(60)]),
    % The try to make this goal succeed.
    stream_to_xml(Stream, DOM),
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

