:- module(
  standards,
  [
    charset/1, % ?Charset:charset
    doctype//2, % +Language:atom
                % +Version:version
    prolog_charset/2, % ?Charset:charset
                      % ?PrologCharset:atom
    reply_page/3, % +Language:language
                  % +Head:list(term)
                  % +Body:list(term)
    reply_page/4, % +Language:language
                  % +Version:version
                  % +Head:list(term)
                  % +Body:list(term)
    root_element/4, % ?Language:atom
                    % +Head:list
                    % +Body:list
                    % -Root:list
    root_element/5, % ?Language:atom
                    % ?Version:version
                    % +Head:list
                    % +Body:list
                    % -Root:list
    set_stream/1 % +Attribute:compound
  ]
).

/** <module> Standards

Methods for serving Web pages according to Web specifications.

@author Wouter Beek
@version Oct2012
*/

:- discontiguous charset/1.

:- discontiguous is_sgml_based/2.

:- discontiguous is_xml_based/2.

%! language(
%!   ?Shorthand:atom,
%!   ?Version:version,
%!   ?MimeType:mime,
%!   ?FDI:fdi,
%!   ?DTD_Link:dtd_link,
%!   ?Default:boolean
%! ) is nondet.

:- discontiguous language/6.

:- discontiguous namespace/2.

:- discontiguous root_element/5.



% DOCTYPE %

doctype(Language, Version) -->
  ['<!DOCTYPE'],
  ['HTML'],
  public_system(Language, Version),
  fdi(Language, Version),
  dtd_link(Language, Version),
  ['>'].

dtd_link(Language, Version) -->
  {\+(is_html5(Language, Version))},
  [DTD_Link],
  {language(Language, Version, _Mime, _FDI, DTD_Link, true)}.
dtd_link(Language, Version) -->
  {is_html5(Language, Version)},
  [].

fdi(Language, Version) -->
  {\+(is_html5(Language, Version))},
  [FDI],
  {language(Language, Version, _Mime, FDI, _DTD_Link, true)}.
fdi(Language, Version) -->
  {is_html5(Language, Version)},
  [].

is_html5(Language, Version):-
  member(Language, [html, xhtml]),
  member(Version, [version(5/_/_, _)]).

public_system(Language, Version) -->
  {\+(is_html5(Language, Version))},
  ['PUBLIC'].
public_system(Language, Version) -->
  {\+(is_html5(Language, Version))},
  ['SYSTEM'].
public_system(Language, Version) -->
  {is_html5(Language, Version)},
  [].



% GENERIC %

body_element(Contents, element(body, [], Contents)).

charset('utf-8').

%! default_version(+Language:atom, -DefaultVersion:version) is det.
%! default_version(+Language:atom, +DefaultVersion:version) is semidet.
% Either returns the default version for a given language or suucceeds
% only if the given version is the default version of the language.
% What is the default version of a language is made up by ourselves.
%
% @arg Language The atomic name of a language standard.
% @arg DefaultVersion A version term.

default_version(Language, DefaultVersion):-
  language(Language, DefaultVersion, _MIME, _FDI, _DTD_Link, true).

head_element(Contents, element(head, [], Contents)).

prolog_charset('utf-8', utf8).

%! reply_page(
%!   +Language:atom,
%!   +Head:list(compound),
%!   +Body:list(compound)
%! ) is det.
% Serves a Web page for the default version of the given specification
% language.
% The default version is set by language/3.
%
% @arg Language The atomic name of a spacification language.
% @arg Head A list of compound terms.
% @arg Body A list of compound terms.

reply_page(Language, Head, Body):-
  default_version(Language, Version),
  reply_page(Language, Version, Head, Body).

%! reply_page(
%!   +Language:atom,
%!   +Version:version,
%!   +Head:list(compound),
%!   +Bodt:list(compound)
%! ) is det.
% Serves a Web page to the current output stream.
%
% @arg Language The language that the Web page is formatted in.
% @arg Version The version of the language specification.
% @arg Head A list of compound terms.
% @arg Body A list of compound terms.

reply_page(Language, Version, Head, Body):-
  % The encoding of the standard =|http-handler|= stream is =ascii=.
  % Since =|utf-8|= is more common on the Web, we must first change this
  % stream property.
  charset(Charset),
  prolog_charset(Charset, PrologCharset),
  set_stream(encoding(PrologCharset)),

  % HTTP header.
  language(Language, Version, MIME, _FDI, _DTD_Link, true),
  format('Content-type: ~w; charset=~w~n~n', [MIME, Charset]),

%%%%! DOCTYYPE
%%%%doctype(Language, Version, DOCTYPE, []),
%%%%atomic_list_concat(DOCTYPE, ' ', DOCTYPE1),
%%%%write(DOCTYPE1),nl,

  root_element(Language, Version, Head, Body, Root),

  sgml_or_xml_write(
    Language,
    Version,
    Root,
    [
      % The value of DocType never matters. But the setting must be added
      % otherwise no doctype is emitted in the absence of the public and
      % system options.
      doctype(_DocType),
      % Write the XML declaration. Does nothing when writing SGML.
      header(true)
    ]
  ).

root_element(Language, Head, Body, Root):-
  language(Language, Version, _MIME, _FDI, _DTD_Link, true),
  root_element(Language, Version, Head, Body, Root).

%! set_stream(+Attribute:compound) is det.
% Changes a property of the current stream.
%
% @arg Attribute One of the compound terms defined for set_stream/2.
% @see set_stream/2

set_stream(Attribute):-
  current_output(Stream),
  set_stream(Stream, Attribute).

%! sgml_or_xml_write(Language, Version, Term, Options) is det.
% Either calls xml_write/2 or sgml_write/2 based on the language and version
% of the Web standard used.
% This allows options to be specified for both at once.
%
% @arg Language The language that the Web page is formatted in.
% @arg Version The version of the language specification.
% @arg Term A compound term representing the contents of the Web page.
% @arg Options The list of options that is described at xml_write/3.

sgml_or_xml_write(Language, Version, Term, Options):-
  is_xml_based(Language, Version),
  !,
%%%%language(xml, XML_Version, _XML_MIME, _XML_FDI, _XML_DTD_Link, true),
%%%%xml_declaration(XML_Version, yes, XML_Declaration, []),
%%%%atomic_list_concat(XML_Declaration, ' ', XML_Declaration1),
%%%%write(XML_Declaration1),nl,
  xml_write(Term, Options).
sgml_or_xml_write(Language, Version, Term, Options):-
  is_sgml_based(Language, Version),
  !,
  sgml_write(Term, Options).
sgml_or_xml_write(Language, Version, _Term, _Options):-
  debug(
    standards,
    'Language ~w version ~w seems to be neither XML nor SGML based. Cannot parse.',
    [Language, Version]
  ).



% HTML

is_sgml_based(html, _Version).

language(
  html,
  version(2/0/0, _Unknown),
  'text/html',
  '"-//IETF//DTD HTML 2.0//EN"',
  '',
  false
).
language(
  html,
  version(3/2/0, 1997),
  'text/html',
  '"-//W3C//DTD HTML 3.2 Final//EN"',
  '',
  false
).
language(
  html,
  version(4/01/'Strict', 1999),
  'text/html',
  '"-//W3C//DTD HTML 4.01//EN"',
  '"http://www.w3.org/TR/html4/strict.dtd"',
  false
).
language(
  html,
  version(4/01/'Strict', 1999),
  'text/html',
  '"-//W3C//DTD HTML 4.01 Transitional//EN"',
  '"http://www.w3.org/TR/html4/loose.dtd"',
  false
).
language(
  html,
  version(4/01/'Strict', 1999),
  'text/html',
  '"-//W3C//DTD HTML 4.01 Frameset//EN"',
  '"http://www.w3.org/TR/html4/frameset.dtd"',
  false
).
language(
  html,
  version(5/0/0, 2014),
  'text/html',
  '',
  '',
  true
).



% SVG
%
% The markup:
%
% ~~~{.xml}
% <svg width="12cm" height="12cm" version="1.1" xmlns="http://www.w3.org/2000/svg">
%   <circle cx="6cm" cy="6cm" r="5cm" fill="white" stroke="black" stroke-width="1"/>
%   <circle cx="6cm" cy="1cm" r="1mm" fill="white" stroke="black" stroke-width="1"/>
% </svg>
% ~~~
%
% The term:
%
% ~~~{.pl}
% [element(svg,[width=12cm,height=12cm,version=1.1,xmlns=http://www.w3.org/2000/svg],[
%     element(circle,[cx=6cm,cy=6cm,r=5cm,fill=white,stroke=black,stroke-width=1],[]),
%     element(circle,[cx=6cm,cy=1cm,r=1mm,fill=white,stroke=black,stroke-width=1],[])
% ])]
% ~~~

is_xml_based(svg, _Version).

language(
  svg,
  version(1/0/0, 2001),
  'image/svg+xml',
  '"-//W3C//DTD SVG 1.0//EN"',
  '"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd"',
  false
).
language(
  svg,
  version(1/1/'Basic', 2003),
  'image/svg+xml',
  '"-//W3C//DTD SVG 1.1 Basic//EN"',
  '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-basic.dtd"',
  false
).
language(
  svg,
  version(1/1/'Full', 2003),
  'image/svg+xml',
  '"-//W3C//DTD SVG 1.1//EN"',
  '',
  false
).
language(
  svg,
  version(1/1/'Tiny', 2003),
  'image/svg+xml',
  '"-//W3C//DTD SVG 1.1 Tiny//EN"',
  '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd"',
  false
).
language(
  svg,
  version(1/1/'Second Edition', 2011),
  'image/svg+xml',
  '"-//W3C//DTD SVG 1.1//EN"',
  '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"',
  true
).

namespace(svg, 'http://www.w3.org/2000/svg').

root_element(
  svg,
  version(Major/Minor/_Variant, _Year),
  Head,
  Body,
  element(
    svg,
    [
      version=Version,
      xmlns=Namespace
    |
      Head
    ],
    Body
  )
):-
  format(atom(Version), '~w.~w', [Major, Minor]),
  namespace(svg, Namespace).



% XHTML
%
% The markup:
% ~~~{.xml}
% <html xmlns="http://www.w3.org/1999/xhtml">
%   <head>
%     <title>Test</title>
%   </head>
%   <body>Test</body>
% </html>
% ~~~
%
% The term:
% ~~~{.pl}
% [element(html,[xmlns=http://www.w3.org/1999/xhtml],[
%     element(head,[],[element(title,[],['Test'])]),
%     element(body,[],['Test'])
% ])]
% ~~~

is_xml_based(xhtml, _Version).

language(
  xhtml,
  version(1/0/'Frameset', 2000),
  'text/html',
  '"-//W3C//DTD XHTML 1.0 Frameset//EN"',
  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"',
  false
).
language(
  xhtml,
  version(1/0/'Strict', 2000),
  'text/html',
  '"-//W3C//DTD XHTML 1.0 Strict//EN"',
  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"',
  false
).
language(
  xhtml,
  version(1/0/'Transitional', 2000),
  'text/html',
  '"-//W3C//DTD XHTML 1.0 Transitional//EN"',
  '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"',
  false
).
language(
  xhtml,
  version(1/1/'Second Edition', 2010),
  'application/xhtml+xml',
  '"-//W3C//DTD XHTML 1.1//EN"',
  '"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"',
  false
).
language(
  xhtml,
  version(5/0/0, 2014),
  'application/xhtml+xml',
  _FTI,
  _DTD_Link,
  true
).
language(
  'xhtml-print',
  version(1/0/0, 2006),
  'application/xhtml+xml',
  _Unknown_FDI,
  _Unknown_DTD_Link,
  true
).

namespace(xhtml, 'http://www.w3.org/1999/xhtml').

root_element(
  xhtml,
  _Version,
  Head,
  Body,
  element(
    html,
    [charset=Charset, xmlns=XHTML_Namespace, 'xmlns:svg'=SVG_Namespace],
    [Head1, Body1]
  )
):-
  charset(Charset),
  namespace(svg, SVG_Namespace),
  namespace(xhtml, XHTML_Namespace),
  head_element(Head, Head1),
  body_element(Body, Body1).



% XML %

is_xml_based(xml, _Version).

language(
  xml,
  version(1/0/'Fifth Edition', 2008),
  'text/xml',
  _No_FDI,
  _No_DTD_Link,
  true
).
language(
  xml,
  version(1/1/'Second Edition', 2006),
  'text/xml',
  _No_FDI,
  _No_DTD_Link,
  false
).
