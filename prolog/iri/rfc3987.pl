:- module(
  rfc3987,
  [
    'IRI'//1, % ?Iri:atom
    'IRI'//5, % ?Scheme:string
              % ?Authority:compound
              % ?Segments:list(string)
              % ?Query:string
              % ?Fragment:string
    'IRI-reference'//5 % ?Scheme:string
                       % ?Authority:compound
                       % ?Segments:list(string)
                       % ?Query:string
                       % ?Fragment:string
  ]
).

/** <module> RFC 3987: Internationalized Resource Identifiers (IRIs)

## Definitions

  * *Character*
    A member of a set of elements used for the organization,
    control, or representation of data.
  * *|character encoding|*
    A method of representing a sequence of characters as a sequence of octets.
    Also, a method of (unambiguously) converting a sequence of octets into
    a sequence of characters.
  * *|Character repertoire|*
    A set of characters.
  * *Charset*
    The name of a parameter or attribute used to identify
    a character encoding.
  * *|IRI reference|*
    An IRI reference may be absolute or relative.
    However, the "IRI" that results from such a reference only includes
    absolute IRIs; any relative IRI references are resolved to their
    absolute form.
  * *Octet*
    An ordered sequence of eight bits considered as a unit.
  * *|Presentation element|*
    A presentation form corresponding to a protocol element; for example,
    using a wider range of characters.
  * *|Protocol element|*
    Any portion of a message that affects processing of that message
    by the protocol in question.
  * *|Running text|*
    Human text (paragraphs, sentences, phrases) with syntax according to
    orthographic conventions of a natural language, as opposed to syntax
    defined for ease of processing by machines (e.g., markup,
    programming languages).
  * *|UCS: Universal Character Set|*
    The coded character set defined by ISO/IEC 10646 and the Unicode Standard.

@author Wouter Beek
@compat [RFC 3987](http://tools.ietf.org/html/rfc3987)
@version 2015/08, 2015/11
*/

:- use_module(library(string_ext)).
:- use_module(library(uri)).
:- use_module(library(uri/uri_code)).
:- use_module(library(uri/uri_fragment)).
:- use_module(library(uri/uri_hier)).
:- use_module(library(uri/uri_query)).
:- use_module(library(uri/uri_relative)).
:- use_module(library(uri/uri_scheme)).





%! 'absolute-IRI'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string
%! )// .
% ```abnf
% absolute-IRI   = scheme ":" ihier-part [ "?" iquery ]
% ```

'absolute-IRI'(Scheme, Auth, Segments, Query) -->
  scheme(Scheme),
  ":",
  'hier-part'(true, Scheme, Auth, Segments),
  ("?", query(true, Query) ; "").



%! 'IRI'(?Iri:atom)// .

'IRI'(Iri) -->
  {var(Iri)}, !,
  'IRI'(Scheme, Auth, Segments, Query, Frag),
  {
    atomic_list_concat([''|Segments], '/', Path),
    uri_components(Iri, uri_components(Scheme,Auth,Path,Query,Frag))
  }.
'IRI'(Iri) -->
  {
    uri_components(Iri, uri_components(Scheme0,Auth0,Path0,Query0,Frag0)),
    maplist(
      atom_string,
      [Scheme0,Auth0,Path0,Query0,Frag0],
      [Scheme,Auth,Path,Query,Frag]
    ),
    string_list_concat([""|Segments], "/", Path)
  },
  'IRI'(Scheme, Auth, Segments, Query, Frag).


%! 'IRI'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% ```abnf
% IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
% ```

'IRI'(Scheme, Auth, Segments, Query, Frag) -->
  scheme(Scheme),
  ":",
  'hier-part'(true, Scheme, Auth, Segments),
  ("?", query(true, Query) ;   ""),
  ("#", fragment(true, Frag) ; "").



%! 'IRI-reference'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% There are two types of IRI reference: (1) IRI, (2) IRI relative reference.
%
% ```abnf
% IRI-reference = IRI / irelative-ref
% ```

'IRI-reference'(Scheme, Auth, Segments, Query, Frag) -->
  'IRI'(Scheme, Auth, Segments, Query, Frag).
'IRI-reference'(Scheme, Auth, Segments, Query, Frag) -->
  'relative-ref'(true, Scheme, Auth, Segments, Query, Frag).
