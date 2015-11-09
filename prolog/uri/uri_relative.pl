:- module(
  uri_relative,
  [
    'relative-part'//4, % +Iri:boolean
                        % ?Scheme:string
                        % ?Authority:compound
                        % ?Segments:list(string)
    'relative-ref'//6 % +Iri:boolean
                      % ?Scheme:string
                      % ?Authority:compound
                      % ?Segments:list(string)
                      % ?Query:string,
                      % ?Fragment:string
  ]
).

/** <module> RFC 3986 & RFC 3987: Relative

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@see http://tools.ietf.org/html/rfc3987
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(uri/uri_authority)).
:- use_module(library(uri/uri_fragment)).
:- use_module(library(uri/uri_hier)).
:- use_module(library(uri/uri_query)).





%! 'relative-part'(
%!   +Iri:boolean,
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string)
%! )// .
% ```abnf
% relative-part =   "//" authority path-abempty
%                 / path-absolute
%                 / path-noscheme
%                 / path-empty
% irelative-part =   "//" iauthority ipath-abempty
%                  / ipath-absolute
%                  / ipath-noscheme
%                  / ipath-empty
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'relative-part'(I, Scheme, Auth, Segments) -->
  "//",
  authority(I, Scheme, Auth),
  'path-abempty'(I, Segments).
'relative-part'(I, _, _, Segments) --> 'path-absolute'(I, Segments).
'relative-part'(I, _, _, Segments) --> 'path-noscheme'(I, Segments).
'relative-part'(I, _, _, Segments) --> 'path-empty'(I, Segments).



%! 'relative-ref'(
%!   +Iri:boolean,
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% Relative reference.
%
% ```abnf
%  relative-ref =  relative-part [ "?"  query ] [ "#"  fragment ]
% irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'relative-ref'(I, Scheme, Auth, Segments, Query, Frag) -->
  'relative-part'(I, Scheme, Auth, Segments),
  ("?", query(I, Query) ; ""),
  ("#", fragment(I, Frag) ; "").
