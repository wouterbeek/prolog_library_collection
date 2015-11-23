:- module(
  rfc3986,
  [
    'absolute-URI'//4, % ?Scheme:string
                       % ?Authority:compound
                       % ?Segments:list(string)
                       % ?Query:string
    'hier-part'//2, % ?Authority:compound
                    % ?Segments:list(string)
    'relative-part'//3, % ?Scheme:string
                        % ?Authority:compound
                        % ?Segments:list(string)
    'relative-ref'//5, % ?Scheme:string
                       % ?Authority:compound
                       % ?Segments:list(string)
                       % ?Query:string
                       % ?Fragment:string
    'URI'//5 % ?Scheme:string
             % ?Authority:compound
             % ?Segments:list(string)
             % ?Query:string
             % ?Fragment:string
  ]
).

/** <module> RFC 3986

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/11
*/

:- use_module(library(uri/rfc3986_component)).





%! 'absolute-URI'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string
%! )// .
% ```abnf
% absolute-URI = scheme ":" hier-part [ "?" query ]
% ```

'absolute-URI'(Scheme, Auth, L, Query) -->
  scheme(Scheme),
  ":",
  'hier-part'(Auth, L),
  ("?" -> query(Query) ; "").



%! 'hier-part'(?Authority:compound, ?Segments:list(string))// .
% ```abnf
% hier-part = "//" authority path-abempty
%           / path-absolute
%           / path-rootless
%           / path-empty
% ```

'hier-part'(Auth, L) --> "//", !, authority(Auth), 'path-abempty'(L).
'hier-part'(_, L)    --> 'path-absolute'(L), !.
'hier-part'(_, L)    --> 'path-rootless'(L), !.
'hier-part'(_, L)    --> 'path-empty'(L).



%! 'relative-part'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string)
%! )// .
% ```abnf
% relative-part = "//" authority path-abempty
%               / path-absolute
%               / path-noscheme
%               / path-empty
% irelative-part = "//" iauthority ipath-abempty
%                / ipath-absolute
%                / ipath-noscheme
%                / ipath-empty
% ```

'relative-part'(Scheme, Auth, L) -->
  "//", !, authority(Scheme, Auth), 'path-abempty'(L).
'relative-part'(_, _, L) --> 'path-absolute'(L), !.
'relative-part'(_, _, L) --> 'path-noscheme'(L), !.
'relative-part'(_, _, L) --> 'path-empty'(L).



%! 'relative-ref'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% Relative URI reference.
%
% ```abnf
% relative-ref = relative-part [ "?" query ] [ "#" fragment ]
% irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
% ```

'relative-ref'(Scheme, Auth, L, Query, Frag) -->
  'relative-part'(Scheme, Auth, L),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Frag) ; "").



%! 'URI'(
%!   ?Scheme:string,
%!   ?Authority:compound,
%!   ?Segments:list(string),
%!   ?Query:string,
%!   ?Fragment:string
%! )// .
% ```abnf
% URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
% ```

'URI'(Scheme, Auth, L, Query, Fragment) -->
  scheme(Scheme),
  ":",
  'hier-part'(Auth, L),
  ("?" -> query(Query) ; ""),
  ("#" -> fragment(Fragment) ; "").
