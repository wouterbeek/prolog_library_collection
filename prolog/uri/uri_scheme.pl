:- module(
  uri_scheme,
  [
    scheme//1 % ?Scheme:string
  ]
).

/** <module> RFC 3986 & RFC 3987: Scheme component

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@see [Guidelines and Registration Procedures for New URI Schemes](https://tools.ietf.org/html/rfc4395)
@see [IANA URI Scheme Register](http://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml)
@see [RFC 3987](http://tools.ietf.org/html/rfc3987)
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).





%! scheme(?Scheme:string)// .
% An US-ASCII letter, followed by a sequence consisting of
% US-ASCII letters, digits, plus, dash, and dot characters.
%
% ```abnf
% scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

scheme(S) -->
  dcg_string(scheme_codes1, S),
  {memberchk(S, [ftp,http,https,mailto])}.

scheme_codes1([H|T]) --> 'ALPHA'(H), !, scheme_codes2(T).
scheme_codes2([H|T]) --> 'ALPHA'(H), !, scheme_codes2(T).
scheme_codes2([H|T]) --> 'DIGIT'(_, H), !, scheme_codes2(T).
scheme_codes2([0'+|T]) --> "+", !, scheme_codes2(T).
scheme_codes2([0'-|T]) --> "-", !, scheme_codes2(T).
scheme_codes2([0'.|T]) --> ".", !, scheme_codes2(T).
scheme_codes2([]) --> [].
