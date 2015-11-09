:- module(
  uri_fragment,
  [
    fragment//2 % ?Iri:boolean
                % ?Fragment:string
  ]
).

/** <module> RFC 3986 & RFC 3987: Fragment component

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@see tools.ietf.org/html/rfc3986 
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/uri_code)).





%! fragment(?Iri:boolean, ?Fragment:string)// .
% ```anbf
%  fragment = *(  pchar / "/" / "?" )
% ifragment = *( ipchar / "/" / "?" )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

fragment(I, S) --> dcg_string(fragment_codes(I), S).

fragment_codes(I, [H|T]) -->
  (   pchar(I, H)
  ;   forward_slash(H)
  ;   question_mark(H)
  ),
  fragment_codes(I, T).
fragment_codes(_, []) --> [].
