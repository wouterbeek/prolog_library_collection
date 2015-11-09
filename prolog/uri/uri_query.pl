:- module(
  uri_query,
  [
    query//2 % ?Iri:boolean
             % ?Query:string
  ]
).

/** <module. RFC 3986 & RFC 3987: Query component

@author Wouter Beek
@compat RFC 3986
@compat RFC 3987
@version 2015
*/

:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(uri/uri_code)).





%! query(?Iri:boolean, ?Query:string)// .
% ```abnf
%  query = *(  pchar /            "/" / "?" )
% iquery = *( ipchar / iprivate / "/" / "?" )
% ```
%
% @compat RFC 3986
% @compat RFC 3987

query(I, S) --> dcg_string(query_codes(I), S).

query_codes(I, [H|T]) -->
  (   pchar(I, H)
  ;   {I = true},
      private(H)
  ;   forward_slash(H)
  ;   question_mark(H)
  ),
  query_codes(I, T).
query_codes(_, []) --> [].
