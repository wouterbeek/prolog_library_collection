:- module(
  rfc1738_helpers,
  [
    alphadigits//1, % ?Codes:list(code)
    content1//1, % ?Content:string
    content2//1 % ?Content:string
  ]
).

/** <module> RFC 1738: Helper predicates

@author Wouter Beek
@compat RFC 1738
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(url/rfc1738_code)).
:- use_module(library(url/rfc1738_component)).





alphadigits([H|T]) --> alphadigit(H), !, alphadigits(T).
alphadigits([0'-|T]) --> "-", !, alphadigits(T).
alphadigits([]) --> "".



% uchar | ";" | "?" | "&" | "="
content1(S) --> dcg_string(content1_codes, S).
content1_codes([H|T]) --> uchar(H), !, content1_codes(T).
content1_codes([0';|T]) --> ";", !, content1_codes(T).
content1_codes([0'?|T]) --> "?", !, content1_codes(T).
content1_codes([0'&|T]) --> "&", !, content1_codes(T).
content1_codes([0'=|T]) --> "=", !, content1_codes(T).
content1_codes([]) --> "".



% uchar | ";" | ":" | "@" | "&" | "="
content2(S) --> dcg_string(content2_codes, S).
content2_codes([H|T]) --> uchar(H), !, content2_codes(T).
content2_codes([0';|T]) --> ";", !, content2_codes(T).
content2_codes([0':|T]) --> ":", !, content2_codes(T).
content2_codes([0'@|T]) --> "@", !, content2_codes(T).
content2_codes([0'&|T]) --> "&", !, content2_codes(T).
content2_codes([0'=|T]) --> "=", !, content2_codes(T).
content2_codes([]) --> "".
