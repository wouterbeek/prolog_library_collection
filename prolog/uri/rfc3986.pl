:- module(
  rfc3986,
  [
    'pct-encoded'//1 % ?Code:code
  ]
).

/** <module> RFC 3986: URIs?

@author Wouter Beek
@compat RFC 3986
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/rfc5234)).





%! 'pct-encoded'(?Code:between(0,255))// .
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) -->
  "%",
  '#'(2, 'HEXDIG', C, [convert(1-positional)]).
