:- module(
  rfc3986,
  [
    'pct-encoded'//1 % ?Code:code
  ]
).

/** <module> RFC 3986: ???

@author Wouter Beek
@versio 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/rfc2234)).





%! 'pct-encoded'(?Code:between(0,255))// .
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) -->
  "%",
  '#'(2, 'HEXDIG', C, [convert(1-positional)]).
