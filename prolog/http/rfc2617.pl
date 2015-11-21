:- module(
  rfc2617,
  [
    challenge//1
  ]
).

/** <module> RFC 2617: HTTP Authentication

@author Wouter Beek
@deprecated
@see https://tools.ietf.org/html/rfc2617
@version 2015/11
*/

:- use_module(library(abnf_list)).





%! challenge// .
% ```abnf
% challenge   = auth-scheme 1*SP 1#auth-param
% ```

challenge --> 'auth-scheme', '+SP', '1#'('auth-param', Params).
