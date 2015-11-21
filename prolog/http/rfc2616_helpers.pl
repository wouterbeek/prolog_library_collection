:- module(
  rfc2616_helpers,
  [
    parameters//1 % ?Parameters:list(pair(string))
  ]
).

/** <module> RFC 2616: Helper predicates

@author Wouter Beek
@compat RFC 2616
@depracated
@version 2015/11
*/

:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_token)).





%! parameters(?Parameters:list(pair(string)))// .

parameters([H|T]) --> ";", !, ('LWS', ! ; ""),  parameter(H), parameters(T).
parameters([]) --> "".
