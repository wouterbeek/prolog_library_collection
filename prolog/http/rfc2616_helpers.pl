:- module(
  rfc2616_helpers,
  [
    abnf_list//2, % :Dcg_1
                  % Contents:list
    parameters//1 % ?Parameters:list(pair(string))
  ]
).

/** <module> RFC 2616: Helper predicates

@author Wouter Beek
@compat RFC 2616
@depracated
@version 2015/11
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_token)).

:- meta_predicate(abnf_list(3,-,?,?)).





%! abnf_list(:Dcg_1, ?Contents:list)// .

abnf_list(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H), !,
  abnf_list_sep,
  abnf_list(Dcg_1, T).
abnf_list(_, [])        --> "".
abnf_list_sep --> 'LWS', !, abnf_list_sep.
abnf_list_sep --> ",",   !, abnf_list_sep.
abnf_list_sep --> "".



%! parameters(?Parameters:list(pair(string)))// .

parameters([H|T]) --> ";", !, ('LWS', ! ; ""),  parameter(H), parameters(T).
parameters([]) --> "".
