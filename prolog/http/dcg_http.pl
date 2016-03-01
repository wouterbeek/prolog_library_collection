:- module(
  dcg_http,
  [
    '*#'//2, % :Dcg_1, -Contents:list
    '+#'//2  % :Dcg_1, -Contents:list
  ]
).

/** <module> DCG rules for HTTP

@author Wouter Beek
@compat RFC 2616
@depracated
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), ['LWS'//0]).

:- meta_predicate(*#(3,-,?,?)).
:- meta_predicate(+#(3,-,?,?)).





%! *#(:Dcg_1, ?Contents:list)// .

*#(Dcg_1, [H|T]) --> dcg_call(Dcg_1, H), !, *(sep), *#(Dcg_1, T).
*#(_, [])        --> "".



%! +#(:Dcg_1, ?Contents:list)// .

+#(Dcg_1, [H|T]) --> dcg_call(Dcg_1, H), !, *(sep), *#(Dcg_1, T).





% HELPERS %

sep --> 'LWS'.
sep --> ",".
