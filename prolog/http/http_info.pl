:- module(
  http_info,
  [
    http_scheme/1,       % ?Scheme
    http_status_label/2, % +StatusCode, -Label
    is_http_error/1      % +StatusCode
  ]
).

/** <module> HTTP: Information

Predicates that supply some form of information about HTTP.

@author Wouter Beek
@version 2015/10-2016/01
*/

:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/http_header)). % Private.





%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).



%! http_status_label(+Code:between(100,599), -Label:atom) is det.

http_status_label(Code, Label):-
  http_header:status_number_fact(Fact, Code),
  atom_phrase(http_header:status_comment(Fact), Label).



%! is_http_error(+Status:positive_integer) is semidet.

is_http_error(Status):-
  between(400, 599, Status).
