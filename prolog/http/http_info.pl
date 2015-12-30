:- module(
  http_info,
  [
    http_scheme/1, % ?Scheme:atom
    http_status_code//1, % +Code:between(100,599)
    http_status_label/2, % +Code:between(100,599)
                         % -Label:atom
    is_http_error/1 % +Status:positive_integer
  ]
).

/** <module> HTTP: Information

Predicates that supply some form of information about HTTP.

@author Wouter Beek
@version 2015/10-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/http_header)). % Private.





%! http_scheme(+Scheme:atom) is semidet.
%! http_scheme(-Scheme:atom) is multi.

http_scheme(http).
http_scheme(https).



%! http_status_code(+Code:between(100,599))// is det.

http_status_code(Code) -->
  "HTTP status code ",
  integer(Code),
  " ",
  {http_status_label(Code, Label)},
  "(", atom(Label), ")",
  ".".



%! http_status_label(+Code:between(100,599), -Label:atom) is det.

http_status_label(Code, Label):-
   http_header:status_number_fact(Fact, Code),
   atom_phrase(http_header:status_comment(Fact), Label).



%! is_http_error(+Status:positive_integer) is semidet.

is_http_error(Status):-
  between(400, 599, Status).
