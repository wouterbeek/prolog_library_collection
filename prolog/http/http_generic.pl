:- module(
  http_generic,
  [
    http_status_reason/2  % +Status, -Reason
  ]
).

/** <module> HTTP generic

@author Wouter Beek
@version 2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http_header), []).

:- multifile
    uri:default_port/2.

uri:default_port(http, 80).
uri:default_port(https, 443).





%! http_status_reason(+Status:between(100,599), -Reason:string) is det.

http_status_reason(Status, Reason):-
  http_header:status_number_fact(Fact, Status),
  string_phrase(http_header:status_comment(Fact), Reason).
