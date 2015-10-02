:- module(
  http_deb,
  [
    http_status_code//1, % +Code:between(100,599)
    http_status_label/2 % +Code:between(100,599)
                        % -Label:atom
  ]
).

/** <module> HTTP debug

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/http_header)). % Private.





%! http_status_code(+Code:between(100,599))// is det.

http_status_code(Code) -->
  "HTTP status code ",
  integer(Code),
  " ",
  {http_header:status_number_fact(Fact, Code)},
  bracketed(http_header:status_comment(Fact)),
  ".".



%! http_status_label(+Code:between(100,599), -Label:atom) is det.

http_status_label(Code, Label):-
   http_header:status_number_fact(Fact, Code),
   atom_phrase(http_status_code(Fact), Label).
