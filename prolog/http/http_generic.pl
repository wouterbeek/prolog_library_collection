:- module(
  http_generic,
  [
    http_header_name_label/2, % +Name, -Label
    http_status_reason/2      % +Status, -Reason
  ]
).

/** <module> HTTP generics

@author Wouter Beek
@version 2017/08-2017/09
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg)).
:- use_module(library(http/http_header), []).

:- multifile
    uri:default_port/2.

uri:default_port(http, 80).
uri:default_port(https, 443).





%! http_header_name_label(+Name:atom, -Label:string) is det.

http_header_name_label(Name, Label) :-
  atomic_list_concat(Atoms, -, Name),
  maplist(atom_capitalize, Atoms, CAtoms),
  atomics_to_string(CAtoms, " ", Label).



%! http_status_reason(+Status:between(100,599), -Reason:string) is det.

http_status_reason(Status, Reason):-
  http_header:status_number_fact(Fact, Status),
  string_phrase(http_header:status_comment(Fact), Reason).
