:- module(
  http_ext,
  [
    http_iri/3 % +Request, +Prefix, -Iri
  ]
).

:- use_module(library(semweb/rdf_db)).



%! http_iri(+Request, +Prefix, -Iri) is det.

http_iri(Req, Prefix, Iri) :-
  memberchk(request_uri(Local0), Req),
  sub_atom(Local0, 1, _, 0, Local),
  rdf_global_id(Prefix:Local, Iri).
