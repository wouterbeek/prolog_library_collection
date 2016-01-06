:- module(
  http_receive,
  [
    http_method/2,      % +Request, -Method
    http_output/2,      % +Request, -Output
    http_request_iri/3, % +Request, +Prefix, -Iri
    http_request_uri/2, % +Request, -Uri
    http_search/3,      % +Request, +Key, -Value
    http_search_pl/3    % +Request, +Key, -Value
  ]
).

/** <module> HTTP receive

Support for receiving an HTTP reply.

@author Wouter Beek
@version 2015/08, 2015/12-2016/01
*/

:- use_module(library(rdf/rdf_prefix)).





%! http_method(+Request:list(compound), -Method:atom) is det.

http_method(Req, M):-
  memberchk(method(M), Req).



%! http_output(+Request:list(compound), -Output:stream) is det.

http_output(Req, Out):-
  memberchk(pool(client(_,_,_,Out)), Req).



%! http_request_iri(+Request:list(compound), +Prefix:atom, -Iri:atom) is det.

http_request_iri(Req, Prefix, Iri):-
  memberchk(request_uri(Local0), Req),
  sub_atom(Local0, 1, _, 0, Local),
  rdf_global_id(Prefix:Local, Iri).



%! http_request_uri(+Request:list(compound), -Uri:atom) is det.

http_request_uri(Req, Uri):-
  memberchk(request_uri(Uri), Req).



%! http_search(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search(Request, Key, Val):-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).



%! http_search_pl(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search_pl(Request, Key, Val):-
  http_search(Request, Key, Atom),
  term_to_atom(Val, Atom).
