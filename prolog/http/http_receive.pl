:- module(
  http_receive,
  [
    http_parsed_header_pair/2, % +Header:compound
                               % -ParsedHeaderPair:pair(atom)
    http_search/3 % +Request:list(compound)
                  % +Key:atom
                  % -Value:atom
  ]
).

/** <module> HTTP receive

Support for receiving an HTTP reply.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/rfc6266)).





%! http_parsed_header_pair(+Header:compound, -ParsedHeaderPair:pair(atom)) is det.

http_parsed_header_pair(Comp, N-V):-
  Comp =.. [N0,V0],
  http_restore_header_name(N0, N),
  http_parse_header_value(N, V0, V).


http_restore_header_name(N0, N):-
  atomic_list_concat(L0, '_', N0),
  maplist(capitalize_atom, L0, L),
  atomic_list_concat(L, -, N).


http_parse_header_value(N, V0, V):-
  Dcg_0 =.. [N,V],
  atom_phrase(Dcg_0, V0).



%! http_search(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search(Request, Key, Val):-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).
