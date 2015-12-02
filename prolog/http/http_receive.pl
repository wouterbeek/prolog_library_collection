:- module(
  http_receive,
  [
    http_search/3, % +Request:list(compound)
                   % +Key:atom
                   % -Value:atom
    http_search_pl/3 % +Request:list(compound)
                     % +Key:atom
                     % -Value:atom
  ]
).

/** <module> HTTP receive

Support for receiving an HTTP reply.

@author Wouter Beek
@version 2015/08, 2015/12
*/





%! http_search(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search(Request, Key, Val):-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).



%! http_search_pl(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search_pl(Request, Key, Val):-
  http_search(Request, Key, Atom),
  term_to_atom(Val, Atom).
