:- module(
  http_receive,
  [
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



%! http_search(+Request:list(compound), +Key:atom, -Value:atom) is det.

http_search(Request, Key, Val):-
  memberchk(search(L), Request),
  memberchk(Key=Val, L).
