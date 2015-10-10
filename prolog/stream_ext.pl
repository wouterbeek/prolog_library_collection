:- module(
  stream_ext,
  [
    stream_metadata/2 % +Stream:stream
                      % -Metadata:dict
  ]
).

/** <module> Stream extensions

@author Wouter Beek
@version 2015/10
*/





stream_metadata(S, M):-
  findall(P, (stream_property(S, P0), property_pair0(P0, P)), L),
  dict_pairs(M, stream, L).

property_pair0(Property, Pair):-
  Property =.. [Key|Values],
  property_pair0(Key, Values, Pair).

property_pair0(position, [Pos], Key-Val):- !,
  stream_position_data(Key, Pos, Val).
property_pair0(Key, [], Key-true):- !.
property_pair0(Key, [Val], Key-Val).
