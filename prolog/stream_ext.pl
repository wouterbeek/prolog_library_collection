:- module(
  stream_ext,
  [
    stream_metadata/2 % +Stream, -Metadata
  ]
).

/** <module> Stream extensions

@author Wouter Beek
@version 2015/10, 2016/01
*/

:- use_module(library(dict_ext)).





stream_metadata(S, D):-
  findall(P, (stream_property(S, P0), property_pair0(P0, P)), L),
  dict_pairs(D, L).

property_pair0(Property, Pair):-
  Property =.. [Key|Values],
  property_pair0(Key, Values, Pair).

property_pair0(position, [Pos], Key-Val):- !,
  stream_position_data(Key, Pos, Val).
property_pair0(Key, [], Key-true):- !.
property_pair0(Key, [Val], Key-Val).
