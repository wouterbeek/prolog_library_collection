:- module(
  stream_ext,
  [
    exists_stream_alias/1 % +Alias
  ]
).

/** <module> Stream extensions

@author Wouter Beek
@version 2015/10, 2016/01, 2016/03
*/





%! exists_stream_alias(+Alias) is det.

exists_stream_alias(Alias) :-
  stream_property(_, alias(Alias)).
