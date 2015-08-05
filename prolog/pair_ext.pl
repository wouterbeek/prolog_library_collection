:- module(
  pair_ext,
  [
    'group_pairs_by_key@'/2 % +Pairs:list(pair)
                            % -GroupedPairs:list(pair)
  ]
).
:- reexport(library(pairs)).

/** <module> Pair extensions

Additional support for dealing with pairs.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(error)).

:- multifile(error:has_type/2).
error:has_type(pair, _-_).
error:has_type(pair(Type), X-Y):-
  error:has_type(pair(Type,Type), X-Y).
error:has_type(pair(Type1,Type2), X-Y):-
  maplist(error:has_type, [Type1,Type2], [X,Y]).





%! 'group_pairs_by_key@'(+Pairs:list(pair), -GroupedPairs:list(pair))
% is det.

'group_pairs_by_key@'([], []).
'group_pairs_by_key@'([M-N|T0], [M-[N|TN]|T]):-
  'same_key@'(M, T0, TN, T1),
  'group_pairs_by_key@'(T1, T).

'same_key@'(M0, [M-N|T0], [N|TN], T):-
  M0 =@= M, !,
  'same_key@'(M, T0, TN, T).
'same_key@'(_, L, [], L).
