:- module(
  nlp_lang,
  [
    current_lrange/1, % -LRange
    current_ltag/1,   % -LTag
    current_ltag/2    % +LTags, -LTag
  ]
).

/** <module> NLP: Language setting

@author Wouter Beek
@version 2016/02, 2016/04
*/

:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(settings)).

:- setting(user:lrange, list(atom), ['en-US'], '').




%! current_lrange(-LRange) is det.

current_lrange(LRange) :-
  user:setting(lrange, LRange).



%! current_ltag(-LTag) is det.

current_ltag(LTag) :-
  current_lrange(LRange),
  lrange_to_ltag(LRange, LTag).


%! current_ltag(+LTags, -LTag) is det.

current_ltag(LTags, LTag) :-
  current_ltag(LTag),
  memberchk(LTag, LTags).



%! lrange_to_ltag(+LRange, -LTag) is det.

lrange_to_ltag(LRange, LTag2) :-
  member(LTag1, LRange),
  atomic_list_concat(L, -, LTag1),
  longest_to_shortest_prefix0(Prefix, L),
  Prefix \== [],
  atomic_list_concat(Prefix, -, LTag2).

longest_to_shortest_prefix0(L, L).
longest_to_shortest_prefix0(Prefix, L1) :-
  append(L2, [_], L1),
  longest_to_shortest_prefix0(Prefix, L2).
