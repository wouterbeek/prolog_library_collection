:- module(
  nlp_lang,
  [
    current_lrange/1, % -LRange
    current_ltag/1,   % -LTag
    current_ltag/2,   % +LTags, -LTag
    lstring/2         % +Name, -Str
  ]
).

/** <module> NLP: Language setting

@author Wouter Beek
@version 2016/02, 2016/04, 2016/06, 2016/09
*/

:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(settings)).

:- setting(nlp:lrange, list(atom), ['en-US'], '').

:- multifile
    nlp:lstring/3.





%! current_lrange(-LRange) is det.

current_lrange(LRange) :-
  nlp:setting(lrange, LRange).



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



%! lstring(+Name, -Str) is det.

lstring(Name, Str) :-
  current_ltag(LTag),
  nlp:lstring(LTag, Name, Str).
