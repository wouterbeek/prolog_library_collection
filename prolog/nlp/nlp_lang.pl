:- module(
  nlp_lang,
  [
    current_lrange/1, % -LRange
    current_ltag/1    % -LTag
  ]
).

/** <module> NLP: Language setting

@author Wouter Beek
@version 2016/02
*/

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



%! lrange_to_ltag(+LRange, -LTag) is det.

lrange_to_ltag(LRange, en) :-
  basic_filtering(LRange, en), !.
lrange_to_ltag(LRange, nl) :-
  basic_filtering(LRange, nl), !.
