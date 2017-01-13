:- module(
  setting_ext,
  [
    setting_nonvar/2 % +Key, -Val
  ]
).
:- reexport(library(settings)).

/** <module> Settings extension

@author Wouter Beek
@version 2017/01
*/





%! setting_nonvar(+Key, -Val) is semidet.

setting_nonvar(Key, Val) :-
  setting(Key, Val),
  nonvar(Val).
