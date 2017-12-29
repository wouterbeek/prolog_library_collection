:- module(
  thread_ext,
  [
    thread_self_property/1 % ?Property
  ]
).
:- reexport(library(thread)).

/** <module> Thread extensions

@author Wouter Beek
@version 2017/12
*/





%! thread_self_property(+Property:compound) is semidet.
%! thread_self_property(-Property:compound) is multi.

thread_self_property(Property) :-
  thread_self(Thread),
  thread_property(Thread, Property).
