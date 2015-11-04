:- module(
  option_ext,
  [
    if_option/3, % +Option:compound
                 % +Options:list(compound)
                 % :Goal_0
    option_pair/2 % ?Option:compound
                  % ?Pair:pair
  ]
).
:- reexport(library(option)).

/** <module> Option extensions

@author Wouter Beek
@version 2015/07, 2015/10
*/

:- meta_predicate(if_option(+,+,0)).





%! if_option(+Option:compound, +Options:list(compound), :Goal_0) is det.

if_option(Opt, Opts, Goal_0):-
  option(Opt, Opts), !,
  Goal_0.
if_option(_, _, _).



%! option_pair(+Option:compound, +Pair:pair) is semidet.
%! option_pair(+Option:compound, -Pair:pair) is det.
%! option_pair(-Option:compound, +Pair:pair) is det.

option_pair(Opt, N-V):-
  Opt =.. [N,V].
