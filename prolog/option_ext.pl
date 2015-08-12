:- module(
  option_ext,
  [
    if_option/3 % +Option:compound
                % +Options:list(compound)
                % :Goal
  ]
).
:- reexport(library(option)).

/** <module> Option extensions

Translation between options and their constituent parts:

```prolog
maplist(\Opt^Value^'=..'(Opt,[Name,Value]), Opts, Values)
```

---

@author Wouter Beek
@version 2015/07
*/

:- meta_predicate(if_option(+,+,0)).





%! if_option(+Option:compound, +Options:list(compound), :Goal_0) is det.

if_option(Opt, Opts, Goal_0):-
  option(Opt, Opts), !,
  Goal_0.
if_option(_, _, _).
