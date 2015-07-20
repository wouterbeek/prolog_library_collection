:- module(
  dcg_debug,
  [
    dcg_debug/2 % +Topic
                % :Dcg
  ]
).

/** <module> DCG Debug

DCG-based debug tools.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(debug)).

:- use_module(plc(dcg/dcg_generics)).

:- meta_predicate(dcg_debug(+,//)).





%! dcg_debug(+Topic:atom, :Dcg) is det.

dcg_debug(Topic, Dcg):-
  atom_phrase(Dcg, Atom),
  debug(Topic, Atom, []).
