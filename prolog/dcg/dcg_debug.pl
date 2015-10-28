:- module(
  dcg_debug,
  [
    dcg_debug/2 % +Topic:compound
                % :Dcg_0
  ]
).
:- reexport(library(debug)).

/** <module> DCG debug

DCG-based debug tools.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(dcg/dcg_phrase)).

:- meta_predicate(dcg_debug(+,//)).





%! dcg_debug(+Topic:compound, :Dcg_0) is det.
% Write the first generation of Dcg_0 as a debug message with given Topic.

dcg_debug(Topic, Dcg_0):-
  string_phrase(Dcg_0, S),
  debug(Topic, "~s", [S]).
