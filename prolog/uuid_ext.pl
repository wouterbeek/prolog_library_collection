:- module(uuid, [uuid_no_hyphen/1]).

/** <module> UUID

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(uuid)).



%! uuid_no_hyphen(-Uuid:atom) is det.
% Succeeds with a new UUID without the annoying hyphens
% included by the standard library predicate.

uuid_no_hyphen(Id):-
  uuid(Id0),
  atomic_list_concat(Comps, -, Id0),
  atomic_list_concat(Comps, Id).
