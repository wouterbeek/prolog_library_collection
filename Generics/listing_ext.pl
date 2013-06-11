:- module(
  listing_ext,
  [
    listing/2 % +Stream:stream
              % :Pred
  ]
).

/** <module> LISTING_EXT

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(apply)).

:- meta_predicate(listing(+,:)).



list_clauserefs(_Stream, []):-
  !.
list_clauserefs(Stream, L):-
  is_list(L),
  !,
  maplist(list_clauserefs(Stream), L).
list_clauserefs(Stream, Ref) :-
  clause(Head, Body, Ref),
  portray_clause(Stream, (Head :- Body)).

list_clauses(Stream, Pred, Source):-
  strip_module(Pred, Module, Head),
  (
    clause(Pred, Body),
    write_module(Stream, Module, Source, Head),
    portray_clause(Stream, (Head:-Body)),
    fail
  ;
    true
  ).

list_declarations(Stream, Pred, Source) :-
  findall(
    Decl,
    prolog_listing:declaration(Pred, Source, Decl),
    Decls
  ),
  (
    Decls == []
 ->
    true
  ;
    write_declarations(Stream, Decls, Source),
    format(Stream, '~n', [])
  ).

list_predicate(Stream, Pred, Context):-
  predicate_property(Pred, undefined),
  !,
  prolog_listing:decl_term(Pred, Context, Decl),
  format(Stream, '%   Undefined: ~q~n', [Decl]).
list_predicate(Stream, Pred, Context) :-
  predicate_property(Pred, foreign),
  !,
  prolog_listing:decl_term(Pred, Context, Decl),
  format(Stream, '%   Foreign: ~q~n', [Decl]).
list_predicate(Stream, Pred, Context) :-
  notify_changed(Stream, Pred, Context),
  list_declarations(Stream, Pred, Context),
  list_clauses(Stream, Pred, Context).

%! list_predicates(+Stream:stream, :Preds:list(pi), :Spec) is det.

list_predicates(Stream, PIs, Context:X):-
  member(PI, PIs),
  prolog_listing:pi_to_head(PI, Pred),
  prolog_listing:unify_args(Pred, X),
  '$define_predicate'(Pred),
  strip_module(Pred, Module, Head),
  list_predicate(Stream, Module:Head, Context),
  nl(Stream),
  fail.
list_predicates(_Stream, _Preds, _Spec).

%! listing(+Stream:stream, :Pred)
% @see Same as listing/1, but writes to the given stream.

% Ignore variables.
listing(_Stream, V):-
  var(V),
  !,
  throw(error(instantiation_error, _Context)).
listing(_Stream, []):-
  !.
listing(Stream, [X | Rest]):-
  !,
  listing(Stream, X),
  listing(Stream, Rest).
listing(Stream, X):-
  (
    prolog:locate_clauses(X, ClauseRefs)
  ->
    list_clauserefs(Stream, ClauseRefs)
  ;
    '$find_predicate'(X, Preds),
    list_predicates(Stream, Preds, X)
).

notify_changed(Stream, Pred, Context) :-
  strip_module(Pred, user, Head),
  predicate_property(Head, built_in),
  \+ predicate_property(Head, (dynamic)), !,
  prolog_listing:decl_term(Pred, Context, Decl),
  format(
    Stream,
    '%   NOTE: system definition has been overruled for ~q~n',
    [Decl]
  ).
notify_changed(_Stream, _Pred, _Context).

write_declarations(_Stream, [], _):-
  !.
write_declarations(Stream, [H | T], Module) :-
  format(Stream, ':- ~q.~n', [H]),
  write_declarations(Stream, T, Module).

write_module(_Stream, Module, Context, Head):-
  prolog_listing:hide_module(Module, Context, Head),
  !.
write_module(Stream, Module, _Context, _Head):-
  format(Stream, '~q:', [Module]).

