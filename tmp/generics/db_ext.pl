:- module(
  db_ext,
  [
    db_add/1, % +New
    db_add_clause/2, % +Head:term
                     % +Body:or([list(term),term])
    db_add_clause/3, % +Module:atom
                     % +Head:term
                     % +Body:or([list(term),term])
    db_add_dcg_rule/2, % +Head:term
                       % +Body:or([list(term),term])
    db_add_dcg_rule/3, % +Module:atom
                       % +Head:term
                       % +Body:or([list(term),term])
    db_add_novel/1, % :New
    db_replace/2, % +New:ground
                  % +Pattern:list(oneof([e,r]))
    db_replace_all/2, % :Old
                      % +New:ground
    db_replace_some/2 % :Old
                      % +New:ground
  ]
).

/** <module> Database extensions

Replacement predicates should specify which parameters
 are to be replaced and which are to stay the same.

For example [1] should replace [2] but not [3].
```prolog
[1]   rdf_namespace_color(rdf, red)
[2]   rdf_namespace_color(rdf, blue)
[3]   rdf_namespace_color(rdfs, blue)
```

@author Wouter Beek
@version 2013/04-2013/05, 2013/08, 2014/07, 2014/12
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate(db_add_novel(:)).
:- meta_predicate(db_replace_all(:,+)).
:- meta_predicate(db_replace_some(:,+)).





%! db_add(+New) is det.
% Same as `assert(New)`; for consistency with other predicates in this module.

db_add(New):-
  assert(New).


%! db_add_clause(+Head:term, +Body:term) is det.
% Wrapper around db_add_clause/3, adding a clause to the `user` module.

db_add_clause(Head, Body):-
  db_add_clause(user, Head, Body).

%! db_add_clause(+Module:atom, +Head:term, Body:term) is det.
% Simplifies the assertion of clauses.

db_add_clause(Module, Head, Body1):-
  (
    is_list(Body1)
  ->
    construct_body(Body1, Body2)
  ;
    Body2 = Body1
  ),
  Clause =.. [':-',Head,Body2],
  assert(Module:Clause).


%! db_add_dcg_rule(+Head:term, +Body:term) is det.
% Wrapper around db_add_dcg_rule/3, adding a DCG rule to the `user` module.

db_add_dcg_rule(Head, Body):-
  db_add_dcg_rule(user, Head, Body).

%! db_add_dcg_rule(+Module:atom, +Head:term, +Body:term) is det.

db_add_dcg_rule(Module, Head, Body1):-
  (
    is_list(Body1)
  ->
    construct_body(Body1, Body2)
  ;
    Body2 = Body1
  ),
  Dcg =.. ['-->',Head,Body2],
  dcg_translate_rule(Dcg, Clause),
  assert(Module:Clause).


%! db_add_novel(:New) is det.
% Asserts the given fact, only if it does not already exist.
% Succeeds without doing anything, otherwise.

db_add_novel(Mod:New):-
  Mod:New, !.
db_add_novel(Mod:New):-
  Mod:assert(New).


%! db_replace(+New, +Pattern:list(oneof([e,r]))) is det.
% Replaces at most one asserted fact (if present) with another one.
%
% In the pattern:
%   * =e= stands for arguments that should be the same
%     as in the given (new) fact.
%   * =r= stands for arguments that should be different
%     in a fact in order to count as an old fact.
%
% @throws instantiation_error if `Pattern` is uninstantiated,
% @throws type_error if `Pattern` is not a list.

db_replace(_, Pattern):-
  var(Pattern), !,
  instantiation_error(Pattern).
db_replace(_, Pattern):-
  \+ is_list(Pattern), !,
  type_error(list, Pattern).
db_replace(_, Pattern):-
  \+ maplist(pattern_value, Pattern), !,
  domain_error(list(oneof([e,r])), Pattern).
db_replace(New, Pattern):-
  findall(
    Old,
    (
      find_pattern_match(New, Pattern, Old),
      db_replace_some(Old, New)
    ),
    Olds
  ),
  Olds \== [], !.
db_replace(New, _):-
  assert(New).


%! db_replace_all(:Old, +New:ground) is det.
% Replaces all matches of `Old` with a single occurrence of `New`.
%
% Succeeds even if no match for `Old` is found,
% mimicking universal quantification.

db_replace_all(Old, New):-
  retractall(Old),
  assert(New).
db_replace_all(_, _).


%! dh_replace_first(:Old, +New:ground) is det.
% Replace the first match for `Old` with a single occurrence of `New`.
%
% Which match is first is chosen arbitrarily.
%
% Fails silently if no match of `Old` occurs.
% mimicking existential quantification.

db_replace_some(Old, New):-
  retract(Old),
  assert(New).



% Helpers.

construct_body([Body], Body):- !.
construct_body([X,Y], Body):- !,
  Body =.. [',',X,Y].
construct_body([X|T], Outer):-
  construct_body(T, Inner),
  Outer =.. [',',X,Inner].


%! find_pattern_match(:New, +Pattern:list(oneof([e,r])), :Old) is nondet.
% Returns an old fact that matches the given new fact description,
% using the given pattern specification.
%
% The pattern consists of the following values:
%   * `e`
%     Indicates that an argument in the old fact should be the same
%     as in the new fact.
%   * `r`
%     Indicates that an argument in the old fact should be different
%     from the one in the new fact.

find_pattern_match(Module:New, Pattern, Module:Old):-
  New =.. [Predicate|NewArguments],
  maplist(match_argument, NewArguments, Pattern, OldArguments),
  Old =.. [Predicate|OldArguments].

match_argument(X, e, X):- !.
match_argument(_, r, _).


pattern_value(e).
pattern_value(r).

