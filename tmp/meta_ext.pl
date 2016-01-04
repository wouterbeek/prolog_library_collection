:- module(
  meta_ext,
  [
% GENERIC CALLS
    generic/3, % :GenericPredicate
               % :Context
               % +Arguments:list

% IDLE
    true/1,
    true/2,
    true/3,
    true/4,
    true/5,

% MAPLIST RELATED PREDICATES
    app_list/3, % +Preds:list
                % +Args:list
                % -Results:list
    maplist_pairs/3, % :Goal
                     % +List1:list
                     % -List2:list
    mapset/3, % :Goal
              % +List:list
              % -Set:ordset
    nth0_call/3, % :Goal
                 % +Index:nonneg
                 % +Argument
    nth0_call/4, % +Options:list(nvpair)
                 % :Goal
                 % +Index:nonneg
                 % +Argument

% MODULES
    modules/1, % -Modules:list(atom)
    update_datastructure/4, % :Goal
                            % +OldDatastructure
                            % +Arguments:list
                            % -NewDatastructurte

% RETRY
    loop_until_true/1 % :Goal
  ]
).

/** <module> Meta extensions

Extensions to the SWI-Prolog meta predicates.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12,
         2014/03-2014/06, 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).

:- use_module(plc(generics/error_ext)).
:- use_module(plc(generics/list_ext)).
:- use_module(plc(prolog/pl_control)).

:- meta_predicate(generic(:,:,+)).
:- meta_predicate(maplist_pairs(3,+,-)).
:- meta_predicate(mapset(2,+,-)).
:- meta_predicate(nth0_call(1,+,+)).
:- meta_predicate(nth0_call(+,1,+,+)).
:- meta_predicate(loop_until_true(0)).
:- meta_predicate(loop_until_true0(+,0)).
:- meta_predicate(update_datastructure(3,+,+,-)).

:- dynamic(tmp/1).



% GENERIC CALLS %

%! generic(:GenericPredicate, :Context, +Arguments:list)
% This uses the naming convention that similar predicates share
% the same prefix.
%
% @arg GenericPredicate The predicate prefix,
%        denoting the generic part of the operation.
% @arg Context The predicate suffix,
%        denoting the specific part of the operation.
% @arg Arguments An argitrary number of arguments.

generic(P1, Context, Args):-
  % Make sure the calling module prefix is discarded.
  strip_module(P1, M, P0),
  strip_module(Context, M, Context0),
  atomic_list_concat([P0, Context0], '_', P2),
  length(Args, Arity),
  if_then(
    current_predicate(M:P2/Arity),
    apply(M:P2, Args)
  ).



% IDLE %

true(_).
true(_,_).
true(_,_,_).
true(_,_,_,_).
true(_,_,_,_,_).




% MAPLIST RELATED PREDICATES %

%! app_list(+Preds:list, +Args:list, -Results:list) is det.
% Applies multiple predicates to a static list of arguments.
% Returns the results of applying the given predicates to the given argument
% list. The number of results is the number of predicates. The arguments are
% the same for every predicate call.

app_list([], _Args, []).
app_list([Module:Pred | Preds], Args, [Result | Results]):-
  append(Args, [Result], Args0),
  Call =.. [Pred | Args0],
  call(Module:Call),
  app_list(Preds, Args, Results).

%! maplist_pairs(:Goal, +List1:list, -List2:list) is det.
% Applies the given goal to all pairs of elements occuring in `List1`.

maplist_pairs(Goal, List1, List2):-
  findall(
    Result,
    (
      member(Element1, Element2, List1),
      call(Goal, Element1, Element2, Result)
    ),
    List2
  ).

%! mapset(:Goal, +List:list(term), -Set:ordset(term)) is det.
% The sorted version of maplist/3.
%
% @arg Goal A goal.
% @arg List A list of terms.
% @arg Set An ordered set of terms.

mapset(Goal, List, Set):-
  maplist(Goal, List, NewList),
  sort(NewList, Set).


%! nth0_call(:Goal, +Index:nonneg, +Argument) .
%! nth0_call(+Options:list(nvpair), :Goal, +Index:nonneg, +Argument) .
% The following options are supported:
%   * `minus(+UseMinus:boolean)`
%     When `true` (default `false`), uses nth0_minus/4
%     instead of nth0/4. See module [list_ext].

nth0_call(Goal, I, X):-
  nth0_call([], Goal, I, X).

nth0_call(O1, Goal, I, X):-
  Goal =.. [Pred|Args1],

  % Insert the extra argument.
  (
    option(minus(true), O1, false)
  ->
    nth0_minus(I, Args2, X, Args1)
  ;
    nth0(I, Args2, X, Args1)
  ),

  apply(Pred, Args2).



% MODULES %

%! modules(-Modules:list(atom)) is det.
% Returns a list of the names of all the loaded modules.
%
% @arg Modules A list of atomic module names.

modules(Modules):-
  findall(
    Module,
    current_module(Module),
    Modules
  ).



%! update_datastructure(
%!   :Goal,
%!   +OldDatastructure,
%!   +Arguments:list,
%!   -NewDatastructurte
%! ) is det.
% Prolog cannot do pass-by-reference.
% This means that when a datastructure has to be repeatedly updated,
% both its old and its new version have to be passed around in full.
% Examples of these are in the SWI-Prolog libraries for association lists
% and ordered sets.

update_datastructure(_, Datastructure, [], Datastructure).
update_datastructure(Goal, Datastructure1, [H|T], Datastructure3):-
  call(Goal, Datastructure1, H, Datastructure2),
  update_datastructure(Goal, Datastructure2, T, Datastructure3).



% RETRY %

%! loop_until_true(:Goal) .

loop_until_true(Goal):-
  catch(
    Goal,
    Exception,
    loop_until_true0(Exception, Goal)
  ).

loop_until_true0(Exception, _):-
  var(Exception), !.
loop_until_true0(Exception, Goal):-
  debug(loop_until_true, '~w', Exception),
  Goal.

