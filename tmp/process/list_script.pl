:- module(
  list_script,
  [
    list_script/3 % :Goal_1
                  % +Todo:list
                  % +Options:list(nvpair)
  ]
).

/** <module> List script

Runs a given goal for a given list of arguments and prints the process
on a per-item basis.
Also keeps track of items that could not be processed.

@author Wouter Beek
@version 2015/02
*/

:- use_module(library(dcg/basics)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(dcg/dcg_atom)).
:- use_module(plc(dcg/dcg_bracket)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(dcg/dcg_debug)).
:- use_module(plc(dcg/dcg_pl_term)).
:- use_module(plc(process/progress)).

:- predicate_options(list_script/3, 3, [
     message(+atom),
     notdone(-list),
     overview(+boolean),
     skip(+nonneg),
     with_mutex(+boolean)
   ]).

:- meta_predicate(list_script(1,+,+)).
:- meta_predicate(list_script(1,+,+,+,-,-,?)).





%! list_script(:Goal_1, +Todo:list, +Options:list(nvpair)) is det.
% Processes the items in `Todo` using the given goal
% and places the items either in the `Done` or in the `NotDone` list.
% The `Done` list can be pre-instantiated.
%
% The following options are supported:
%   - message(+atom)
%     Set the message that is displayed upon completing an item.
%   - notdone(-list)
%     Returns the sublist of items that could not be processed.
%   - overview(+boolean)
%     Whether to show an overview afterwards (`false` by default).
%   - skip(+nonneg)
%     Skips the first _M_ items in Todo.
%   - with_mutex(+atom)
%     Run the goal for each item inside the given mutex.

list_script(Goal_1, Todo0, Options):-
  length(Todo0, N),

  % Process option `message`.
  (   option(message(Msg0), Options)
  ->  Msg = Msg0
  ;   Msg = 'Processed'
  ),

  % Process option `skip`.
  (   option(skip(M), Options)
  ->  length(Skip, M),
      append(Skip, Todo, Todo0)
  ;   M = 0,
      Todo = Todo0
  ),

  % Process list.
  option(with_mutex(Mutex), Options, _VAR),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex),

  % Process option `notdone`.
  (   option(notdone(NotDone0), Options)
  ->  NotDone0 = NotDone
  ;   true
  ),

  % Show an overview of processing the list.
  (   option(overview(true), Options)
  ->  dcg_debug(list_script, items_done(Done, N)),
      dcg_debug(list_script, items_not_done(NotDone,N))
  ;   true
  ).


%! list_script(
%!   :Goal_1,
%!   +Message:atom,
%!   +Counter:pair(nonneg),
%!   +Todo:list(term),
%!   -Done:list(term),
%!   -NotDone:list(term),
%!   ?Mutex:atom
%! ) is det.

% Nothing to do.
list_script(_, _, counter(N,N), [], [], [], _):- !.
% One more TODO item gets pushed to DONE.
list_script(Goal_1, Msg, counter(M0,N), [X|Todo], [X|Done], NotDone, Mutex):-
  (   ground(Mutex)
  ->  with_mutex(Mutex, call(Goal_1, X))
  ;   call(Goal_1, X)
  ), !,
  M is M0 + 1,
  % Retrieve the current index, based on the previous index.
  dcg_debug(list_script, item_done(counter(M,N), Msg)),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex).
% A TODO item could not be processed; pushed to NOT-DONE.
list_script(Goal_1, Msg, counter(M0,N), [X|Todo], Done, [X|NotDone], Mutex):-
  M is M0 + 1,
  dcg_debug(list_script, item_not_done(counter(M,N), Msg)),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex).





% DEBUG %

counter(counter(M,N)) -->
  bracketed(square, (
    thousands_integer(M),
    "/",
    thousands_integer(N))
  ).

enumerate_item(X) -->
  "  - ",
  dcg_pl_term(X).

enumerate_items([]) --> [].
enumerate_items([H|T]) -->
  enumerate_item(H),
  enumerate_items(T).

item_done(Counter, Msg) -->
  item_processed(done, Counter, Msg).

item_not_done(Counter, Msg) -->
  item_processed(not_done, Counter, Msg).

item_processed(Mode, Counter, Msg) -->
  mode(Mode),
  " ",
  counter(Counter),
  " ",
  message(Msg).

items_done([], _) --> !, [].
items_done(L, N) -->
  items_processed(done, L, N).

items_not_done([], _) --> !, [].
items_not_done(L, N) -->
  items_processed(not_done, L, N),
  enumerate_items(L).

items_processed(Mode, L, N) -->
  mode(Mode),
  " ",
  progress_bar(L, N).

message(Msg) -->
  atom(Msg).

mode(done) -->
  atom('[DONE]').
mode(not_done) -->
  atom('[NOT-DONE]').

progress_bar(L, N) -->
  {
    length(L, M),
    progress_bar(M, N, Bar)
  },
  atom(Bar).
