:- module(
  list_script,
  [
    list_script/3 % :Goal_1, +Todo:list, +Opts
  ]
).

/** <module> List script

Runs a given goal for a given list of arguments and prints the process
on a per-item basis.
Also keeps track of items that could not be processed.

@author Wouter Beek
@version 2015/08, 2015/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(lists)).
:- use_module(library(progress)).

:- predicate_options(list_script/3, 3, [
     message(+atom),
     notdone(-list),
     overview(+boolean),
     skip(+nonneg),
     with_mutex(+boolean)
   ]).

:- meta_predicate
    list_script(1, +, +),
    list_script(1, +, +, +, -, -, ?).





%! list_script(:Goal_1, +Todo:list, +Opts) is det.
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

list_script(Goal_1, Todo0, Opts):-
  length(Todo0, N),
  option(message(Msg), Opts, 'Processed'),

  % Process option `skip`.
  (   option(skip(M), Opts)
  ->  length(Skip, M),
      append(Skip, Todo, Todo0)
  ;   M = 0,
      Todo = Todo0
  ),

  % Process list.
  option(with_mutex(Mutex), Opts, _VAR),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex),
  
  % Process option `notdone`.
  option(notdone(NotDone), Opts, true),

  % Show an overview of processing the list.
  (   option(overview(true), Opts)
  ->  debug(list_script, items_done(Done, N)),
      debug(list_script, items_not_done(NotDone,N))
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
  debug(list_script, item_done(counter(M,N), Msg)),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex).
% A TODO item could not be processed; pushed to NOT-DONE.
list_script(Goal_1, Msg, counter(M0,N), [X|Todo], Done, [X|NotDone], Mutex):-
  M is M0 + 1,
  debug(list_script, item_not_done(counter(M,N), Msg)),
  list_script(Goal_1, Msg, counter(M,N), Todo, Done, NotDone, Mutex).





% DEBUG %

counter(counter(M,N)) -->
  "[", thousands(M), "/", thousands(N), "]".

enumerate_item(X) -->
  "  - ", term(X).

'enumerate_item*'([H|T]) --> enumerate_item(H), 'enumerate_item*'(T).
'enumerate_item*'([]) --> "".

item_done(Counter, Msg) -->
  item_processed(done, Counter, Msg).

item_not_done(Counter, Msg) -->
  item_processed(not_done, Counter, Msg).

item_processed(Mode, Counter, Msg) -->
  mode(Mode), " ", counter(Counter), " ", message(Msg).

items_done([], _) --> !, [].
items_done(L, N) -->
  items_processed(done, L, N).

items_not_done([], _) --> !, [].
items_not_done(L, N) -->
  items_processed(not_done, L, N),
  'enumerate_item*'(L).

items_processed(Mode, L, N) -->
  mode(Mode), " ", progress_bar(L, N).

message(Msg) -->
  atom(Msg).

mode(done) -->
  atom('[DONE]').
mode(not_done) -->
  atom('[NOT-DONE]').

progress_bar(L, N) -->
  {length(L, M), progress_bar(M, N, Bar)},
  atom(Bar).
