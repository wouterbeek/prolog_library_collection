:- module(
  batch,
  [
    process_batch/2, % :Goal_1, +L
    process_batch/3  % :Goal_1, +L, +Opts
  ]
).

/** <module> Batch processing

Runs a given goal for a given batch of items and prints the process
on a per-item basis.  The process may not succeed for some items.
Such is life.  This module at least keeps track of the items that
cannot be processed.

@author Wouter Beek
@version 2015/08, 2015/10, 2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(lists)).

:- meta_predicate
    call_with_mutex(1, +, ?),
    process_batch(1, +),
    process_batch(1, +, +),
    process_batch(1, +, +, -, -, ?).





%! process_batch(:Goal_1, +L) is det.
%! process_batch(:Goal_1, +L, +Opts) is det.
% Process the items in L using the given Goal_1.
% Place items either in the `Done` or in the `NotDone` list.
%
% The following options are supported:
%   - failed(-list)
%     Returns the sublist of items that could not be processed.
%   - skip(+nonneg)
%     Allow a specific number of items from L to skipped.
%     Default is `0`.
%   - with_mutex(+atom)
%     Run Goal_1 for each item in L within a specific mutex.
%   - verbose(+boolean)
%     Whether to show an overview afterwards.
%     Default is `false`.

process_batch(Goal_1, L) :-
  process_batch(Goal_1, L, []).


process_batch(Goal_1, L, Opts) :-
  length(L, N),
  option(skip(Skip), Opts, 0),
  length(L1, Skip),
  append(L1, L2, L),
  option(with_mutex(Mutex), Opts, _NoMutex),
  process_batch(Goal_1, Skip-N, L2, Done, NotDone, Mutex),
  ignore(option(notdone(NotDone), Opts)),
  (   option(verbose(false), Opts)
  ->  true
  ;   length(Done, M),
      debug(process_batch, progress_bar(M, N))
  ).


%! process_batch(
%!   :Goal_1,
%!   +Counter:pair(nonneg),
%!   +Todo,
%!   -Done,
%!   -NotDone,
%!   ?Mutex
%! ) is det.

process_batch(_, _, [], [], [], _) :- !.
% One more item gets pushed to DONE!
process_batch(Goal_1, M1-N, [H|Todo], [H|Done], NotDone, Mutex) :-
  call_with_mutex(Goal_1, H, Mutex), !,
  M2 is M1 + 1,
  debug(process_batch, "~D/~D", [M2,N]),
  process_batch(Goal_1, M2-N, Todo, Done, NotDone, Mutex).
% An item could not be processed; pushed to NOT-DONE.
process_batch(Goal_1, M1-N, [X|Todo], Done, [X|NotDone], Mutex) :-
  M2 is M1 + 1,
  process_batch(Goal_1, M2-N, Todo, Done, NotDone, Mutex).





% HELPERS %

call_with_mutex(Goal_1, X, Mutex) :-
  ground(Mutex), !,
  with_mutex(Mutex, call(Goal_1, X)).
call_with_mutex(Goal_1, X, _) :-
  call(Goal_1, X).
