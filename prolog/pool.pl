:- module(
  pool,
  [
    add_resource/2,   % +Pool, +Resource
    add_worker/3,     % +Pool, :Goal_1, +Opts
    pool/1,           % ?Pool
    print_pool/1,     % ?Pool
    print_pools/0,
    remove_resource/2 % +Pool, -Resource
  ]
).

/** <module> Pool

@author Wouter Beek
@version 2015/12, 2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(solution_sequences)).

%:- debug(pool(add)).

:- meta_predicate(add_worker(+,2,+)).
:- meta_predicate(pool_worker(+,2,+)).

:- predicate_options(add_worker/3, 3, [
     alias(-atom),
     pass_to(pool_worker/3, 3)
   ]).
:- predicate_options(pool_worker/3, 3, [
     wait(+nonneg)
   ]).


%! pool(?Pool, ?Term) is nondet.
% Currently in pool pending processing.

%! pooling(?Pool, ?Term) is nondet.
% Currently being processed.

%! pooled(?Pool, ?Term) is nondet.
% Previously processed.

:- dynamic
    pool/2,
    pooling/2,
    pooled/2.





%! add_resource(+Pool, +Resource) is det.

add_resource(Pool, X):-
  with_mutex(pool, add_resource0(Pool, X)).

add_resource0(Pool, X):-
  pooled(Pool, X), !,
  debug(pool(skip), "~w was already pooled in ~w", [X,Pool]).
add_resource0(Pool, X):-
  pooling(Pool, X), !,
  debug(pool(skip), "~w is currently pooling in ~w", [X,Pool]).
add_resource0(Pool, X):-
  pool(Pool, X), !,
  debug(pool(skip), "~w is already in pool ~w", [X,Pool]).
add_resource0(Pool, X):-
  assertz(pool(Pool,X)),
  debug(pool(add), "Added ~w to pool ~w", [X,Pool]).



%! add_worker(+Pool, :Goal_2, +Options:list(compound)) is det.
% Options are passed to pool_worker/3.

add_worker(Pool, Goal_2, Opts):-
  flag(Pool, N, N + 1),
  format(atom(Alias), "~w_~d", [Pool,N]),
  ignore(option(alias(Alias), Opts)),
  thread_create(pool_worker(Pool, Goal_2, Opts), _, [detached(true)]).



%! pool(+Pool) is semidet.
%! pool(-Pool) is nondet.

pool(Pool):-
  distinct(Pool, pool0(Pool)).
pool0(Pool):- pool(Pool, _).
pool0(Pool):- pooling(Pool, _).
pool0(Pool):- pooled(Pool, _).



%! pool_worker(+Pool, :Goal_2, +Options:list(compound)) is det.
% The following options are supported:
%   * wait(+nonneg)
%     Default is `1'.

pool_worker(Pool, Goal_2, Opts):-
  remove_resource(Pool, X), !,
  call(Goal_2, X, Ys),
  with_mutex(pool, (
    retract(pooling(Pool,X)),
    assert(pooled(Pool,X)),
    maplist(add_resource0(Pool), Ys)
  )),
  pool_worker(Pool, Goal_2, Opts).
pool_worker(Pool, Goal_2, Opts):-
  option(wait(N), Opts, 1),
  sleep(N),
  pool_worker(Pool, Goal_2, Opts).



%! print_pool(+Pool) is det.
%! print_pool(-Pool) is nondet.

print_pool(Pool):-
  % Enforce determinism for instantiation `(+)'.
  (var(Pool) -> pool(Pool) ; once(pool(Pool))),
  aggregate_all(count, pool(Pool, _), NPool),
  aggregate_all(count, pooling(Pool, _), NPooling),
  aggregate_all(count, pooled(Pool, _), NPooled),
  format(
    "Pool ~w:~n  Pending: ~D~n  Processing: ~D~n  Processed: ~D~n",
    [Pool,NPool,NPooling,NPooled]
  ).



%! print_pools is det.

print_pools:-
  print_pool(_),
  fail.
print_pools.



%! remove_resource(+Pool, -Resource) is det.

remove_resource(Pool, Res):-
  with_mutex(pool, (retract(pool(Pool,Res)), assert(pooling(Pool,Res)))).
