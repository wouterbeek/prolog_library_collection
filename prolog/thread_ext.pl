:- module(
  thread_ext,
  [
    create_detached_thread/1, % :Goal_0
    create_detached_thread/2, % +Alias, :Goal_0
    thread_list/0,
    thread_monitor/0,
    thread_name/2,            % ?Id:handle, ?Name:atom
    thread_self_property/1,   % ?Property
    threaded_maplist/2,       % :Goal_1, ?Args1
    threaded_maplist/3        % :Goal_1, ?Args1, ?Args2
  ]
).
:- reexport(library(thread)).

/** <module> Thread extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).

:- meta_predicate
    create_detached_thread(0),
    create_detached_thread(+, 0),
    threaded_maplist(1, ?),
    threaded_maplist(2, ?, ?).





%! create_detached_thread(:Goal_0) is det.
%! create_detached_thread(+Alias:atom, :Goal_0) is det.

create_detached_thread(Goal_0) :-
  thread_create(Goal_0, _, [detached(true)]).


create_detached_thread(Alias, Goal_0) :-
  thread_create(Goal_0, _, [alias(Alias),detached(true)]).



%! thread_list is det.

thread_list :-
  aggregate_all(
    set(Name-Status),
    (
      thread_property(Id, status(Status)),
      thread_name(Id, Name)
    ),
    Pairs
  ),
  forall(
    member(Name-Status, Pairs),
    format(user_output, "~a\t~a\n", [Name,Status])
  ).



%! thread_monitor is det.
%
% Wrapper that starts the thread monitor.

thread_monitor :-
  prolog_ide(thread_monitor).



%! thread_name(+Id:handle, -Alias:atom) is det.

thread_name(Id, Alias) :-
  thread_property(Id, alias(Alias)), !.
thread_name(Id, Id).



%! thread_self_property(+Property:compound) is semidet.
%! thread_self_property(-Property:compound) is multi.

thread_self_property(Property) :-
  thread_self(Thread),
  thread_property(Thread, Property).



%! threaded_maplist(:Goal_1, ?Args1:list) is det.
%! threaded_maplist(:Goal_2, ?Args1:list, ?Args2:list) is det.

threaded_maplist(Mod:Goal_1, Args1) :-
  current_prolog_flag(cpu_count, N),
  maplist(make_goal_(Mod:Goal_1), Args1, Goals),
  concurrent(N, Goals, []).
make_goal_(Mod:Goal_1, Arg1, Mod:Goal_0) :-
  Goal_1 =.. [Pred|Args1],
  append(Args1, [Arg1], Args2),
  Goal_0 =.. [Pred|Args2].


threaded_maplist(Mod:Goal_2, Args1, Args2) :-
  current_prolog_flag(cpu_count, N),
  maplist(make_goal_(Mod:Goal_2), Args1, Args2, Goals),
  concurrent(N, Goals, []).
make_goal_(Mod:Goal_2, Arg1, Arg2, Mod:Goal_0) :-
  Goal_2 =.. [Pred|Args1],
  append(Args1, [Arg1,Arg2], Args2),
  Goal_0 =.. [Pred|Args2].
