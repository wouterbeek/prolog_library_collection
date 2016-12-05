:- module(
  thread_ext,
  [
    attached_thread/1,           % :Goal_0
    call_on_wildcard/2,          % +Wildcard, :Goal_1
    call_on_wildcard/3,          % +Wildcard, :Goal_1, +Opts
    create_thread/1,             % :Goal_0
    default_number_of_threads/1, % ?NumberOfThreads
    detached_thread/1,           % :Goal_0
    intermittent_thread/3,       % :Goal_0, :EndGoal_0, +Interval
    intermittent_thread/4,       % :Goal_0, :EndGoal_0, +Interval, +Opts
    print_thread/0,
    print_thread/1,              % +Name
    print_threads/0,
    thread_name/1,               % -Name
    threadsafe_alias/2,          % +Alias, -TAlias
    threadsafe_format/3,         % +Alias, +Format, +Args
    threadsafe_name/2            % +Name1, -Name2
  ]
).

/** <module> Thread extensions

@author Wouter Beek
@version 2015/10, 2016/01-2016/03, 2016/05-2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(print_ext)).
:- use_module(library(stream_ext)).

:- meta_predicate
    attached_thread(0),
    call_on_wildcard(+, 1),
    call_on_wildcard(+, 1, +),
    create_thread(0),
    detached_thread(0),
    intermittent_goal(0, 0, +),
    intermittent_thread(0, 0, +),
    intermittent_thread(0, 0, +, +).





%! attached_thread(:Goal_0) is det.

attached_thread(Goal_0) :-
  thread_create(Goal_0, _, []).



%! call_on_wildcard(+Wildcard, :Goal_1) is det.
%! call_on_wildcard(+Wildcard, :Goal_1, +Opts) is det.
%
% The following options are supported:
%
%   * concurrent(+positive_integer) The number of thread that are used
%   in parallel processing multiple files.

call_on_wildcard(Wildcard0, Goal_1) :-
  call_on_wildcard(Wildcard0, Goal_1, _{}).


call_on_wildcard(Wildcard0, Goal_1, Opts) :-
  (   dict_get(search_path, Opts, Search)
  ->  Spec =.. [Search,.],
      absolute_file_name(Spec, Prefix),
      directory_file_path(Prefix, Wildcard0, Wildcard)
  ;   Wildcard = Wildcard0
  ),
  expand_file_name(Wildcard, Files),
  (   get_dict(concurrent, Opts, true)
  ->  concurrent_maplist(Goal_1, Files)
  ;   maplist(Goal_1, Files)
  ).



%! create_thread(:Goal_0) is det.

create_thread(Goal_0) :-
  thread_create(Goal_0, _, []).



%! default_number_of_threads(+NumberOfThreads:positive_integer) is semidet.
%! default_number_of_threads(-NumberOfThreads:positive_integer) is det.

default_number_of_threads(N) :-
  current_prolog_flag(cpu_count, N).



%! detached_thread(:Goal_0) is det.

detached_thread(Goal_0) :-
  thread_create(Goal_0, _, [detached(true)]).



%! intermittent_goal(:Goal_0, :EndGoal_0, +Interval:positive_integer) is det.
% Performs the given goal interspersed with time intervals
% of the given duration.
%
% If the end goal succeeds the thread succeeds
% (i.e., intermittent goal execution stops).

intermittent_goal(_, EndGoal_0, _) :-
  EndGoal_0, !.
intermittent_goal(Goal_0, EndGoal_0, I) :-
  Goal_0,
  sleep(I),
  intermittent_goal(Goal_0, EndGoal_0, I).



%! intermittent_thread(:Goal_0, :EndGoal_0, +Interval) is det.
%! intermittent_thread(
%!   :Goal_0,
%!   :EndGoal_0,
%!   +Interval:positive_integer,
%!   +Opts
%! ) is det.
% The following options are supported:
%   * id(?atom)
%   * Other options are passed to thread_create/3.

intermittent_thread(Goal_0, EndGoal_0, I) :-
  intermittent_thread(Goal_0, EndGoal_0, I, []).
intermittent_thread(Goal_0, EndGoal_0, I, Opts) :-
  ignore(option(id(Id), Opts)),
  thread_create(intermittent_goal(Goal_0, EndGoal_0, I), Id, Opts).



print_thread:-
  thread_name(Name),
  print_thread(Name).


print_thread(Name) :-
  dcg_with_output_to(user_output, thread(0, Name)).



print_threads:-
  % Print the threads in alphabetical order.
  aggregate_all(set(Name), thread_property(_, alias(Name)), Names),
  dcg_with_output_to(user_output, threads(0, Names)).



%! thread_name(-Name:atom) is det.
% Returns the name of the current thread.

thread_name(Name) :-
  thread_self(Id),
  thread_property(Id, alias(Name)), !.
thread_name(Name) :-
  thread_self(Name).



threadsafe_alias(Alias, TAlias) :-
  threadsafe_name(Alias, TAlias),
  exists_stream_alias(TAlias).



threadsafe_format(Alias, Format, Args) :-
  threadsafe_alias(Alias, TAlias),
  format(TAlias, Format, Args).



threadsafe_name(Name1, Name2) :-
  thread_name(Name0),
  atomic_list_concat([Name0,Name1], Name2).





% GRAMMAR %

thread(I, Name) -->
  {
    thread_property(Id, alias(Name)),
    thread_property(Id, status(Status))
  },
  tab(I),
  atom(Name),
  ": ",
  atom(Status).


thread_items(I, [H|T]) -->
  thread(I, H), !,
  thread_items(I, T).
thread_items(_, []) --> [].


threads(I1, L) -->
  {succ(I1, I2)},
  section(I1, "Thead overview", thread_items(I2, L)).
