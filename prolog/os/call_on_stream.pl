:- module(
  call_on_stream,
  [
    call_on_stream/4 % +Input
                     % +Mode:oneof([append,read,write])
                     % :Goal_2
                     % +Options:list(compound)
  ]
).

/** <module> Open any stream

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).

:- meta_predicate(call_on_stream(+,+,2,+)).

:- predicate_options(call_on_stream/4, 4, [
     pass_to(call_on_archive/3, 3),
     pass_to(open_any2/5)
   ]).





call_on_stream(In, Mode, Goal_2, Opts):-
  read_mode(Mode), !,
  call_on_archive(In, Goal_2, Opts).
call_on_stream(In, Mode, Goal_2, Opts0):-
  write_mode(Mode), !,
  merge_options([metadata(M)], Opts0, Opts),
  setup_call_cleanup(
    open_any2(In, Mode, Write, Close_0, Opts),
    call(Goal_2, M, Write),
    close_any2(Close_0)
  ).
