:- module(
  debug_gui,
  [
    dmon/0,
    gtrace_failure/1, % :Goal_0
    tmon/0
  ]
).

/** <module> GUI (XPCE) support for debugging

*/

:- use_module(library(swi_ide)).

:- meta_predicate
    gtrace_failure(0).





%! dmon is det.
%
% Wrapper that starts the debug monitor.

dmon :-
  prolog_ide(debug_monitor).



%! gtrace_failure(:Goal_0) is det.

gtrace_failure(Goal_0) :-
  catch(Goal_0, _, fail), !.
gtrace_failure(Goal_0) :-
  gtrace,
  Goal_0.



%! tmon is det.
%
% Wrapper that starts the thread monitor.

tmon :-
  prolog_ide(thread_monitor).
