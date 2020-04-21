:- module(
  debug_gui,
  [
    dmon/0,
    tmon/0
  ]
).

/** <module> GUI (XPCE) support for debugging

*/

:- use_module(library(swi_ide)).





%! dmon is det.
%
% Wrapper that starts the debug monitor.

dmon :-
  prolog_ide(debug_monitor).



%! tmon is det.
%
% Wrapper that starts the thread monitor.

tmon :-
  prolog_ide(thread_monitor).
