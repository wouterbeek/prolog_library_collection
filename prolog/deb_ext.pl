:- module(
  deb_ext,
  [
    msg_emphasis/1, % +Message:atom
    msg_normal/1, % +Message:atom
    msg_notification/1, % +Message:atom
    msg_success/1, % +Message:atom
    verbose_call/1, % :Goal_0
    verbose_call/2, % +Message:atom
                    % :Goal_0
    msg_warning/1 % +Message:atom
  ]
).

/** <module> Debug extensions

Tools that ease debugging SWI-Prolog programs.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(ansi_term)).

:- meta_predicate(verbose_call(0)).
:- meta_predicate(verbose_call(+,0)).





msg_emphasis(X):-
  ansi_format([italic], '~a', [X]).



msg_normal(X):-
  ansi_format([], '~a', [X]).



msg_notification(X):-
  ansi_format([bold,fg(yellow)], '~a', [X]).



msg_success(X):-
  ansi_format([bold,fg(green)], '~a', [X]).



%! verbose_call(:Goal_0) is det.

verbose_call(Goal_0):-
  term_to_atom(Goal_0, Msg),
  verbose_call(Msg, Goal_0).


%! verbose_call(+Message:atom, :Goal_0) is det.

verbose_call(Msg, Goal_0):-
  setup_call_catcher_cleanup(
    call_start(Msg),
    Goal_0,
    E,
    (   E == true
    ->  call_success(Msg)
    ;   call_failure(Msg, E)
    )
  ).

call_failure(Msg, E):-
  msg_warning('[FAILURE]'),
  msg_normal(' Process '),
  msg_emphasis(Msg),
  msg_normal(':'),
  nl,
  msg_normal(E).

call_start(Msg):-
  msg_normal('Starting process '),
  msg_emphasis(Msg),
  msg_normal('.'),
  nl.

call_success(Msg):-
  msg_success('[SUCCESS]'),
  msg_normal(' Ending process '),
  msg_emphasis(Msg),
  msg_normal('.'),
  nl.



msg_warning(X):-
  ansi_format([bold,fg(red)], '~a', [X]).
