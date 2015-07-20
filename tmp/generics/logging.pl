:- module(
  logging,
  [
    read_log_entry/5, % ?Category:atom
                      % ?DateTime:atom
                      % ?Kind:atom
                      % ?Term:compound
                      % ?Message:atom
    write_log_entry/4 % ?Category:atom
                      % ?Kind:atom
                      % ?Term:compound
                      % ?Message:atom
  ]
).

/** <module> Logging

Logging infrastructure.

@author Wouter Beek
@version 2015/03, 2015/06
*/

%:- use_module(library(debug)).
:- use_module(library(persistency)).

:- use_module(plc(os/date_ext)).

:- persistent(
  log_entry(
    category:atom,
    datetime:atom,
    kind:atom,
    term:compound,
    message:atom
  )
).

:- dynamic(prolog:debug_print_hook/3).
:- multifile(prolog:debug_print_hook/3).

:- dynamic(logging:message_kind/1).
:- multifile(logging:message_kind/1).

logging:message_kind(error).
logging:message_kind(warning).

:- dynamic(user:message_hook/3).
:- multifile(user:message_hook/3).

:- initialization(init).

init:-
  absolute_file_name(data('error.log'), File, [access(write)]),
  db_attach(File, []).





%! read_log_entry(
%!   ?Category:atom,
%!   ?DateTime:atom,
%!   ?Kind:atom,
%!   ?Term:compound,
%!   ?Message:atom
%! ) is nondet.

read_log_entry(Category, DateTime, Kind, Term, Msg):-
  log_entry(Category, DateTime, Kind, Term, Msg).



%! write_log_entry(
%!   ?Category:atom,
%!   ?Kind:atom,
%!   ?Term:compound,
%!   ?Message:atom
%! ) is det.

write_log_entry(Cat, Kind, Term, Msg):-
  get_date(Date0),
  iso8601_date(Date0, Date),
  assert_log_entry(Cat, Date, Kind, Term, Msg).





% HOOKS %

prolog:debug_print_hook(Topic, Format, Args):-
  get_date(Date0),
  iso8601_date(Date0, Date),
  format(atom(Msg), Format, Args),
  format(user_output, '~a~n', [Msg]),
  assert_log_entry(Topic, Date, informational, debug(Format,Args), Msg).



user:message_hook(Term, Kind, Lines):-
  once(logging:message_kind(Kind)),
  print_message_lines(atom(Msg), '', Lines),
  format(user_error, '~a~n', [Msg]),
  get_date(Date0),
  iso8601_date(Date0, Date),
  assert_log_entry(unknown, Date, Kind, Term, Msg).
