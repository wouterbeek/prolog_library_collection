:- module(
  web_message,
  [
    log_web/1, % -Markup:list
    web_message/1 % +Term
  ]
).

/** <module> Web message

Acts on messages printed by print_message/2.

@author Wouter Beek
@version 2013/02, 2013/04
*/

:- use_module(generics(file_ext)).
:- use_module(html(html)).
:- use_module(library(http/http_open)).
:- use_module(server(error_web)).
:- use_module(server(wallace)).

:- dynamic(current_log_row/1).



log_web([HTML_Table]):-
  current_log_file(File),
  csv_read_file(File, Rows, [arity(4), functor(row)]),
  findall(
    [Situation, DateTime, Category, Message],
    member(row(Situation, DateTime, Category, Message), Rows),
    TRs
  ),
  list_to_table(
    [header(true)],
    [['Situation', 'DateTime', 'Category', 'Message'] | TRs],
    HTML_Table
  ).

prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
  error_web(Exception, Markup),
  push(status_pane, html, wallace, Markup),
  !.
prolog:debug_print_hook(_Type, 'EXCEPTION', [Exception]):-
  !,
  gtrace, %DEB
  format(user, '~w', [Exception]). %DEB
prolog:debug_print_hook(Type, Format, Arguments):-
  format(atom(Message), Format, Arguments),
  push(
    status_pane,
    html,
    wallace,
    [element(p, [], ['[', Type, ']', ' ', Message])]
  ),
  append_to_log(Type, Format, Arguments).

web_message(open_uri(_URI)):-
  format(user, 'YES!', []),
  wallace_uri(URI),
  http_open(
    URI,
    _Stream,
    [
      request_header(msg=test),
      request_header('Content-Type'='application/x-www-form-urlencoded')
    ]
  ).

