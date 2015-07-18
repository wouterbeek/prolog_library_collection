:- module(
  http_server,
  [
    start_server/0,
    start_server/1 % +Options:list(compound)
  ]
).

/** <module> HTTP server

Starting a SWI-Prolog server.


# Developer info

Use the following settings while developing on localhost:

```prolog
:- set_setting_default(http:public_host, localhost).
:- set_setting_default(http:public_port, setting(http:port)).
```

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(error)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_session)). % Session support.
:- use_module(library(http/thread_httpd)).
:- use_module(library(option)).
:- use_module(library(optparse)).
:- use_module(library(settings)).

:- meta_predicate(start_server(+,:,+)).

:- predicate_options(start_server/1, 1, [
     port(+between(1000,9999)),
     pass_to(start_server/3, 3)
   ]).
:- predicate_options(start_server/3, 3, [
     use_existing(+boolean),
     pass_to(http_sever/2, 2)
   ]).

% Define the default application server port in the same way ClioPatria does.
:- if(\+ current_setting(http:port)).
  :- setting(
    http:port,
    nonneg,
    env('PORT',3020),
    'The default port the HTTP server listens to.'
  ).
:- endif.



%! start_server is det.

start_server:-
  start_server([]).

%! start_server(+Options:list(compound)) is det.
%
% @throws instantiation_error If port is not instantiated.
% @throws type_error If port is instantiated to a non-port number.

start_server(Opts1):-
  % Command-line options.
  opt_arguments(
    [
      [default(false),longflags([debug]),opt(debug),type(boolean)],
      [default(3020),longflags([port]),opt(port),type(integer)]
    ],
    Opts2,
    _
  ),
  merge_options(Opts1, Opts2, Opts3),
  
  % Set port option.
  (   select_option(port(Port), Opts3, Opts4)
  ->  true
  ;   setting(http:port, Port),
      Opts4 = Opts3
  ),
  
  start_server(Port, http_dispatch, Opts4).

%! start_server(+Port:integer, :Goal_0, +Options:list(compound)) is det.
% The following options are supported:
%   * use_existing(+UseExisting:boolean)
%     If there is already a server running at the given port,
%     we use that server.
%   * Other options are passed to http_server/2.
%
% @throws instantiation_error If port is not instantiated.
% @throws type_error If port is instantiated to a non-port number.

% Loop the sequence of server port numbers.
start_server(Port, Goal_0, Opts):-
  is_of_type(between(1000, 9999), Port), !,
  start_server(1000, Goal_0, Opts).
% A server is already running at the given port.
start_server(Port, _, Opts):-
  http_server_property(Port, start_time(_)),
  option(use_existing(true), Opts), !,
  print_message(informational, server_using_existing(Port)).
% The current port is already in use, so try another one.
start_server(Port1, Goal_0, Opts):-
  http_server_property(Port1, start_time(_)), !,
  print_message(informational, server_running(Port1)),
  succ(Port1, Port2),
  start_server(Port2, Goal_0, Opts).
% No server is running yet, so start a new server.
start_server(Port, Goal_0, Opts1):-
  % Estimate the number of workes based on the number of CPU cores.
  current_prolog_flag(cpu_count, NCores),
  NWorkers is NCores * 2,

  merge_options(Opts1, [port(Port),workers(NWorkers)], Opts2),
  http_server(Goal_0, Opts2),

  % Make sure the server is shut down whenever SWI-Prolog shuts down.
  at_halt(stop_server(Port)),

  % Info.
  http_log('Starting weblog demo on port ~w~n', [Port]),
  print_message(informational, server_started(Port)).


%! stop_server is det.

stop_server:-
  setting(http:port, Port),
  stop_server(Port).

%! stop_server(+Port:between(1000,9999)) is det.

stop_server(Port):-
  http_server_property(Port, _), !,
  http_stop_server(Port, []),
  print_message(informational, server_stopped(Port)).
stop_server(_).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(server_running(Port,Start)) -->
  ['The server at port ~d is already in use (start time: '-[Port]],
  time_started(Start),
  [').'].
prolog:message(server_started(Port)) -->
  ['The server started on port ~w.'-[Port]].
prolog:message(server_stopped(Port)) -->
  ['The server at port ~d has stopped.'-[Port]].
prolog:message(server_using_existing(Port)) -->
  ['The server at port ~d is reused (start time: '-[Port]],
  time_started(Port),
  [').'].

time_started(Port) -->
  {
    http_server_property(Port, start_time(Start)), !,
    http_timestamp(Start, Text)
  },
  [Text].
