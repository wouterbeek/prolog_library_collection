:- module(
  deb_ext,
  [
    debug_all_files/0,
    if_debug/2, % +Flag:atom
                % :Goal
    msg_emphasis/1, % +Message:atom
    msg_normal/1, % +Message:atom
    msg_notification/1, % +Message:atom
    msg_success/1, % +Message:atom
    msg_warning/1, % +Message:atom
    start_pldoc_server/0,
    verbose_call/1, % :Goal_0
    verbose_call/2 % +Message:atom
                    % :Goal_0
  ]
).
:- reexport(library(debug)).

/** <module> Debug extensions

Tools that ease debugging SWI-Prolog programs.

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(gui_tracer)).
:- use_module(library(os/dir_ext)).
:- use_module(library(pldoc)).
:- use_module(library(portray_text)).

:- meta_predicate(if_debug(+,0)).
:- meta_predicate(verbose_call(0)).
:- meta_predicate(verbose_call(+,0)).

:- set_prolog_flag(
  answer_write_options,
  [max_depth(100),portrayed(true),spacing(next_argument)]
).
:- set_prolog_flag(
  debugger_write_options,
  [max_depth(100),portrayed(true),spacing(next_argument)]
).

:- portray_text(true).
:- set_portray_text(ellipsis, 1000).

:- dynamic(user:debug_mode).
:- multifile(user:debug_mode).

:- initialization(init_debug_mode).

init_debug_mode:-
  % Set the debug mode flag.
  (   user:debug_mode
  ->  true
  ;   assert(user:debug_mode)
  ),
  % Avoid errors when using gtrace/0 in threads.
  guitracer.





%! debug_all_files is det.
% Loads all subdirectories and Prolog files contained in those directories.

debug_all_files:-
  absolute_file_name(project(.), Dir, [access(read),file_type(directory)]),
  directory_files(
    Dir,
    Files1,
    [
      file_types([prolog]),
      include_directories(false),
      include_self(false),
      recursive(true)
    ]
  ),
  exclude(do_not_load, Files1, Files2),
  maplist(use_module0, Files2).
use_module0(File):-
  print_message(informational, loading_module(File)),
  use_module(File).

do_not_load(File1):-
  file_base_name(File1, File2),
  file_name_extension(File3, pl, File2),
  do_not_load0(File3).

do_not_load0(dcg_ascii).
do_not_load0(dcg_unicode).



%! if_debug(+Flag:atom, :Goal) .
% Calls the given goal only if the given flag is an active debugging topic.
%
% @see library(debug)

if_debug(Flag, _Goal):-
  \+ debugging(Flag), !.
if_debug(_Flag, Goal):-
  call(Goal).



msg_emphasis(X):-
  ansi_format([italic], '~a', [X]).



msg_normal(X):-
  ansi_format([], '~a', [X]).



msg_notification(X):-
  ansi_format([bold,fg(yellow)], '~a', [X]).



msg_success(X):-
  ansi_format([bold,fg(green)], '~a', [X]).



msg_warning(X):-
  ansi_format([bold,fg(red)], '~a', [X]).



%! start_pldoc_server is det.
% The plDoc server should be started *before* documented modules are loaded.

start_pldoc_server:-
  doc_server(9999).



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
    (   E == exit
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
  msg_normal(.),
  nl.

call_success(Msg):-
  msg_success('[SUCCESS]'),
  msg_normal(' Ending process '),
  msg_emphasis(Msg),
  msg_normal(.),
  nl.





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(loading_module(File)) -->
  ['[M] ',File].
