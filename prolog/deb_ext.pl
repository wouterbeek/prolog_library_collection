:- module(
  deb_ext,
  [
    call_collect_messages/1, % :Goal_0
    call_collect_messages/3, % :Goal_0
                             % -Status:compound
                             % -Messages:list(compound)
    debug_all_files/0,
    if_debug/2, % +Flag:compound
                % :Goal_0
    msg_emphasis/1, % +Message:atom
    msg_error/1, % +Error:compound
    msg_normal/1, % +Message:atom
    msg_notification/1, % +Message:atom
    msg_success/1, % +Message:atom
    msg_warning/1, % +Message:atom
    verbose_call/1, % :Goal_0
    verbose_call/2 % +Message:atom
                   % :Goal_0
  ]
).
:- reexport(library(debug)).

/** <module> Debug extensions

Tools that ease debugging SWI-Prolog programs.

@author Wouter Beek
@version 2015/07-2015/10
*/

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(check_installation)). % Private predicates.
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(debug)).
:- use_module(library(http/http_deb)).
:- use_module(library(lists)).
:- use_module(library(os/dir_ext)).
:- use_module(library(portray_text)).

:- meta_predicate(call_collect_messages(0)).
:- meta_predicate(call_collect_messages(0,-,-)).
:- meta_predicate(if_debug(+,0)).
:- meta_predicate(verbose_call(0)).
:- meta_predicate(verbose_call(+,0)).

:- dynamic(user:debug_mode).
:- multifile(user:debug_mode).

:- initialization(init_debug_mode).

init_debug_mode:-
  % Set the debug mode flag.
  (user:debug_mode -> true ; assert(user:debug_mode)).





%! call_collect_messages(:Goal_0) is det.

call_collect_messages(Goal_0):-
  call_collect_messages(Goal_0, Status, Messages),
  process_warnings(Messages),
  process_status(Status).

process_status(true):- !.
process_status(fail):- !,
  fail.
process_status(Exception):-
  print_message(warning, Exception).

process_warnings(Messages):-
  debugging(deb_ext), !,
  forall(
    member(Warning, Messages),
    debug(deb_ext, '[WARNING] ~a', [Warning])
  ).
process_warnings(Messages):-
  length(Messages, N),
  (   N =:= 0
  ->  true
  ;   print_message(warnings, number_of_warnings(N))
  ).

%! call_collect_messages(
%!   :Goal_0,
%!   -Status:compound,
%!   -Messages:list(compound)
%! ) is det.

call_collect_messages(Goal_0, Status, Messages):-
  check_installation:run_collect_messages(Goal_0, Status, Messages).



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



%! if_debug(+Flag:compound, :Goal_0) is det.
% Calls the given goal only if the given flag is an active debugging topic.
%
% @see library(debug)

if_debug(Flag, _):-
  \+ debugging(Flag), !.
if_debug(_, Goal_0):-
  call(Goal_0).



msg_emphasis(X):-
  ansi_format([italic], '~a', [X]).



%! msg_error(+Error:compound) is det.
% Prints an error message to standard output.

msg_error(E):-
  atom_phrase(msg_error(E), A),
  msg_warning(A).


msg_normal(X):-
  ansi_format([], '~a', [X]).



msg_notification(X):-
  ansi_format([bold,fg(yellow)], '~a', [X]).



msg_success(X):-
  ansi_format([bold,fg(green)], '~a', [X]).



msg_warning(X):-
  ansi_format([bold,fg(red)], '~a', [X]).


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

%! msg_error(+Error:compound)// is det.

msg_error(exception(E)) --> !,
  msg_error(E).
msg_error(error(socket_error(Msg),_)) --> !,
  "Socket error: ",
  atom(Msg).
msg_error(error(permission_error(url,Iri),context(_,status(Code,_)))) --> !,
  "No permission to download from IRI ",
  iri(Iri),
  ". ",
  http_status_code(Code).

:- multifile(prolog:message//1).

prolog:message(loading_module(File)) -->
  ['[M] ',File].

prolog:message(number_of_warnings(N)) -->
  ['~D warnings'-[N]].
