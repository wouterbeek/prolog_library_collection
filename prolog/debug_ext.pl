:- module(
  debug_ext,
  [
    call_collect_messages/1, % :Goal_0
    call_collect_messages/3, % :Goal_0
                             % -Status:compound
                             % -Messages:list(compound)
    debug_all_files/0,
    if_debug/2, % ?Flag:compound
                % :Goal_0
    msg_emphasis/1, % +Format:string
    msg_emphasis/2, % +Format:string
                    % +Arguments:list
    msg_error/1, % +Error:compound
    msg_normal/1, % +Format:string
    msg_normal/2, % +Format:string
                  % +Arguments:list
    msg_notification/1, % +Format:string
    msg_notification/2, % +Format:string
                        % +Arguments:list
    msg_success/1, % +Format:string
    msg_success/2, % +Format:string
                   % +Arguments:list
    msg_warning/1, % +Format:string
    msg_warning/2, % +Format:string
                   % +Arguments:list
    verbose/1, % :Goal_0
    verbose/2, % ?Flag:compound
               % :Goal_0
    verbose/3, % ?Flag:compound
               % :Goal_0
               % +Format:string
    verbose/4 % ?Flag:compound
              % :Goal_0
              % +Format:string
              % +Arguments:list
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
:- use_module(library(default)).
:- use_module(library(http/http_deb)).
:- use_module(library(lists)).
:- use_module(library(os/dir_ext)).
:- use_module(library(portray_text)).

:- meta_predicate(call_collect_messages(0)).
:- meta_predicate(call_collect_messages(0,-,-)).
:- meta_predicate(if_debug(?,0)).
:- meta_predicate(verbose(0)).
:- meta_predicate(verbose(?,0)).
:- meta_predicate(verbose(?,0,+)).
:- meta_predicate(verbose(?,0,+,+)).

:- debug(debug_ext).





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
  debugging(debug_ext), !,
  forall(
    member(Warning, Messages),
    debug(debug_ext, "[WARNING] ~a", [Warning])
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



%! if_debug(?Flag:compound, :Goal_0) is det.
% Calls the given goal only if the given flag is an active debugging topic.
% Succeeds if Flag is uninstantiated.
%
% @see library(debug)

if_debug(Flag, _):-
  var(Flag), !.
if_debug(Flag, _):-
  \+ debugging(Flag), !.
if_debug(_, Goal_0):-
  call(Goal_0).



%! msg_emphasis(+Format:string) is det.
% Wrapper around msg_emphasis/2 with no arguments.

msg_emphasis(Format):-
  msg_emphasis(Format, []).

%! msg_emphasis(+Format:string, +Arguments:list) is det.
% Prints an emphasized message, using ANSI properties.

msg_emphasis(Format, Args):-
  ansi_format([italic], Format, Args).



%! msg_error(+Error:compound) is det.
% Prints an error compound term using a grammar for errors.

msg_error(E):-
  atom_phrase(msg_error(E), A),
  msg_warning(A).


%! msg_normal(+Format:string) is det.
% Wrapper around msg_normal/2 with no arguments.

msg_normal(Format):-
  msg_normal(Format, []).

%! msg_normal(+Format:string, +Arguments:list) is det.
% Prints a normal messages, using no ANSI properties.

msg_normal(Format, Args):-
  ansi_format([], Format, Args).



%! msg_notfication(+Format:string) is det.
% Wrapper around msg_notification/2 with no arguments.

msg_notification(Format):-
  msg_notification(Format, []).

%! msg_notification(+Format:string, +Arguments:list) is det.
% Prints a notification message, using ANSI properties.

msg_notification(Format, Args):-
  ansi_format([bold,fg(yellow)], Format, Args).



%! msg_success(+Format:string) is det.
% Wrapper around msg_success/2 with no arguments.

msg_success(Format):-
  msg_success(Format, []).

%! msg_success(+Format:string, +Arguments:list) is det.
% Prints a success message, using ANSI properties.

msg_success(Format, Args):-
  ansi_format([bold,fg(green)], Format, Args).



%! msg_warning(+Format:string) is det.
% Wrapper around msg_warning/2 with no arguments.

msg_warning(Format):-
  msg_warning(Format, []).

%! msg_warning(+Format:string, +Arguments:list) is det.
% Prints a warnings message, using ANSI properties.

msg_warning(Format, Args):-
  ansi_format([bold,fg(red)], Format, Args).



%! verbose(:Goal_0) is det.
% Wrapper around verbose/2.

verbose(Goal_0):-
  verbose(_, Goal_0).

%! verbose(?Frag:compound, :Goal_0) is det.
% Wrapper around verbose/3.

verbose(Flag, Goal_0):-
  term_string(Goal_0, Format),
  verbose(Flag, Goal_0, Format).

%! verbose(+Flag:compound, :Goal_0, +Format:string) is det.
% Wrapper around verbose/4.

verbose(Flag, Goal_0, Format):-
  verbose(Flag, Goal_0, Format, []).

%! verbose(?Flag:compound, :Goal_0, +Format:string, +Arguments:list) is det.
% Verbose call of Goal_0.
%
% If Flag is instantiated then messages are only displayed if the
% flag is a currently active debug flag.
% Otherwise, i.e., if Flag is uninstantiated, all messages are displayed.

verbose(Flag, Goal_0, Format, Args):-
  defval(debug_ext, Flag),
  if_debug(Flag, ansi_format([], Format, Args)),
  (   catch(Goal_0, Error, true)
  ->  if_debug(Flag,
        (   var(Error)
        ->  msg_success("~`.t success~72|")
        ;   message_to_string(Error, String),
            msg_warning("~`.t ERROR: ~w~72|", [String])
        )
      )
  ;   if_debug(Flag, msg_warning("~`.t ERROR: (failed)~72|"))
  ).





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
