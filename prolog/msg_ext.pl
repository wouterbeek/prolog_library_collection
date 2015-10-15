:- module(
  msg_ext,
  [
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
    msg_warning/2 % +Format:string
                  % +Arguments:list
  ]
).

/** <module> Message extensions

Prints messages for the user.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(ansi_term)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(http/http_deb)).





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
  string_phrase(msg_error(E), S),
  msg_warning(S).



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





% MESSAGES %

%! msg_error(+Error:compound)// is det.

msg_error(exception(E)) --> !,
  msg_error(E).
msg_error(error(existence_error(procedure,Pred),context(CallingContext,_))) --> !,
  "Predicate ",
  predicate(Pred),
  " does not exist within calling context ",
  predicate(CallingContext),
  ".".
msg_error(error(socket_error(Msg),_)) --> !,
  "Socket error: ",
  atom(Msg).
msg_error(error(permission_error(url,Iri),context(_,status(Code,_)))) --> !,
  "No permission to download from IRI ",
  iri(Iri),
  ". ",
  http_status_code(Code).
msg_error(E) -->
  {gtrace}, %DEB
  msg_error(E).

predicate(Mod:PredLet/Arity) -->
  atom(Mod),
  ":",
  atom(PredLet),
  "/",
  integer(Arity).
