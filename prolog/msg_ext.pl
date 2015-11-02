:- module(
  msg_ext,
  [
    msg_emphasis/1, % +Format:string
    msg_emphasis/2, % +Format:string
                    % +Arguments:list
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
    verbose/2, % :Goal_0
               % +Format:string
    verbose/3 % :Goal_0
              % +Format:string
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
:- use_module(library(debug_ext)).
:- use_module(library(http/http_deb)).

:- meta_predicate(verbose(0)).
:- meta_predicate(verbose(0,+)).
:- meta_predicate(verbose(0,+,+)).

:- multifile(user:message_hook/3).





%! msg_emphasis(+Format:string) is det.
% Wrapper around msg_emphasis/2 with no arguments.

msg_emphasis(Format):-
  msg_emphasis(Format, []).


%! msg_emphasis(+Format:string, +Arguments:list) is det.
% Prints an emphasized message, using ANSI properties.

msg_emphasis(Format, Args):-
  ansi_format([italic], Format, Args).



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
  term_string(Goal_0, Format),
  verbose(Goal_0, Format).


%! verbose(:Goal_0, +Format:string) is det.
% Wrapper around verbose/3.

verbose(Goal_0, Format):-
  verbose(Goal_0, Format, []).


%! verbose(:Goal_0, +Format:string, +Arguments:list) is det.
% Verbose call of Goal_0.
%
% If Flag is instantiated then messages are only displayed if the
% flag is a currently active debug flag.
% Otherwise, i.e., if Flag is uninstantiated, all messages are displayed.

verbose(Goal_0, Format, Args):-
  get_time(Start),
  msg_normal(Format, Args),
  (   catch(Goal_0, Error, true)
  ->  (   var(Error)
      ->  get_time(End),
          Delta is End - Start,
          msg_success("~`.t success (~2f sec.)~72|~n", [Delta])
      ;   message_to_string(Error, String),
          msg_warning("~`.t ERROR: ~w~72|~n", [String])
      )
  ;   msg_warning("~`.t ERROR: (failed)~72|~n")
  ).
