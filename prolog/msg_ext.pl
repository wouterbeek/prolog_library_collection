:- module(
  msg_ext,
  [
    msg_emphasis/1,     % +Format
    msg_emphasis/2,     % +Format, +Args
    msg_normal/1,       % +Format
    msg_normal/2,       % +Format, +Args
    msg_notification/1, % +Format
    msg_notification/2, % +Format, +Args
    msg_success/1,      % +Format
    msg_success/2,      % +Format, +Args
    msg_warning/1,      % +Format
    msg_warning/2,      % +Format, +Args
    verbose/1,          % :Goal_0
    verbose/2,          % :Goal_0, +Format
    verbose/3           % :Goal_0, +Format, +Args
  ]
).

/** <module> Message extensions

Prints messages for the user.

@author Wouter Beek
@version 2015/10-2015/11, 2016/01-2016/02
*/

:- use_module(library(ansi_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_ext)).

:- meta_predicate
    verbose(0),
    verbose(0,+),
    verbose(0,+,+),
    verbose0(0),
    verbose0(0,+),
    verbose0(0,+,+).

:- multifile
    user:message_hook/3.





%! msg_emphasis(+Format) is det.
% Wrapper around msg_emphasis/2 with no arguments.

msg_emphasis(Format):-
  msg_emphasis(Format, []).


%! msg_emphasis(+Format, +Args) is det.
% Prints an emphasized message, using ANSI properties.

msg_emphasis(Format, Args):-
  ansi_format(user_outut, [italic], Format, Args).



%! msg_normal(+Format) is det.
% Wrapper around msg_normal/2 with no arguments.

msg_normal(Format):-
  msg_normal(Format, []).


%! msg_normal(+Format, +Args) is det.
% Prints a normal messages, using no ANSI properties.

msg_normal(Format, Args):-
  ansi_format(user_output, [], Format, Args).



%! msg_notfication(+Format) is det.
% Wrapper around msg_notification/2 with no arguments.

msg_notification(Format):-
  msg_notification(Format, []).


%! msg_notification(+Format, +Args) is det.
% Prints a notification message, using ANSI properties.

msg_notification(Format, Args):-
  ansi_format(user_output, [bold,fg(yellow)], Format, Args).



%! msg_success(+Format) is det.
% Wrapper around msg_success/2 with no arguments.

msg_success(Format):-
  msg_success(Format, []).


%! msg_success(+Format, +Args) is det.
% Prints a success message, using ANSI properties.

msg_success(Format, Args):-
  ansi_format(user_output, [bold,fg(green)], Format, Args).



%! msg_warning(+Format) is det.
% Wrapper around msg_warning/2 with no arguments.

msg_warning(Format):-
  msg_warning(Format, []).


%! msg_warning(+Format, +Args) is det.
% Prints a warnings message, using ANSI properties.

msg_warning(Format, Args):-
  ansi_format(user_error, [bold,fg(red)], Format, Args).



%! verbose(:Goal_0) is det.
% Wrapper around verbose/2.

verbose(Goal_0):-
  verbose0(Goal_0),
  nl.


%! verbose(:Goal_0, +Format) is det.
% Wrapper around verbose/3.

verbose(Goal_0, Format):-
  verbose0(Goal_0, Format),
  nl.


%! verbose(:Goal_0, +Format, +Args) is det.
% Verbose call of Goal_0.
%
% If Flag is instantiated then messages are only displayed if the
% flag is a currently active debug flag.
% Otherwise, i.e., if Flag is uninstantiated, all messages are displayed.

verbose(Goal_0, Format, Args):-
  verbose0(Goal_0, Format, Args),
  nl.


verbose0(Goal_0):-
  term_string(Goal_0, Format),
  verbose0(Goal_0, Format).


verbose0(Goal_0, Format):-
  verbose0(Goal_0, Format, []).


verbose0(Goal_0, Format, Args):-
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
