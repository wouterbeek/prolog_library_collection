:- module(
  logging,
  [
    append_to_log/1, % +Format:atom
    append_to_log/2, % +Format:atom
                     % +Arguments:list(term)
    append_to_log/3, % +Category:atom
                     % +Format:atom
                     % +Arguments:list(term)
    close_log_stream/1, % +Stream:atom
    create_log_file/2, % -File:atom
                       % -Stream:atom
    create_log_file/3, % +Situation:atom
                       % -File:atom
                       % -Stream:atom
    current_log_file/1, % ?File:file
    current_log_stream/1, % ?Stream:stream
    disable_log_mode/0,
    enable_log_mode/0,
    end_log/0,
    send_log/1, % +File:atom
    set_current_log_file/1, % ?File:atom
    set_current_log_stream/1, % ?Stream:stream
    set_situation/1, % +Situation:atom
    situation/1, % ?Situation:atom
    start_log/0
  ]
).

/** <module> Logging

Methods for logging.

@author Wouter Beek
@author Sander Latour
@version 2012/05-2012/07, 2013/03-2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(os_ext)).
:- use_module(library(http/http_client)).

:- multifile(prolog:message/1).

:- dynamic(current_log_file/1).
:- dynamic(current_log_stream/1).
:- dynamic(log_mode/1).
:- db_add_novel(log_mode(false)).
:- dynamic(situation0/1).

:- db_add_novel(user:prolog_file_type(log, log)).

init:-
  file_search_path(log, _Directory),
  !.
init:-
  % @tbd This fails on Windows because absolute_file_path/3 gives unexpected
  %      results. Maybe a bug in swipl.
  assert_home_subdirectory(log),
  project_name(Project),
  format(atom(Project0), '.~w', [Project]),
  assert(user:file_search_path(personal, home(Project0))),
  assert(user:file_search_path(log, personal(log))).
:- init. %TBD



%! append_to_log(+Format:atom) is det.
% Logs the given message in the current log file.
%
% @arg Format An atomic message.
% @see Like format/1.

append_to_log(Format):-
  append_to_log(Format, []).

%! append_to_log(+Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file.
%
% @arg Format An atomic message.
% @arg Arguments A list of terms.
% @see Like format/2.

append_to_log(Format, Arguments):-
  append_to_log(generic, Format, Arguments).

%! append_to_log(+Category:atom, +Format:atom, +Arguments:list(term)) is det.
% Logs the given message in the current log file under the given category.
%
% @arg Category An atom.
% @arg Format An atomic message.
% @arg Arguments A list of terms.

append_to_log(Category, Format, Arguments):-
  format(atom(Message), Format, Arguments),
  append_to_log_(Category, Message).

append_to_log_(_Category, _Message):-
  log_mode(false),
  !.
append_to_log_(Category, Message):-
  current_log_stream(Stream),
  !,
  date_time(DateTime),
  situation(Situation),
  csv_write_stream(
    Stream,
    [row(Situation, DateTime, Category, Message)],
    [file_type(comma_separated_values)]
  ),
  flush_output(Stream).
append_to_log_(Kind, Message):-
  print_message(error, cannot_log(Kind, Message)).
prolog:message(cannot_log(Kind, Message)):-
  [
    ansi([bold], '[~w] ', [Kind]),
    ansi([], 'Could not log message "', []),
    ansi([faint], '~w', [Message]),
    ansi([], '".', [])
  ].

%! close_log_stream(+Stream) is det.
% Closes the given log stream.
%
% @arg Stream A stream.

close_log_stream(Stream):-
  flush_output(Stream),
  close(Stream).

%! create_log_file(-File:atom, -Stream) is det.
% Creates a log file in the log file directory and returns the absolute
% path of that file as well as its stream name.
%
% @arg File The atomic name of a file's path.
% @arg Stream The atomic name of a file's stream.

create_log_file(File, Stream):-
  situation(Situation),
  create_log_file(Situation, File, Stream).

%! create_log_file(+Situation:atom, -File:atom, -Stream:atom) is det.
% Creates a log file in the log file directory and returns the absolute
% path of that file as well as its stream.
%
% @arg Situation An atomic descriptor of a logging situation.
% @arg File The atomic name of a file's path.
% @arg Stream The atomic name of a file's stream.

create_log_file(Situation, AbsoluteFile, Stream):-
  absolute_file_name(log(Situation), Dir),
  date_directories(Dir, LogDir),
  current_time(FileName),
  create_file(LogDir, FileName, log, AbsoluteFile),
  open(AbsoluteFile, write, Stream, [close_on_abort(true), type(text)]).

disable_log_mode:-
  log_mode(false),
  !.
disable_log_mode:-
  db_replace(log_mode(true), log_mode(false)).

enable_log_mode:-
  log_mode(true),
  !.
enable_log_mode:-
  db_replace(log_mode(false), log_mode(true)).

%! end_log is det.
% Ends the current logging activity.

end_log:-
  log_mode(false),
  !.
end_log:-
  append_to_log(build, 'Goodnight!', []),
  current_log_file(File),
  (
    send_log(File)
  ;
    true
  ),
  current_log_stream(Stream),
  close_log_stream(Stream).

%! send_log(File) is det.
% Sends the log that is stored in the given file to the logging server.
%
% @arg File The atomic name of a file.

send_log(File):-
  open(File, read, Stream, []),
  read_stream_to_codes(Stream, Codes),
  file_base_name(File, Base),
  format(atom(URL), 'http://www.wouterbeek.com/post.php?filename=~w', [Base]),
  catch(
    http_post(URL, codes('text/xml;charset=utf-8', Codes), Reply, []),
    _Error,
    fail
  ),
  send(@pce, write_ln, Reply).

%! set_current_log_file(+File:atom) is det.
% Sets the current file where logging messages are stored to.
%
% @arg File The atomic name of a file.

set_current_log_file(File):-
  retractall(current_log_file(_File)),
  assert(current_log_file(File)).

%! set_current_log_stream(+Stream) is det.
% Sets the current stream where logging messages are written to.
%
% @arg Stream A stream.

set_current_log_stream(Stream):-
  retractall(current_log_stream(_Stream)),
  assert(current_log_stream(Stream)).

% Already set.
set_situation(Situation):-
  situation0(Situation),
  !,
  fail.
% Set for the first (any only) time.
set_situation(Situation):-
  assert(situation0(Situation)).

situation(Situation):-
  situation0(Situation),
  !.
situation(no_situation).

%! start_log is det.
% Starts logging.
% This does nothing in case log mode is turned off.

start_log:-
  log_mode(false),
  !.
start_log:-
  create_log_file(File, Stream),
  set_current_log_file(File),
  set_current_log_stream(Stream),
  append_to_log(build, 'Goodday!', []).
