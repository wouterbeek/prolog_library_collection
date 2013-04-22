:- module(
  os_ext,
  [
% DATE & TIME
    current_date/1, % -Date:atom
    current_time/1, % -Time:atom
    date_directories/2, % +Dir:atom
                        % -DateDir:atom
    date_time/1, % -DateTime:term
    hash_date/1, % -Hash:atom
    latest_file/2, % +Files:list(atom)
                   % -File:atom
    posix_date/1, % -Date:atom
    posix_time/1, % -Time:atom

% DELETING FILES
    delete_files/2, % +Directory:atom
                    % +FileType:atom
    delete_files/2, % +Directory:atom
                    % +FileTypes:list(atom)

% HOME DIRECTORIES
    assert_home_directory/0,
    assert_home_subdirectory/1, % +Subdirectory:list(atom)

% OS IDENTIFICATION
    is_mac/0,
    is_unix/0,
    is_windows/0,
    os_dependent_call/1, % :Goal

% MacOS SPECIFIC
    mac_y_pixel/2, % +YPixel:number
                   % -MacYPixel:number
    set_os_flag/0,

% OPENING FILES
    open_dot/1, % +File:file
    open_pdf/1, % +File:file

% SCRIPTS
    run_script/1, % +Script:atom

% SHELL
    shell_status/1, % +Status:integer

% SWI-PROLOG
    check_prolog_version/0,

% TERMINAL EMULATORS %
    ansi_format/4, % +Stream:stream
                   % +Attributes
                   % +Format:atom
                   % Args:list

% WWWW
    open_in_webbrowser/1 % +URI:atom
  ]
).

/** <module> OS extensions

This module contains the OS extensions for SWI-Prolog.

@author Wouter Beek
@version 2011/11-2012/06, 2012/12-2013/02
*/

:- use_module(generics(exception_handling)).
:- use_module(generics(file_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(http/http_header)).
:- use_module(library(www_browser)).

:- multifile(prolog:message/3).

:- meta_predicate os_dependent_call(:).



% DATE & TIME %

%% current_date(-Date:atom) is det.
% Returns an atom representing the current date,
% using the ISO 8601 date format, but with underscored instead of
% dashes.

current_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%Y_%m_%d', TimeStamp).

%% current_time(-Time:atom) is det.
% Returns an atomic representation of the current time.

current_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%H_%M_%S', TimeStamp).

%% date_directories(+Dir:atom, -DateDir:atom) is det.

date_directories(Dir, DateDir):-
  get_time(TimeStamp),
  format_time(atom(Day), '%d', TimeStamp),
  format_time(atom(Month), '%m', TimeStamp),
  format_time(atom(Year), '%Y', TimeStamp),
  RelativeSubDirs1 =.. [Month, Day],
  RelativeSubDirs2 =.. [Year, RelativeSubDirs1],
  RelativeDirs =.. [Dir, RelativeSubDirs2],
  nested_dir_name(RelativeDirs, DateDir).

%% date_time(-DateTime:term) is det.
% Returns a term describing the current date and time.
% This uses the RFC 112 convention for date/time notation.
%
% @compat RFC-1123
% @param DateTime A term representing a date and time.

date_time(DateTime):-
  get_time(TimeStamp),
  http_timestamp(TimeStamp, DateTime).

%% hash_date(-Hash:atom) is det.
% Returns the hash of the current timestamp.
%
% @param Hash An atomic hash.

hash_date(Hash):-
  get_time(TimeStamp),
  term_hash(TimeStamp, Hash).

latest_file([File | Files], LatestFile):-
  time_file(File, CurrentTime),
  latest_file(Files, CurrentTime/File, LatestFile).

latest_file([], _LatestTime/LatestFile, LatestFile).
latest_file([File | Files], CurrentTime/CurrentFile, LatestFile):-
  time_file(File, Time),
  (
    Time > CurrentTime
  ->
    NewCurrentTime = Time,
    NewCurrentFile = File
  ;
    NewCurrentTime = CurrentTime,
    NewCurrentFile = CurrentFile
  ),
  latest_file(Files, NewCurrentTime/NewCurrentFile, LatestFile).

%% posix_date(-Date:atom) is det.
% Returns the current date.
%
% @compat POSIX strftime()
% @param Date A compound term of the form =Year/Month/Day=,
%        where =Year= consists of 4, =Month= consists of 2,
%        and =Day= consists of 2 digits.

posix_date(Date):-
  get_time(TimeStamp),
  format_time(atom(Date), '%F', TimeStamp).

%% posix_time(Time) is det.
% Returns the current time.
%
% @compat POSIX strftime()
% @param Time The atomic default textual representation of a time in PraSem,
%        i.e. =Hour:Minute:Second=.

posix_time(Time):-
  get_time(TimeStamp),
  format_time(atom(Time), '%T', TimeStamp).



% DELETE FILES %

%% delete_files(+Directory:atom, +FileType:atom) is det.
%% delete_files(+Directory:atom, +FileTypes:list(atom)) is det.
% Deletes all file in the given directory that are of the given file type.
%
% @param Directory The atomic absolute name of a directory.
% @param FileType The atomic name of a file type, registered via
%        prolog_file_type/2.

delete_files(Directory, FileType):-
  \+(is_list(FileType)),
  !,
  user:prolog_file_type(Extension, FileType),
  format(atom(RegularExpression), '.*\\.~w', [Extension]),
  path_walk_tree(Directory, RegularExpression, Files),
  % Skip any errors, e.g. permission errors for opened files.
  catch(maplist(delete_file, Files), _Error, true).
delete_files(Directory, FileTypes):-
  maplist(delete_files(Directory), FileTypes).



% HOME DIRECTORIES

assert_home_directory:-
  file_search_path(home, _),
  !.
assert_home_directory:-
  % The personal or user directory is relatively difficult to
  % construe, since I only know how to retrieve the user's home
  % path using the XPCE library.
  new(PCEDirectory, directory('~')),
  get(PCEDirectory, path, HomeDirectory),
  assert(file_search_path(home, HomeDirectory)).

assert_home_subdirectory(Subdirectory):-
  assert_home_directory,
  project_name(Project),
  format(atom(Project0), '.~w', [Project]),
  Subdirectory0 =.. [Project0, Subdirectory],
  nested_dir_name(home(Subdirectory0), _AbsoluteDirectory).



% OS IDENTIFICATION %

%% is_mac is semidet.
% Succeeds if the running OS is Mac OS-X.
%
% This presupposes that set_os_flag/0 has been run, in order to create the
% Prolog flag that is accessed here.

is_mac:-
  current_prolog_flag(mac, true).

%% is_unix is semidet.
% Succeeds if the running OS is Unix.

is_unix:-
  current_prolog_flag(unix, true).

%% is_windows is semidet.
% Succeeds if the running OS is Windows.

is_windows:-
  current_prolog_flag(windows, true).

%% os_dependent_call(:Goal)
% Allows goals to be carried out without the caller having to paying attention
% to OS-specific considerations.
%
% The convention is that one can check for an OS by calling =is_OS=.
%
% The convention is that a predicate =pred= has variants =pred_OS= for
% every supported OS.
%
% The supported operating systems are registered with supported_os/1.

os_dependent_call(Goal):-
  supported_os(OS),
  format(atom(Check), 'is_~w', [OS]),
  call(Check),
  !,
  strip_module(Goal, _Module, Call),
  Call =.. [Pred | Args],
  format(atom(Pred0), '~w_~w', [Pred, OS]),
  Call0 =.. [Pred0 | Args],
  call(Call0).
os_dependent_call(Goal):-
  debug(deb, 'The call ~w is not supported on your OS.', [Goal]).

supported_os(mac).
supported_os(unix).
supported_os(windows).



% MacOS SPECIFIC %

%% mac_y_pixel(+YPixel:integer, -MacYPixel:integer) is det.
% Returns the Mac OS-X equivalent of the y-position of an onscreen pixel.
% Max OS-X uses 21 pixels for the title bar.
%
% @param YPixel The normal y-position of a pixel.
% @param MacYPixel The y-position of a pixel adapted for display
%        on a Mac OS-X system.

mac_y_pixel(YPixel, MacYPixel):-
  MacYPixel is YPixel + 21.

%% set_os_flag is det.
% Distinguish between unices and MacOSX.
% @see Since this is no [[current_prolog_flag/2]] for Mac OS-X yet.

set_os_flag:-
  is_mac,
  !,
  create_prolog_flag(mac, true, [access(read_only), type(boolean)]).
set_os_flag.



% OPENING FILES %

%% open_dot(File) is det.
% Opens the given DOT file.
%
% @param File The atomic name of a DOT file.

open_dot(File):-
  os_dependent_call(open_dot(File)).

open_dot_unix(Base):-
  file_name_extension(Base, gv, File),
  process_create(path(dotty), [File, '&'], []).

%% open_pdf(+File:atom) is det.
% Opens the given PDF file.
%
% @param File The atomic name of a PDF file.

open_pdf(File):-
  os_dependent_call(open_pdf(File)).

:- if(is_unix).
open_pdf_unix(File):-
  process_create(path(xpdf), [File, '&'], [process(PID)]),
  process_wait(PID, exit(ShellStatus)),
  catch(
    shell_status(ShellStatus),
    error(shell_error(FormalMessage), _Context),
    throw(
      error(
        shell_error(FormalMessage),
        context(
          'os_ext:open_pdf_unix/1',
          'Command \'xpdf\' could not be found.'
        )
      )
    )
  ).
:- endif.

:- if(is_windows).
open_pdf_windows(_File).
:- endif.



% SCRIPTS %

%% run_script(+Script:atom) is det.
% Runs the given script.
%
% @param Script The atomic name of script file.

run_script(Script):-
  os_dependent_call(run_script(Script)).

:- if(is_unix).
run_script_unix(Script):-
  process_create(path(Script), [], []).
:- endif.

:- if(is_windows).
run_script_windows(Script):-
  file_name_type(Script, batch, File),
  win_exec(File, normal).
:- endif.



% SHELL %

shell_error(Formal, Context):-
  throw(
    error(
      shell_error(Formal),
      context(shell_status/1, Context)
    )
  ).

%% shell_status(+Status:integer) is det.
% Handling of shell exit status codes.
%
% @throws shell_error Throws a shell error when a shell process exits with
%         a non-zero code.

shell_status(0):-
  !.
shell_status(StatusCode):-
  shell_status(StatusCode, Formal, Context),
  !,
  shell_error(Formal, Context).

shell_status(1, 'Catchall for general errors.', 'Miscellaneous errors.').
shell_status(2, 'Misuse for general errors.',
 'Seldom seen, usually defaults to exit code 1.').
shell_status(126, 'Command cannot be executed.',
  'Permission problem or command is not an executable.').
shell_status(127, 'Command not found.', 'Command could not be found.').
shell_status(128, 'Invalid argument to the exit command.',
  'The exit command takes only integer args in the range 0-255.').
shell_status(130,	'Script terminated by Control-C	', '').

%% windows_shell_command(+Command) is det.
% @tbd Test this.

windows_shell_command(Command):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C', Command],
    [stdin(std), stdout(std), stderr(std)]
  ).

%% windows_shell_command(+Command, +File:atom) is det.
% @tbd Test this.

windows_shell_command(Command, File):-
  getenv('COMSPEC', Shell),
  process_create(
    Shell,
    ['/C', Command, file(File)],
    [stdin(std), stdout(std), stderr(std)]
  ).



% SWI-PROLOG %

%% check_prolog_version is semidet.
% Checks whether a correct version of SWI-Prolog is installed.

check_prolog_version:-
  current_prolog_flag(version, CurrentVersion),
  minimum_prolog_version(MinimumMajor/MinimumMinor/MinimumPatch),
  MinimumVersion is
    (MinimumMajor * 10000) + (MinimumMinor * 100) + (MinimumPatch),
  (
    CurrentVersion >= MinimumVersion
  ->
    true
  ;
    print_message(
      error,
      outdated_version(swipl, CurrentVersion, MinimumVersion)
    ),
    fail
  ).

%% minimum_prolog_version(-Version:compound) is det.
% The minimal SWI-Prolog version that is needed for the features the
% application uses.
%
% During active development, i.e. now, I pay little attention to
% compatibility with older SWI-Prolog versions. I try to run the
% latest development release on Linux systems (currently:
% Arch and Fedora) and Windows systems (currently: 7 and 8)
% (all my systems are 64-bit). I always try to use new SWI-Prolog
% features immediately in order to keep up with recent advances in
% logic programming. I do not try to keep the codebase compatible
% with other Prologs (e.g., Yap), which is a nontrivial chore in the
% absense of broad standards. All this means that the required version
% number is probably set higher than it need be. Thus, feel free to
% lower the number and try PraSem out on an older SWI-Prolog version.
%
% This is currently set to 6.3.10.
% 6.2.6 is the latest stable release.
% 6.3.10 is the latest development release.
% @param The version indicator is of the form =|Major/Minor/Paths|=,
%        with three integers.

minimum_prolog_version(6/3/10).

prolog:message(outdated_version(Component, Current, Minimum)) -->
  [
    ansi([fg(red), intensity(normal)], 'Your version of ', []),
    ansi([bold, fg(red)], '~w', [Component]),
    ansi([fg(red), intensity(normal)], ' is outdated. You are using version ', [])
  ],
  prolog:message(version(Current)),
  [ansi([fg(red), intensity(normal)], ' whereas the minimum required version is ', [])],
  prolog:message(version(Minimum)).



% TERMINAL EMULATORS %

%% ansi_format(+Stream:stream, +Attributes, +Format:atom, +Args:list) is det.
% Like the swipl builtin ansi_format/3, but allows writing to an arbitrary
% output stream.

ansi_format(Stream, Attributes, Format, Args):-
  current_output(MainStream),
  set_output(Stream),
  ansi_format(Attributes, Format, Args),
  flush_output(Stream),
  set_output(MainStream).



% WWW %

%% open_in_webbrowser(+URI:uri) is det.
% Opens the given URI in the default webbrowser.
%
% @param URI A URI.

open_in_webbrowser(URI):-
  catch(
    (
      www_open_url(URI),
      print_message(informational, open_uri(URI))
    ),
    _Error,
    print_message(informational, open_uri(URI))
  ).

:- if(current_module(web_message)).
prolog:message(Message) -->
  {web_message:web_message(Message)}.
:- endif.
prolog:message(open_uri(URI)) -->
  [
    ansi([], 'Opening URI resource ', []),
    ansi([bg(yellow)], '~w', [URI]),
    ansi([], ' in Web browser.', [])
  ].

