:- module(
  os_ext,
  [
    is_apple/0,
    is_unix/0,
    is_windows/0,
    mac_y_pixel/2, % +YPixel:number
                   % -MacYPixel:number
    os_dependent_call/1 % :Goal
  ]
).

/** <module> OS_EXT

Operating System interactions.

@author Wouter Beek
@version 2011/11-2012/06, 2013/12-2013/02, 2013/05-2013/07
*/

:- use_module(library(debug)).
:- use_module(library(process)).

:- meta_predicate(os_dependent_call(:)).



%! is_apple is semidet.
% Succeeds if the running OS is owned by Apple, i.e. Mac OS-X.

is_apple:-
  current_prolog_flag(apple, true).

%! is_unix is semidet.
% Succeeds if the running OS is Unix.

is_unix:-
  current_prolog_flag(unix, true).

%! is_windows is semidet.
% Succeeds if the running OS is Windows.

is_windows:-
  current_prolog_flag(windows, true).


%! mac_y_pixel(+YPixel:integer, -MacYPixel:integer) is det.
% Returns the Mac OS-X equivalent of the y-position of an onscreen pixel.
%
% Mac OS-X uses 21 pixels for the title bar.
%
% @arg YPixel The normal y-position of a pixel.
% @arg MacYPixel The y-position of a pixel adapted for display
%        on a Mac OS-X system.

mac_y_pixel(YPixel, MacYPixel):-
  MacYPixel is YPixel + 21.


%! os_dependent_call(:Goal)
% Allows goals to be carried out without the caller having to paying
% attention to OS-specific considerations.
%
% The convention is that one can check for an OS by calling =is_OS=.
%
% The convention is that a predicate =pred= has variants =pred_OS= for
% every supported OS.
%
% The supported operating systems are registered with supported_os/1.

os_dependent_call(Goal):-
  strip_module(Goal, Module, PlainGoal),
  supported_os(OS),
  format(atom(Check), 'is_~w', [OS]),
  call(Check), !,
  PlainGoal =.. [Pred | Args],
  format(atom(OS_Pred), '~w_~w', [Pred, OS]),
  OS_Goal =.. [OS_Pred | Args],
  call(Module:OS_Goal).
os_dependent_call(Goal):-
  debug(os_ext, 'The call ~w is not supported on your OS.', [Goal]).


%! supported_os(?OS_Name:oneof([apple,unix,windows])) is nondet.
% The names of supported OS-es.
% The name for Mac-OS is made up since it is not supported by SWI-Prolog.

supported_os(apple).
supported_os(unix).
supported_os(windows).

