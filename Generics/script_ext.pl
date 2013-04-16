:- module(
  script_ext,
  [
    in_script_mode/0,
    start_script_mode/0,
    stop_script_mode/0
  ]
).

/** <module> SCRIPT EXT

Extensions for scripting and running automated scripts.

We need to indicate whether we are in script mode or not, since in
script mode there may be bahviors that require user interaction
blocking the script.

@author Wouter Beek
@version 2012/07
*/

:- dynamic(script_mode/1).

:- assert(script_mode(false)).



%% in_script_mode is det.
% Succeeds if script mode is on.

in_script_mode:-
  script_mode(true).

%% start_script_mode is det.
% Starts running in script mode.

start_script_mode:-
  script_mode(true),
  !.
start_script_mode:-
  retract(script_mode(false)),
  assert(script_mode(true)).

%% stop_script_mode is det.
% Stops running in script mode.

stop_script_mode:-
  script_mode(false),
  !.
stop_script_mode:-
  retract(script_mode(true)),
  assert(script_mode(false)).
