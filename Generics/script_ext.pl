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
script mode there may be behaviors that require user interaction
blocking the script.

@author Wouter Beek
@version 2012/07, 2013/05
*/

:- use_module(generics(db_ext)).

:- dynamic(script_mode/1).

:- db_add_novel(script_mode(false)).



%% in_script_mode is det.
% Succeeds if script mode is on.

in_script_mode:-
  script_mode(true).

%% start_script_mode is det.
% Starts running in script mode.

start_script_mode:-
  db_replace_novel(script_mode(false), script_mode(true)).

%% stop_script_mode is det.
% Stops running in script mode.

stop_script_mode:-
  db_replace_novel(script_mode(true), script_mode(false)).
