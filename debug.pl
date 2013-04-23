% Debug script for PGC.

% Do not write module loads to the standard output stream.
:- set_prolog_flag(verbose_load, silent).

% Before doing much else, we start the documentation server that
% generates Web sites based on the plDoc commenting in the swipl code files.
%:- use_module(library(pldoc)).
%:- doc_server(2222, [edit(true)]).
%:- use_module(library(http/http_path)).
%http:location(pldoc, root(help), [priority(1000)]).
%:- http_handler(pldoc, documentation, []).

% This library allows for exploiting the color and attribute facilities
% of most modern terminals using ANSI escape sequences.
% The Windows console (swipl-win) does not (yet) support ANSI (color)
% codes.
:- use_module(library(ansi_term)).

  % Write lists of ASCII numbers as strings to the terminal.
:- use_module(library(portray_text)).
:- portray_text(true).

% Enforce more stringent style checking.
:- style_check(+string).
:- style_check(+charset).

% Set the swipl terminal state via PCE.
% When swipl is started from within a terminal this does not change
% anything, so this setting applies e.g. to contexts where PraSem
% would be loaded by a shortcut on the Desktop.
%:- ignore(send(@pce, show_console, iconic)).
:- ignore(send(@pce, show_console, open)).

% Debug monitor.
% [TODO] The PCE-based debug monitor in swipl is not the most versatile
% debug tool in existence. I would like to write a Web-based version at
% some point.
%:- use_module(library(swi_ide)).
%:- prolog_ide(debug_monitor).

:- [load].

