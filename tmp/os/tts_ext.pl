:- module(
  tts_ext,
  [
    text_to_speech/1 % +Input:or([atom,string])
  ]
).

/** <module> TTS

Text-to-speech predicates.

@author Wouter Beek
@version 2013/06-2013/07, 2015/03
*/

:- use_module(library(process)).

:- use_module(plc(os/os_ext)).
:- use_module(plc(process/process_ext)).

:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).

user:module_uses(tts_ext, program(espeak)).





%! text_to_speech(+Input:or([atom,string])) is det.
% Creates a process for a program that can turn text into speech.
%
% This is currently implemented for Unix systems with espeak in the PATH.
%
% @tbd Add support for Windows.
% @tbd Test support for Mac.

text_to_speech(Input):-
  os_dependent_call(text_to_speech(Input)).

text_to_speech_unix(Input):-
  handle_process(espeak, ['--',Input], [detached(false),program(espeak)]).
