:- module(
  tts,
  [
    tts/1 % +Message:string
  ]
).

/** <module> Text-to-Speech (TTS)

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(error)).
:- use_module(library(os/external_program)).
:- use_module(library(os/process_ext)).

:- dynamic(user:module_uses/2).
:- multifile(user:module_uses/2).

user:module_uses(tts, program(espeak)).





%! tts(+Message:string) is det.
% Uses the external program /espeak/ in order to turn the given
% text message into speech.
%
% @throws existence_error if Program does not exist.

tts(Msg):-
  tts(Msg, espeak).


%! tts(+Message:string, +Program:atom) is det.
% @throws existence_error if Program does not exist.

tts(Msg, Prog):-
  exists_program(Prog), !,
  run_process(Prog, ['--',Msg], [detached(false),program(Prog)]).
tts(_, Prog):-
  existence_error(program, Prog).
