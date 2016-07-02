:- module(
  detect_language,
  [
    detect_language/2, % +Txt, -Lang
    detect_languages/2 % +Txt, -Langs:list(pair(between(0.0,1.0)),atom)
  ]
).

/** <module> Language detection

Detect the natural language of a given piece of text.

This module is a wrapper around the external library
[**language-detection**](https://code.google.com/p/language-detection/).

@author Wouter Beek
@version 2015/08, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_file)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(os/process_ext)).
:- use_module(library(yall)).





%! detect_language(+Txt, -Lang) is det.
%! detect_languages(+Txt, -Langs:list(pair(between(0.0,1.0)),atom)) is det.

detect_language(Txt, Lang):-
  detect_languages(Txt, Langs),
  last(Langs, _-Lang).


detect_languages(Txt, Langs):-
  absolute_file_name(lang, File0, [access(write)]),
  thread_file(File0, File),
  absolute_file_name(
    'lib/langdetect-09-13-2011/lib/langdetect',
    Jar,
    [access(read),extensions([jar])]
  ),
  absolute_file_name(
    'lib/langdetect-09-13-2011/profiles',
    Dir,
    [access(read),file_type(directory)]
  ),
  setup_call_cleanup(
    call_to_stream(File, {Txt}/[Out]>>write(Out, Txt)),
    run_jar(
      Jar,
      ['--detectlang','-d',file(Dir),file(File)],
      [output_goal({Cs}/[Out]>>read_stream_to_codes(Out, Cs))]
    ),
    delete_file(File)
  ),
  once(phrase(detect_languages(Langs), Cs)).


%! detect_languages(-Languages:list(pair(between(0.0,1.0)),atom))// is det.
% Parses output returned by the langage-detection command-line tool.
% The output is a list of pairs consisting of a confidence value and
% an abbreviated language name.

detect_languages(Langs) -->
  file_path(_),
  ":",
  "[", languages(Pairs), "]",
  {keysort(Pairs, Langs)},
  "\n".


languages(L) -->
  language(H), !,
  (   ",",
      whites
  ->  languages(T),
      {L = [H|T]}
  ;   {L = [H]}
  ).


language(Conf-Lang) -->
  ...(Cs),
  ":",
  float(Conf),
  {atom_codes(Lang, Cs)}.
