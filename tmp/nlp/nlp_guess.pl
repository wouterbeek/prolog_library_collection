:- module(
  nlp_guess,
  [
    nlp_guess/2,           % +Str, -Lang
    nlp_guess_candidates/2 % +Str, -Langs:list(pair(between(0.0,1.0)),atom)
  ]
).

/** <module> Language guessing

Guess the natural language of a given string.

This module is a wrapper around the external library
[**language-detection**](https://code.google.com/p/language-detection/).

@author Wouter Beek
@version 2015/08, 2016/05, 2017/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_file)).
:- use_module(library(lists)).
:- use_module(library(process)).





%! nlp_guess(+Str, -Lang) is det.
%! nlp_guess_candidates(+Str, -Langs:list(pair(between(0.0,1.0)),atom)) is det.

nlp_guess(Str, Lang):-
  nlp_guess_candidates(Str, Langs),
  last(Langs, _-Lang).


nlp_guess_candidates(Str, Langs):-
  absolute_file_name(
    library(nlp/lib/'langdetect-09-13-2011'/lib/langdetect),
    Jar,
    [access(read),extensions([jar])]
  ),
  absolute_file_name(
    library(nlp/lib/'langdetect-09-13-2011'/profiles),
    Dir,
    [access(read),file_type(directory)]
  ),
  setup_call_cleanup(
    (
      tmp_file_stream(text, TmpFile, Stream),
      write(Stream, Str),
      close(Stream)
    ),
    process_out(
      java,
      ['-jar',file(Jar),'--detectlang','-d',file(Dir),file(TmpFile)],
      {Langs}/[ProcOut]>>phrase_from_stream(ProcOut, nlp_guess_candidates(Langs))
    ),
    delete_file(TmpFile)
  ).


%! nlp_guess_candidates(-Langs:list(pair(between(0.0,1.0)),atom))// is det.
%
% Parses output returned by the langage-detection command-line tool.
% The output is a list of pairs consisting of a confidence value and
% an abbreviated language name.

nlp_guess_candidates(Langs) -->
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
