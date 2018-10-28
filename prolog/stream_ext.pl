:- encoding(utf8).
:- module(
  stream_ext,
  [
    copy_stream_type/2,      % +In, +Out
    guess_encoding/2,        % +In, -Encoding
    number_of_open_files/1,  % -N
    read_line_to_atom/2,     % +In, -Atom
    read_stream_to_atom/2,   % +In, -Atom
    read_stream_to_string/2, % +In, -String
    recode_stream/3,         % +In, +FromEncoding, -Out
    stream_line_column/3,    % +Stream, -Line, -Column
    stream_metadata/2,       % +Stream, -Metadata
    wc/2                     % +In, -Stats
  ]
).

/** <module> Stream extensions

Uses the external programs `iconv' and `uchardet'.

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(debug_ext)).
:- use_module(library(file_ext), []).
:- use_module(library(hash_stream)).
:- use_module(library(os_ext)).
:- use_module(library(string_ext)).
:- use_module(library(thread_ext)).

:- thread_local
   debug_indent/1.

debug_indent(0).





%! copy_stream_type(+In:stream, +Out:stream) is det.
%
% Like copy_stream_data/2, but also sets the stream type of Out to
% match the stream type of In, if needed,

copy_stream_type(In, Out) :-
  stream_property(In, type(Type)),
  set_stream(Out, type(Type)),
  copy_stream_data(In, Out).



%! guess_encoding(+In:stream, -Encoding:atom) is det.
%
% If the encoding cannot be guessed (`unknown'), the error
% cannot_guess_encoding/0 is thrown.

% The value bom causes the stream to check whether the current
% character is a Unicode BOM marker.  If a BOM marker is found, the
% encoding is set accordingly and the call succeeds; otherwise the
% call fails.
guess_encoding(In, utf8) :-
  set_stream(In, encoding(bom)), !.
guess_encoding(In, Enc) :-
  process_create(
    path(uchardet),
    [],
    [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
  ),
  set_stream(ProcIn, encoding(octet)),
  call_cleanup(
    copy_stream_data(In, ProcIn),
    close(ProcIn)
  ),
  call_cleanup(
    (
      read_string(ProcOut, String1),
      string_strip(String1, "\n", String2),
      atom_string(Enc0, String2)
    ),
    close(ProcOut)
  ),
  clean_encoding_(Enc0, Enc),
  (Enc == unknown -> throw(error(cannot_guess_encoding,guess_encoding/2)) ; true).



%! number_of_open_files(-N:nonneg) is det.

number_of_open_files(N) :-
  expand_file_name('/proc/self/fd/*', Files),
  length(Files, N).



%! read_line_to_atom(+In:stream, -Atom:atom) is nondet.

read_line_to_atom(In, Atom) :-
  repeat,
  read_line_to_codes(In, Codes),
  (   Codes == end_of_file
  ->  !, fail
  ;   atom_codes(Atom, Codes)
  ).



%! read_stream_to_atom(+In:stream, -Atom:atom) is det.

read_stream_to_atom(In, Atom) :-
  read_stream_to_codes(In, Codes),
  atom_codes(Atom, Codes).



%! read_stream_to_string(+In:stream, -String:string) is det.

read_stream_to_string(In, String) :-
  read_stream_to_codes(In, Codes),
  string_codes(String, Codes).



%! recode_stream(+In:stream, +FromEncoding:atom, -Out:stream) is det.
%
% We only recode to UTF-8.
%
% See the output of command ~iconv -l~ for the supported encodings.
%
% Assumes that In and Out are binary streams.

recode_stream(In, Enc0, Out) :-
  clean_encoding_(Enc0, Enc),
  process_create(
    path(iconv),
    ['-c','-f',Enc,'-t','utf-8',-],
    [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
  ),
  create_detached_thread(
    call_cleanup(
      copy_stream_type(In, ProcIn),
      close(ProcIn)
    )
  ),
  set_stream(ProcOut, type(binary)),
  (   ground(Out)
  ->  call_cleanup(
        copy_stream_data(ProcOut, Out),
        close(ProcOut)
      )
  ;   ProcOut = Out
  ).



%! stream_line_column(+Stream:stream, -Line:nonneg, -Column:nonneg) is det.

stream_line_column(Stream, Line, Column) :-
  stream_property(Stream, position(Pos)),
  stream_position_data(line_count, Pos, Line),
  stream_position_data(line_position, Pos, Column).



%! stream_metadata(+Stream:stream, -Metadata:dict) is det.

stream_metadata(Stream, Meta) :-
  stream_property(Stream, position(Pos)),
  stream_position_data(byte_count, Pos, NumBytes),
  stream_position_data(char_count, Pos, NumChars),
  stream_position_data(line_count, Pos, NumLines),
  stream_property(Stream, newline(Newline)),
  Meta = _{
    bytes: NumBytes,
    characters: NumChars,
    lines: NumLines,
    newline: Newline
  }.



/*
%! wc(+In:stream, -Lines:nonneg) is det.
%
% Native implementation of line count.
%
% @tbd Compare performance with wc/4.

wc(In, Lines) :-
  Counter = count(0),
  repeat,
  read_line_to_codes(In, Codes),
  (   Codes == end_of_file
  ->  !,
      arg(1, Counter, Lines)
  ;   arg(1, Counter, Count1),
      Count2 is Count1 + 1,
      nb_setarg(1, Counter, Count2),
      fail
  ).
*/

%! wc(+In:stream, -Stats:dict) is det.
%
% Linux-only parsing of GNU wc output.

wc(In, Stats) :-
  setup_call_cleanup(
    process_create(path(wc), [], [stdin(pipe(ProcIn)),stdout(pipe(Out))]),
    (
      create_detached_thread(
        call_cleanup(
          copy_stream_data(In, ProcIn),
          close(ProcIn)
        )
      ),
      read_wc(Out, Lines, Words, Bytes)
    ),
    close(Out)
  ),
  Stats = _{
    number_of_bytes: Bytes,
    number_of_lines: Lines,
    number_of_words: Words
  }.

read_wc(Out, Lines, Words, Bytes) :-
  read_stream_to_codes(Out, Codes),
  phrase(read_wc(Lines, Words, Bytes), Codes, _).

% E.g., `427 1818 13512 README.md`.
read_wc(Lines, Words, Bytes) -->
  whites,
  integer(Lines),
  whites,
  integer(Words),
  whites,
  integer(Bytes).





% GENERICS %

%! clean_encoding_(+Encoding:atom, -CleanEncoding:atom) is det.

clean_encoding_(Enc1, Enc4) :-
  downcase_atom(Enc1, Enc2),
  (encoding_alias_(Enc2, Enc3) -> true ; Enc3 = Enc2),
  (encoding_promotion_(Enc3, Enc4) -> true ; Enc4 = Enc3).

encoding_alias_('iso-8859-15.latin1', 'iso8859-15').
encoding_alias_(macroman, macintosh).
encoding_alias_('us-ascii', ascii).
encoding_alias_('utf-8', utf8).

encoding_promotion_(ascii, utf8).
