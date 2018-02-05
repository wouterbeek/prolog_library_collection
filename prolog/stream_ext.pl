:- encoding(utf8).
:- module(
  stream_ext,
  [
    copy_stream_type/2,     % +In, +Out
    guess_encoding/2,       % +In, -Encoding
    number_of_open_files/1, % -N
    open_binary_string/2,   % +String, -In
    read_line_to_atom/2,    % +In, -Atom
    recode_stream/2,        % +FromEncoding, +In
    recode_stream/3,        % +FromEncoding, +In, -Out
    stream_metadata/3,      % +Stream, +Metadata1, -Metadata2
    stream_hash_metadata/4, % +Stream, +Metadata1, -Metadata2, +Options
    wc/2                    % +In, -Stats
  ]
).
:- reexport(library(readutil)).

/** <module> Stream extensions

Uses the external programs `iconv' and `uchardet'.

@author Wouter Beek
@version 2017/06-2018/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext), []).
:- use_module(library(hash_stream)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(readutil)).
:- use_module(library(string_ext)).
:- use_module(library(thread_ext)).
:- use_module(library(xml/xml_parser)).
:- use_module(library(yall)).

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
% @tbd Guess encoding from XML prolog.

/*
guess_encoding(In, Encoding2) :-
  % Encoding is mentioned in the XML declaration.
  phrase_from_stream(In, 'XMLDecl'(_, Encoding1, _)),
  nonvar(Encoding1), !,
  downcase_atom(Encoding1, Encoding2).
*/
guess_encoding(In, Encoding) :-
  setup_call_cleanup(
    process_in_open(uchardet, In, Out),
    read_file_encoding(Out, Encoding),
    close(Out)
  ).

read_file_encoding(Out, Encoding) :-
  read_string(Out, String1),
  string_strip(String1, "\n", String2),
  atom_string(Encoding, String2).



%! number_of_open_files(-N:nonneg) is det.

number_of_open_files(N) :-
  expand_file_name('/proc/self/fd/*', Files),
  length(Files, N).



%! open_binary_string(+String:string, -In:stream) is det.

open_binary_string(String, In) :-
  open_string(String, In),
  set_stream(In, type(binary)).



%! read_line_to_atom(+In:stream, -Atom:atom) is nondet.

read_line_to_atom(In, A) :-
  repeat,
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  !, fail
  ;   atom_codes(A, Cs)
  ).



%! recode_stream(+FromEncoding:atom, +In:stream) is det.
%
% Recoding to UTF-8 that does not require a new stream.

recode_stream(Encoding0, In) :-
  clean_encoding(Encoding0, Encoding),
  (   Encoding == octet
  ->  true
  ;   % The value bom causes the stream to check whether the current
      % character is a Unicode BOM marker.  If a BOM marker is found,
      % the encoding is set accordingly and the call succeeds.
      % Otherwise the call fails.
      set_stream(In, encoding(bom))
  ->  true
  ;   memberchk(Encoding, [ascii,utf8])
  ->  set_stream(In, encoding(utf8))
  ).



%! recode_stream(+FromEncoding:atom, +In:stream, -Out:stream) is det.
%
% We only recode to UTF-8.
%
% See the output of command ~iconv -l~ for the supported encodings.

recode_stream(Encoding0, In, Out) :-
  clean_encoding(Encoding0, Encoding),
  print_message(informational, recode(Encoding)),
  process_in_open(iconv, ['-c','-f',Encoding,'-t','utf-8'], In, Out).

clean_encoding(Dirty, Clean) :-
  downcase_atom(Dirty, Atom),
  (encoding_alias(Atom, Clean) -> true ; Clean = Atom).

encoding_alias(macroman, macintosh).
encoding_alias('utf-8', utf8).


%! stream_hash_metadata(+Stream:stream, +Metadata1:list(dict),
%!                      -Metadata2:list(dict), +Options:list(compound)) is det.

stream_hash_metadata(Stream, [Dict1|Metadata], [Dict3|Metadata], Options) :-
  stream_metadata(Stream, [Dict1|Metadata], [Dict2|Metadata]),
  (   (   option(md5(Value), Options)
      ->  Key = md5
      ;   option(sha1(Value), Options)
      ->  Key = sha1
      ;   option(sha224(Value), Options)
      ->  Key = sha224
      ;   option(sha256(Value), Options)
      ->  Key = sha256
      ;   option(sha384(Value), Options)
      ->  Key = sha384
      ;   option(sha512(Value), Options)
      ->  Key = sha512
      )
  ->  dict_put(Key, Dict2, Value, Dict3)
  ;   Dict3 = Dict2
  ).



%! stream_metadata(+Stream:stream, +Metadata1:list(dict),
%!                 -Metadata2:list(dict)) is det.

stream_metadata(Stream, [Dict1|Metadata], [Dict2|Metadata]) :-
  stream_property(Stream, position(Position)),
  stream_position_data(byte_count, Position, NumberOfBytes),
  stream_position_data(char_count, Position, NumberOfChars),
  stream_position_data(line_count, Position, NumberOfLines),
  stream_property(Stream, newline(Newline)),
  merge_dicts(
    Dict1,
    _{
      newline: Newline,
      number_of_bytes: NumberOfBytes,
      number_of_chars: NumberOfChars,
      number_of_lines: NumberOfLines
    },
    Dict2
  ).





% ARGUMENTS TO META-PREDICATES %

%! copy_stream_data(+In:stream, +Out:stream, +Metadata1:list(dict),
%!                  -Metadata:list(dict)) is det.
%
% copy_stream_data/2 with metadata.

copy_stream_data(In, Out, Metadata, Metadata) :-
  copy_stream_data(In, Out).





% COMMON METADATA EXTRACTIONS

%! metadata_status_code(+Metadata:list(dict), -Status:between(100,599)) is det.

metadata_status_code(Metadata, Status) :-
  member(Dict, Metadata),
  dict_get(status, Dict, Status).



%! metadata_uri(+Metadata:list(dict), -Uri:atom) is semidet.

metadata_uri(Metadata, Uri) :-
  member(Dict, Metadata),
  dict_get(uri, Dict, Uri), !.



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
    process_in_open(wc, In, Out),
    read_wc(Out, Lines, Words, Bytes),
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





% HELPERS %

%! open_hash(+Stream1:stream, -Stream2:stream,
%!           +Options:list(compound)) is semidet.

open_hash(Stream1, Stream2, Options1) :-
  (   option(md5(_), Options1)
  ->  Algorithm = md5
  ;   option(sha1(_), Options1)
  ->  Algorithm = sha1
  ;   option(sha224(_), Options1)
  ->  Algorithm = sha224
  ;   option(sha256(_), Options1)
  ->  Algorithm = sha256
  ;   option(sha384(_), Options1)
  ->  Algorithm = sha384
  ;   option(sha512(_), Options1)
  ->  Algorithm = sha512
  ),
  Options2 = [algorithm(Algorithm),close_parent(false)],
  open_hash_stream(Stream1, Stream2, Options2).
