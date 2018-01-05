:- encoding(utf8).
:- module(
  stream_ext,
  [
    copy_stream_type/2,      % +In, +Out
    guess_string_encoding/2, % +String, -Encoding
    normalize_encoding/2,    % +Encoding1, -Encoding2
    read_line_to_atom/2,     % +In, -Atom
    recode_stream/4,         % +FromEnc, +In1, -In2, :Close_0
    stream_metadata/3,       % +Stream, +Metadata1, -Metadata2
    stream_hash_metadata/4   % +Stream, +Metadata1, -Metadata2, +Options
  ]
).
:- reexport(library(readutil)).

/** <module> Stream extensions

Uses the external programs `iconv' and `uchardet'.

@author Wouter Beek
@version 2017/06-2018/01
*/

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



%! guess_string_encoding(+String:string, -Encoding:atom) is det.
%
% `Encoding' is converted to lowercase.

guess_string_encoding(String, Enc2) :-
  % Enc is mentioned in the XML declaration.
  string_phrase('XMLDecl'(_, Enc1, _), String, _),
  nonvar(Enc1), !,
  downcase_atom(Enc1, Enc2).
guess_string_encoding(String, Enc) :-
  open_binary_string(String, In),
  setup_call_cleanup(
    process_create(
      path(uchardet),
      [],
      [
        process(Pid),
        stderr(pipe(ProcErr)),
        stdin(pipe(ProcIn)),
        stdout(pipe(ProcOut))
      ]
    ),
    (
      flag(uchardet, N, N+1),
      format(atom(AliasErr), "uchardet-err-~d", [N]),
      create_detached_thread(AliasErr, copy_and_close(ProcErr, user_error)),
      call_cleanup(
        copy_stream_data(In, ProcIn),
        close(ProcIn)
      ),
      read_string(ProcOut, String1),
      string_strip(String1, "\n", String2),
      process_wait(Pid, exit(Status)),
      (Status =:= 0 -> true ; print_message(warning, process_status(Status)))
    ),
    close(ProcOut)
  ),
  % Trick to return the encoding as an atom.
  downcase_atom(String2, Enc).

copy_and_close(In, _) :-
  at_end_of_stream(In), !,
  close(In).
copy_and_close(In, Out) :-
  read_line_to_codes(In, Codes),
  split_lines(Codes, Lines),
  forall(
    member(Line, Lines),
    format(Out, "~s\n", [Line])
  ),
  copy_and_close(In, Out).

split_lines([], []) :- !.
split_lines(Codes1, [Line|Lines]) :-
  append(LineCodes, [0'\n|Codes2], Codes1), !,
  string_codes(Line, LineCodes),
  split_lines(Codes2, Lines).
split_lines(Line, [Line]).



%! normalize_encoding(+Encoding1:atom, -Encoding2:atom) is det.

normalize_encoding(Enc1, Enc3) :-
  downcase_atom(Enc1, Enc2),
  (encoding_alias(Enc2, Enc3) -> true ; Enc3 = Enc2).

encoding_alias(macroman, macintosh).
encoding_alias(utf8, 'utf-8').



%! open_binary_string(+String:string, -In:stream) is det.

open_binary_string(Str, In) :-
  open_string(Str, In),
  set_stream(In, type(binary)).



%! read_line_to_atom(+In:stream, -Atom:atom) is nondet.

read_line_to_atom(In, A) :-
  repeat,
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  !, fail
  ;   atom_codes(A, Cs)
  ).



%! recode_stream(+FromEnc:atom, +In1:stream, -In2:stream, :Close_0) is det.
%
% In1 is closed by this predicate.
%
% @arg ToEnd The encoding that is used to interpret the contents of
%            the input stream.  Use `octet' for binary content, and
%            `utf8' for textual content.

recode_stream(octet, In1, In2, Close_0) :- !,
  recode_stream(octet, In1, octet, In2, Close_0).
recode_stream(FromEnc, In1, In2, Close_0) :-
  recode_stream(FromEnc, In1, utf8, In2, Close_0).

recode_stream(_, In, octet, In, true) :- !.
% “The value bom causes the stream to check whether the current
% character is a Unicode BOM marker.  If a BOM marker is found, the
% encoding is set accordingly and the call succeeds.  Otherwise the
% call fails.”
recode_stream(_, In, utf8, In, true) :-
  set_stream(In, encoding(bom)), !.
recode_stream(FromEnc, In, utf8, In, true) :-
  memberchk(FromEnc, [ascii,'ascii/unknown','us-ascii','utf-8']), !,
  set_stream(In, encoding(utf8)).
recode_stream(FromEnc, In, utf8, ProcOut, close(In)) :-
  indent_debug(1, encoding, "> ~a RECODE", [FromEnc]),
  process_create(
    path(iconv),
    ['-c','-f',FromEnc,'-t','utf-8'],
    [
      process(Pid),
      %stderr(pipe(ProcErr)),
      stderr(null),
      stdin(pipe(ProcIn)),
      stdout(pipe(ProcOut))
    ]
  ),
  flag(iconv, N, N+1),
  %format(atom(AliasErr), "iconv-err-~d", [N]),
  format(atom(AliasIn), "iconv-cin~d", [N]),
  %create_detached_thread(AliasErr, copy_and_close(ProcErr, user_error)),
  create_detached_thread(AliasIn, copy_stream_data(In, ProcIn)),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).



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
