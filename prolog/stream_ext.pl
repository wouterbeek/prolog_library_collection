:- module(
  stream_ext,
  [
    copy_stream_type/2,      % +In, +Out
    read_line_to_atom/2,     % +In, -Atom
  % HELPER PREDICATES
    normalize_encoding/2,    % +Encoding1, -Encoding2
    recode_stream/3,         % +FromEnc, +In1, -In2
    stream_metadata/3,       % +Stream, +Metadata1, -Metadata2
    stream_hash_metadata/4,  % +Stream, +Metadata1, -Metadata2, +Options
  % ARGUMENTS TO META-PREDICATES
    copy_stream_data/4,      % +In, +Out, +Metadata1, -Metadata2
  % COMMON METADATA EXTRACTIONS
    metadata_status_code/2,  % +Metadata, -Status
    metadata_uri/2           % +Metadata, -Uri
  ]
).
:- reexport(library(readutil)).

/** <module> Stream extensions

Uses the external programs `iconv' and `uchardet'.

@author Wouter Beek
@version 2017/06-2017/10
*/

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
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(xml/xml_parser)).

:- setting(
     guess_stream_encoding_length,
     positive_integer,
     2000,
     "The number of characters that are used to determine the encoding of a given input stream.  Incorrect guesses were observed with value 1K."
   ).
:- setting(
     temporary_directory,
     any,
     _,
     "The directory that is used for storing temporary files."
   ).

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



%! guess_stream_encoding(+In:stream, -Encoding:atom) is det.

guess_stream_encoding(In, Encoding) :-
  setting(guess_stream_encoding_length, Length),
  peek_string(In, Length, String),
  guess_string_encoding(String, Encoding).



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
      thread_create(copy_stream_data(In, ProcIn), _, [detached(true)]),
      thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
      read_string(ProcOut, String1),
      split_string(String1, "", "\n", [String2|_]),
      process_wait(Pid, exit(Status)),
      (Status =:= 0 -> true ; print_message(warning, process_status(Status)))
    ),
    close(ProcOut)
  ),
  downcase_atom(String2, Enc).



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



%! recode_stream(+FromEnc:atom, +In1:stream, -In2:stream) is det.
%
% In1 is closed by this predicate.
%
% @arg ToEnd The encoding that is used to interpret the contents of
%            the input stream.  Use `octet' for binary content, and
%            `utf8' for textual content.

recode_stream(octet, In1, In2) :- !,
  recode_stream(octet, In1, octet, In2).
recode_stream(FromEnc, In1, In2) :-
  recode_stream(FromEnc, In1, utf8, In2).

recode_stream(_, In, octet, In) :- !.
% “The value bom causes the stream to check whether the current
% character is a Unicode BOM marker.  If a BOM marker is found, the
% encoding is set accordingly and the call succeeds.  Otherwise the
% call fails.”
recode_stream(_, In, utf8, In) :-
  set_stream(In, encoding(bom)), !.
recode_stream(FromEnc, In, utf8, In) :-
  memberchk(FromEnc, [ascii,'ascii/unknown','us-ascii','utf-8']), !,
  set_stream(In, encoding(utf8)).
recode_stream(FromEnc, In, utf8, ProcOut) :-
  indent_debug(1, encoding, "> ~a RECODE", [FromEnc]),
  process_create(
    path(iconv),
    ['-c','-f',FromEnc,'-t','utf-8'],
    [
      process(Pid),
      stderr(pipe(ProcErr)),
      stdin(pipe(ProcIn)),
      stdout(pipe(ProcOut))
    ]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(In, ProcIn), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).



%! sort_stream(+In:stream, -Out:stream) is det.
%! sort_stream(+In:stream, -Out:stream, +Options:list(compound)) is det.
%
% @arg Options The following options are supported:
%
%      * buffer_size(+nonneg)
%
%        Optionally, the size of the buffer in kilobytes.
%
%      * duplicates(+boolean)
%
%        Whether duplicates are allowed in the result.  Default is
%        `true'.
%
%      * numeric(+boolean)
%
%        Whether numberic sort is performed.  Default is `false'.
%
%      * output(+atom)
%
%        The name of the output file, as processed by
%        `absolute_file_name/[2,3]'.  Default is the input file.
%
%      * temporary_directory(+atom)
%
%        The directory that is used for storing intermediary results
%        of sorting.  Default is the value of setting
%        `temporary_directory'.
%
%      * threads(+positive_integer)
%
%        The number of threads that is used.  Default is the number of
%        available processors, but not larger than 8.  Larger numbers
%        have diminishing returns.  Using $n$ threads increases the
%        memory use by $\log n$.
%
%      * utf8(+boolean)
%
%        Whether the environment is set to UTF-8 encoding.  Default is
%        `false'.

sort_stream(In, Out) :-
  sort_stream(In, Out, []).


sort_stream(In, ProcOut, Options1) :-
  select_option(env(Env1), Options1, Options2, []),
  select_option(utf8(Utf8), Options2, Options3, false),
  (Utf8 == true -> Env2 = [] ; Env2 = ['LC_ALL'='C']),
  append(Env1, Env2, Env3),
  merge_options(
    [
      env(Env3),
      process(Pid),
      stderr(pipe(ProcErr)),
      stdin(pipe(ProcIn)),
      stdout(pipe(ProcOut))
    ],
    Options3,
    Options4
  ),
  (   \+ option(temporary_directory(_), Options3),
      setting(temporary_directory, Directory),
      ground(Directory)
  ->  merge_options([temporary_directory(Directory)], Options3, Arguments)
  ;   Arguments = Options3
  ),
  process_flags(sort_flag, Arguments, Flags),
  process_create(path(sort), Flags, Options4),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(In, ProcIn), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).

sort_flag(buffer_size(Size), Flag) :-
  must_be(nonneg, Size),
  format(atom(Flag), '--buffer-size=~d', [Size]).
% -u
sort_flag(duplicates(KeepDuplicates), '--unique') :-
  must_be(boolean, KeepDuplicates),
  KeepDuplicates == false.
sort_flag(numeric(IsNumeric), '--numeric-sort') :-
  must_be(boolean, IsNumeric),
  IsNumeric == true.
% -o
sort_flag(output(OutFileSpec), Flag) :-
  absolute_file_name(OutFileSpec, OutFile, [access(write)]),
  format(atom(Flag), '--output=~a', [OutFile]).
sort_flag(threads(NumberOfThreads), Flag) :-
  must_be(positive_integer, NumberOfThreads),
  NumberOfThreads > 0,
  format(atom(Flag), '--parallel=~d', [NumberOfThreads]).
% -T
sort_flag(temporary_directory(Directory), Flag) :-
  must_be(directory, Directory),
  format(atom(Flag), '--temporary-directory=~a', [Directory]).



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

%! archive_format(+Format:atom) is semidet.
%! archive_format(-Format:atom) is multi.
%
% Format are the names of supported archive formats.  The `mtree'
% format is not supported, because archives in that format are regular
% text files and result in false positives.

archive_format('7zip').
archive_format(ar).
archive_format(cab).
archive_format(cpio).
archive_format(empty).
archive_format(gnutar).
archive_format(iso9660).
archive_format(lha).
%archive_format(mtree).
archive_format(rar).
archive_format(raw).
archive_format(tar).
archive_format(xar).
archive_format(zip).



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
