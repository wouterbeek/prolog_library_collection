:- module(
  stream_ext,
  [
    call_on_stream/2,        % +In, :Goal_3
    call_on_stream/3,        % +In, :Goal_3, +Options
    call_to_null/1,          % :Goal_1
    call_to_stream/2,        % +Out, :Goal_3
    call_to_stream/3,        % +Out, :Goal_3, +Options
    copy_stream_type/2,      % +In, +Out
    read_line_to_atom/2,     % +In, -Atom
  % HELPER PREDICATES
    normalize_encoding/2,    % +Encoding1, -Encoding2
    print_err/1,             % +Err
    recode_stream/3,         % +FromEnc, +In1, -In2
    stream_metadata/3,       % +Stream, +Metadata1, -Metadata2
    stream_hash_metadata/4,  % +Stream, +Metadata1, -Metadata2, +Options
  % ARGUMENTS TO META-PREDICATES
    copy_stream_data/4,      % +In, +Out, +Metadata1, -Metadata2
    true_metadata/3,         % +In, +Metadata1, -Metadata2
    true_metadata/4,         % -Metadata2, +In, +Metadata1, -Metadata2
    true_null/3,             % +In, +Metadata1, -Metadata2
  % COMMON METADATA EXTRACTIONS
    metadata_content_type/2, % +Metadata, -MediaType
    metadata_status_code/2,  % +Metadata, -Status
    metadata_uri/2           % +Metadata, -Uri
  ]
).
:- reexport(library(readutil)).

/** <module> Stream extensions

Uses the external programs `iconv' and `uchardet'.

@author Wouter Beek
@version 2017/06-2017/09
*/

:- use_module(library(archive)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext), []).
:- use_module(library(hash_stream)).
:- use_module(library(http/rfc7231)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(readutil)).
:- use_module(library(settings)).
:- use_module(library(string_ext)).
:- use_module(library(xml/xml_parser)).

:- meta_predicate
    call_on_archive(+, 3, +, -, +),
    call_on_recoded_stream(+, 3, +, -, +),
    call_on_sorted_stream(+, 3, +, -, +),
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream(+, 3, +, -, +),
    call_on_stream_hash(+, 3, +, -, +),
    call_to_null(1),
    call_to_stream(+, 3),
    call_to_stream(+, 3, +),
    call_to_stream(+, 3, -, +),
    call_to_stream1(+, +, 3, +, -, +),
    call_to_stream2(+, 3, +, -, +).

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





%! call_on_stream(+In:stream, :Goal_3) is nondet.
%! call_on_stream(+In:stream, :Goal_3, +Options:list(compound)) is nondet.
%
% @arg Metadata is a dictionary containing metadata about opening of
%      the stream.
%
%      The following metadata properties are included:
%
%        * newline(--oneof([dos,posix]))
%
%        * number_of_bytes(--nonneg)
%
%        * number_of_chars(--nonneg)
%
%        * number_of_lines(--nonneg)
%
%      The following options are supported:
%
%        * md5(--atom)
%
%        * sha1(--atom)
%
%        * sha224(--atom)
%
%        * sha256(--atom)
%
%        * sha384(--atom)
%
%        * sha512(--atom)
%
% @arg Options The following options are supported:
%
%      * decompression(+boolean)
%
%        Default is `true'.
%
%      * sort(+oneof([lexicographic,none,numeric]))
%
%        In which way the stream is sorted.  Default is `none'.
%
%      * md5(--atom)
%
%      * sha1(--atom)
%
%      * sha224(--atom)
%
%      * sha256(--atom)
%
%      * sha384(--atom)
%
%      * sha512(--atom)
%
%      * Other options are passed to archive_data_stream/3 and
%        zopen/3.

call_on_stream(In, Goal_3) :-
  call_on_stream(In, Goal_3, []).


call_on_stream(In, Goal_3, Options) :-
  call_on_stream(In, Goal_3, [], Metadata, Options),
  ignore(option(metadata(Metadata), Options)).


% archive_data_stream/3 fails in case input stream In1 is empty.  We
% do not want the call of Goal_3 to fail in such cases.
call_on_stream(In, Goal_3, Metadata1, Metadata2, Options) :-
  at_end_of_stream(In), !,
  call_on_stream_hash(In, Goal_3, Metadata1, Metadata2, Options).
call_on_stream(In, Goal_3, Metadata1, Metadata3, Options) :-
  option(decompression(false), Options), !,
  call_on_stream_hash(In, Goal_3, Metadata1, Metadata2, Options),
  stream_hash_metadata(In, Metadata2, Metadata3, Options).
call_on_stream(In, Goal_3, Metadata1, Metadata2, Options) :-
  setup_call_cleanup(
    call_on_stream_setup(In, Archive),
    call_on_archive(Archive, Goal_3, Metadata1, Metadata2, Options),
    call_on_stream_cleanup(Archive, In)
  ).

call_on_stream_setup(In, Archive) :-
  findall(format(Format), archive_format(Format), Options1),
  merge_options([close_parent(false),filter(all)], Options1, Options2),
  archive_open(In, Archive, Options2),
  indent_debug(1, stream_ext, "> ~w OPEN ARCHIVE ~w", [In,Archive]).

call_on_stream_cleanup(Archive, In) :-
  indent_debug(-1, stream_ext, "< ~w CLOSE ARCHIVE ~w", [Archive,In]),
  archive_close(Archive).

%! call_on_archive(+Archive:blob, :Goal_3, +Metadata1:list(dict),
%!                 -Metadata2:list(dict), +Options:list(compound)) is nondet.

call_on_archive(Archive, Goal_3, Metadata1, Metadata5, Options1) :-
  merge_options([meta_data(Metadata2)], Options1, Options2),
  archive_data_stream(Archive, In, Options2), %NONDET
  (debugging(stream_ext) -> Metadata2 = [Dict|_], Entry = Dict.name ; true),
  indent_debug(1, stream_ext, "> ~w OPEN ENTRY ~w ‘~a’", [Archive,In,Entry]),
  reverse(Metadata2, Metadata3),
  append(Metadata3, Metadata1, Metadata4),
  call_cleanup(
    call_on_recoded_stream(In, Goal_3, Metadata4, Metadata5, Options1),
    call_on_archive_cleanup(In, Entry,Archive)
  ).

call_on_archive_cleanup(In, Entry,Archive) :-
  indent_debug(-1, stream_ext, "< ~w ‘~a’ CLOSE ENTRY ~w", [In,Entry,Archive]),
  close(In).

%! call_on_recoded_stream(+In:stream, :Goal_3, +Metadata1:list(dict),
%!                        -Metadata2:list(dict),
%!                        +Options:list(compound)) is det.

call_on_recoded_stream(In1, Goal_3, Metadata1, Metadata3, Options) :-
  setup_call_cleanup(
    call_on_recoded_stream_setup(In1, In2, Options),
    (
      call_on_sorted_stream(In2, Goal_3, Metadata1, Metadata2, Options),
      stream_metadata(In2, Metadata2, Metadata3)
    ),
    call_on_recoded_stream_cleanup(In1, In2)
  ).

call_on_recoded_stream_setup(In1, In2, Options) :-
  (   option(from_encoding(FromEnc0), Options)
  ->  normalize_encoding(FromEnc0, FromEnc)
  ;   guess_stream_encoding(In1, FromEnc)
  ),
  (   option(to_encoding(ToEnc), Options)
  ->  must_be(oneof([octet,utf8]), ToEnc)
  ;   (FromEnc == octet -> ToEnc = octet ; ToEnc = utf8)
  ),
  recode_stream(FromEnc, In1, ToEnc, In2),
  (   In1 == In2
  ->  true
  ;   indent_debug(1, stream_ext, "> ~w RECODE ~w", [In1,In2])
  ).

call_on_recoded_stream_cleanup(In, In) :- !.
call_on_recoded_stream_cleanup(In1, In2) :-
  indent_debug(-1, stream_ext, "< ~w RECODE ~w", [In1,In2]),
  close(In2).

%! call_on_sorted_stream(+In1:stream, :Goal_3, +Metadata1:list(dict),
%!                       -Metadata3:list(dict),
%!                       +Options:list(compound)) is det.

call_on_sorted_stream(In1, Goal_3, Metadata1, Metadata3, Options1) :-
  select_option(sort(Mode), Options1, Options2, none),
  must_be(oneof([lexicographic,none,numeric]), Mode),
  setup_call_cleanup(
    call_on_sorted_stream_setup(Mode, In1, In2, Options2),
    (
      call_on_stream_hash(In2, Goal_3, Metadata1, Metadata2, Options2),
      stream_hash_metadata(In2, Metadata2, Metadata3, Options2)
    ),
    call_on_sorted_stream_cleanup(In1, In2)
  ).

call_on_sorted_stream_setup(none, In, In, _) :- !.
call_on_sorted_stream_setup(Mode, In1, In2, Options1) :-
  sort_options(Options1, Mode, Options2),
  sort_stream(In1, In2, Options2),
  indent_debug(1, stream_ext, "> ~w SORT ~w", [In1,In2]).

sort_options(Options, lexicographic, Options) :- !.
sort_options(Options1, numeric, Options2) :-
  merge_options([numeric(true)], Options1, Options2).

call_on_sorted_stream_cleanup(In, In) :- !.
call_on_sorted_stream_cleanup(In1, In2) :-
  indent_debug(-1, stream_ext, "< ~w SORT ~w", [In2,In1]),
  close(In2).

%! call_on_stream_hash(+In:stream, :Goal_3, +Metadata:list(dict),
%!                     -Metadata:list(dict), +Options:list(compound)) is det.

call_on_stream_hash(In1, Goal_3, Metadata1, Metadata3, Options) :-
  setup_call_cleanup(
    call_on_stream_hash_setup(In1, In2, Options),
    (
      call(Goal_3, In2, Metadata1, Metadata2),
      stream_hash_metadata(In2, Metadata2, Metadata3, Options)
    ),
    call_on_stream_hash_cleanup(In1, In2)
  ).

call_on_stream_hash_setup(In1, In2, Options) :-
  open_hash(In1, In2, Options), !,
  indent_debug(1, stream_ext, "> ~w CALCULATE HASH ~w", [In1,In2]).
call_on_stream_hash_setup(In, In, _).

call_on_stream_hash_cleanup(In, In) :- !.
call_on_stream_hash_cleanup(In1, In2) :-
  indent_debug(-1, stream_ext, "< ~w CALCULATE HASH ~w", [In2,In1]),
  close(In2).



%! call_to_null(:Goal_1) is det.

call_to_null(Goal_1) :-
  setup_call_cleanup(
    open_null_stream(Out),
    call(Goal_1, Out),
    close(Out)
  ).



%! call_to_stream(+Out:stream, :Goal_3) is det.
%! call_to_stream(+Out:stream, :Goal_3, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * compression(+oneof([deflate,gzip,none,raw_deflate]))
%
%     Default is `gzip'.
%
%   * metadata(-list(dict))
%
%   * Other options are passed to zopen/3, open_hash/3,
%     stream_metadata/3, and stream_hash_metadata/4.

call_to_stream(Out, Goal_3) :-
  call_to_stream(Out, Goal_3, []).


call_to_stream(Out, Goal_3, Options) :-
  call_to_stream(Out, Goal_3, Metadata, Options),
  ignore(option(metadata(Metadata), Options)).


call_to_stream(Out, Goal_3, Metadata, Options1) :-
  % zopen/3 defines the format/1 option, but we use the more
  % descriptive compression/1 option.
  select_option(compression(Format), Options1, Options2, gzip),
  merge_options([format(Format)], Options2, Options3),
  call_to_stream1(Format, Out, Goal_3, [output_stream{}], Metadata, Options3).

% Allow the stream to be compressed.
call_to_stream1(none, Out, Goal_3, Metadata1, Metadata2, Options) :- !,
  call_to_stream2(Out, Goal_3, Metadata1, Metadata2, Options).
call_to_stream1(_, Out1, Goal_3, Metadata1, Metadata2, Options1) :-
  merge_options([close_parent(false)], Options1, Options2),
  setup_call_cleanup(
    zopen(Out1, Out2, Options2),
    call_to_stream2(Out2, Goal_3, Metadata1, Metadata2, Options1),
    close(Out2)
  ).

call_to_stream2(Out1, Goal_3, Metadata1, Metadata4, Options) :-
  % Allows the stream to be hashed.
  setup_call_cleanup(
    open_hash(Out1, Out2, Options),
    (
      call(Goal_3, Out2, Metadata1, Metadata2),
      stream_hash_metadata(Out2, Metadata2, Metadata3, Options)
    ),
    close(Out2)
  ), !,
  stream_hash_metadata(Out1, Metadata3, Metadata4, Options).
call_to_stream2(Out, Goal_3, Metadata1, Metadata2, _) :-
  call(Goal_3, Out, Metadata1, Metadata2).



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

guess_string_encoding(String, Encoding2) :-
  % Encoding is mentioned in the XML declaration.
  string_phrase('XMLDecl'(_, Encoding1, _), String, _),
  nonvar(Encoding1), !,
  downcase_atom(Encoding1, Encoding2).
guess_string_encoding(String, Encoding3) :-
  open_binary_string(String, In),
  setup_call_cleanup(
    process_open(uchardet, In, Out),
    (
      read_string(Out, Encoding1),
      split_string(Encoding1, "", "\n", [Encoding2|_])
    ),
    close(Out)
  ),
  downcase_atom(Encoding2, Encoding3).



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



%! print_err(+Err:stream) is det.
%
% Print content from an error stream using
% `print_message(warning,err(<STRING>))'.

print_err(Err) :-
  call_cleanup(
    print_err_(Err),
    close(Err)).

print_err_(Err) :-
  read_line_to_string(Err, String),
  (   String == end_of_file
  ->  true
  ;   print_message(warning, err(String)),
      print_err_(Err)
  ).



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
recode_stream(FromEnc, In1, utf8, In2) :-
  indent_debug(1, encoding, "> ~a RECODE", [FromEnc]),
  process_open(iconv, In1, ['-c','-f',FromEnc,'-t','utf-8'], In2).



%! sort_stream(+In1:stream, -In2:stream) is det.
%! sort_stream(+In1:stream, -In2:stream, +Options:list(compound)) is det.
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
%
% Other options are passed to process_open/5.

sort_stream(In1, In2) :-
  sort_stream(In1, In2, []).


sort_stream(In1, In2, Options1) :-
  select_option(env(Env1), Options1, Options2, []),
  select_option(utf8(Utf8), Options2, Options3, false),
  (Utf8 == true -> Env2 = [] ; Env2 = ['LC_ALL'='C']),
  append(Env1, Env2, Env3),
  merge_options([env(Env3)], Options3, Options4),
  (   \+ option(temporary_directory(_), Options3),
      setting(temporary_directory, Directory),
      ground(Directory)
  ->  merge_options([temporary_directory(Directory)], Options3, Arguments)
  ;   Arguments = Options3
  ),
  process_flags(sort_flag, Arguments, Flags),
  process_open(sort, In1, Flags, In2, Options4).

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



%! true_metadata(+In:stream, +Metadata1:list(dict),
%!               -Metadata2:list(dict)) is det.
%
% Succeed.

true_metadata(_, Metadata, Metadata).



%! true_metadata(-Metadata2:list(dict), +In:stream, +Metadata1:list(dict),
%!               -Metadata2:list(dict)) is det.
%
% Return metadata and succeed.

true_metadata(Metadata, _, Metadata, Metadata).



%! true_null(+In:stream, +Metadata1:list(dict), -Metadata2:list(dict)) is det.
%
% Copy content to null stream and succeed.

true_null(In, Metadata, Metadata) :-
  call_to_null(copy_stream_data(In)).





% COMMON METADATA EXTRACTIONS

%! metadata_content_type(+Metadata:list(dict), -MediaType:compound) is semidet.
%
% Fails if the metadata does not contain a `Content-Type' header.

metadata_content_type(Metadata, MediaType) :-
  member(Dict1, Metadata),
  dict_get(headers, Dict1, Dict2),
  dict_get('content-type', Dict2, [ContentType|_]), !,
  (   atom_phrase('content-type'(MediaType), ContentType)
  ->  true
  ;   type_error(media_type, ContentType)
  ).



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
