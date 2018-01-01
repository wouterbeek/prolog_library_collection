%! call_on_uri(+UriSpec:term, :Goal_3) is nondet.
%! call_on_uri(+UriSpec:term, :Goal_3, +Options:list(compound)) is nondet.
%
% @arg UriSpec is either an atomic URI, an atomic file name, a
%      compound term uri/5 as handled by uri_comps/2, or a compound
%      term as processed by absolute_file_name/[2,3].

call_on_uri(UriSpec, Goal_3) :-
  call_on_uri(UriSpec, Goal_3, []).


call_on_uri(UriSpec, Goal_3, Options) :-
  uri_spec(UriSpec, read, Uri),
  uri_components(Uri, uri_components(Scheme,_,_,_,_)),
  call_on_uri_scheme(Scheme, Uri, Goal_3, Metadata, Options),
  ignore(option(metadata(Metadata), Options)).

uri_spec(uri(Uri), _, Uri) :- !.
uri_spec(uri(Scheme,Authority,Segments,Query,Fragment), _, Uri) :- !,
  uri_comps(Uri, uri(Scheme,Authority,Segments,Query,Fragment)).
uri_spec(Uri, _, Uri) :-
  uri_is_global(Uri), !.
uri_spec(FileSpec, Mode, Uri) :-
  absolute_file_name(FileSpec, File, [access(Mode),expand(true)]),
  uri_file_name(Uri, File).

call_on_uri_scheme(file, Uri, Goal_3, Metadata3, Options) :- !,
  uri_file_name(Uri, File),
  setup_call_cleanup(
    open(File, read, Stream, Options),
    (
      stream_ext:call_on_stream(Stream, Goal_3, [uri{mode:read,uri:Uri}], Metadata1, Options),
      stream_hash_metadata(Stream, Metadata1, Metadata2, Options)
    ),
    close(Stream)
  ),
  % Due to non-determinism, stream may or may not yet be closed..
  (var(Metadata2) -> Metadata3 = Metadata1 ; Metadata3 = Metadata2).
call_on_uri_scheme(http, Uri, Goal_3, Metadata, Options) :- !,
  http_client2:call_on_http(Uri, Goal_3, Metadata, Options).
call_on_uri_scheme(https, Uri, Goal_3, Metadata, Options) :-
  call_on_uri_scheme(http, Uri, Goal_3, Metadata, Options).



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
  ;   guess_stream_encoding(In1, 10 000, FromEnc)
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
