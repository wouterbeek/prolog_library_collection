:- module(
  io,
  [
    archive_path/2,          % +Source, -InPath
    call_on_stream/2,        % +Source, :Goal_3
    call_on_stream/3,        % +Source, :Goal_3, +SourceOpts
    call_onto_stream/3,      % +Source, +Sink, :Goal_4
    call_onto_stream/5,      % +Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts
    call_onto_streams/4,     % +Source, +Sink1, +Sink2, :Goal_5
    call_onto_streams/6,     % +Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +SinkOpts
    call_onto_streams/7,     % +Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +Sink1Opts, +Sink2Opts
    call_to_atom/2,          % :Goal_1, -A
    call_to_codes/2,         % :Goal_1, -Cs
    call_to_stream/2,        % +Sink, :Goal_1
    call_to_stream/3,        % +Sink, :Goal_1, +SinkOpts
    call_to_streams/3,       % +Sink1, +Sink2, :Goal_2
    call_to_streams/4,       % +Sink1, +Sink2, :Goal_2, +SinkOpts
    call_to_streams/5,       % +Sink1, +Sink2, :Goal_2, +Sink1Opts, +Sink2Opts
    call_to_string/2,        % :Goal_1, -Str
    close_any2/1,            % +Close
    close_any2/2,            % +Close, +Mode
    copy_stream_data/4,      % +In, +InPath1, -InPath2, +Out
    is_archive_file/1,       % +File
    open_any2/6,             % +Spec, +Mode, -Stream, -Close, -Path, +SourceOpts
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
   %read_stream_to_codes/2,  % +In, -Cs
    read_stream_to_string/2, % +In, -Str
    read_to_atom/2,          % +Source, -A
    read_to_codes/2,         % +Source, -Cs
    read_to_string/2,        % +Source, -Str
    source_base_uri/2,       % +InPath, -BaseUri
    source_entry_name/2,     % +InPath, -EntryName
    source_entry_segments/2, % +InPath, -Segments
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

The following external programs are used:

  * iconv

  * uchardet

The following debug flags are used:

  * io(close)

  * io(open)

  * io(recode)

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07-2017/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(hash_stream)).
:- use_module(library(http/http_io)).
:- use_module(library(os_ext)).
:- use_module(library(print_ext)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(xml/xml_parser)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- meta_predicate
    call_on_archive0(+, 3, +, -, +),
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream0(+, 3, +, -, +),
    call_onto_stream(+, +, 4),
    call_onto_stream(+, +, 4, +, +),
    call_onto_stream0(+, 4, +, +, +, -),
    call_onto_streams(+, +, +, 5),
    call_onto_streams(+, +, +, 5, +, +),
    call_onto_streams(+, +, +, 5, +, +, +),
    call_onto_streams0(+, +, 5, +, +, +, +, -),
    call_to_atom(1, -),
    call_to_codes(1, -),
    call_to_compressed_stream(+, 1, +, -, +),
    call_to_something(1, +, -),
    call_to_stream(+, 1),
    call_to_stream(+, 1, +),
    call_to_streams(+, +, 2),
    call_to_streams(+, +, 2, +),
    call_to_streams(+, +, 2, +, +),
    call_to_streams0(+, +, 2, +, -, +, +, -, +),
    call_to_string(1, +).

:- multifile
    error:has_type/2.

error:has_type(read_mode, Term) :-
  error:has_type(oneof([read]), Term).

error:has_type(write_mode, Term) :-
  error:has_type(oneof([append,write]), Term).





%! archive_path(+Source, -InPath) is nondet.
%
% Enumerates the entry paths in the archive stored at Source.

archive_path(Source, InPath) :-
  call_on_stream(Source, archive_path0(InPath)).

archive_path0(InPath, _, InPath, InPath).



%! call_on_stream(+Source, :Goal_3) is det.
%! call_on_stream(+Source, :Goal_3, +SourceOpts) is det.
%
% The following call is made:
%
% ```
% call(:Goal_3, +In, +InPath1, -InPath2)
% ```
%
% The following options are supported:
%
%   * from_encoding(+atom)
%
%     The encoding of the Source.  This is recoded to UTF-8 at the
%     data/raw entry level.
%
%   * entry_name(?atom)
%
%   * metadata(-dict)
%
%     * '@id'(atom)
%
%     * '@type'(oneof([entry]))
%
%     * filetype(oneof([block_device,character_device,directory,fifo,file,link,socket]))
%
%     * format(atom)
%
%     * link_target(atom)
%
%     * mtime(float)
%
%     * size(nonneg)
%
%   * recode(+boolean)
%
%     Whether or not source data streams should be recoded.  Default
%     is `false`.  Recoding uses the value of option from_encoding/1
%     when given of is guessed using the external process ‘uchardet’.
%
%   * Other options are passed to:
%
%       * archive_open/3
%
%       * open_any2/6
%
% @tbd Support paths of entry names (option entry_name/1) for nested
% archives.

call_on_stream(Source, Goal_3) :-
  call_on_stream(Source, Goal_3, []).


call_on_stream(Source, Goal_3, SourceOpts) :-
  setup_call_cleanup(
    open_any2(Source, read, In, Close, InPath1, SourceOpts),
    (
      call_on_stream0(In, Goal_3, InPath1, InPath2, SourceOpts),
      close_metadata(Close, InPath2, InPath3, SourceOpts)
    ),
    close_any2(Close, read)
  ),
  ignore(option(metadata(InPath3), SourceOpts)).


% Empty input stream.  E.g., 204 reply body.
call_on_stream0(In, Goal_3, InPath1, InPath2, _) :-
  at_end_of_stream(In), !,
  call(Goal_3, In, InPath1, InPath2).
% Already a stream, leave as is.
call_on_stream0(In, Goal_3, [InEntry1], InPath, _) :-
  _{'@type': stream} :< InEntry1, !,
  call(Goal_3, In, [InEntry1], InPath).
% Leaf/raw archive entry.
call_on_stream0(In1, Goal_3, InPath1, InPath2, SourceOpts) :-
  InPath1 = [InEntry|_],
  _{'@type': entry, format: raw} :< InEntry, !,
  setup_call_cleanup(
    recode_stream(In1, In2, Close, SourceOpts),
    call(Goal_3, In2, InPath1, InPath2),
    close_any2(Close, read)
  ).
% Non-leaf/raw archive entry.
call_on_stream0(In, Goal_3, InPath1, InPath2, SourceOpts) :-
  findall(format(Format), archive_format(Format, true), Formats),
  setup_call_cleanup(
    (
      archive_open(stream(In), Arch, [close_parent(false),filter(all)|Formats]),
      indent_debug(in, io(open), "R» ~w → ~w", [In,Arch])
    ),
    call_on_archive0(Arch, Goal_3, InPath1, InPath2, SourceOpts), %NONDET
    (
      indent_debug(out, io(close), "«R ~w", [Arch]),
      archive_close(Arch)
    )
  ).

call_on_archive0(Archive, Goal_3, [InEntry2|InPath1], InPath3, SourceOpts) :-
  option(entry_name(EntryNameMatch), SourceOpts, _),
  archive_property(Archive, filter(Filters)),
  repeat,
  (   % @bug error(archive_error(1001,Truncated input file (needed
      % 2049346009 bytes, only 0 available)),_12880)
      archive_next_header(Archive, EntryName)
  ->  findall(Prop, archive_header_property(Archive, Prop), Props),
      dict_create(
        InEntry1,
        ['@id'(EntryName),'@type'(entry),filters(Filters)|Props]
      ),
      % If the entry name is `data` then proceed.  If the entry name
      % is uninstantiated then proceed.  If entry name is instantiated
      % and does not occur in the current archive header then fail
      % (repeat/0 will iterate to the next archive header).
      (   % This is the last entry in this nested branch.  We
          % therefore close the choicepoint created by repeat/0.  Not
          % closing this choicepoint would cause archive_next_header/2
          % to throw an exception.
          EntryName == data,
          InEntry1.format == raw
      ->  !
      ;   % If the given entry name occurs in the current archive
          % header then _red cut_, because we are in the correct entry
          % branch.
          EntryNameMatch == EntryName
      ->  !
      ;   % Instantiate the entry name to support the ‘entry_name’
          % option.
          EntryNameMatch = EntryName
      ),
      (   memberchk(filetype(file), Props)
      ->  setup_call_cleanup(
            (
              archive_open_entry(Archive, In),
              indent_debug(in, io(open), "R» ~w → ~a → ~w", [Archive,EntryName,In])
            ),
            (
              call_on_stream0(
                In,
                Goal_3,
                [InEntry1,InEntry2|InPath1],
                InPath2,
                SourceOpts
              ),
              close_metadata(In, InPath2, InPath3)
            ),
            (
              debug_close(In, read),
              close(In)
            )
          )
      ;   % Skip over non-file entries.
          fail
      )
  ;   % There are no more entries in this archive.
      !,
      fail
  ).



%! call_onto_stream(+Source, +Sink, :Goal_4) is det.
%! call_onto_stream(+Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts) is det.
%
% The following call is made:
% `call(Goal_4, +In, +InPath1, -InPath2, +Out)`.
%
% Options are passed to:
%
%   * call_on_stream/3
%
%   * call_to_stream/3

call_onto_stream(Source, Sink, Goal_4) :-
  call_onto_stream(Source, Sink, Goal_4, [], []).


call_onto_stream(Source, Sink, Goal_4, SourceOpts, SinkOpts) :-
  call_on_stream(
    Source,
    call_onto_stream0(Sink, Goal_4, SinkOpts),
    SourceOpts
  ).


call_onto_stream0(Sink, Goal_4, SinkOpts, In, InPath1, InPath2) :-
  goal_manipulation(Goal_4, [In,InPath1,InPath2], Goal_1),
  call_to_stream(Sink, Goal_1, SinkOpts).



%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +SinkOpts) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +Sink1Opts, +Sink2Opts) is det.

call_onto_streams(Source, Sink1, Sink2, Goal_5) :-
  call_onto_streams(Source, Sink1, Sink2, Goal_5, [], [], []).


call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, SinkOpts) :-
  call_onto_streams(
    Source,
    Sink1,
    Sink2,
    Goal_5,
    SourceOpts,
    SinkOpts,
    SinkOpts
  ).


call_onto_streams(
  Source,
  Sink1,
  Sink2,
  Goal_5,
  SourceOpts,
  Sink1Opts,
  Sink2Opts
) :-
  call_on_stream(
    Source,
    call_onto_streams0(Sink1, Sink2, Goal_5, Sink1Opts, Sink2Opts),
    SourceOpts
  ).


call_onto_streams0(
  Sink1,
  Sink2,
  Goal_5,
  Sink1Opts,
  Sink2Opts,
  In,
  InPath1,
  InPath2
) :-
  goal_manipulation(Goal_5, [In,InPath1,InPath2], Goal_2),
  call_to_streams(Sink1, Sink2, Goal_2, Sink1Opts, Sink2Opts).



%! call_to_atom(:Goal_1, -A) is det.

call_to_atom(Goal_1, A) :-
  call_to_something(Goal_1, atom, A).



%! call_to_codes(:Goal_1, -Cs) is det.

call_to_codes(Goal_1, Cs) :-
  call_to_something(Goal_1, codes, Cs).



%! call_to_stream(+Sink, :Goal_1) is det.
%! call_to_stream(+Sink, :Goal_1, +SinkOpts) is det.
%
% The following options are supported:
%
%   * compression(+boolean)
%
%     Whether gzip file compression should be used.  The default is
%     `true`.
%
%   * metadata(-dict)
%
%     * compression(oneof([gzip,none]))
%
%     * mode(oneof([append,write]))
%
%   * mode(+oneof([append,write]))
%
%     The default is `write`.
%
%   * Other options are passed to open_any2/6.

call_to_stream(Sink, Goal_1) :-
  call_to_stream(Sink, Goal_1, []).


call_to_stream(Sink, Goal_1, SinkOpts) :-
  option(mode(Mode), SinkOpts, write),
  must_be(write_mode, Mode),
  setup_call_cleanup(
    open_any2(Sink, Mode, Out, Close, OutPath1, SinkOpts),
    (
      call_to_compressed_stream(Out, Goal_1, OutPath1, OutPath2, SinkOpts),
      close_metadata(Close, OutPath2, OutPath3, SinkOpts)
    ),
    close_any2(Close, Mode)
  ),
  % @bug: OutPath3 is now uninstantiated.
  ignore(option(metadata(OutPath3), SinkOpts)).


call_to_compressed_stream(
  Out1,
  Goal_1,
  [OutEntry1|OutPath],
  [OutEntry2|OutPath],
  SinkOpts
) :-
  option(compression(true), SinkOpts, true), !,
  Format = gzip,
  put_dict(compression, OutEntry1, Format, OutEntry2),
  setup_call_cleanup(
    (
      zopen(Out1, Out2, [close_parent(false),format(Format)]),
      indent_debug(in, io(open), "ZW» ~w → ~a → ~w", [Out1,Format,Out2])
    ),
    call(Goal_1, Out2),
    (
      close(Out2),
      indent_debug(out, io(close), "«ZW ~w", [Out2])
    )
  ).
call_to_compressed_stream(Out, Goal_1, OutPath, OutPath, _) :-
  call(Goal_1, Out).



%! call_to_streams(+Sink1, +Sink2, :Goal_2) is det.
%! call_to_streams(+Sink1, +Sink2, :Goal_2, +SinkOpts) is det.
%! call_to_streams(+Sink1, +Sink2, :Goal_2, +Sink1Opts, +Sink2Opts) is det.
%
% The following options are supported:
%
%   * compression(+boolean) Whether gzip file compression should be
%   used.  The default is `true`.
%
%   * metadata(-dict)
%
%   * mode(+oneof([append,write])) The default is `write`.
%
%   * Other options are passed to open_any2/6.

call_to_streams(Sink1, Sink2, Goal_2) :-
  call_to_streams(Sink1, Sink2, Goal_2, [], []).


call_to_streams(Sink1, Sink2, Goal_2, SinkOpts) :-
  call_to_streams(Sink1, Sink2, Goal_2, SinkOpts, SinkOpts).


call_to_streams(Sink1, Sink2, Goal_2, Sink1Opts, Sink2Opts) :-
  option(mode(Mode1), Sink1Opts, write),
  option(mode(Mode2), Sink2Opts, write),
  setup_call_cleanup(
    (
      open_any2(Sink1, Mode1, Out1, Close1, OutPath1a, Sink1Opts),
      open_any2(Sink2, Mode2, Out2, Close2, OutPath1b, Sink2Opts)
    ),
    (
      call_to_streams0(
        Out1,
        Out2,
        Goal_2,
        OutPath1a,
        OutPath1b,
        Sink1Opts,
        OutPath2a,
        OutPath2b,
        Sink2Opts
      ),
      close_metadata(Close1, OutPath2b, OutPath3b, Sink2Opts),
      close_metadata(Close2, OutPath2a, OutPath3a, Sink1Opts)
    ),
    (
      close_any2(Close1, Mode1),
      close_any2(Close2, Mode2)
    )
  ),
  % @bug: OutPath3a is now uninstantiated.
  % @bug: OutPath3b is now uninstantiated.
  ignore(option(metadata1(OutPath3a), Sink1Opts)),
  ignore(option(metadata2(OutPath3b), Sink2Opts)).


call_to_streams0(
  Out1a,
  Out2,
  Goal_2,
  OutPath1a,
  OutPath1b,
  Sink1Opts,
  OutPath2a,
  OutPath2b,
  Sink2Opts
) :-
  call_to_compressed_stream(
    Out1a,
    {Out2,Goal_2,OutPath1a,OutPath2a,Sink2Opts}/[Out1b]>>(
      goal_manipulation(Goal_2, [Out1b], Goal_1),
      call_to_compressed_stream(Out2, Goal_1, OutPath1a, OutPath2a, Sink2Opts)
    ),
    OutPath1b,
    OutPath2b,
    Sink1Opts
  ).



%! call_to_string(:Goal_1, -Str) is det.

call_to_string(Goal_1, Str) :-
  call_to_something(Goal_1, string, Str).



%! close_medata(+Close, +Path1, -Path2) is det.
%! close_medata(+Close, +Path1, -Path2, +Opts) is det.
%
% Options are passed to close_metadata_hash/2.

close_metadata(Close, Path1, Path2) :-
  close_metadata(Close, Path1, Path2, []).


close_metadata(Stream, [Entry1|Path], [Entry2|Path], Opts) :-
  is_stream(Stream), !,
  stream_metadata(Stream, Entry1, Entry2),
  close_metadata_hash(Stream, Opts).
close_metadata(true, Path, Path, _).


%! copy_stream_data(+In, +InPath1, -InPath2, +Out) is det.

copy_stream_data(In, InPath, InPath, Out) :-
  copy_stream_data(In, Out).



%! archive_file_extension(+Extension:atom) is semidet.
%! archive_file_extension(-Extension:atom) is multi.
%
% Often occurring file extensions for archvies.

archive_file_extension(cab).
archive_file_extension(rar).
archive_file_extension(tar).
archive_file_extension(xar).
archive_file_extension(zip).



%! is_archive_file(+File) is semidet.
%
% Succeeds if File is a common way of naming an archive file.

is_archive_file(File) :-
  file_extensions(File, Exts),
  once((
    member(Ext, Exts),
    archive_file_extension(Ext)
  )).



%! read_mode(+Mode) is semidet.
%! read_mode(-Mode) is multi.
%
% Mode is one of the following values:
%
%   * read

read_mode(read).



%! read_stream_to_atom(+In, -A) is det.

read_stream_to_atom(In, A) :-
  read_stream_to_codes(In, Cs),
  atom_codes(A, Cs).



%! read_stream_to_string(+In, -Str) is det.

read_stream_to_string(In, Str) :-
  read_stream_to_codes(In, Cs),
  string_codes(Str, Cs).



%! read_to_atom(+Source, -A) is det.

read_to_atom(Source, A) :-
  read_to_codes(Source, Cs),
  atom_codes(A, Cs).



%! read_to_codes(+Source, -Cs) is det.

read_to_codes(Source, Cs) :-
  call_on_stream(Source, read_stream_to_codes(Cs)).

read_stream_to_codes(Cs, In, InPath, InPath) :-
  read_stream_to_codes(In, Cs).



%! read_to_string(+Source, -Str) is det.

read_to_string(Source, Str) :-
  read_to_codes(Source, Cs),
  string_codes(Str, Cs).



%! source_base_uri(+InPath, -BaseUri) is semidet.

source_base_uri(InPath, BaseUri) :-
  member(InEntry, InPath),
  (   _{'@id': Uri, '@type': uri} :< InEntry
  ->  !, uri_remove_fragment(Uri, BaseUri)
  ;   _{'@id': File, '@type': file} :< InEntry
  ->  !, uri_file_name(BaseUri, File)
  ).



%! source_entry_name(+InPath, -Name) is det.
%
% EntryName is either:
%
%   1. the concatenation of the non-raw archive entry names in InPath,
%      or
%
%   2. ‘data’.

source_entry_name(InPath, Name) :-
  source_entry_segments(InPath, Segments),
  Segments \== [], !,
  atomic_list_concat(Segments, /, Name).
source_entry_name(_, data).



%! source_entry_segments(+InPath, -Segments) is det.
%
% Segments is a list of non-raw archive enty names, in the order in
% which they appear in InPath.

source_entry_segments(InPath, Segments) :-
  source_entry_segments(InPath, [], Segments).

source_entry_segments([], Segments, Segments) :- !.
% Include segments of non-raw entires.
source_entry_segments([H1|T1], T2, Segments) :-
  _{'@type': entry, '@id': H2, format: Format} :< H1,
  Format \== raw, !,
  source_entry_segments(T1, [H2|T2], Segments).
% Skip the segments of raw archive entries and skip things that are
% not archive entries.
source_entry_segments([_|T], L, Segments) :-
  source_entry_segments(T, L, Segments).



%! write_mode(+Mode) is semidet.
%! write_mode(-Mode) is multi.
%
% Mode is one of the following values:
%
%   * append
%
%   * write

write_mode(append).
write_mode(write).





% HELPERS %

%! call_to_something(:Goal_1, +Type, -Result) is det.

call_to_something(Goal_1, Type, Result) :-
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      setup_call_cleanup(
        open_memory_file(Handle, write, Out),
        call(Goal_1, Out),
        close(Out)
      ),
      memory_file_to_something(Handle, Type, Result)
    ),
    free_memory_file(Handle)
  ).



%! close_any2(+Close) is det.

close_any2(Close) :-
  ground(Close),
  (is_stream(Close) -> close(Close) ; Close == true).
close_any2(Close) :-
  instantiation_error(Close).


%! close_any2(+Close, +Mode) is det.

close_any2(Close, Mode) :-
  ground(Close),
  (is_stream(Close) -> debug_close(Close, Mode), close(Close) ; Close == true).
close_any2(Close, _) :-
  instantiation_error(Close).



%! debug_close(+Stream, +Mode) is det.

debug_close(Stream, Mode) :-
  (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
  indent_debug(out, io(close), "«~s ~w", [Lbl,Stream]).



%! goal_manipulation(+Goal_M, +Args, -Goal_N) is det.

goal_manipulation(Mod:Goal_M, Args, Mod:Goal_N) :-
  Goal_M =.. Comps1,
  append(Comps1, Args, Comps2),
  Goal_N =.. Comps2.



%! memory_file_to_something(
%!   +Handle,
%!   +Type:oneof([atom,codes,string]),
%!   -Result
%! ) is det.

memory_file_to_something(Handle, atom, A) :- !,
  memory_file_to_atom(Handle, A).
memory_file_to_something(Handle, codes, Cs) :- !,
  memory_file_to_codes(Handle, Cs).
memory_file_to_something(Handle, string, Str) :- !,
  memory_file_to_string(Handle, Str).



%! open_any2(+Spec, +Mode, -Stream, -Close, -Path, +Opts) is det.
%
% The following specifications are supported:
%
%   * file(+compound)
%
%   * stream(+stream)
%
%   * string(+string)
%
%   * uri(+atom)
%
% The following options are supported:
%
%   * metadata(-dict)
%
%     * file(atom)
%
%   * Other options are passed to:
%
%     * open_any/5
%
%     * http_io:http_open_any/4

open_any2(Spec, Mode, Stream2, Close2, Path, Opts) :-
  open_any2_variant(Spec, Mode, Stream1, Close1, Path, Opts),
  (   Close1 == true
  ->  true
  ;   (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
      indent_debug(in, io(open), "~s» ~w → ~w", [Lbl,Spec,Stream1])
  ),
  open_any2_hash(Stream1, Close1, Stream2, Close2, Opts).


open_any2_variant(atom(A), Mode, Stream, Close, InPath, Opts) :- !,
  atom_string(A, Str),
  open_any2_variant(string(Str), Mode, Stream, Close, InPath, Opts).
open_any2_variant(file(Spec), Mode, Stream, Stream, [Entry], Opts) :- !,
  (   compound(Spec)
  ->  merge_options([access(Mode)], Opts, PathOpts),
      absolute_file_name(Spec, File, PathOpts)
  ;   % Allow ‘~’ to be used.
      expand_file_name(Spec, [File|_])
  ),
  open_options(Mode, OpenOpts),
  open(File, Mode, Stream, OpenOpts),
  Entry = _{'@id': File, '@type': file, mode: Mode}.
open_any2_variant(stream(Stream), Mode, Stream, true, [Entry], _) :- !,
  stream_property(Stream, mode(Mode)),
  Entry = _{'@type': stream, mode: Mode}.
open_any2_variant(string(Str), read, In, In, [InEntry], _) :- !,
  open_binary_string(Str, In),
  InEntry = _{'@type': string, mode: read}.
open_any2_variant(uri(Uri), read, In, In, InPath, Opts) :- !,
  http_io:http_open_any(Uri, In, InPath, Opts).
% Guess whether the specification is a file, stream or URI (no
% strings).
open_any2_variant(Spec1, Mode, Stream, Close, Path, Opts) :-
  (   is_stream(Spec1)
  ->  Spec2 = stream(Spec1)
  ;   atomic(Spec1)
  ->  (   uri_file_name(Spec1, File)
      ->  Spec2 = file(File)
      ;   uri_is_global(Spec1)
      ->  Spec2 = uri(Spec1)
      ;   Spec2 = file(Spec1)
      )
  ;   Spec2 = file(Spec1)
  ),
  open_any2_variant(Spec2, Mode, Stream, Close, Path, Opts).

open_options(Mode, [type(binary)]) :-
  read_mode(Mode), !.
open_options(Mode, [encoding(utf8),type(text)]) :-
  write_mode(Mode), !.
