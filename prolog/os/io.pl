:- module(
  io,
  [
    call_on_stream/2,        % +Source, :Goal_3
    call_on_stream/3,        % +Source, :Goal_3, +SourceOpts
    call_onto_stream/3,      % +Source, +Sink, :Goal_4
    call_onto_stream/5,      % +Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts
    call_onto_streams/4,     % +Source, +Sink1, +Sink2, :Goal_5
    call_onto_streams/6,     % +Source
                             % +Sink1
                             % +Sink2
                             % :Goal_5
                             % +SourceOpts
                             % +SinkOpts
    call_onto_streams/7,     % +Source
                             % +Sink1
                             % +Sink2
                             % :Goal_5
                             % +SourceOpts
                             % +Sink1Opts
                             % +Sink2Opts
    call_to_atom/2,          % :Goal_1, -A
    call_to_codes/2,         % :Goal_1, -Cs
    call_to_stream/2,        % +Sink, :Goal_1
    call_to_stream/3,        % +Sink, :Goal_1, +SinkOpts
    call_to_streams/3,       % +Sink1, +Sink2, :Goal_2
    call_to_streams/4,       % +Sink1, +Sink2, :Goal_2, +SinkOpts
    call_to_streams/5,       % +Sink1, +Sink2, :Goal_2, +Sink1Opts, +Sink2Opts
    call_to_string/2,        % :Goal_1, -Str
    copy_stream_data/4,      % -Out, +In, +InPath1, -InPath2
    process_open/3,          % +Cmd, +In, -Out
    process_open/4,          % +Cmd, +In, +Args, -Out
    read_line_to_atom/2,     % +In, -A
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
    read_stream_to_string/2, % +In, -Str
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

The following debug flags are used:

  * io

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07-2016/12
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(hash_stream)).
:- use_module(library(http/http_io)).
:- use_module(library(iostream)).
:- use_module(library(os/archive_ext)).
:- use_module(library(process)).
:- use_module(library(typecheck)).
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





%! call_on_stream(+Source, :Goal_3) is det.
%! call_on_stream(+Source, :Goal_3, +SourceOpts) is det.
%
% The following call is made: `call(Goal_3, In)`.
%
% The following options are supported:
%
%   * entry_name(?atom)
%
%   * metadata(-dict)
%
%     * filetype(oneof([block_device,character_device,directory,fifo,file,link,socket]))
%
%     * format(atom)
%
%     * link_target(atom)
%
%     * mtime(float)
%
%     * name(atom)
%
%     * size(nonneg)
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
    open_any2(Source, read, In, Close_0, InPath1, SourceOpts),
    (
      call_on_stream0(In, Goal_3, InPath1, InPath2, SourceOpts),
      close_metadata(Close_0, InPath2, InPath3, SourceOpts)
    ),
    close_any2(Close_0)
  ),
  % @bug: InPath3 is now uninstantiated.
  ignore(option(metadata(InPath3), SourceOpts)).


% HTTP error status code may result in no stream at all.
call_on_stream0(In, _, InPath, InPath, _) :-
  var(In), !.
% Empty input stream.
call_on_stream0(In, _, InPath, InPath, _) :-
  at_end_of_stream(In), !.
% Data input stream.
call_on_stream0(In1, Goal_3, [InEntry|InPath1], InPath2, SourceOpts) :-
  get_dict(format, InEntry, raw),
  get_dict(name, InEntry, data), !,
  call(Goal_3, In, [InEntry|InPath1], InPath2).
% Compressed and/or packaged input stream.
call_on_stream0(In, Goal_3, InPath1, InPath2, SourceOpts) :-
  findall(format(Format), archive_format(Format, true), Formats),
  setup_call_cleanup(
    archive_open(stream(In), Arch, [close_parent(false),filter(all)|Formats]),
    (
      indent_debug(in, io, "R» ~w → ~w", [In,Arch]),
      call_on_archive0(Arch, Goal_3, InPath1, InPath2, SourceOpts),
      indent_debug(out, io, "«R ~w", [Arch])
    ),
    archive_close(Arch)
  ).


call_on_archive0(Arch, Goal_3, [InEntry2|InPath1], InPath4, SourceOpts) :-
  option(entry_name(EntryNameMatch), SourceOpts, _),
  archive_property(Arch, filter(Filters)),
  repeat,
  (   archive_next_header(Arch, EntryName)
  ->  findall(Prop, archive_header_property(Arch, Prop), Props),
      dict_create(InEntry1, [filters(Filters),name(EntryName)|Props]),
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
          EntryName == EntryNameMatch
      ->  !
      ;   % If the given entry name did not match the current archive
          % header then fail, because we are in the wrong entry
          % branch.
          var(EntryNameMatch)
      ),
      (   memberchk(filetype(file), Props)
      ->  setup_call_cleanup(
            archive_open_entry(Arch, In),
            (
              indent_debug(in, io, "R» ~w → ~a → ~w", [Arch,EntryName,In]),
              call_on_stream0(
                In,
                Goal_3,
                [InEntry1,InEntry2|InPath1],
                InPath3,
                SourceOpts
              ),
              % close_metadata/3 also emits the closing debug message.
              close_metadata(close(In), InPath3, InPath4)
            ),
            close_any2(close(In))
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
    open_any2(Sink, Mode, Out, Close_0, OutPath1, SinkOpts),
    (
      call_to_compressed_stream(Out, Goal_1, OutPath1, OutPath2, SinkOpts),
      close_metadata(Close_0, OutPath2, OutPath3, SinkOpts)
    ),
    close_any2(Close_0)
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
    zopen(Out1, Out2, [close_parent(false),format(Format)]),
    (
      indent_debug(in, io, "ZW» ~w → ~a → ~w", [Out1,Format,Out2]),
      call(Goal_1, Out2),
      indent_debug(out, io, "«ZW ~w", [Out2])
    ),
    close(Out2)
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
      open_any2(Sink1, Mode1, Out1, Close1_0, OutPath1a, Sink1Opts),
      open_any2(Sink2, Mode2, Out2, Close2_0, OutPath1b, Sink2Opts)
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
      close_metadata(Close1_0, OutPath2b, OutPath3b, Sink2Opts),
      close_metadata(Close2_0, OutPath2a, OutPath3a, Sink1Opts)
    ),
    (
      close_any2(Close1_0),
      close_any2(Close2_0)
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



%! close_any2(+Close_0) is det.

close_any2(close(Stream)) :- !,
  close(Stream).
close_any2(true).



%! close_medata(+Close_0, +Path1, -Path2) is det.
%! close_medata(+Close_0, +Path1, -Path2, +Opts) is det.
%
% Options are passed to close_metadata_hash/2.

close_metadata(Close_0, Path1, Path2) :-
  close_metadata(Close_0, Path1, Path2, []).


close_metadata(close(Stream), [Entry1|Path], [Entry2|Path], Opts) :- !,
  stream_metadata(Stream, Entry1, Entry2),
  stream_property(Stream, mode(Mode)),
  close_metadata_hash(Stream, Opts),
  (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
  indent_debug(out, io, "«~s ~w", [Lbl,Stream]).
close_metadata(true, Path, Path, _).


%! close_metadata_hash(+Stream, +Opts) is det.
%
% The following options are supported:
%
%   * The following hash options are supported:
%
%     * md5(-atom)
%
%     * sha1(-atom)
%
%     * sha224(-atom)
%
%     * sha256(-atom)
%
%     * sha384(-atom)
%
%     * sha512(-atom)

close_metadata_hash(Stream, Opts) :-
  (   option(md5(Hash), Opts)
  ->  true
  ;   option(sha1(Hash), Opts)
  ->  true
  ;   option(sha224(Hash), Opts)
  ->  true
  ;   option(sha256(Hash), Opts)
  ->  true
  ;   option(sha384(Hash), Opts)
  ->  true
  ;   option(sha512(Hash), Opts)
  ->  true
  ), !,
  stream_hash(Stream, Hash).
close_metadata_hash(_, _).



%! copy_stream_data(-Out, +In, +InPath1, -InPath2) is det.

copy_stream_data(Out, In, InPath, InPath) :-
  copy_stream_data(In, Out).



%! open_any2(+Spec, +Mode, -Stream, -Close_0, -Path, +Opts) is det.
%
% The following options are supported:
%
%   * The following hash options are supported:
%
%     * md5(--atom)
%
%     * sha1(--atom)
%
%     * sha224(--atom)
%
%     * sha256(--atom)
%
%     * sha384(--atom)
%
%     * sha512(--atom)
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

open_any2(stream(Stream), Mode, Stream, Close_0, Path, Opts) :- !,
  open_any2(Stream, Mode, Stream, Close_0, Path, Opts).
open_any2(Spec, Mode, Stream2, Close_0, Path, Opts) :-
  (   is_http_iri(Spec)
  ->  Mode = read,
      http_io:http_open_any(Spec, Stream1, Path, Opts),
      Close_0 = close(Stream2)
  ;   (   atom(Spec)
      ->  % Allow ‘~’ to be used.
          expand_file_name(Spec, [ExpandedSpec|_])
      ;   ExpandedSpec = Spec
      ),
      open_any(ExpandedSpec, Mode, Stream1, _, Opts),
      (   ExpandedSpec == Stream1
      ->  Close_0 = true,
          Entry1 = _{stream: Stream1}
      ;   absolute_file_name(ExpandedSpec, File),
          Close_0 = close(Stream2),
          Entry1 = _{file: File},
          (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
          indent_debug(in, io, "~s» ~w → ~w", [Lbl,ExpandedSpec,Stream1])
      ),
      put_dict(mode, Entry1, Mode, Entry2),
      Path = [Entry2]
  ),
  open_any2_hash(Stream1, Stream2, Opts).


open_any2_hash(Stream1, Stream2, Opts) :-
  (   option(md5(_), Opts)
  ->  Alg = md5
  ;   option(sha1(_), Opts)
  ->  Alg = sha1
  ;   option(sha224(_), Opts)
  ->  Alg = sha224
  ;   option(sha256(_), Opts)
  ->  Alg = sha256
  ;   option(sha384(_), Opts)
  ->  Alg = sha384
  ;   option(sha512(_), Opts)
  ->  Alg = sha512
  ), !,
  open_hash_stream(Stream1, Stream2, [algorithm(Alg)]).
open_any2_hash(Stream, Stream, _).



%! read_line_to_atom(+In, -A) is det.

read_line_to_atom(In, A) :-
  read_line_to_codes(In, Cs),
  atom_codes(A, Cs).



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



%! process_open(+Cmd, +In, +Out) is det.
%! process_open(+Cmd, +In, +Args, +Out) is det.

process_open(Cmd, In1, Out) :-
  process_open(Cmd, In1, [], Out).


process_open(Cmd, In1, Args, Out) :-
  stream_property(In1, type(Type)),
  process_create(
    path(Cmd),
    Args,
    [stderr(pipe(Err)),stdin(pipe(In2)),stdout(pipe(Out))]
  ),
  set_stream(In2, type(Type)),
  copy_stream_data(In1, In2),
  close(In2),
  read_string(Err, Msg),
  (Msg == "" -> true ; msg_warning(Msg)).



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



%! stream_metadata(+Stream, +Entry1, -Entry2) is det.
%
% Succeeds if Entry2 is a dictionary that contains the metadata
% properties of Stream.  The following metadata properties are
% included:
%
%   * byte_count(nonneg)
%
%   * char_count(nonneg)
%
%   * line_count(nonneg)
%
%   * newline(oneof([dos,posix]))

stream_metadata(Stream, Entry1, Entry2) :-
  stream_property(Stream, position(Pos)),
  stream_position_data(byte_count, Pos, NumBytes),
  stream_position_data(char_count, Pos, NumChars),
  stream_position_data(line_count, Pos, NumLines),
  stream_property(Stream, newline(Newline)),
  merge_dicts(
    Entry1,
    _{
      byte_count: NumBytes,
      char_count: NumChars,
      line_count: NumLines,
      newline: Newline
    },
    Entry2
  ).
