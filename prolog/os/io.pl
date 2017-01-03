:- module(
  io,
  [
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
    copy_stream_data/4,      % -Out, +In, +InPath1, -InPath2
    guess_stream_encoding/2, % +In, -Enc
    open_binary_string/2,    % +Str, -In
    process_open/3,          % +Cmd, +In, -Out
    process_open/4,          % +Cmd, +In, +Args, -Out
    read_line_to_atom/2,     % +In, -A
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
    read_stream_to_string/2, % +In, -Str
    recode_stream/4,         % +In1, -In2, -Close, +Opts
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

The following debug flags are used:

  * io

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07-2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(hash_stream)).
:- use_module(library(http/http_io)).
:- use_module(library(os/archive_ext)).
:- use_module(library(print_ext)).
:- use_module(library(process)).
:- use_module(library(string_ext)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).
:- use_module(library(xml/xml_parse)).
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
%   * from_encoding(+atom)
%
%     The encoding of the Source.  This is recoded to UTF-8 at the
%     data/raw entry level.
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
    close_any2(Close)
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
call_on_stream0(In1, Goal_3, InPath1, InPath2, SourceOpts) :-
  InPath1 = [InEntry|_],
  get_dict(format, InEntry, raw),
  get_dict(name, InEntry, data), !,
  setup_call_cleanup(
    recode_stream(In1, In2, Close, SourceOpts),
    call(Goal_3, In2, InPath1, InPath2),
    close_any2(Close)
  ).
% Compressed and/or packaged input stream.
call_on_stream0(In, Goal_3, InPath1, InPath2, SourceOpts) :-
  findall(format(Format), archive_format(Format, true), Formats),
  setup_call_cleanup(
    archive_open(stream(In), Arch, [close_parent(false),filter(all)|Formats]),
    (
      indent_debug(in, io(open), "R» ~w → ~w", [In,Arch]),
      call_on_archive0(Arch, Goal_3, InPath1, InPath2, SourceOpts),
      indent_debug(out, io(close), "«R ~w", [Arch])
    ),
    archive_close(Arch)
  ).


call_on_archive0(Arch, Goal_3, [InEntry2|InPath1], InPath3, SourceOpts) :-
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
              indent_debug(in, io(open), "R» ~w → ~a → ~w", [Arch,EntryName,In]),
              call_on_stream0(
                In,
                Goal_3,
                [InEntry1,InEntry2|InPath1],
                InPath2,
                SourceOpts
              ),
              % close_metadata/3 also emits the closing debug message.
              close_metadata(In, InPath2, InPath3)
            ),
            close(In)
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
    close_any2(Close)
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
      indent_debug(in, io(open), "ZW» ~w → ~a → ~w", [Out1,Format,Out2]),
      call(Goal_1, Out2),
      indent_debug(out, io(close), "«ZW ~w", [Out2])
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
      close_any2(Close1),
      close_any2(Close2)
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
  stream_property(Stream, mode(Mode)),
  close_metadata_hash(Stream, Opts),
  (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
  indent_debug(out, io(close), "«~s ~w", [Lbl,Stream]).
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



%! guess_stream_encoding(+In, -Enc) is det.

guess_stream_encoding(In1, Enc) :-
  peek_string(In1, 4000, Chunk),
  guess_string_encoding(Chunk, Enc).



%! guess_string_encoding(+Str, -Enc) is det.
%
% Encoding Enc is converted to lowercase.

guess_string_encoding(Str, Enc2) :-
  string_phrase('XMLDecl'(_, Enc1, _), Str, _), !,
  downcase_atom(Enc1, Enc2).
guess_string_encoding(Str, Enc3) :-
  setup_call_cleanup(
    open_binary_string(Str, In),
    setup_call_cleanup(
      process_open(uchardet, In, Out),
      (
        read_string(Out, Enc1),
        split_string(Enc1, "", "\n", [Enc2|_])
      ),
      close(Out)
    ),
    close(In)
  ),
  downcase_atom(Enc2, Enc3).



%! open_binary_string(+Str, -In) is det.

open_binary_string(Str, In) :-
  open_string(Str, In),
  set_stream(In, type(binary)).



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
  process_create(
    path(Cmd),
    Args,
    [stdin(pipe(In2)),stdout(pipe(Out))]
    %%%%[stderr(pipe(Err)),stdin(pipe(In2)),stdout(pipe(Out))]
  ),
  set_stream(In2, type(binary)),
  call_cleanup(
    copy_stream_data(In1, In2),
    close(In2)
  ).
  % @bug Does not terminate for
  % http://lists.w3.org/Archives/Public/html-tidy/2011JulSep/0001.html
  %%%%call_cleanup(
  %%%%  (
  %%%%    read_stream_to_string(Err, Msg),
  %%%%    (Msg == "" -> true ; msg_warning(Msg))
  %%%%  ),
  %%%%  close(Err)
  %%%%).



%! recode_stream(+In1, -In2, -Close, +Opts) is det.

recode_stream(In1, In2, In2, SourceOpts) :-
  option(recode(true), SourceOpts),
  option(from_encoding(Enc), SourceOpts, _VAR),
  (var(Enc) -> guess_stream_encoding(In1, Enc) ; true),
  \+ memberchk(Enc, [ascii,'ascii/unknown','utf-8','us-ascii']), !,
  debug(io(recode), "~a → utf-8", [Enc]),
  process_open(iconv, In1, ['-c','-f',Enc,'-t','utf-8'], In2).
recode_stream(In, In, true, _).



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

open_any2(Spec, Mode, Stream2, Close, Path, Opts) :-
  open_any2_variant(Spec, Mode, Stream1, Close, Path, Opts),
  (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
  indent_debug(in, io(open), "~s» ~w → ~w", [Lbl,Spec,Stream1]),
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
  % The parent is closed automatically when the child is closed.
  open_hash_stream(Stream1, Stream2, [algorithm(Alg)]).
open_any2_hash(Stream, Stream, _).


open_any2_variant(file(Spec), Mode, Stream, Stream, [Entry], Opts) :- !,
  (   compound(Spec)
  ->  merge_options([access(Mode)], Opts, PathOpts),
      absolute_file_name(Spec, File, PathOpts)
  ;   % Allow ‘~’ to be used.
      expand_file_name(Spec, [File|_])
  ),
  open(File, Mode, Stream, [type(binary)]),
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
  ->  (   uri_file_name(Spec1, Spec2)
      ->  Spec2 = file(Spec1)
      ;   is_http_iri(Spec1)
      ->  Spec2 = uri(Spec1)
      ;   Spec2 = file(Spec1)
      )
  ;   Spec2 = file(Spec1)
  ),
  open_any2_variant(Spec2, Mode, Stream, Close, Path, Opts).



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
