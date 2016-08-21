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
    read_line_to_atom/2,     % +In, -A
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
    read_stream_to_string/2, % +In, -Str
    stream_metadata/2,       % +Stream, -Meta
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

The following debug flags are used:

  * io

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_io), []).
:- use_module(library(iostream)).
:- use_module(library(os/archive_ext)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- meta_predicate
    call_on_archive0(+, +, 3, +, -),
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream0(+, +, 3, +, -),
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
  option(entry_name(Entry0), SourceOpts, _),
  setup_call_cleanup(
    open_any2(Source, read, In, Close_0, L1, SourceOpts),
    call_on_stream0(In, Entry0, Goal_3, L1, L2),
    close_any2(Close_0, L2, L3)
  ),
  ignore(option(metadata(L3), SourceOpts)).


% No input stream, e.g., HTTP status code 409.
call_on_stream0(In, _, _, L, L) :-
  var(In), !.
% Empty input stream.
call_on_stream0(In, _, _, L, L) :-
  at_end_of_stream(In), !.
% Data input stream.
call_on_stream0(In, _, Goal_3, [H|T], L) :-
  get_dict(format, H, raw),
  get_dict(name, H, data), !,
  call(Goal_3, In, [H|T], L).
% Compressed and/or packaged input stream.
call_on_stream0(In, Entry0, Goal_3, L1, L2) :-
  findall(format(Format), archive_format(Format, true), Formats),
  setup_call_cleanup(
    (
      archive_open(stream(In), Arch, [close_parent(false),filter(all)|Formats]),
      indent_debug(in, io, "R> ~w → ~w", [In,Arch])
    ),
    call_on_archive0(Arch, Entry0, Goal_3, L1, L2),
    ( % @tbd THIS SEEMS TO BE SKIPPED SOMETIMES!
      archive_close(Arch),
      indent_debug(out, io, "<R ~w", [Arch])
    )
  ).


call_on_archive0(Arch, Entry0, Goal_3, [H1|T1], L4) :-
  archive_property(Arch, filter(Filters)),
  repeat,
  (   archive_next_header(Arch, Entry)
  ->  findall(Property, archive_header_property(Arch, Property), Properties),
      dict_create(H0, [filters(Filters),name(Entry)|Properties]),
      % If the entry name is `data` then proceed.  If the entry name
      % is uninstantiated then proceed.  If entry name is instantiated and does not occur in the
      % current archive header then fail (repeat/0 will iterate to the
      % next archive header).
      (   % This is the last entry in this nested branch.  We
          % therefore close the choicepoint created by repeat/0.  Not
          % closing this choicepoint would cause archive_next_header/2
          % to throw an exception.
          Entry == data,
          H0.format == raw
      ->  !,
          H2 = H1.put(H0),
          L2 = [H2|T1]
      ;   L2 = [H0,H1|T1],
          (   % If the given entry name occurs in the current archive
              % header then red cut, because we are in the correct entry
              % branch.
              Entry == Entry0
          ->  !
          ;   % If the given entry name did not match the current archive
              % header then fail, because we are in the wrong entry
              % branch.
              var(Entry0)
          )
      ),
      (   memberchk(filetype(file), Properties)
      ->  setup_call_cleanup(
            (
              archive_open_entry(Arch, In),
              indent_debug(in, io, "R> ~w → ~a → ~w", [Arch,Entry,In])
            ),
            call_on_stream0(In, Entry0, Goal_3, L2, L3),
            close_any2(close(In), L3, L4)
          )
      ;   fail
      )
  ;   !,
      fail
  ).



%! call_onto_stream(+Source, +Sink, :Goal_4) is det.
%! call_onto_stream(+Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts) is det.
%
% The following call is made: `call(Goal_4, +In, +Path1, -Path2, +Out)`.
%
% Options are passed to:
%
%   * call_on_stream/3
%
%   * call_to_stream/3

call_onto_stream(Source, Sink, Goal_4) :-
  call_onto_stream(Source, Sink, Goal_4, [], []).


call_onto_stream(Source, Sink, Goal_4, SourceOpts, SinkOpts) :-
  call_on_stream(Source, call_onto_stream0(Sink, Goal_4, SinkOpts), SourceOpts).


call_onto_stream0(Sink, Goal_4, SinkOpts, In, L1, L2) :-
  goal_manipulation(Goal_4, [In,L1,L2], Goal_1),
  call_to_stream(Sink, Goal_1, SinkOpts).



%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +SinkOpts) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +Sink1Opts, +Sink2Opts) is det.

call_onto_streams(Source, Sink1, Sink2, Goal_5) :-
  call_onto_streams(Source, Sink1, Sink2, Goal_5, [], [], []).


call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, SinkOpts) :-
  call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, SinkOpts, SinkOpts).


call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, Sink1Opts, Sink2Opts) :-
  call_on_stream(Source, call_onto_streams0(Sink1, Sink2, Goal_5, Sink1Opts, Sink2Opts), SourceOpts).


call_onto_streams0(Sink1, Sink2, Goal_5, Sink1Opts, Sink2Opts, In, L1, L2) :-
  goal_manipulation(Goal_5, [In,L1,L2], Goal_2),
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
%   * compression(+boolean) Whether gzip file compression should be
%   used.  The default is `true`.
%
%   * metadata(-dict)
%
%     * compression(oneof([gzip,none]))
%
%     * mode(oneof([append,write]))
%
%   * mode(+oneof([append,write])) The default is `write`.
%
%   * Other options are passed to open_any2/6.

call_to_stream(Sink, Goal_1) :-
  call_to_stream(Sink, Goal_1, []).


call_to_stream(Sink, Goal_1, SinkOpts) :-
  option(mode(Mode), SinkOpts, write),
  must_be(write_mode, Mode),
  setup_call_cleanup(
    open_any2(Sink, Mode, Out, Close_0, L1, SinkOpts),
    call_to_compressed_stream(Out, Goal_1, L1, L2, SinkOpts),
    close_any2(Close_0, L2, L3)
  ),
  ignore(option(metadata(L3), SinkOpts)).


call_to_compressed_stream(Out1, Goal_1, [H1|T], [H2|T], SinkOpts) :-
  option(compression(true), SinkOpts, true), !,
  put_dict(compression, H1, gzip, H2),
  setup_call_cleanup(
    (
      zopen(Out1, Out2, [close_parent(false),format(gzip)]),
      indent_debug(in, io, "ZW> ~w → ~w", [Out1,Out2])
    ),
    call(Goal_1, Out2),
    (
      close(Out2),
      indent_debug(out, io, "<ZW ~w", [Out2])
    )
  ).
call_to_compressed_stream(Out, Goal_1, L, L, _) :-
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
    open_any2(Sink1, Mode1, Out1, Close1_0, L1a, Sink1Opts),
    setup_call_cleanup(
      open_any2(Sink2, Mode2, Out2, Close2_0, L1b, Sink2Opts),
      call_to_streams0(Out1, Out2, Goal_2, L1a, L1b, Sink1Opts, L2a, L2b, Sink2Opts),
      close_any2(Close1_0, L2b, L3b)
    ),
    close_any2(Close2_0, L2a, L3a)
  ),
  ignore(option(metadata1(L3a), Sink1Opts)),
  ignore(option(metadata2(L3b), Sink2Opts)).


call_to_streams0(Out1a, Out2, Goal_2, L1a, L1b, Sink1Opts, L2a, L2b, Sink2Opts) :-
  call_to_compressed_stream(
    Out1a,
    {Out2,Goal_2,L1a,L2a,Sink2Opts}/[Out1b]>>(
      goal_manipulation(Goal_2, [Out1b], Goal_1),
      call_to_compressed_stream(Out2, Goal_1, L1a, L2a, Sink2Opts)
    ),
    L1b,
    L2b,
    Sink1Opts
  ).



%! call_to_string(:Goal_1, -Str) is det.

call_to_string(Goal_1, Str) :-
  call_to_something(Goal_1, string, Str).



%! close_any2(+Close_0, +L1, -L2) is det.

close_any2(close(Stream), [H1|T], [H2|T]) :- !,
  stream_metadata(Stream, H0),
  H2 = H1.put(H0),
  stream_property(Stream, mode(Mode)),
  close(Stream),
  (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
  indent_debug(out, io, "<~s ~w", [Lbl,Stream]).
close_any2(true, L, L).



%! open_any2(+Spec, +Mode, -Stream, -Close_0, -Path, +Opts) is det.
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

open_any2(Iri, read, In, close(In), L, Opts) :-
  is_http_iri(Iri), !,
  http_io:http_open_any(Iri, In, L, Opts).
open_any2(Spec, Mode, Stream, Close_0, [H2], Opts) :-
  open_any(Spec, Mode, Stream, _, Opts),
  (   Spec == Stream
  ->  Close_0 = true,
      H1 = _{stream: Stream}
  ;   absolute_file_name(Spec, File),
      Close_0 = close(Stream),
      H1 = _{file: File},
      (read_mode(Mode) -> Lbl = "R" ; write_mode(Mode) -> Lbl = "W"),
      indent_debug(in, io, "~s> ~w → ~w", [Lbl,Spec,Stream])
  ),
  put_dict(mode, H1, Mode, H2).



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



%! stream_metadata(+Stream, -Meta) is det.
%
% Succeeds if Meta is a dictionary that contains the metadata
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

stream_metadata(Stream, Meta):-
  stream_property(Stream, position(Pos)),
  stream_position_data(byte_count, Pos, NumBytes),
  stream_position_data(char_count, Pos, NumChars),
  stream_position_data(line_count, Pos, NumLines),
  stream_property(Stream, newline(Newline)),
  Meta = _{
    byte_count: NumBytes,
    char_count: NumChars,
    line_count: NumLines,
    newline: Newline
  }.



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



%! memory_file_to_something(+Handle, +Type:oneof([atom,codes,string]), -Result) is det.

memory_file_to_something(Handle, atom, A) :- !,
  memory_file_to_atom(Handle, A).
memory_file_to_something(Handle, codes, Cs) :- !,
  memory_file_to_codes(Handle, Cs).
memory_file_to_something(Handle, string, Str) :- !,
  memory_file_to_string(Handle, Str).
