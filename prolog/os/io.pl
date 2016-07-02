:- module(
  io,
  [
    call_on_stream/2,        % +Source, :Goal_3
    call_on_stream/3,        % +Source, :Goal_3, +Opts
    call_onto_stream/3,      % +Source, +Sink, :Goal_4
    call_onto_stream/4,      % +Source, +Sink, :Goal_4, +Opts
    call_to_atom/2,          % :Goal_1, -A
    call_to_codes/2,         % :Goal_1, -Cs
    call_to_stream/2,        % +Sink, :Goal_1
    call_to_stream/3,        % +Sink, :Goal_1, +Opts
    call_to_streams/3,       % +Sink1, +Sink2, :Goal_2
    call_to_streams/5,       % +Sink1, +Sink2, :Goal_2, +Opts1, +Opts2
    call_to_string/2,        % :Goal_1, -Str
    read_line_to_atom/2,     % +In, -A
    read_line_to_string/2,   % +In, -Str
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
    read_stream_to_string/2, % +In, -Str
    stream_compression/2,    % +Stream1, -Stream2
    stream_metadata/2,       % +Stream, -Meta
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07
*/

:- use_module(library(iostream)).
:- use_module(library(zlib)).

:- meta_predicate
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream0(+, +, 3, -, +),
    call_onto_stream(+, +, 4),
    call_onto_stream(+, +, 4, +),
    call_onto_stream0(+, 4, +, +, +, -),
    call_to_atom(1, -),
    call_to_codes(1, -),
    call_to_stream(+, 1),
    call_to_stream(+, 1, +),
    call_to_streams(+, +, 2),
    call_to_streams(+, +, 2, +, +),
    call_to_string(1, +).





%! call_on_stream(+Source, :Goal_3) is det.
%! call_on_stream(+Source, :Goal_3, +Opts) is det.
%
% The following call is made: `call(Goal_3, In)`.
%
% The following options are supported:
%
%   * entry_name(?atom)
%
%   * metadata(-dict)
%
%   * Other options are passed to:
%
%       * archive_data_stream/3
%       * archive_open/3
%       * open_any/5

call_on_stream(Source, Goal_3) :-
  call_on_stream(Source, Goal_3, []).


call_on_stream(Source, Goal_3, Opts) :-
  % This allows the calling context to request one specific entity.
  option(entry_name(EntryName), Opts, _),
  % Archive options that cannot be overridden.
  findall(format(Format), archive_format(Format, true), FormatOpts),
  merge_options([close_parent(false)|FormatOpts], Opts, ArchOpts1),
  % Archive options that can be overridden.
  merge_options(ArchOpts1, [filter(all)], ArchOpts2),
  setup_call_cleanup(
    open_any(Source, read, In1, Close, Opts),
    setup_call_cleanup(
      archive_open(In1, Arch, ArchOpts2),
      % Semi-deterministic if an archive entry name is given.
      (   nonvar(EntryName)
      ->  once(call_on_stream0(Arch, EntryName, Goal_3, Meta1, Opts))
      ;   call_on_stream0(Arch, EntryName, Goal_3, Meta1, Opts)
      ),
      archive_close(Arch)
    ),
    close_any(Close, Meta1, Meta2)
  ),
  ignore(option(metadata(Meta2), Opts)).


call_on_stream0(Arch, EntryName, Goal_3, Meta3, Opts1) :-
  merge_options([meta_data(MetaPath)], Opts1, Opts2),
  archive_data_stream(Arch, In2, Opts2), !,
  (   MetaPath = [MetaEntry|_],
      EntryName = MetaEntry.name
  ->  Meta1 = _{path: MetaPath},
      call_cleanup(
        call(Goal_3, Meta1, Meta2, In2),
        close_any(close(In2), Meta2, Meta3)
      )
  ;   close(In2),
      fail
  ).



%! call_onto_stream(+Source, +Sink, :Goal_4) is det.
%! call_onto_stream(+Source, +Sink, :Goal_4, +Opts) is det.
%
% The following call is made: `call(Goal_4, In, Meta1, Meta2, Out)`.
%
% Options are passed to:
%
%   * call_on_stream/3
%   * call_to_stream/3

call_onto_stream(Source, Sink, Goal_4) :-
  call_onto_stream(Source, Sink, Goal_4, []).


call_onto_stream(Source, Sink, Goal_4, InOpts) :-
  select_option(metadata(_), InOpts, OutOpts),
  call_on_stream(Source, call_onto_stream0(Sink, Goal_4, OutOpts), InOpts).


call_onto_stream0(Sink, Goal_4, OutOpts, In, Meta1, Meta2) :-
  Goal_4 =.. Comps1,
  append(Comps0, [In,Meta1,Meta2], Comps1),
  append(Comps0, [Meta1,Meta2], Comps2),
  Goal_1 =.. Comps2,
  call_to_stream(Sink, Goal_1, OutOpts).



%! call_to_atom(:Goal_1, -A) is det.

call_to_atom(Goal_1, A) :-
  call_to_something0(Goal_1, atom, A).



%! call_to_codes(:Goal_1, -Cs) is det.

call_to_codes(Goal_1, Cs) :-
  call_to_something0(Goal_1, codes, Cs).



call_to_something0(Goal_1, Type, Result) :-
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      setup_call_cleanup(
        open_memory_file(Handle, write, Out),
        call(Goal_1, Out),
        close(Out)
      ),
      memory_file_to_something0(Handle, Type, Result)
    ),
    free_memory_file(Handle)
  ).


memory_file_to_something0(Handle, atom, A) :- !,
  memory_file_to_atom(Handle, A).
memory_file_to_something0(Handle, codes, Cs) :- !,
  memory_file_to_codes(Handle, Cs).
memory_file_to_something0(Handle, string, Str) :- !,
  memory_file_to_string(Handle, Str).



%! call_to_stream(+Sink, :Goal_1) is det.
%! call_to_stream(+Sink, :Goal_1, +Opts) is det.
%
% The following options are supported:
%
%   * compression(+boolean) Whether gzip file compression should be
%   used.  The default is `true`.
%
%   * metadata(-dict)
%
%   * mode(+oneof([append,write])) The default is `append`.
%
%   * Other options are passed to open_any/5.

call_to_stream(Sink, Goal_1) :-
  call_to_stream(Sink, Goal_1, []).


call_to_stream(Sink, Goal_1, Opts) :-
  option(mode(Mode), Opts, append),
  setup_call_cleanup(
    (
      open_any(Sink, Mode, Out1, Close, Opts),
      (   option(compression(true), Opts, true)
      ->  write_stream_compression(Out1, Out2)
      ;   Out2 = Out1
      )
    ),
    call(Goal_1, Out2),
    close_any(Close, Meta)
  ),
  ignore(option(metadata(Meta), Opts)).



%! call_to_streams(+Sink1, +Sink2, :Goal_2) is det.
%! call_to_streams(+Sink1, +Sink2, :Goal_2, +Opts1, +Opts2) is det.
%
% The following options are supported:
%
%   * compression(+boolean) Whether gzip file compression should be
%   used.  The default is `true`.
%
%   * metadata(-dict)
%
%   * mode(+oneof([append,write])) The default is `append`.
%
%   * Other options are passed to open_any/5.


call_to_streams(Sink1, Sink2, Goal_2) :-
  call_to_streams(Sink1, Sink2, Goal_2, [], []).


call_to_streams(Sink1, Sink2, Goal_2, Opts1, Opts2) :-
  option(mode(Mode1), Opts1, append),
  option(mode(Mode2), Opts2, append),
  setup_call_cleanup(
    (
      open_any(Sink1, Mode1, Out1a, Close1, Opts1),
      compression0(Out1a, Out1b, Opts1),
      open_any(Sink2, Mode2, Out2a, Close2, Opts2),
      compression0(Out2a, Out2b, Opts2)
    ),
    call(Goal_2, Out1b, Out2b),
    (
      close_any(Close1, Meta1),
      close_any(Close2, Meta2)
    )
  ),
  ignore(option(metadata(Meta1), Opts1)),
  ignore(option(metadata(Meta2), Opts2)).


compression0(Out1, Out2, Opts) :-
  option(compression(true), Opts, true), !,
  write_stream_compression(Out1, Out2).
compression0(Out, Out, _).



%! call_to_string(:Goal_1, -Str) is det.

call_to_string(Goal_1, Str) :-
  call_to_something0(Goal_1, string, Str).



%! close_any(+Close, -Meta) is det.
%! close_any(+Close, +Meta1, -Meta2) is det.

close_any(Close, Meta) :-
  close_any(Close, _{}, Meta).


close_any(Close, Meta1, Meta2) :-
  (   Close = close(Stream)
  ->  stream_metadata(Stream, StreamMeta),
      Meta2 = Meta1.put(StreamMeta)
  ;   Meta2 = Meta1
  ),
  close_any(Close).



%! read_line_to_atom(+In, -A) is det.

read_line_to_atom(In, A) :-
  read_line_to_codes(In, Cs),
  atom_codes(A, Cs).



%! read_line_to_string(+In, -Str) is det.

read_line_to_string(In, Str) :-
  read_line_to_codes(In, Cs),
  string_codes(Str, Cs).



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



%! read_stream_to_string(+In, -S) is det.

read_stream_to_string(In, S) :-
  read_stream_to_codes(In, Cs),
  string_codes(S, Cs).



%! stream_compression(+Stream1, -Stream2) is det.

stream_compression(Stream1, Stream2) :-
  zopen(Stream1, Stream2, [close_parent(true),format(gzip)]).



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
