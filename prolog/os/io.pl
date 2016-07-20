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
    close_any/2,             % +Close, -Meta
    close_any/3,             % +Close, +Meta1, -Meta2
    read_line_to_atom/2,     % +In, -A
    read_mode/1,             % ?Mode
    read_stream_to_atom/2,   % +In, -A
    read_stream_to_string/2, % +In, -Str
    stream_metadata/2,       % +Stream, -Meta
    write_mode/1             % ?Mode
  ]
).

/** <module> I/O

@author Wouter Beek
@tbd Implement metadata using backtrackable setval.
@version 2016/07
*/

:- use_module(library(debug)).
:- use_module(library(iostream)).
:- use_module(library(os/archive_ext)).
:- use_module(library(zlib)).

:- meta_predicate
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
    call_to_stream(+, 1),
    call_to_stream(+, 1, +),
    call_to_streams(+, +, 2),
    call_to_streams(+, +, 2, +),
    call_to_streams(+, +, 2, +, +),
    call_to_string(1, +).





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
%   * Other options are passed to:
%
%       * archive_data_stream/3
%       * archive_open/3
%       * open_any/5

call_on_stream(Source, Goal_3) :-
  call_on_stream(Source, Goal_3, []).


call_on_stream(Source, Goal_3, SourceOpts) :-
  % This allows the calling context to request one specific entity.
  option(entry_name(EntryName), SourceOpts, _),
  merge_options([metadata(Meta1)], SourceOpts, OpenOpts),
  open_any(Source, read, In, _, OpenOpts),
  (var(Meta1) -> Meta1 = _{} ; true),
  (   source_base_iri(Source, BaseIri)
  ->  put_dict(base_iri, Meta1, BaseIri, Meta2)
  ;   Meta2 = Meta1
  ),
  % Semi-deterministic if an archive entry name is given.
  (   nonvar(EntryName)
  ->  once(call_on_stream0(In, EntryName, Goal_3, Meta2, Meta3))
  ;   call_on_stream0(In, EntryName, Goal_3, Meta2, Meta3)
  ),
  ignore(option(metadata(Meta3), SourceOpts)).


call_on_stream0(In1, EntryName, Goal_3, Meta1, Meta4) :-
  % NONDET
  open_archive_by_stream(In1, Path, In2),
  % Even though the first input stream `In1` used UTF-8 encoding, the
  % streams that come out of archives are `octet`.  This is corrected
  % here.
  set_stream(In2, encoding(utf8)),
  (   Path = [Entry|_],
      EntryName = Entry.name
  ->  put_dict(entry_path, Meta1, Path, Meta2),
      call_cleanup(
        call(Goal_3, In2, Meta2, Meta3),
        close_any(close(In2), Meta3, Meta4)
      )
  ;   close(In2),
      fail
  ).



%! call_onto_stream(+Source, +Sink, :Goal_4) is det.
%! call_onto_stream(+Source, +Sink, :Goal_4, +SourceOpts, +SinkOpts) is det.
%
% The following call is made: `call(Goal_4, In, Meta1, Meta2, Out)`.
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


call_onto_stream0(Sink, Mod:Goal_4, SinkOpts, In, Meta1, Meta2) :-
  Goal_4 =.. Comps1,
  append(Comps1, [In,Meta1,Meta2], Comps2),
  %%%%append(Comps0, [Meta1,Meta2], Comps2),
  Goal_1 =.. Comps2,
  call_to_stream(Sink, Mod:Goal_1, SinkOpts).



%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +SinkOpts) is det.
%! call_onto_streams(+Source, +Sink1, +Sink2, :Goal_5, +SourceOpts, +Sink1Opts, +Sink2Opts) is det.

call_onto_streams(Source, Sink1, Sink2, Goal_5) :-
  call_onto_streams(Source, Sink1, Sink2, Goal_5, [], [], []).


call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, SinkOpts) :-
  call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, SinkOpts, SinkOpts).


call_onto_streams(Source, Sink1, Sink2, Goal_5, SourceOpts, Sink1Opts, Sink2Opts) :-
  call_on_stream(Source, call_onto_streams0(Sink1, Sink2, Goal_5, Sink1Opts, Sink2Opts), SourceOpts).


call_onto_streams0(Sink1, Sink2, Mod:Goal_5, Sink1Opts, Sink2Opts, In, Meta1, Meta2) :-
  Goal_5 =.. Comps1,
  append(Comps1, [In,Meta1,Meta2], Comps2),
  %%%%append(Comps0, [Meta1,Meta2], Comps2),
  Goal_2 =.. Comps2,
  call_to_streams(Sink1, Sink2, Mod:Goal_2, Sink1Opts, Sink2Opts).



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
%! call_to_stream(+Sink, :Goal_1, +SinkOpts) is det.
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


call_to_stream(Sink, Goal_1, SinkOpts) :-
  option(mode(Mode), SinkOpts, append),
  setup_call_cleanup(
    (
      open_any(Sink, Mode, Out1, Close1, SinkOpts),
      write_stream_compression(Out1, Close1, Mode, Out2, Close2, SinkOpts)
    ),
    call(Goal_1, Out2),
    close_any(Close2, Meta)
  ),
  ignore(option(metadata(Meta), SinkOpts)).



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
%   * mode(+oneof([append,write])) The default is `append`.
%
%   * Other options are passed to open_any/5.


call_to_streams(Sink1, Sink2, Goal_2) :-
  call_to_streams(Sink1, Sink2, Goal_2, [], []).


call_to_streams(Sink1, Sink2, Goal_2, SinkOpts) :-
  call_to_streams(Sink1, Sink2, Goal_2, SinkOpts, SinkOpts).


call_to_streams(Sink1, Sink2, Goal_2, Sink1Opts, Sink2Opts) :-
  option(mode(Mode1), Sink1Opts, append),
  option(mode(Mode2), Sink2Opts, append),
  setup_call_cleanup(
    (
      open_any(Sink1, Mode1, Out1a, Close1a, Sink1Opts),
      write_stream_compression(Out1a, Close1a, Mode1, Out1b, Close1b, Sink1Opts),
      open_any(Sink2, Mode2, Out2a, Close2a, Sink2Opts),
      write_stream_compression(Out2a, Close2a, Mode2, Out2b, Close2b, Sink2Opts)
    ),
    call(Goal_2, Out1b, Out2b),
    (
      close_any(Close1b, Meta1),
      close_any(Close2b, Meta2)
    )
  ),
  ignore(option(metadata(Meta1), Sink1Opts)),
  ignore(option(metadata(Meta2), Sink2Opts)).



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



%! source_base_iri(+Source, -Base) is det.

source_base_iri(Source, Iri) :-
  atom(Source), !,
  uri_file_name(Iri, Source).
source_base_iri(Comp, Iri) :-
  compound(Comp), !,
  absolute_file_name(Comp, Path),
  source_base_iri(Path, Iri).



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



%! write_stream_compression(+Out1, +Close1, +Mode, -Out2, -Close2, +Opts) is det.

write_stream_compression(Out1, _, Mode, Out2, close(Out2), Opts) :-
  write_mode(Mode),
  option(compression(true), Opts, true), !,
  zopen(Out1, Out2, [close_parent(true),format(gzip)]).
write_stream_compression(Out, Close, _, Out, Close, _).
