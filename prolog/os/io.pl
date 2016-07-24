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
:- use_module(library(zlib)).

:- meta_predicate
    call_on_archive0(+, +, +, 3, +, -),
    call_on_stream(+, 3),
    call_on_stream(+, 3, +),
    call_on_stream0(+, +, +, 3, +, -),
    call_onto_stream(+, +, 4),
    call_onto_stream(+, +, 4, +, +),
    call_onto_stream0(+, 4, +, +, +, -),
    call_onto_streams(+, +, +, 5),
    call_onto_streams(+, +, +, 5, +, +),
    call_onto_streams(+, +, +, 5, +, +, +),
    call_onto_streams0(+, +, 5, +, +, +, +, -),
    call_to_atom(1, -),
    call_to_codes(1, -),
    call_to_something(1, +, -),
    call_to_stream(+, 1),
    call_to_stream(+, 1, +),
    call_to_stream0(+, 1, +),
    call_to_streams(+, +, 2),
    call_to_streams(+, +, 2, +),
    call_to_streams(+, +, 2, +, +),
    call_to_streams0(+, +, 2, +, +),
    call_to_string(1, +).

:- multifile
    error:has_type/2.

error:has_type(read_mode, T) :-
  error:has_type(oneof([read]), T).

error:has_type(write_mode, T) :-
  error:has_type(oneof([append,write]), T).





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
%       * archive_open/3
%
%       * open_any2/5
%
% @tbd Support paths of entry names (option entry_name/1) for nested
% archives.

call_on_stream(Source, Goal_3) :-
  call_on_stream(Source, Goal_3, []).


call_on_stream(Source, Goal_3, SourceOpts) :-
  merge_options([metadata(Meta1)], SourceOpts, OpenOpts),
  set_base_iri(Meta1, Source, Meta2),
  option(entry_name(Entry0), SourceOpts, _),
  setup_call_cleanup(
    open_any2(Source, read, In, _, OpenOpts),
    call_on_stream0([], In, Entry0, Goal_3, Meta2, Meta3),
    close_any2(In, Meta3, Meta4)
  ),
  ignore(option(metadata(Meta4), SourceOpts)).


call_on_stream0([H|T], In, _, Goal_3, Meta1, Meta3) :-
  H.format == raw,
  H.name == data, !,
  put_dict(entry_path, Meta1, T, Meta2),
  call(Goal_3, In, Meta2, Meta3).
call_on_stream0(Path, In0, Entry0, Goal_3, Meta1, Meta2) :-
  findall(format(Format), archive_format(Format, true), Formats),
  setup_call_cleanup(
    (
      archive_open(stream(In0), Arch, [close_parent(false),filter(all)|Formats]),
      indent_debug(in, io, "R> ~w → ~w", [In0,Arch])
    ),
    call_on_archive0(Path, Arch, Entry0, Goal_3, Meta1, Meta2),
    ( % @tbd THIS SEEMS TO BE SKIPPED SOMETIMES!
      archive_close(Arch),
      indent_debug(out, io, "<R ~w", [Arch])
    )
  ).


call_on_archive0(T, Arch, Entry0, Goal_3, Meta1, Meta3) :-
  archive_property(Arch, filter(Filters)),
  repeat,
  (   archive_next_header(Arch, Entry)
  ->  findall(Property, archive_header_property(Arch, Property), Properties),
      dict_create(H, [filters(Filters),name(Entry)|Properties]),
      % If the entry name is `data` then proceed.  If the entry name
      % is uninstantiated then proceed.  If entry name is instantiated and does not occur in the
      % current archive header then fail (repeat/0 will iterate to the
      % next archive header).
      (   % This is the last entry in this nested branch.  We
          % therefore close the choicepoint created by repeat/0.  Not
          % closing this choicepoint would cause archive_next_header/2
          % to throw an exception.
          Entry == data,
          H.format == raw
      ->  !
      ;   % If the given entry name occurs in the current archive
          % header then red cut, because we are in the correct entry
          % branch.
          Entry == Entry0
      ->  !
      ;   % If the given entry name did not match the current archive
          % header then fail, because we are in the wrong entry
          % branch.
          var(Entry0)
      ),
      (   memberchk(filetype(file), Properties)
      ->  setup_call_cleanup(
            (
              archive_open_entry(Arch, In),
              indent_debug(in, io, "R> ~w → ~a → ~w", [Arch,Entry,In])
            ),
            (
              % Archive entries are always encoded as octet.  We
              % change this to UTF-8.
              set_stream(In, encoding(utf8)),
              call_on_stream0([H|T], In, Entry0, Goal_3, Meta1, Meta2)
            ),
            close_any2(In, Meta2, Meta3)
          )
      ;   fail
      )
  ;   !,
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
  goal_manipulation(Goal_4, [In,Meta1,Meta2], Goal_1),
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


call_onto_streams0(Sink1, Sink2, Goal_5, Sink1Opts, Sink2Opts, In, Meta1, Meta2) :-
  goal_manipulation(Goal_5, [In,Meta1,Meta2], Goal_2),
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
%   * mode(+oneof([append,write])) The default is `append`.
%
%   * Other options are passed to open_any2/5.

call_to_stream(Sink, Goal_1) :-
  call_to_stream(Sink, Goal_1, []).


call_to_stream(Sink, Goal_1, SinkOpts) :-
  option(mode(Mode), SinkOpts, append),
  must_be(write_mode, Mode),
  setup_call_cleanup(
    open_any2(Sink, Mode, Out, Close, SinkOpts),
    call_to_stream0(Out, Goal_1, SinkOpts),
    close_any2(Close, Meta)
  ),
  ignore(option(metadata(Meta), SinkOpts)).


call_to_stream0(Out1a, Goal_1, SinkOpts) :-
  (   option(compression(true), SinkOpts, true)
  ->  setup_call_cleanup(
        zopen(Out1a, Out1b, [close_parent(false),format(gzip)]),
        call(Goal_1, Out1b),
        close(Out1b)
      )
  ;   call(Goal_1, Out1a)
  ).



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
%   * Other options are passed to open_any2/5.

call_to_streams(Sink1, Sink2, Goal_2) :-
  call_to_streams(Sink1, Sink2, Goal_2, [], []).


call_to_streams(Sink1, Sink2, Goal_2, SinkOpts) :-
  call_to_streams(Sink1, Sink2, Goal_2, SinkOpts, SinkOpts).


call_to_streams(Sink1, Sink2, Goal_2, Sink1Opts, Sink2Opts) :-
  option(mode(Mode1), Sink1Opts, append),
  option(mode(Mode2), Sink2Opts, append),
  setup_call_cleanup(
    open_any2(Sink1, Mode1, Out1, Close1, Sink1Opts),
    setup_call_cleanup(
      open_any2(Sink2, Mode2, Out2, Close2, Sink2Opts),
      call_to_stream0(Out1, Out2, Goal_2, Sink1Opts, Sink2Opts),
      close_any2(Close1, Meta1)
    ),
    close_any2(Close2, Meta2)
  ),
  ignore(option(metadata(Meta1), Sink1Opts)),
  ignore(option(metadata(Meta2), Sink2Opts)).


call_to_streams0(Out1a, Out2, Goal_2, Sink1Opts, Sink2Opts) :-
  option(compression(true), Sink1Opts, true), !,
  setup_call_cleanup(
    zopen(Out1a, Out1b, [close_parent(false),format(gzip)]),
    (
      goal_manipulation(Goal_2, [Out1b], Goal_1),
      call_to_stream0(Out2, Goal_1, Sink2Opts)
    ),
    close(Out1b)
  ).
call_to_streams0(Out1, Out2, Goal_2, _, Sink2Opts) :-
  goal_manipulation(Goal_2, [Out1], Goal_1),
  call_to_stream0(Out2, Goal_1, Sink2Opts).



%! call_to_string(:Goal_1, -Str) is det.

call_to_string(Goal_1, Str) :-
  call_to_something(Goal_1, string, Str).



%! close_any2(+Close, -Meta) is det.
%! close_any2(+Close, +Meta1, -Meta2) is det.

close_any2(Close, Meta) :-
  close_any2(Close, _{}, Meta).


close_any2(Close, Meta1, Meta2) :-
  close_stream0(Close, Stream),
  stream_metadata(Stream, StreamMeta),
  Meta2 = Meta1.put(StreamMeta),
  stream_property(Stream, mode(Mode)),
  close(Stream),
  (read_mode(Mode) -> M = "R" ; write_mode(Mode) -> M = "W"),
  indent_debug(out, io, "<~s ~w", [M,Stream]).


close_stream0(close(Stream), Stream) :- !.
close_stream0(Stream, Stream).



%! open_any2(+Spec, +Mode, -Stream, -Close, +Opts) is det.

open_any2(Iri, read, In, Close, Opts) :-
  is_http_iri(Iri), !,
  http_io:http_open_any(Iri, In0, Close0, Opts),
  iostream:input_options(In0, In, Close0, Close, Opts).
open_any2(Spec, Mode, Stream, Close, Opts) :-
  open_any(Spec, Mode, Stream, Close, Opts),
  (read_mode(Mode) -> M = "R" ; write_mode(Mode) -> M = "W"),
  indent_debug(in, io, "~s> ~w → ~w", [M,Spec,Stream]).



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



%! set_base_iri(+Meta1, +Source, -Meta2) is det.

set_base_iri(Meta1, Source, Meta2) :-
  (var(Meta1) -> Meta1 = _{} ; true),
  (   source_base_iri(Source, BaseIri)
  ->  put_dict(base_iri, Meta1, BaseIri, Meta2)
  ;   Meta2 = Meta1
  ).
