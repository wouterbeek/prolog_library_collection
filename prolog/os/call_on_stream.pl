:- module(
  call_on_stream,
  [
    append_to_stream/2, % +Source, :Goal_2
    append_to_stream/3, % +Source
                        % :Goal_2
                        % +Options:list(compound)
    read_from_stream/2, % +Source, :Goal_2
    read_from_stream/3, % +Source
                        % :Goal_2
                        % +Options:list(compound)
    write_to_stream/2, % +Source, :Goal_2
    write_to_stream/3 % +Source
                      % :Goal_2
                      % +Options:list(compound)
  ]
).

/** <module> Call on stream

Apply an arbitrary goal to a given read/write/append stream.

@author Wouter Beek
@version 2015/10-2015/11
*/

:- use_module(library(option)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/open_any2)).

:- meta_predicate(append_or_write_to_stream(+,+,2,+)).
:- meta_predicate(append_to_stream(+,2)).
:- meta_predicate(append_to_stream(+,2,+)).
:- meta_predicate(read_from_stream(+,2)).
:- meta_predicate(read_from_stream(+,2,+)).
:- meta_predicate(write_to_stream(+,2)).
:- meta_predicate(write_to_stream(+,2,+)).

:- predicate_options(append_to_stream/3, 3, [
     pass_to(append_or_write_to_stream/4, 4)
   ]).
:- predicate_options(append_or_write_to_stream/4, 4, [
     pass_to(open_any2/5)
   ]).
:- predicate_options(read_from_stream/3, 3, [
     pass_to(call_on_archive/3, 3),
     pass_to(open_any2/5)
   ]).
:- predicate_options(write_to_stream/3, 3, [
     pass_to(append_or_write_to_stream/4, 4)
   ]).





%! append_to_stream(+Source, :Goal_2) is det.
% Wrapper around append_to_stream/3 with default options.

append_to_stream(Source, Goal_2):-
  append_to_stream(Source, Goal_2, []).


%! append_to_stream(+Source, :Goal_2, +Options:list(compound)) is det.
% Calls Goal_2 on a metadata dictionary and an append stream (in that order).

append_to_stream(Source, Goal_2, Opts):-
  append_or_write_to_stream(Source, append, Goal_2, Opts).



%! read_from_stream(+Source, :Goal_2) is det.
% Wrapper around read_from_stream/3 with default options.

read_from_stream(Source, Goal_2):-
  read_from_stream(Source, Goal_2, []).


%! read_from_stream(+Source, :Goal_2, +Options:list(compound)) is det.
% Calls Goal_2 on a metadata dictionary and a read stream (in that order).

read_from_stream(Source, Goal_2, Opts):-
  call_on_archive(Source, Goal_2, Opts).



%! write_to_stream(+Source, :Goal_2) is det.
% Wrapper around write_to_stream/3 with default options.

write_to_stream(Source, Goal_2):-
  write_to_stream(Source, Goal_2, []).


%! write_to_stream(+Source, :Goal_2, +Options:list(compound)) is det.
% Calls Goal_2 on a metadata dictionary and a write stream (in that order).

write_to_stream(Source, Goal_2, Opts):-
  append_or_write_to_stream(Source, write, Goal_2, Opts).





% HELPERS %

%! append_or_write_to_stream(
%!   +Source,
%!   +Mode:oneof([append,write]),
%!   :Goal_2,
%!   +Options:list(compound)
%! ) is det.
% Append and write mode share a lot of functionality.

append_or_write_to_stream(Source, Mode, Goal_2, Opts1):-
  merge_options([metadata(M)], Opts1, Opts2),
  setup_call_cleanup(
    open_any2(Source, Mode, Write, Close_0, Opts2),
    call(Goal_2, M, Write),
    close_any2(Close_0)
  ).
