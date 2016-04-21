:- module(
  io_ext,
  [
    read_input_to_atom/2,   % +Source, -A
    read_input_to_codes/2,  % +Source, -Cs
    read_input_to_string/2, % +Source, -S
    write_output_to_atom/2, % :Goal_1, -A
    write_stream_to_file/2, % +Source, +Sink
    write_stream_to_file/3  % +Source, +Sink, +Opts
  ]
).
:- reexport(library(readutil)).

/** <module> Input/output or source/sink extensions

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/01-2016/02, 2016/04
*/

:- use_module(library(os/open_any2)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

:- meta_predicate
    write_output_to_atom(1, -).





%! read_input_to_atom(+Source, -A) is det.

read_input_to_atom(Source, A) :-
  read_input_to_codes(Source, Cs),
  atom_codes(A, Cs).



%! read_input_to_codes(+Source, -Cs) is det.

read_input_to_codes(Source, Cs) :-
  call_on_stream(Source, [In,_,_]>>read_stream_to_codes(In, Cs)).



%! read_input_to_line(+Source, -Cs) is nondet.

read_input_to_line(Source, Cs) :-
  call_on_stream(Source, [In,_,_]>>read_input_to_line(In, Cs)).



%! read_input_to_string(+Source, -S) is det.

read_input_to_string(Source, S) :-
  read_input_to_codes(Source, Cs),
  string_codes(S, Cs).



%! write_output_to_atom(:Goal_1, -A) is det.
% Is called as `call(Goal_1, Out)`.

write_output_to_atom(Goal_1, A) :-
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      setup_call_cleanup(
        open_memory_file(Handle, write, Out),
        call(Goal_1, Out),
        close(Out)
      ),
      memory_file_to_atom(Handle, A)
    ),
    free_memory_file(Handle)
  ).



%! write_stream_to_file(+Source, +Sink) is det.
%! write_stream_to_file(+Source, +Sink, +Opts) is det.

write_stream_to_file(Source, Sink) :-
  write_stream_to_file(Source, Sink, []).


write_stream_to_file(Source, Sink, Opts0) :-
  merge_options([type(binary)], Opts0, Opts),
  call_onto_stream(
    Source,
    Sink,
    [In,_,_,Out,_,_]>>copy_stream_data(In, Out),
    Opts
  ).
