:- module(
  io_ext,
  [
    print_input/2,          % +Source, +Opts
    read_input_to_atom/2,   % +Source, -Content
    read_input_to_codes/2,  % +Source, -Content
    read_input_to_string/2, % +Source, -Content
    write_output_to_atom/2, % :Goal_1, -Content
    write_stream_to_file/2, % +Sink,   +File
    write_stream_to_file/3  % +Sink,   +File, +Opts
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

:- meta_predicate
    write_output_to_atom(1, -).





%! print_input(+Source, +Opts) is det.

print_input(Source, Opts) :-
  forall(read_input_to_line(Source, Cs), print_line(Cs, Opts)).



%! print_line(+Cs, +Opts) is det.

print_line(Cs, Opts) :-
  (option(indent(I), Opts) -> tab(I) ; true),
  format("~s~n", [Cs]).



%! read_input_to_atom(+Source, -A) is det.

read_input_to_atom(Source, A) :-
  read_input_to_codes(Source, Cs),
  atom_codes(A, Cs).



%! read_input_to_codes(+Source, -Cs) is det.

read_input_to_codes(Source, Cs) :-
  call_on_stream(Source, read_input_to_codes0(Cs)).

read_input_to_codes0(Cs, _, In) :-
  read_stream_to_codes(In, Cs).



%! read_input_to_line(+Source, -Cs) is nondet.

read_input_to_line(Source, Cs) :-
  call_on_stream(Source, read_input_to_line0(Cs)).

read_input_to_line0(Cs, _, In) :-
  read_line_to_codes(In, Cs).



%! read_input_to_string(+Source, -S) is det.

read_input_to_string(Source, S) :-
  read_input_to_codes(Source, Cs),
  string_codes(S, Cs).



%! write_output_to_atom(:Goal_1, -Atom) is det.
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



%! write_stream_to_file(+Sink, +File) is det.
%! write_stream_to_file(+Sink, +File, +Opts) is det.

write_stream_to_file(Source, Sink) :-
  write_stream_to_file(Source, Sink, []).


write_stream_to_file(Source, Sink, Opts0) :-
  merge_options([type(binary)], Opts0, Opts),
  call_onto_stream(Source, Sink, write_stream_to_file0, Opts).

write_stream_to_file0(_, In, _, Out) :-
  copy_stream_data(In, Out).
