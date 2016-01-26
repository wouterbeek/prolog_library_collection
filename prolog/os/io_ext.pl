:- module(
  io_ext,
  [
    print_input/2,          % +Input, +Opts
    read_input_to_atom/2,   % +Input, -Content
    read_input_to_codes/2,  % +Input, -Content
    read_input_to_string/2, % +Input, -Content
    write_output_to_atom/2, % :Goal_1, -Content
    write_stream_to_file/2  % +Read, +File
  ]
).
:- reexport(library(readutil)).

/** <module> Input/Output Extensions

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2015/08, 2015/10-2015/11, 2016/01
*/

:- use_module(library(os/open_any2)).
:- use_module(library(readutil)).

:- meta_predicate(write_output_to_atom(1,-)).

:- predicate_options(print_input/2, 2, [
     pass_to(print_line/2, 2)
   ]).
:- predicate_options(print_line/2, 2, [
     indent(+nonneg)
   ]).





%! print_input(+Input, +Opts) is det.

print_input(In, Opts):-
  forall(read_input_to_line(In, Cs), print_line(Cs, Opts)).

%! print_line(+Cs, +Opts) is det.

print_line(Cs, Opts):-
  (option(indent(I), Opts) -> tab(I) ; true),
  format('~s~n', [Cs]).



%! read_input_to_atom(+Input, -Atom) is det.

read_input_to_atom(In, A):-
  read_input_to_codes(In, Cs),
  atom_codes(A, Cs).



%! read_input_to_codes(+Input, -Cs) is det.

read_input_to_codes(In, Cs):-
  setup_call_cleanup(
    open_any2(In, read, Read, Close_0, [encoding(utf8)]),
    read_stream_to_codes(Read, Cs),
    close_any2(Close_0)
  ).



%! read_input_to_line(+Input, -Cs) is nondet.

read_input_to_line(In, Cs):-
  setup_call_cleanup(
    open_any2(In, read, Read, Close_0, [encoding(utf8)]),
    % NONDET.
    read_line_to_codes(Read, Cs),
    close_any(Close_0)
  ).



%! read_input_to_string(+Input, -String) is det.

read_input_to_string(In, S):-
  read_input_to_codes(In, Cs),
  string_codes(S, Cs).



%! write_output_to_atom(:Goal_1, -Atom) is det.
% Is called as call(Goal_1, Write).

write_output_to_atom(Goal_1, Atom):-
  setup_call_cleanup(
    new_memory_file(Handle),
    (
      setup_call_cleanup(
        open_memory_file(Handle, write, Write),
        call(Goal_1, Write),
        close(Write)
      ),
      memory_file_to_atom(Handle, Atom)
    ),
    free_memory_file(Handle)
  ).



%! write_stream_to_file(+Read, +File) is det.

write_stream_to_file(Read, File):-
  setup_call_cleanup(
    open(File, write, Write, [type(binary)]),
    copy_stream_data(Read, Write),
    close(Write)
  ).
