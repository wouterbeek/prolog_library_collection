:- module(
  io_ext,
  [
    print_input/2, % +Spec
                   % +Options:list(compound)
    read_input_to_atom/2, % +Spec
                          % -Content:atom
    read_input_to_codes/2, % +Spec
                           % -Content:list(code)
    read_input_to_string/2 % +Spec
                           % -Content:string
  ]
).
:- reexport(library(readutil)).

/** <module> Input/Output Extensions

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(http/http_open)). % Support reading from URIs.
:- use_module(library(iostream)).
:- use_module(library(readutil)).

:- predicate_options(print_input/2, 2, [
     pass_to(print_line/2, 2)
   ]).
:- predicate_options(print_line/2, 2, [
     indent(+nonneg)
   ]).





%! print_input(+Spec, +Options:list(compound)) is det.

print_input(Spec, Opts):-
  forall(read_input_to_line(Spec, Cs), print_line(Cs, Opts)).

%! print_line(+Content:list(code), +Options:list(compound)) is det.

print_line(Cs, Opts):-
  (option(indent(I), Opts) -> tab(I) ; true),
  format('~s~n', [Cs]).



%! read_input_to_atom(+Spec, -Atom:atom) is det.

read_input_to_atom(Spec, A):-
  read_input_to_codes(Spec, Cs),
  atom_codes(A, Cs).



%! read_input_to_codes(+Spec, -Codes:list(code)) is det.

read_input_to_codes(Spec, Cs):-
  setup_call_cleanup(
    open_any(Spec, read, Read, Close, [encoding(utf8)]),
    read_stream_to_codes(Read, Cs),
    close_any(Close)
  ).



%! read_input_to_line(+Spec, -Line:list(code)) is nondet.

read_input_to_line(Spec, Cs):-
  setup_call_cleanup(
    open_any(Spec, read, Read, Close, [encoding(utf8)]),
    % NONDET.
    read_line_to_codes(Read, Cs),
    close_any(Close)
  ).



%! read_input_to_string(+Spec, -String:string) is det.

read_input_to_string(Spec, S):-
  read_input_to_codes(Spec, Cs),
  string_codes(S, Cs).
