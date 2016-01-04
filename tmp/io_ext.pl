:- module(
  io_ext,
  [
    copy_stream_line/2, % +From:stream
                        % +To:stream
    read_stream_to_file/2, % +Stream:stream
                           % +File:atom
    write_atom_to_file/2, % +Atom:atom
                          % +File:atom
    write_codes_to_file/2, % +Codes:list(code)
                           % +File:atom
    write_term_to_stream/2 % +Term:term
                           % -Stream:stream
  ]
).

/** <module> Input/Output Extensions

Predicates that extend the swipl builtin I/O predicates operating on streams.

@author Wouter Beek
@version 2013/01, 2013/06, 2013/08, 2014/01, 2014/03-2014/04, 2014/09-2014/11
*/

:- use_module(library(memfile)).
:- use_module(library(readutil)).

:- use_module(plc(generics/code_ext)).





%! copy_stream_line(+From:stream, +To:stream) is det.
% Copy the next line on the former stream to the latter stream.

copy_stream_line(From, To):-
  read_line_to_codes(From, Codes),
  put_codes(To, Codes),
  flush_output(To).



% Stores an atomic stream to the given file.

read_stream_to_file(Read, File):-
  setup_call_cleanup(
    open(File, write, Write, [type(binary)]),
    copy_stream_data(Read, Write),
    close(Write)
  ).



%! write_atom_to_file(+Atom:atom, +File:atom) is det.
% Stores the given atom in the given file.

write_atom_to_file(Atom, File):-
  access_file(File, write),
  setup_call_cleanup(
    open(File, write, Write),
    format(Write, '~w', [Atom]),
    close(Write)
  ).



%! write_codes_to_file(+Codes:list(code), +File:atom) is det.

write_codes_to_file(Codes, File):-
  access_file(File, write),
  setup_call_cleanup(
    open(File, write, Stream, [encoding(utf8),type(text)]),
    put_codes(Stream, Codes),
    close(Stream)
  ).



%! write_term_to_stream(+Term:term, -Read:stream) is det.

write_term_to_stream(Term, Read):-
  term_to_atom(Term, Atom),
  atom_to_memory_file(Atom, Handle),
  open_memory_file(Handle, read, Read, [free_on_close(true)]).
