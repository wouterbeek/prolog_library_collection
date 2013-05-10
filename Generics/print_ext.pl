:- module(
  print_ext,
  [
    print_indent/2, % +Stream:stream
                    % +Indent:integer
    print_list/2 % +Stream:stream
                 % +List:list
  ]
).

/** <module> PRINT

Predicates for printing stuff.

---+ proof

A datatype of the form

==
proof(Conclusion, Premises)
==

where =Conclusion= is written using =print_conclusion/3= and =Premises=
is a list of proofs and/or premises that are written using =print_premise/3=.

@author Wouter Beek
@version 2013/01-2013/02, 2013/04
*/

:- use_module(generics(atom_ext)). % Meta-calls.
:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(memfile)).

% The number of spaces that go into one indent.
indent_size(2).



%% print_indent(+Stream:stream, +Indent:integer) is det.
% Print the given number of indents to the given stream.

print_indent(Stream, Indent):-
  indent_size(IndentSize),
  Spaces is Indent * IndentSize,
  multi(format(Stream, ' ', []), Spaces).

%% print_list(
%%   +Out:oneof([atom_handle,codes_handle,stream]),
%%   +List:list
%% ) is det.
% @see Wrapper predicate for print_list/3, using no indent.

print_list(Out, List):-
  print_list(Out, 0, List).

%% print_list(
%%   +Out:oneof([atom_handle,codes_handle,stream]),
%%   +Indent:integer,
%%   +List:list
%% ) is det.
% Prints the elements of the given list to the given output stream or handle.
%
% Lists are printed recursively, using indentation relative to the given
% indentation level.
%
% @see The use of an atom and codes handle is copied from with_output_to/2.

print_list(Stream, Indent, List):-
  is_stream(Stream),
  print_list(Stream, Indent, List).
% Print the list to an atom or codes handle, as in with_output_to/2.
print_list(Out, Indent, List):-
  (
    Out = atom(_Atom)
  ;
    Out = codes(_Codes)
  ),
  !,
  setup_call_cleanup(
    (
      new_memory_file(Handle),
      open_memory_file(Handle, write, Stream)
    ),
    (
      print_list0(Stream, Indent, List),
      (
        Out = atom(Atom)
      ->
        memory_file_to_atom(Handle, Atom)
      ;
        Out = codes(Codes)
      ->
        memory_file_to_codes(Handle, Codes)
      )
    ),
    (
      close(Stream),
      free_memory_file(Handle)
    )
  ).

% Done!
print_list0(_Stream, _Indent, []):-
  !.
% Nested lists.
print_list0(Stream, Indent, [H | T]):-
  is_list(H),
  !,
  NewIndent is Indent + 1,
  print_list0(Stream, NewIndent, H),
  print_list0(Stream, Indent, T).
% Next...
print_list0(Stream, Indent, [H | T]):-
  print_indent(Stream, Indent),
  format(Stream, '~w', [H]),
  % @tbd Is this really needed?
  if_then(is_stream(Stream), flush_output(Stream)),
  format(Stream, '\n', []),
  print_list0(Stream, Indent, T).

% @tbd The predicates that appear below should be unified with some RDF module
%      used for exporting triples and with some TMS module used for exporting
%      justification chains.

print_proposition(Stream, Options, rdf(S, P, O)):-
  maplist(vertex_naming(Options), [S, P, O], [S0, P0, O0]),
  option(indent(Indent), Options, 0),
  option(index(Index), Options, 'c'),
  print_indent(Stream, Indent),
  format(Stream, '[~w] ~w ~w ~w\n', [Index, S0, P0, O0]).

print_proposition0(Stream, Options, Proposition):-
  print_proposition(Stream, Options, Proposition),
  !.
print_proposition0(Stream, Options, Proposition):-
  option(indent(Indent), Options, 0),
  option(index(Index), Options, c),
  print_indent(Stream, Indent),
  format(Stream, '[~w]:\t~w', [Index, Proposition]).

print_proof(_Stream, _Options, []).
print_proof(Stream, Options, [proof(Conclusion, Premises) | Proofs]):-
  print_proposition0(Stream, Options, Conclusion),
  select_option(indent(Indent), Options, Options0),
  succ(Indent, NewIndent),
  select_option(index(Index), Options0, Options1, 1),
  succ(Index, NewIndex),
  print_proof(Stream, [indent(NewIndent), index(1) | Options1], Premises),
  print_proof(Stream, [indent(Indent), index(NewIndex) | Options1], Proofs).
