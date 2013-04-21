:- module(
  print_ext,
  [
    print_indent/2, % +Stream:stream
                    % +Indent:integer
    print_list/2, % +Stream:stream
                  % +List:list
    print_proof/3 % +Stream:stream
                  % +Options:list
                  % +Proof:compound
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



print_indent(Stream, Indent):-
  Spaces is Indent * 2,
  multi(format(Stream, ' ', []), Spaces).

%% print_list(+Stream:stream, +List:list) is det.

print_list(atom(Atom), List):-
  !,
  new_memory_file(Handle),
  open_memory_file(Handle, write, Stream),
  print_list(Stream, List),
  close(Stream),
  memory_file_to_atom(Handle, Atom),
  free_memory_file(Handle).
print_list(Stream, List):-
  is_stream(Stream),
  print_list(Stream, 0, List).

print_list(_Stream, _Indent, []):-
  !.
print_list(Stream, Indent, [H | T]):-
  is_list(H),
  !,
  NewIndent is Indent + 1,
  print_list(Stream, NewIndent, H),
  print_list(Stream, Indent, T).
print_list(Stream, Indent, [H | T]):-
  print_indent(Stream, Indent),
  format(Stream, '~w', [H]),
  if_then(is_stream(Stream), flush_output(Stream)),
  format(Stream, '\n', []),
  print_list(Stream, Indent, T).

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

prolog:message(version(Version)) -->
  {Major is Version // 10000,
   Minor is (Version rem Major) // 100,
   Patch is Version rem 100},
  ['~w.~w.~w'-[Major, Minor, Patch]].

