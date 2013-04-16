:- module(
  print,
  [
    indent/2, % +Stream:stream
              % +Indent:integer
    lines/2, % +Stream:stream
             % +L:list
    list/2, % +Stream:stream
            % +L:list
    proof/3 % +Stream:stream
            % +Options:list
            % +Proof:compound
  ]
).

/** <module> Print

Predicates for printing stuff.

---+ proof

A datatype of the form

==
proof(Conclusion, Premises)
==

where =Conclusion= is written using =print_conclusion/3= and =Premises=
is a list of proofs and/or premises that are written using =print_premise/3=.

@author Wouter Beek
@version 2013/01-2013/02
*/

:- use_module(generic(atom_ext)). % Meta-calls.
:- use_module(generic(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(rdf(rdf_export)).



indent(Stream, Indent):-
  Spaces is Indent * 2,
  multi(format(Stream, ' ', []), Spaces).

lines(Stream, List):-
  maplist(term_atom, List, Atoms),
  atomic_list_concat(Atoms, '\n', X),
  format(Stream, '~w', [X]).

list(Stream, List):-
  maplist(term_atom, List, Atoms),
  atomic_list_concat(Atoms, ',', X),
  format(Stream, '[~w]', [X]).

print_proposition(Stream, Options, rdf(S, P, O)):-
  maplist(vertex_naming(Options), [S, P, O], [S0, P0, O0]),
  option(indent(Indent), Options, 0),
  option(index(Index), Options, 'c'),
  indent(Stream, Indent),
  format(Stream, '[~w] ~w ~w ~w\n', [Index, S0, P0, O0]).

print_proposition0(Stream, Options, Proposition):-
  print_proposition(Stream, Options, Proposition),
  !.
print_proposition0(Stream, Options, Proposition):-
  option(indent(Indent), Options, 0),
  option(index(Index), Options, c),
  indent(Stream, Indent),
  format(Stream, '[~w]:\t~w', [Index, Proposition]).

proof(_Stream, _Options, []).
proof(Stream, Options, [proof(Conclusion, Premises) | Proofs]):-
  print_proposition0(Stream, Options, Conclusion),
  select_option(indent(Indent), Options, Options0),
  succ(Indent, NewIndent),
  select_option(index(Index), Options0, Options1, 1),
  succ(Index, NewIndex),
  proof(Stream, [indent(NewIndent), index(1) | Options1], Premises),
  proof(Stream, [indent(Indent), index(NewIndex) | Options1], Proofs).

prolog:message(version(Version)) -->
  {Major is Version // 10000,
   Minor is (Version rem Major) // 100,
   Patch is Version rem 100},
  ['~w.~w.~w'-[Major, Minor, Patch]].

