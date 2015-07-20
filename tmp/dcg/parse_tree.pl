:- module(
  parse_tree,
  [
    dcg_atom_codes_pt//3, % :Dcg
                          % -Tree:compound
                          % ?Atom:atom
    parse_tree/3 % +TreeName:atom
                 % +SubTrees:list
                 % -Tree:compound
  ]
).

/** <module> Parse trees

Predicate for manipulating parse trees.

@author Wouter Beek
@version 2013/05-2013/09, 2013/11-2013/12, 2014/06
*/

:- use_module(library(apply)).

:- use_module(plc(dcg/dcg_meta)).

:- meta_predicate(dcg_atom_codes_pt(4,-,?,?,?)).





dcg_atom_codes_pt(Dcg, T, Atom) -->
  {nonvar(Atom)},
  {atom_codes(Atom, Codes)},
  dcg_call_cp(Dcg, SubTs, Codes),
  {parse_tree(atom, SubTs, T)}.
dcg_atom_codes_pt(Dcg, T, Atom) -->
  {var(Atom)},
  dcg_call_cp(Dcg, SubTs, Codes),
  {parse_tree(atom, SubTs, T)},
  {atom_codes(Atom, Codes)}.


%! parse_tree(+TreeName:atom, +SubTrees:list, -Tree:compound)// is det.
% Constructs a tree based on a list of direct subtrees and variables
% (excluded).
%
% The variables come from unused optional rules in the DCG body.
%
% @arg TreeName The atomic name of the grammar rule for which
%      the tree is constructed.
% @arg SubTrees A list of compound terms (direct subtrees)
%      and variables (excluded from the created tree).
% @arg Tree A compound term representing a parse tree.

parse_tree(P, SubTs1, T):-
  include(nonvar, SubTs1, SubTs2),
  exclude(empty_list, SubTs2, SubTs3),
  T =.. [P|SubTs3].

empty_list([]).

