:- module(
  bordat,
  [
    generate_hasse/2 % +Context:compound
                     % -Hasse:ugraph
  ]
).

/** <module> Bordat

Bordat's algorithm for calculating FCA lattices.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(fca/fca)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(set/set_ext)).





%! generate_hasse(+Context:compound, -Hasse:ugraph) is det.

generate_hasse(context(Os,As,I), G):-
  powerset(As, PowAs),
  generate_hasse(context(Os,As,I), [], Vs, [], Es, [], PowAs),
  s_graph_components(G, Vs, Es).


%! generate_hasse(
%!   +Context:compound,
%!   +Vertices:list(compound),
%!   -AllVertices:list(compound),
%!   +Edges:list(pair(compound)),
%!   -AllEdges:list(pair(compound)),
%!   +ParentHistory:ordset(compound),
%!   +Powerset:list(ordset)
%! ) is det.

generate_hasse(_, Vs, Vs, Es, Es, _, []):- !.
generate_hasse(context(Os,As,I), Vs1, Vs, Es1, Es, Parents, [SubAs|SubAss]):-
  concept(context(Os,As,I), concept(_,SubAs), concept(ParentOs,ParentAs)),
  \+ memberchk(concept(ParentOs,ParentAs), Parents), !,
  generate_children(context(Os,As,I), SubAs, Children1),
  exclude(==(concept(ParentOs,ParentAs)), Children1, Children2),
  exclude(\Concept^member(Concept, Vs1), Children2, Children3),
  % Preserve concepts [concept(ParentOs,ParentAs)|Children3].
  append(Vs1, [concept(ParentOs,ParentAs)|Children3], Vs2),
  % Preserve edges between concept(ParentOs,ParentAs) and Children3.
  maplist(pair(concept(ParentOs,ParentAs)), Children3, NewEs),
  append(Es1, NewEs, Es2),
  generate_hasse(
    context(Os,As,I),
    Vs2,
    Vs,
    Es2,
    Es,
    [concept(ParentOs,ParentAs)|Parents],
    SubAss
  ).
generate_hasse(Context, Vs0, Vs, Es0, Es, Parents, [_|PowAs]):-
  generate_hasse(Context, Vs0, Vs, Es0, Es, Parents, PowAs).


generate_children(context(Os,As,I), SubAs, Children):-
  ord_subtract(As, SubAs, NewAs),
  findall(
    Child,
    (
      member(NewA, NewAs),
      % Find the minimal extension to the parent's attribute set
      % that respects the complete pair property.
      ord_add_element(SubAs, NewA, ChildAs0),
      a2o(context(Os,As,I), ChildAs0, ChildOs),
      o2a(context(Os,As,I), ChildOs, ChildAs),
      Child = concept(ChildOs,ChildAs)
    ),
    Children
  ).
