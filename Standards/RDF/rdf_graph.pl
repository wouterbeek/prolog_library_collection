:- module(
  rdf_graph,
  [
    rdf_bnode/2, % ?Graph:graph
                 % ?BNode:bnode
    rdf_graph_equivalence/2, % +Graph1:atom
                             % +Graph2:atom
    rdf_graph_merge/2, % +Graphs:list(atom)
                       % +MergedGraph:atom
    rdf_graph_source_file/2, % +Graph:atom
                             % -File:atom
    rdf_graph_triples/2, % ?Graph:atom
                         % ?Triples:list
    rdf_ground/1, % +Graph:atom
    rdf_is_graph_instance_of/3, % +GraphInstance:atom
                                % +Graph:atom
                                % -BNodeMap:list(list)
    rdf_is_graph_proper_instance_of/2, % +GraphInstance:atom
                                       % +Graph:atom
    rdf_name/2, % ?Graph:atom
                % +Name:oneof([literal,uri])
    rdf_new_graph/2, % +Graph1:atom
                     % -Graph2:atom
    rdf_object/2, % ?Graph:graph
                  % ?Objects:oneof([bnode,literal,uri])
    rdf_predicate/2, % ?Graph:atom
                     % ?Predicate:uri
    rdf_predicate2/2, % +Graph:atom
                      % -Predicates:ordset(uri)
    rdf_subject/2, % ?Graph:atom
                   % ?Subject:oneof([bnode,uri])
    rdf_subjects/2, % +Graph:atom
                    % -Subjects:ordset(uri)
    rdf_triples/2, % +Graph:atom
                   % -Triples:list(rdf_triple)
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ordset(oneof([uri,literal]))
  ]
).

/** <module> RDF graph

Predicates that apply to entire RDF graphs.

Graph theory support for RDF is found in module [rdf_graph_theory].

For conversions from/to serialization formats, see module [rdf_serial].

# Abstract syntax

## RDF triple

An RDF triple contains three components:
    * *Subject*, either an RDF URI reference or a blank node.
    * *Predicate*, an RDF URI reference. Alternatively called _property_.
    * *Object*, either an RDF URI reference, a literal, or a blank node.

## RDF graph

A set of RDF triples.

## RDF node

The subject and objects of an RDF graph.

## RDF graph equivalence

RDF graphs G and G' are equivalent if there is a bijection M betweeen the sets
of nodes of G and G' such that:
    * M maps blank nodes to blank nodes.
    * M(lit)=lit for all RDF literals lit which are nodes of G.
    * M(uri)=uri for all RDF URI references uri which are nodes of G.
    * The triple (s,p,o) is in G if and only if the triple (M(s),p,M(o)) is
      in G'.

## RDF URI reference

A Unicode string encoded in UTF-8 (RFC 2279), without control characters
and producing a valid URI character sequence as defined by RFC 2396.

The encoded Unicode string consists of octets for US-ASCII characters and
%-escaping octets for non-US-ASCII characters.

Disallowed octets must be escaped by the URI escaping mechanism =|%HH|=,
where =HH= is a 2-digit hexadecimal numberal corresponding to the octect
value.

### RDF URI reference equivalence

RDF URI references are equivalent iff they compare as equal,
character by character, as Unicode strings.

## RDF literal

There are two types of literals:
    1. *|Plain literal|*s, that have (1) a lexical form and
       (2) an optional language tag (RFC 3066) that is normalized to
       lowercase.
    2. *|Typed literal|*s, that have (1) a lexical form and
       (2) a datatype URI that is an RDF URI reference.

A lexical form is a Unicode string in NFC (Unicode Normalization Form C).

### Literal equality

Two literals are equal iff:
    * The strings of the two lexical forms compare equal, character by
      character.
    * Either both or neither have language tags.
    * The language tags, if any, compare equal.
    * Either both or neither have datatype URIs.
    * The two datatype URIs, if any, compare equal, character by character.

## Blank nodes

The set of blank nodes, the set of literals, and the set of RDF URI references
are pairwise disjoint.

# TODO

## RDF graph equivalence

Any instance of a graph in which a blank node is mapped to a new blank
node not in the original graph is an instance of the original and also
has it as an instance, and this process can be iterated so that any 1:1
mapping between blank nodes defines an instance of a graph which has the
original graph as an instance. Two such graphs, each an instance of the
other but neither a proper instance, which differ only in the identity
of their blank nodes, are considered to be equivalent. We will treat
such equivalent graphs as identical; this allows us to ignore some
issues which arise from 're-naming' nodeIDs, and is in conformance
with the convention that blank nodes have no label. Equivalent graphs
are mutual instances with an invertible instance mapping.

## RDF graph leanness

An RDF graph is lean if it has no instance which is a proper subgraph of the
graph. Non-lean graphs have internal redundancy and express the same content
as their lean subgraphs.

@author Wouter Beek
@version 2012/01-2013/05
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_export)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(standards(graphviz)).

:- rdf_meta(rdf_bnode(?,r)).
:- rdf_meta(rdf_name(?,r)).
:- rdf_meta(rdf_object(?,r)).
:- rdf_meta(rdf_predicate(?,r)).
:- rdf_meta(rdf_subject(?,r)).



rdf_bnode(Graph, BNode):-
  (
    rdf_subject(Graph, BNode)
  ;
    rdf_object(Graph, BNode)
  ),
  rdf_is_bnode(BNode).

%! rdf_bnode_replace(+SharedBNodes:list, +Triple, -NewTriple) is det.
% Replaces shared bnodes in triples.

% In case there are not shared blank nodes we can skip
% rdf_bnode_replace/4.
rdf_bnode_replace([], rdf(S, P, O, _Graph), rdf(S, P, O)):-
  !.
rdf_bnode_replace(SharedBNodes, rdf(S, P, O, Graph), rdf(NewS, P, NewO)):-
  maplist(rdf_bnode_replace(SharedBNodes, Graph), [S,O], [NewS, NewO]).

rdf_bnode_replace(SharedBNodes, G, R, NewR):-
  rdf_is_bnode(R),
  memberchk(_/G/R, SharedBNodes),
  !,
  rdf_bnode(NewR).
rdf_bnode_replace(_SharedBNodes, _G, R, R).

%! rdf_graph_equivalence(+Graph1:atom, +Graph2:atom) is semidet.

rdf_graph_equivalence(Graph1, Graph2):-
  rdf_graph_equivalence0(Graph1, Graph2),
  rdf_graph_equivalence0(Graph2, Graph1).
rdf_graph_equivalence0(Graph1, Graph2):-
  forall(
    rdf(Subject1, Predicate, Object1, Graph1),
    (
      rdf(Subject2, Predicate, Object2, Graph2),
      rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2),
      rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2)
    )
  ).
rdf_graph_equivalence_subject0(_Graph1, Subject, _Graph2, Subject):-
  rdf_is_resource(Subject),
  !.
rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2):-
  bnode_translation0(Graph1, Subject1, Graph2, Subject2).
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_resource(Object),
  !.
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_literal(Object),
  !.
rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2):-
  bnode_translation0(Graph1, Object1, Graph2, Object2).
bnode_translation0(Graph1, Resource1, Graph2, Resource2):-
  rdf_bnode(Graph1, Resource1),
  rdf_bnode(Graph2, Resource2),
  !.

%! rdf_graph_merge(+Graphs:list(atom), +MergedGraph:atom) is det.
% When merging RDF graphs we have to make sure that their blank nodes are
% standardized apart.

rdf_graph_merge(Graphs, MergedGraph):-
  % Type checking.
  maplist(rdf_graph, Graphs),
  atom(MergedGraph),
  !,

  % Collect the shared blank nodes.
  findall(
    Graph1/Graph2/SharedBNode,
    (
      member(Graph1, Graph2, Graphs),
      % Use the natural order of atomic names.
      % The idea is that we only replace shared blank nodes in
      % the latter graph.
      Graph1 @< Graph2,
      rdf_bnode(Graph1, SharedBNode),
      rdf_bnode(Graph2, SharedBNode)
    ),
    SharedBNodes
  ),

  % Replace the blank nodes.
  forall(
    (
      member(Graph, Graphs),
      rdf(S, P, O, Graph)
    ),
    (
      rdf_bnode_replace(
        SharedBNodes,
        rdf(S, P, O, Graph),
        rdf(NewS, P, NewO)
      ),
      rdf_assert(NewS, P, NewO, MergedGraph)
    )
  ).
rdf_graph(Graph, MergedGraph):-
  rdf_graph(Graph),
  !,
  rdf_graph([Graph], MergedGraph).

%! rdf_graph_source_file(+Graph:atom, -File:atom) is semidet.
% Returns the name of the file from which the graph with the given name
% was loaded.

rdf_graph_source_file(Graph, File):-
  rdf_graph_property(Graph, source(Source)),
  uri_components(
    Source,
    uri_components(file, _Authority, File, _Search, _Fragments)
  ).

% Instantiations (+,+)-semidet and (-,+)-nondet.
rdf_graph_triples(G, Triples):-
  is_list(Triples),
  !,
  rdf_graph(G),
  forall(
    member(rdf(S, P, O, G), Triples),
    rdf(S, P, O, G)
  ),
  \+ (rdf(S, P, O, G), \+ member(rdf(S, P, O, G), Triples)).
% Instantiation (+,-)-det.
rdf_graph_triples(G, Triples):-
  rdf_graph(G),
  !,
  findall(
    rdf(S, P, O, G),
    rdf(S, P, O, G),
    Triples
  ).

%! rdf_ground(+Graph:graph) is semidet.
% Succeeds if the given graph is ground, i.e., contains no blank node.
%! rdf_ground(+Triple) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.
% The predicate cannot be a blank node by definition.

rdf_ground(G):-
  catch(rdf_graph(G), _Exception, fail),
  !,
  forall(
    rdf(S, P, O, G),
    rdf_ground(rdf(S, P, O))
  ).
rdf_ground(rdf(S, _P, O)):-
  \+ rdf_is_bnode(S),
  \+ rdf_is_bnode(O).

%! rdf_is_graph_instance_of(
%!   +GraphInstance:atom,
%!   +Graph:atom,
%!   -BNodeMap:list(list)
%! ) is nondet.
% # RDF graph instance
% Suppose that M is a mapping from a set of blank nodes to some set of
% literals, blank nodes and URI references; then any graph obtained from
% a graph G by replacing some or all of the blank nodes N in G by M(N)
% is an instance of G. Note that any graph is an instance of itself,
% an instance of an instance of G is an instance of G, and if H is
% an instance of G then every triple in H is an instance of some triple in G.

rdf_is_graph_instance_of(GI, G, BNodeMap):-
  maplist(rdf_graph, [GI,G]),
  rdf_graph_triples(G, Triples),
  rdf_is_graph_instance_of0(GI, Triples, [], BNodeMap),
  \+ (rdf(SI, P, OI, GI),
    \+ ((
      member(rdf(S, P, O, G), Triples),
      rdf_is_instance_of(rdf(SI, P, OI, GI), rdf(S, P, O, G), BNodeMap)
    ))
  ).
rdf_is_graph_instance_of0(_GI, [], BNodeMap, BNodeMap).
rdf_is_graph_instance_of0(GI, [rdf(S, P, O, G) | Triples], BNodeMap, Solution):-
  (rdf_is_instance_of(GI, SI, G, S, BNodeMap) ; NewMap1 = [[S,SI]]), !,
  (rdf_is_instance_of(GI, OI, G, O, BNodeMap) ; NewMap2 = [[O,OI]]), !,
  rdf(SI, P, OI, GI),
  append([NewMap1, NewMap2, BNodeMap], NewBNodeMap),
  rdf_is_graph_instance_of0(GI, Triples, NewBNodeMap, Solution).

%! rdf_is_graph_proper_instance_of(
%!   +GraphProperInstance:atom,
%!   +Graph:atom
%! ) is semidet.
% # RDF graph proper instance
% A proper instance of a graph is an instance in which a blank node
% has been replaced by a name, or two blank nodes in the graph have
% been mapped into the same node in the instance.

rdf_is_graph_proper_instance_of(GI, G):-
  rdf_is_graph_instance_of(GI, G, BNodeMap),
  (
    member([_BNode,Name], BNodeMap),
    rdf_name(GI, Name)
  ;
    member([BNode1,BNode], BNodeMap),
    member([BNode2,BNode], BNodeMap),
    BNode1 \== BNode2
  ),
  !.

%! rdf_is_instance_of(+TripleInstance, +Triple) is semidet.

rdf_is_instance_of(rdf(SI, P, OI, GI), rdf(S, P, O, G), BNodeMap):-
  rdf_is_instance_of(GI, SI, G, S, BNodeMap),
  rdf_is_instance_of(GI, OI, G, O, BNodeMap).

%! rdf_is_instance_of(
%!   +GraphInstance:atom,
%!   +ResourceInstance:oneof([bnode,literal,uri]),
%!   +Graph:atom,
%!   +Resource:oneof([bnode,literal,uri]),
%!   +BNodeMap:list(list)
%! ) is semidet.

rdf_is_instance_of(GI, R, _G, R, _BNodeMap):-
  rdf_name(GI, R),
  !.
rdf_is_instance_of(_GI, RI, _G, R, BNodeMap):-
  memberchk([R,RI], BNodeMap).

%! test_rdf_is_graph_instance_of(-Maps:list(list)) is det.
% Tests predicate rdf_is_graph_instance_of/2.
% Should return two lists of mappings from blank nodes to uriRefs.

test_rdf_is_graph_instance_of(Maps):-
  maplist(rdf_unload_graph, [g,gi]),
  rdf_bnode(X1), rdf_bnode(X2), rdf_bnode(X3), rdf_bnode(X4),
  rdf_assert(X1, rdf:p, X2, g), rdf_assert(X3, rdf:p, X4, g),
  rdf_assert(rdf:a, rdf:p, rdf:b, gi), rdf_assert(rdf:c, rdf:p, rdf:d, gi),
  findall(Map, rdf_is_graph_instance_of(gi, g, Map), Maps).

%! rdf_name(?G:atom, ?Name:oneof([literal,uri])) is nondet.
%! rdf_name(-Name) is nondet.
% Succeeds if the given object is an RDF name, i.e., an RDF URI reference or
% an RDF literal.

rdf_name(Graph, Name):-
  rdf_name([literal(true)], Graph, Name).

rdf_name(Options, Graph, Name):-
  nonvar_det(rdf_name0(Options, Graph, Name)).
rdf_name0(_Options, Graph, Name):-
  rdf_subject(Graph, Name),
  \+ rdf_is_bnode(Name).
rdf_name0(_Options, Graph, Name):-
  rdf_predicate(Graph, Name).
rdf_name0(Options, Graph, Name):-
  rdf_object(Graph, Object),
  (
    option(literal(true), Options),
    Object = literal(type(_LexicalValue, Datatype))
  ->
    (Name = Object ; Name = Datatype)
  ;
    rdf_is_bnode(Object)
  ->
    fail
  ;
    Name = Object
  ).

%! rdf_new_graph(+Graph1:atom, -Graph2:atom) is det.

rdf_new_graph(Graph, Graph):-
  \+ rdf_graph(Graph),
  !.
rdf_new_graph(Graph1, Graph3):-
  split_atom_exclusive(Graph1, '_', Splits),
  reverse(Splits, [LastSplit | RSplits]),
  (
    atom_number(LastSplit, OldNumber)
  ->
    NewNumber is OldNumber + 1,
    atom_number(NewLastSplit, NewNumber),
    reverse([NewLastSplit | RSplits], NewSplits)
  ;
    reverse(['1', LastSplit | RSplits], NewSplits)
  ),
  atomic_list_concat(NewSplits, '_', Graph2),
  rdf_new_graph(Graph2, Graph3).

rdf_object(G, O):-
  nonvar_det(rdf_object0(G, O)).
rdf_object0(G, O):-
  rdf(_, _, O, G).

rdf_predicate(G, P):-
  nonvar_det(rdf_predicate0(G, P)).
rdf_predicate0(G, P):-
  rdf(_, P, _, G).

rdf_predicates(Graph, Predicates):-
  setoff(
    Predicate,
    rdf_predicate(Graph, Predicate),
    Predicates
  ).

rdf_subject(G, S):-
  nonvar_det(rdf_subject0(G, S)).
rdf_subject0(G, S):-
  rdf(S, _, _, G).

rdf_subjects(Graph, Subjects):-
  rdf_graph(Graph),
  setoff(
    Subject,
    (
      rdf(Subject, _Predicate, _Object, Graph),
      \+ rdf_is_bnode(Subject)
    ),
    Subjects
  ).

%! rdf_triples(+Graph:atom, -Triples:list(rdf_triple)) is det.
% Returns an unsorted list containing all the triples in a graph.
%
% @arg Graph The atomic name of a loaded RDF graph.
% @arg Triples A list of triple compound term.

rdf_triples(Graph, Triples):-
  findall(rdf(S, P, O), rdf(S, P, O, Graph), Triples).

rdf_vocabulary(Graph, Vocabulary):-
  setoff(
    Name,
    rdf_name([literal(false)], Graph, Name),
    Vocabulary
  ).
