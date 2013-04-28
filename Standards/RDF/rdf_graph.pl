:- module(
  rdf_graph,
  [
    rdf_bnode/2, % ?Graph:graph
                 % ?BNode:bnode
    rdf_clear/1, % +Graph:graph
    rdf_copy_graph/2, % +FromGraph:atom
                      % +ToGraph:atom
    rdf_copy_triples/5, % ?Subject:oneof([bnode,uri])
                        % ?Predicate:uri
                        % ?Object:oneof([bnode,literal,uri])
                        % +FromGraph:atom
                        % +ToGraph:atom
    rdf_graph_equivalence/2, % +Graph1:atom
                             % +Graph2:atom
    rdf_graph_source_file/2, % +Graph:atom
                             % -File:atom
    rdf_ground/1, % +Graph:atom
    rdf_name/2, % ?Graph:atom
                % +Name:oneof([literal,uri])
    rdf_object/2, % ?Graph:graph
                  % ?O:oneof([bnode,literal,uri])
    rdf_predicate/2, % ?Graph:atom
                     % ?P:uri
    rdf_subject/2, % ?Graph:atom
                   % ?S:oneof([bnode,uri])
    rdf_triples/2, % +Graph:atom
                   % -Triples:list(rdf_triple)
    rdf_vocabulary/2 % +Graph:atom
                     % -Vocabulary:ord_set(oneof([uri,literal]))
  ]
).

/** <module> RDF graph

Predicates that apply to entire RDF graphs.

Graph theory support for RDF is found in module [rdf_graph_theory].

For conversions from/to serialization formats, see module [rdf_serial].

---+ Abstract syntax

---++ RDF triple

An RDF triple contains three components:
    * *Subject*, either an RDF URI reference or a blank node.
    * *Predicate*, an RDF URI reference. Alternatively called _property_.
    * *Object*, either an RDF URI reference, a literal, or a blank node.

---++ RDF graph

A set of RDF triples.

---++ RDF node

The subject and objects of an RDF graph.

---++ RDF graph equivalence

RDF graphs G and G' are equivalent if there is a bijection M betweeen the sets
of nodes of G and G' such that:
    * M maps blank nodes to blank nodes.
    * M(lit)=lit for all RDF literals lit which are nodes of G.
    * M(uri)=uri for all RDF URI references uri which are nodes of G.
    * The triple (s,p,o) is in G if and only if the triple (M(s),p,M(o)) is
      in G'.

---++ RDF URI reference

A Unicode string encoded in UTF-8 (RFC 2279), without control characters
and producing a valid URI character sequence as defined by RFC 2396.

The encoded Unicode string consists of octets for US-ASCII characters and
%-escaping octets for non-US-ASCII characters.

Disallowed octets must be escaped by the URI escaping mechanism =|%HH|=,
where =HH= is a 2-digit hexadecimal numberal corresponding to the octect
value.

---+++ RDF URI reference equivalence

RDF URI references are equivalent iff they compare as equal,
character by character, as Unicode strings.

---++ RDF literal

There are two types of literals:
    1. *|Plain literal|*s, that have (1) a lexical form and
       (2) an optional language tag (RFC 3066) that is normalized to
       lowercase.
    2. *|Typed literal|*s, that have (1) a lexical form and
       (2) a datatype URI that is an RDF URI reference.

A lexical form is a Unicode string in NFC (Unicode Normalization Form C).

---+++ Literal equality

Two literals are equal iff:
    * The strings of the two lexical forms compare equal, character by
      character.
    * Either both or neither have language tags.
    * The language tags, if any, compare equal.
    * Either both or neither have datatype URIs.
    * The two datatype URIs, if any, compare equal, character by character.

---++ Blank nodes

The set of blank nodes, the set of literals, and the set of RDF URI references
are pairwise disjoint.

---+ TODO

---++ RDF graph instance

Suppose that M is a mapping from a set of blank nodes to some set of
literals, blank nodes and URI references; then any graph obtained from
a graph G by replacing some or all of the blank nodes N in G by M(N)
is an instance of G. Note that any graph is an instance of itself,
an instance of an instance of G is an instance of G, and if H is
an instance of G then every triple in H is an instance of some triple in G.

---++ RDF graph proper instance

A proper instance of a graph is an instance in which a blank node
has been replaced by a name, or two blank nodes in the graph have
been mapped into the same node in the instance.

---++ RDF graph equivalence

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

---++ RDF graph leanness

An RDF graph is lean if it has no instance which is a proper subgraph of the
graph. Non-lean graphs have internal redundancy and express the same content
as their lean subgraphs.

@author Wouter Beek
@version 2012/01-2013/04
*/

:- use_module(generics(file_ext)).
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
:- rdf_meta(rdf_copy_graph(+,+)).
:- rdf_meta(rdf_copy_triples(r,r,r,+,+)).
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

rdf_clear(Graph):-
  rdf_retractall(_S, _P, _O, Graph).

rdf_copy_graph(FromGraph, ToGraph):-
  rdf_copy_triples(_Subject, _Predicate, _Object, FromGraph, ToGraph).

%% rdf_copy_triples(
%%   ?Subject:uri,
%%   ?Premise:uri,
%%   ?Object:uri,
%%   +FromGraph:atom,
%%   +ToGraph:atom
%% ) is det.
% Copies RDF statements that adhere to the
% subject/predicate/object-restrictions between the given RDF graphs.
%
% @param Subject A resource.
% @param Predictae A resource.
% @param Object A resource.
% @param FromGraph The atomic name of a graph.
% @param ToGraph The atomic name of a graph.

rdf_copy_triples(Subject, Premise, Object, FromGraph, ToGraph):-
  flag(number_of_copied_triples, _OldId, 0),
  forall(
    rdf(Subject, Premise, Object, FromGraph),
    (
      rdf_assert(Subject, Premise, Object, ToGraph),
      flag(number_of_copied_triples, Id, Id + 1)
    )
  ),
  flag(number_of_copied_triples, Id, Id),
  (Id == 1 -> Word = triple ; Word = triples),
  format(user, '~w ~w were copied.\n', [Id, Word]).

%% rdf_graph_equivalence(+Graph1:atom, +Graph2:atom) is semidet.

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

%% rdf_graph_source_file(+Graph:atom, -File:atom) is semidet.
% Returns the name of the file from which the graph with the given name
% was loaded.

rdf_graph_source_file(Graph, File):-
  rdf_graph_property(Graph, source(Source)),
  uri_components(
    Source,
    uri_components(file, _Authority, File, _Search, _Fragments)
  ).

%% rdf_ground(+Graph:graph) is semidet.
% Succeeds if the given graph is ground, i.e., contains no blank node.

rdf_ground(Graph):-
  catch(rdf_graph(Graph), _Exception, fail),
  !,
  forall(
    rdf(S, P, O, Graph),
    rdf_ground(S, P, O)
  ).

%% rdf_ground(+S:subject, +P:predicate, +O:object) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_ground(S, _P, O):-
  \+ rdf_is_bnode(S),
  % The predicate cannot be a blank node by definition.
  \+ rdf_is_bnode(O).

%% rdf_name(?G:atom, ?Name:oneof([literal,uri])) is nondet.
%% rdf_name(-Name) is nondet.
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

rdf_object(G, O):-
  nonvar_det(rdf_object0(G, O)).
rdf_object0(G, O):-
  rdf(_, _, O, G),
  (
    rdf_is_resource(O)
  ;
    rdf_is_literal(O)
  ).

rdf_predicate(G, P):-
  nonvar_det(rdf_predicate0(G, P)).
rdf_predicate0(G, P):-
  rdf(_, P, _, G),
  rdf_is_resource(P).

rdf_subject(G, S):-
  nonvar_det(rdf_subject0(G, S)).
rdf_subject0(G, S):-
  rdf(S, _, _, G),
  rdf_is_resource(S).

%% rdf_triples(+Graph:atom, -Triples:list(rdf_triple)) is det.
% Returns an unsorted list containing all the triples in a graph.
%
% @param Graph The atomic name of a loaded RDF graph.
% @param Triples A list of triple compound term.

rdf_triples(Graph, Triples):-
  findall(rdf(S, P, O), rdf(S, P, O, Graph), Triples).

rdf_vocabulary(Graph, Vocabulary):-
  setoff(
    Name,
    rdf_name([literal(false)], Graph, Name),
    Vocabulary
  ).

