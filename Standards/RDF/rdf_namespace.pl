:- module(
  rdf_namespace,
  [
% RDF NAMESPACE CONVERSION
    rdf_convert_namespace/3, % +Graph:atom
                             % +FromNamespace:atom
                             % +ToNamespace:atom
    rdf_convert_namespace/4, % +FromNamespace:atom
                             % +From:oneof([bnode,literal,uri])
                             % +ToNamespace:atom
                             % -To:oneof([bnode,literal,uri])

% RDF NAMESPACE REGISTRATION
    rdf_current_namespace/2, % +Graph:graph
                             % -Namespace:atom
    rdf_current_namespaces/2, % +Graph:graph
                              % -Namespaces:ordset(atom)
    rdf_register_namespace/2, % +Graph:atom
                              % +Prefix:atom
    rdf_register_namespace/3, % +Graph:atom
                              % +Prefix:atom
                              % +URI:uri

% RDF NAMESPACE EXTRACTION
    rdf_resource_to_namespace/3 % +Resource:uri
                                % -Namespace:atom
                                % -Name:atom
  ]
).

/** <module> RDF namespaces

Namespace support for RDF(S).

@author Wouter Beek
@version 2013/03-2013/05
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(rdf_resource_to_namespace(r,-,-)).

:- dynamic(rdf_current_namespace(_Graph, _Prefix)).



% RDF NAMESPACE CONVERSION %

%% rdf_convert_namespace(
%%   +Graph:atom,
%%   +FromNamespace:atom,
%%   +ToNamespace:atom
%% ) is det.
% Converts all resources in the given graph that have the given namespace
% to similar resources that have another namespace.
% The converted to namespace must be a current RDF prefix.

rdf_convert_namespace(Graph, FromNamespace, ToNamespace):-
  flag(number_of_resource_conversions, _OldId1, 0),
  flag(number_of_triple_conversions,   _OldId2, 0),
  forall(
    rdf(FromS, FromP, FromO, Graph),
    (
      rdf_convert_namespace(FromNamespace, FromS, ToNamespace, ToS),
      rdf_convert_namespace(FromNamespace, FromP, ToNamespace, ToP),
      rdf_convert_namespace(FromNamespace, FromO, ToNamespace, ToO),
      flag(number_of_triple_conversions, ID, ID + 1),
      rdf_retractall(FromS, FromP, FromO, Graph),
      rdf_assert(ToS, ToP, ToO, Graph)
    )
  ),
  flag(number_of_triple_conversions, ID1, ID1 + 1),
  (ID1 == 1 -> Word1 = triple ; Word1 = triples),
  format(user, '~w ~w were converted.\n', [ID1, Word1]),
  flag(number_of_resource_conversions, ID2, ID2 + 1),
  (ID2 == 1 -> Word2 = resource ; Word2 = resources),
  format(user, '~w ~w were converted.\n', [ID2, Word2]).

%% rdf_convert_namespace(
%%   +FromNamespace:atom,
%%   +From:oneof([bnode,literal,uri]),
%%   +ToNamespace:atom,
%%   -To:oneof([bnode,literal,uri])
%% ) is det.
% Converts a resource to the given namespace.
% The namespace must be a current RDF prefix.
% Blank nodes and literals are copied as is (they do not have a namespace).

rdf_convert_namespace(_FromNamespace, BNode, _ToNamespace, BNode):-
  rdf_is_bnode(BNode),
  !.
rdf_convert_namespace(_FromNamespace, Literal, _ToNamespace, Literal):-
  rdf_is_literal(Literal),
  !.
rdf_convert_namespace(FromNamespace, From, ToNamespace, To):-
  rdf_is_resource(From),
  !,
  (
    rdf_global_id(FromNamespace:LocalName, From)
  ->
    rdf_current_prefix(ToNamespace, _URI),
    rdf_global_id(ToNamespace:LocalName, To),
    flag(number_of_resource_conversions, ID, ID + 1)
  ;
    To = From
  ).



% RDF NAMESPACE REGISTRATION %

%% rdf_current_namespaces(+Graph:atom, -Namespaces:ordset(atom)) is det.
% Returns the namespaces that occur in the given graph.
%
% @param Graph The atomic name of a graph.
% @param Namespaces An ordered set of atomic names of namespaces.

rdf_current_namespaces(Graph, Namespaces):-
  setoff(Namespace, rdf_current_namespace(Graph, Namespace), Namespaces).

rdf_register_namespace(Graph, Prefix):-
  % The given prefix must be registered as an XML namespace.
  xml_current_namespace(Prefix, _URI),
  assert(rdf_current_namespace(Graph, Prefix)).

rdf_register_namespace(Graph, Prefix, URI):-
  xml_register_namespace(Prefix, URI),
  assert(rdf_current_namespace(Graph, Prefix)).



% RDF NAMESPACE EXTRACTION %

rdf_lookup_namespace(Graph, Namespace, URI):-
  % A namespace occurs for a resource.
  rdf_term(Graph, Term),
  \+ rdf_is_bnode(Term),
  (
    rdf_is_resource(Term)
  ->
    Resource = Term
  ;
    Term = literal(type(Resource, _Value))
  ),
  rdf_resource_to_namespace(Resource, Namespace, _Name),
  % Only registered prefixes/namespaces are returned.
  rdf_current_prefix(Namespace, URI).

%% rdf_resource_to_namespace(
%%   +Resource:uri,
%%   -Namespace:atom,
%%   -Name:atom
%% ) is det.

rdf_resource_to_namespace(Resource, Namespace, Name):-
  rdf_is_resource(Resource),
  findall(
    Namespace-Name,
    rdf_global_id(Namespace:Name, Resource),
    Pairs
  ),
  (
    Pairs == [Namespace-Name],
    !
  ;
    member(Namespace-Name, Pairs),
    atom_length(Name, NameLength),
    \+ ((
      member(_OtherNamespace-OtherName, Pairs),
      atom_length(OtherName, OtherNameLength),
      OtherNameLength < NameLength
    ))
  ).

