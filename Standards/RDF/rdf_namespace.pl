:- module(
  rdf_namespace,
  [
    rdf_convert_namespace/3, % +Graph:atom
                             % +FromNamespace:atom
                             % +ToNamespace:atom
    rdf_convert_namespace/4, % +FromNamespace:atom
                             % +From:oneof([bnode,literal,uri])
                             % +ToNamespace:atom
                             % -To:oneof([bnode,literal,uri])
    rdf_current_namespaces/1, % -Namespaces:ord_set(atom)
    rdf_current_namespaces/2, % +Graph:graph
                              % -Namespaces:ord_set(atom)
    rdf_known_namespace/2, % ?Prefix:atom
                           % ?URI:uri
    rdf_register_namespace/1, % +Prefix:atom
    rdf_register_namespace/2, % +Prefix:atom
                              % +URI:uri
    rdf_resource_to_namespace/3 % +Resource:uri
                                % -Namespace:atom
                                % -Name:atom
  ]
).

/** <module> RDF namespaces

Namespace support for RDF(S).

@author Wouter Beek
@version 2013/03
*/

:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_resource_to_namespace(r,-,-)).



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

%% rdf_current_namespaces(-Namespaces:list(atom)) is det.
% Returns all current namespace aliases.
%
% @param Namespaces A list of atomic alias names.

rdf_current_namespaces(Namespaces):-
  setoff(
    Namespace,
    rdf_current_prefix(Namespace, _URI),
    Namespaces
  ).

%% rdf_current_namespaces(+Graph:atom, -Namespaces:ord_set(atom)) is det.
% Returns the namespaces that occur in the given graph.
%
% @param Graph The atomic name of a graph.
% @param Namespaces An ordered set of atomic names of namespaces.

rdf_current_namespaces(_Graph, Namespaces):-
  rdf_current_namespaces(Namespaces).

/* THIS IS TOO EXPENSIVE FOR BIGGER GRAPHS.
rdf_current_namespaces(Graph, Namespaces):-
  meta_ext:setoff_alt(
    Namespace,
    rdf_namespace:rdf_lookup_namespace(Graph, Namespace, _URI),
    Namespaces
  ).
*/

%%%% d2r http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#
%%%% map file:/D:/D2RServer/d2r-server-0.7/mapping.n3#

rdf_known_namespace(bag,           'http://lod.geodan.nl/BAG/resource/'           ).
rdf_known_namespace(bags,          'http://lod.geodan.nl/BAG/vocab/resource/'     ).
rdf_known_namespace(dbpedia,       'http://dbpedia.org/resource/'                 ).
rdf_known_namespace('dbpedia-owl', 'http://dbpedia.org/ontology/'                 ).
rdf_known_namespace(dbpprop,       'http://dbpedia.org/property/'                 ).
rdf_known_namespace(dc,            'http://purl.org/dc/terms/'                    ).
rdf_known_namespace(foaf,          'http://xmlns.com/foaf/0.1/'                   ).
rdf_known_namespace(lexvo,         'http://lexvo.org/id/'                         ).
rdf_known_namespace(ogc,           'http://lod.geodan.nl/geosparql_vocab_all.rdf#').
rdf_known_namespace(owl,           'http://www.w3.org/2002/07/owl#'               ).
rdf_known_namespace(plod,          'http://plod.nieuwland.nl/top10nl/resource/'   ).
rdf_known_namespace('powder-s',    'http://www.w3.org/2007/05/powder-s#'          ).
rdf_known_namespace(prv,           'http://purl.org/net/provenance/ns#'           ).
rdf_known_namespace(rdf,           'http://www.w3.org/1999/02/22-rdf-syntax-ns#'  ).
rdf_known_namespace(rdfs,          'http://www.w3.org/2000/01/rdf-schema#'        ).
rdf_known_namespace(skos,          'http://www.w3.org/2004/02/skos/core#'         ).
rdf_known_namespace(umbel,         'http://umbel.org/umbel/rc/'                   ).
rdf_known_namespace(void,          'http://rdfs.org/ns/void#'                     ).
rdf_known_namespace(xsd,           'http://www.w3.org/2001/XMLSchema#'            ).
rdf_known_namespace(yago,          'http://dbpedia.org/class/yago/'               ).

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

rdf_register_namespace(Prefix):-
  once(rdf_known_namespace(Prefix, URI)),
  rdf_register_namespace(Prefix, URI).

rdf_register_namespace(Prefix, URI):-
  rdf_register_prefix(Prefix, URI, [force(false), keep(true)]).

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

