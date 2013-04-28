:- module(
  owl_entailment,
  [
    materialize/0,
    materialize/1, % +Graph:atom
    rdfm/3
  ]
).

/** <module> OWL ENTAILMENT

The function of an entailment module is  to provide an implementation of
rdf/3 that extends basic triple-lookup using the entailment rules of the
semantic web sub language of RDF.

This entailment module does RDFS entailment.

@tbd  Check the completeness

@author Wouter Beek
@version 2013/04
*/

:- use_module(generics(meta_ext)).
:- use_module(library(nb_set)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdfm(o,o,o)).
:- rdf_meta(individual_of(r,r)).
:- rdf_meta(rdfs_individual_of(r,r)).
:- rdf_meta(rdfs_subclass_of(r,r)).
:- rdf_meta(rdfs_subproperty_of(r,r)).

:- rdf_register_prefix(serql, 'http://www.openrdf.org/schema/serql#').



materialize:-
  materialize0(Triples),
  forall(
    member([S, P, O], Triples),
    rdf_assert(S, P, O)
  ).

materialize(Graph):-
  rdf_graph(Graph),
  materialize0(Triples),
  forall(
    member([S, P, O], Triples),
    rdf_assert(S, P, O, Graph)
  ).

materialize0(Triples2):-
  findall(
    [S, P, O],
    rdfm(S, P, O),
    Triples1
  ),
  length(Triples1, Length1),
  sort(Triples1, Triples2),
  length(Triples2, Length2),
  if_then(Length1 == Length2, gtrace).

% @tbd should move to compiler
rdfm(literal(L), _, _):-
  nonvar(L),
  !,
  fail.
% @tbd should move to compiler
rdfm(_, literal(L), _):-
  nonvar(L),
  !,
  fail.
% Take some property P, then query (S,P,O)???
rdfm(S, P, O):-
  var(P),
  !,
  (
    rdf(S, P, O)
  ;
    rdfm(P, rdf:type, rdf:'Property'),
    rdfm(S, P, O),
    \+ rdf(S, P, O)
  ).
% INDIVIDUAL-OF
% Use a superproperty of =|rdf:type|= for finding individuals.
rdfm(S, P, C):-
  rdf_reachable(rdf:type, rdfs:subPropertyOf, P),
  !,
  % Also use classes that result from domain/range restrictions.
  individual_of(S, C).
% SUBCLASS-OF
% Use a superproperty of =|rdfs:subClassOf|= for finding 
rdfm(S, P, O):-
  rdf_reachable(rdfs:subClassOf, rdfs:subPropertyOf, P),
  !,
  (
    (
      nonvar(S)
    ;
      nonvar(O)
    )
  ->
    % Generate from given start.
    rdfs_subclass_of(S, O)
  ;
    % Generated unbounded (slow!)
    individual_of(S, rdfs:'Class'),
    rdfs_subclass_of(S, O)
  ).
% SUBPROPERTY-OF
rdfm(S, rdfs:subPropertyOf, O):-
  !,
  (
    nonvar(S)
  ->
    individual_of(S, rdf:'Property'),
    rdfs_subproperty_of(S, O)
  ;
    nonvar(O)
  ->
    individual_of(O, rdf:'Property'),
    rdfs_subproperty_of(S, O)
  ;
    individual_of(S, rdf:'Property'),
    rdfs_subproperty_of(S, O)
  ).
% SERQL (1/3)
rdfm(S, serql:directSubClassOf, O):-
  !,
  rdf_has(S, rdfs:subClassOf, O).
% SERQL (2/3)
rdfm(S, serql:directType, O):-
  !,
  rdf_has(S, rdf:type, O).
% SERQL (3/3)
rdfm(S, serql:directSubPropertyOf, O):-
  !,
  rdf_has(S, rdfs:subPropertyOf, O).
% Uses the property hierarchy DOWNWARD???.
% Uses inverse properties.
rdfm(S, P, O):-
  rdf_has(S, P, O).

%% individual_of(?Resource, ?Class)

% Instantiation(+,?)
individual_of(Resource, Class):-
  nonvar(Resource),
  !,
  (
    % Literals are individuals of (a subclass of) rdfs:Literal.
    Resource = literal(_)
  ->
    rdfs_subclass_of(Class, rdfs:'Literal')
  ;
    % First find a direct individual relation or a domain/range restriction.
    % Then traverse the class hierarchy upwards for the found class.
    rdfs_has_type(Resource, MyClass),
    rdfs_subclass_of(MyClass, Class)
  ;
    % A resource that has no explicit individual statement is an individual
    % of rdfs:Resource.
    rdf_equal(Class, rdfs:'Resource')
  ).
% Instantiation(?,+)
individual_of(Resource, Class):-
  nonvar(Class),
  !,
  (
    % Every resource in an individual of rdfs:Resource.
    rdf_equal(Class, rdfs:'Resource')
  ->
    rdf_subject(Resource)
  ;
    % Traverse down the class hierarchy.
    rdfs_subclass_of(SubClass, Class),
    rdfs_has_type(Resource, SubClass)
  ).
% Instantiation(?,?)
% Try for every subject resource.
individual_of(Resource, Class):-
  rdf_subject(Resource),
  individual_of(Resource, Class).

%% rdfs_has_type(+Resource, -Class) is nondet.
%% rdfs_has_type(-Resource, +Class) is nondet.
% Perform RDFS entailment rules to enumerate the types of Resource
% or generate all resources entailed by the given class.

rdfs_has_type(Resource, Class):-
  empty_nb_set(Set),
  (
    atom(Resource)
  ->
    (
      rdf_has(Resource, rdf:type, Class)
    ;
      rdf_has(Resource, P, _),
      % [WB] When =|P=inverse_of(URI)|= an exception is thrown.
      rdf_is_resource(P),
      rdf_has(P, rdfs:domain, Class)
    ;
      rdf_has(_, P, Resource),
      rdf_has(P, rdfs:range, Class)
    ),
    add_nb_set(Class, Set, New),
    New == true
  ;
    atom(Class)
  ->
    (
      rdf_has(Resource, rdf:type, Class)
    ;
      rdf_has(P, rdfs:domain, Class),
      rdf_has(Resource, P, _)
    ;
      rdf_has(P, rdfs:range, Class),
      rdf_has(_, P, Resource)
    ),
    add_nb_set(Resource, Set, New),
    New == true
  ;
    throw(error(instantiation_error, _))
  ).

%% rdfs_individual_of(+Resource, +Class) is semidet.
%% rdfs_individual_of(+Resource, -Class) is nondet.
%% rdfs_individual_of(-Resource, +Class) is nondet.
% Generate resources belonging to a class or classes a resource
% belongs to. We assume everything at the `object' end of a triple
% is a class. A validator should confirm this property.
%
% rdfs_individual_of(+,-) does not exploit domain and range
% properties, deriving that if =|rdf(R,P,_)|= is present R must
% satisfy the domain of P (and similar for range).
%
% There are a few hacks:
%     * Any resource is an individual of rdfs:Resource
%     * =|literal(_)|= is an individual of rdfs:Literal

% Instantiations (+,?)
% The individual is given.
rdfs_individual_of(Resource, Class):-
  nonvar(Resource),
  !,
  (
    nonvar(Class)
  ->
    (
      % All resources are individuals of =|rdfs:Resource|=.
      rdf_equal(Class, rdfs:'Resource')
    ->
      true
    ;
      % Instantiation (+,+)
      rdfs_individual_of_r_c(Resource, Class)
    ->
      true
    )
  ;
    % Instantiation (+,-)
    rdfs_individual_of_r_c(Resource, Class)
  ).
% Instantiations (?,+)
% The class is given.
rdfs_individual_of(Resource, Class):-
  nonvar(Class),
  !,
  (
    rdf_equal(Class, rdfs:'Resource')
  ->
    % Again, all resources are individual of =|rdfs:Resource|=.
    rdf_subject(Resource)
  ;
    % Traverse down the class hierarchy.
    rdfs_subclass_of(SubClass, Class),
    rdf_has(Resource, rdf:type, SubClass)
  ).
% Instantiation (-,-)
rdfs_individual_of(_Resource, _Class):-
  throw(error(instantiation_error, _)).

%% rdfs_individual_of_r_c(+Resource, ?Class) is nondet.

% Literals are individuals of =|rdfs:Literal|=.
rdfs_individual_of_r_c(literal(_), Class):-
  !,
  rdfs_subclass_of(Class, rdfs:'Literal').
% Every resource is an individual of the classes up the hierarchy.
% Every resource is an individual of =|rdfs:Resource|=.
rdfs_individual_of_r_c(Resource, Class):-
  (
    % Traverse the subclass hierarchy upwards.
    rdf_has(Resource, rdf:type, MyClass)
  *->
    rdfs_subclass_of(MyClass, Class)
  ;
    % If the resource is not explicitly the individual of a class,
    % then return =|rdfs:Resource|=.
    rdf_equal(Class, rdfs:'Resource')
  ).

%% rdfs_subclass_of(+Class, ?Super) is nondet.
%% rdfs_subclass_of(?Class, +Super) is nondet.
% Generate sub/super classes. rdf_reachable/3 considers the
% rdfs:subPropertyOf relation as well as cycles. Note that by
% definition all classes are subclass of rdfs:Resource, a case
% which is dealt with by the 1st and 3th clauses :-(
%
% According to production 2.4 "rdfs:Datatype", Each instance of
% rdfs:Datatype is a subclass of rdfs:Literal.

% Every class is a subclass of =|rdfs:Resource|=.
% See also clause 3???
rdfs_subclass_of(Class, Super):-
  rdf_equal(rdfs:'Resource', Resource),
  Super == Resource,
  !,
  rdfs_individual_of(Class, rdfs:'Class').
% Use the transitive =|rdfs:subClassOf|= relation.
rdfs_subclass_of(Class, Super):-
  rdf_reachable(Class, rdfs:subClassOf, Super).
% The given subclass is not explicitly a subclass of =|rdfs:Resource|=,
% but it is an individual of =|rdfs:Class|=.
% Then the subclass triple is given by this clause.
% See also clause 1???
rdfs_subclass_of(Class, Super) :-
  nonvar(Class),
  var(Super),
  \+ rdf_reachable(Class, rdfs:subClassOf, rdfs:'Resource'),
  rdfs_individual_of(Class, rdfs:'Class'),
  rdf_equal(Super, rdfs:'Resource').
% Instances of =|rdfs:Datatype|= are subclasses of =|rdfs:Literal|=.
rdfs_subclass_of(Class, Super):-
  (
    nonvar(Class)
  ->
    rdf_has(Class, rdf:type, CType),
    rdf_reachable(CType, rdfs:subClassOf, rdfs:'Datatype'),
    \+ rdf_reachable(Class, rdfs:subClassOf, rdfs:'Literal'),
    rdf_equal(Super, rdfs:'Literal')
  ;
    nonvar(Super)
  ->
    rdf_reachable(Super, rdfs:subClassOf, rdfs:'Literal'),
    rdfs_individual_of(Class, rdfs:'Datatype')
  ).
% If =|owl:Class|= is not explicitly a subclass of =|rdfs:Class|=,
% then we infer it.
rdfs_subclass_of(Class, owl:'Class'):-
  \+ rdfs_subclass_of(owl:'Class', rdfs:'Class'),
  rdfs_subclass_of(Class, rdfs:'Class').

%% rdfs_subproperty_of(+SubProperty, ?Property) is nondet.
%% rdfs_subproperty_of(?SubProperty, +Property) is nondet.
%  Query the property hierarchy.

rdfs_subproperty_of(SubProperty, Property):-
  rdf_reachable(SubProperty, rdfs:subPropertyOf, Property).

