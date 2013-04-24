:- module(
  rdf_build,
  [
% LISTS
    rdf_assert_list/3, % +List:list
                       % -RDF_List:uri
                       % +Graph:atom

% LITERAL ASSERTIONS
    rdf_assert_datatype/5, % +Subject:uri
                           % +Predicate:uri
                           % +DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                           % +Value
                           % +Graph:atom
    rdf_assert_literal/4, % +Subject:uri
                          % +Predicate:uri
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_literal/5, % +Subject:uri
                          % +Predicate:uri
                          % +Language:atom
                          % +Literal:atom
                          % +Graph:atom

% LITERAL UPDATES
    rdf_increment/3, % +Link:uri
                     % +Relation:uri
                     % +Graph:atom
    rdf_overwrite_datatype/5, % +Subject:uri
                              % +Predicate:uri
                              % +DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                              % +Value
                              % +Graph:atom
    rdf_update_datatype/5, % +Subject:uri
                           % +Predicate:uri
                           % +DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                           % +Value
                           % +Graph:atom

% LITERAL RETRACTIONS
    rdf_retractall_datatype/5, % ?Subject:uri
                               % ?Predicate:uri
                               % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                               % ?Value
                               % ?Graph:atom
    rdf_retractall_literal/4, % ?Subject:uri
                              % ?Predicate:uri
                              % ?Literal:atom
                              % ?Graph:atom
    rdf_retractall_literal/5 % ?Subject:uri
                             % ?Predicate:uri
                             % ?Language:atom
                             % ?Literal:atom
                             % ?Graph:atom
  ]
).

/** <module> RDF build

Simple asserion and retraction predicates for RDF, customized for specific
datatypes and literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04
*/

:- use_module(generics(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_read)).

% LISTS
:- rdf_meta(rdf_assert_list(+,r,+)).
% LITERAL ASSERTIONS
:- rdf_meta(rdf_assert_datatype(r,r,+,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+,+)).
% LITERAL UPDATES
:- rdf_meta(rdf_increment(r,r,+)).
:- rdf_meta(rdf_overwrite_datatype(r,r,+,+,+)).
:- rdf_meta(rdf_update_datatype(r,r,+,+,+)).
% LITERAL RETRACTIONS
:- rdf_meta(rdf_retractall_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?,?)).

:- debug(rdf_build).



% LISTS %

%% rdf_assert_list(+List:list, -RDF_List:uri, +Graph:atom) is det.
% Asserts the given, possibly nested list into RDFS.
%
% @param List The, possibly nested, Prolog list.
% @param RDF_List The URI of the node at which the RDF list starts.
% @param Graph The atomic name of a graph or unbound.
%
% @author Wouter Beek, elaborating on Sanders original, allowing the graph
%         to be optional and returning the root of the asserted list.
% @author Sander Latour, who wrote the original version, dealing with
%         nested lists.

rdf_assert_list(List, RDF_List, Graph):-
  add_blank_list_individual(RDF_List, Graph),
  rdf_assert_list0(List, RDF_List, Graph).

rdf_assert_list0([], rdf:nil, _Graph).
rdf_assert_list0([H | T], RDF_List, Graph):-
  (
    is_list(H)
  ->
    rdf_assert_list0(H, H1, Graph)
  ;
    H1 = H
  ),
  rdf_assert(RDF_List, rdf:first, H1, Graph),
  (
    T == []
  ->
    rdf_global_id(rdf:nil, TList)
  ;
    add_blank_list_individual(TList, Graph),
    rdf_assert_list0(T, TList, Graph)
  ),
  rdf_assert(RDF_List, rdf:rest, TList, Graph).

add_blank_list_individual(Blank, Graph):-
  rdf_bnode(Blank),
  rdf_assert(Blank, rdf:type, rdf:'List', Graph).



% LITERAL ASSERTIONS %

%% rdf_assert_datatype(
%%   +Subject:uri,
%%   +Predicate:uri,
%%   +DatatypeName:oneof([]),
%%   +Value,
%%   +Graph:atom
%% ) is det.
% Asserts a float value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_assert_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  rdf_assert(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph).

%% rdf_assert_literal(
%%   +Subject:uri,
%%   +Predicate:uri,
%%   +Literal:atom,
%%   +Graph:atom
%% ) is det.
% Asserts a literal value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_assert_literal/5 also specifies the language.

rdf_assert_literal(Subject, Predicate, Literal, Graph):-
  rdf_assert_literal(Subject, Predicate, en, Literal, Graph).

%% rdf_assert_literal(
%%   +Subject:uri,
%%   +Predicate:uri,
%%   +Language:atom,
%%   +Literal:atom,
%%   +Graph:atom
%% ) is det.
% Asserts a integer value for a resource.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_assert_literal(Subject, Predicate, Language, Literal, Graph):-
  (atom(Literal) -> Literal1 = Literal ; term_to_atom(Literal, Literal1)),
  rdf_assert(Subject, Predicate, literal(lang(Language, Literal1)), Graph).



% LITERAL RETRACTIONS %

%% rdf_retractall_datatype(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?DatatypeName:oneof([]),
%%   ?Value,
%%   ?Graph:atom
%% ) is det.
% Retracts all matching RDF triples that assert an integer property.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param DatatypeName
% @param Value
% @param Graph The atomic name of an RDF graph.

rdf_retractall_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  forall(
    rdf_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph),
    (
      rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
      rdf_retractall(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph)
    )
  ).

%% rdf_retractall_literal(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?Literal:atom,
%%   ?Graph:atom
%% ) is det.
% Retracts all matching RDF triples that assert a literal property.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.
% @see rdf_retractall_literal/5 only retracts triples with literals of
%      a specific name.

rdf_retractall_literal(Subject, Predicate, Literal, Graph):-
  rdf_retractall_literal(Subject, Predicate, _Language, Literal, Graph).

%% rdf_retractall_literal(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?Language:atom,
%%   ?Literal:atom,
%%   ?Graph:atom
%% ) is det.
% Retracts all matching RDF triples that assert a literal property
% in a specific language.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_retractall_literal(Subject, Predicate, Language, Literal, Graph):-
  rdf_retractall(Subject, Predicate, literal(lang(Language, Literal)), Graph).



% LITERAL UPDATES %

%% rdf_increment(+Link:uri, +Relation:uri, +Graph:atom) is det.
% Inrements an integer stored in RDF.
%
% @param Link A resource.
% @param Relation A resource.
% @param Graph The atomic name of a graph.

rdf_increment(Link, Relation, Graph):-
  once(rdf_datatype(Link, Relation, integer, OldInteger, Graph)),
  NewInteger is OldInteger + 1,
  rdf_retractall_datatype(Link, Relation, integer, _OldInteger, Graph),
  rdf_assert_datatype(Link, Relation, integer, NewInteger, Graph).

rdf_overwrite_datatype(Subject, Predicate, Datatype, Value, Graph):-
  rdf_retractall_datatype(Subject, Predicate, Datatype, OldValue, Graph),
  rdf_assert_datatype(Subject, Predicate, Datatype, Value, Graph),
  debug(
    rdf_build,
    'Updated value <~w, ~w, ~w^^~w, ~w> --> <~w, ~w, ~w^^~w, ~w>\n',
    [Subject, Predicate, OldValue, Datatype, Graph, Subject, Predicate, Value, Datatype, Graph]
  ).

rdf_update_datatype(Subject, Predicate, Datatype, Value, Graph):-
  maplist(nonvar, [Subject, Predicate, Datatype, Value, Graph]),
  (
    rdf_datatype(Subject, Predicate, Datatype, Value, Graph)
  ->
    true
  ;
    rdf_overwrite_datatype(Subject, Predicate, Datatype, Value, Graph)
  ).

