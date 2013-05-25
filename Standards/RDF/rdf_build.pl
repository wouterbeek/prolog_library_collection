:- module(
  rdf_build,
  [
% LISTS
    rdf_assert_list/3, % +List:list
                       % -RDF_List:uri
                       % +Graph:atom

% LITERAL ASSERTIONS
    rdf_assert_datatype/5, % +Subject:oneof([bnode,uri])
                           % +Predicate:uri
                           % +Datatype:oneof([atom,uri])
                           % +Value
                           % +Graph:atom
    rdf_assert_literal/4, % +Subject:oneof([bnode,uri])
                          % +Predicate:uri
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_literal/5, % +Subject:oneof([bnode,uri])
                          % +Predicate:uri
                          % +Language:atom
                          % +Literal:atom
                          % +Graph:atom
    rdf_assert_xml_literal/4, % +Subject:oneof([bnode,uri])
                              % +Predicate:uri
                              % +XMLLiteral:xml
                              % +Graph:atom

% LITERAL UPDATES
    rdf_increment/3, % +Link:uri
                     % +Relation:uri
                     % +Graph:atom
    rdf_overwrite_datatype/5, % +Subject:oneof([bnode,uri])
                              % +Predicate:uri
                              % +Datatype:oneof([atom,uri])
                              % +NewValue
                              % +Graph:atom

% LITERAL RETRACTIONS
    rdf_retractall_datatype/5, % ?Subject:oneof([bnode,uri])
                               % ?Predicate:uri
                               % ?Datatype:oneof([atom,uri])
                               % ?Value
                               % ?Graph:atom
    rdf_retractall_literal/4, % ?Subject:oneof([bnode,uri])
                              % ?Predicate:uri
                              % ?Literal:atom
                              % ?Graph:atom
    rdf_retractall_literal/5 % ?Subject:oneof([bnode,uri])
                             % ?Predicate:uri
                             % ?Language:atom
                             % ?Literal:atom
                             % ?Graph:atom
  ]
).

/** <module> RDF build

Simple asserion and retraction predicates for RDF, customized for specific
datatypes and literals.

The supported datatypes:
    * boolean
    * ateTime
    * double
    * float
    * gDay
    * gMonth
    * gYear
    * image
    * integer

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/05
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_typecheck)).
:- use_module(xml(xml_schema_datatypes)).

% LISTS
:- rdf_meta(rdf_assert_list(+,r,+)).
% LITERAL ASSERTIONS
:- rdf_meta(rdf_assert_datatype(r,r,+,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+)).
:- rdf_meta(rdf_assert_literal(r,r,+,+,+)).
:- rdf_meta(rdf_assert_xml_literal(r,r,+,+)).
% LITERAL UPDATES
:- rdf_meta(rdf_increment(r,r,+)).
:- rdf_meta(rdf_overwrite_datatype(r,r,+,+,+)).
% LITERAL RETRACTIONS
:- rdf_meta(rdf_retractall_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?)).
:- rdf_meta(rdf_retractall_literal(r,r,?,?,?)).

:- debug(rdf_build).



% LISTS %

%! rdf_assert_list(+List:list, -RDF_List:uri, +Graph:atom) is det.
% Asserts the given, possibly nested list into RDF.
%
% @arg List The, possibly nested, Prolog list.
% @arg RDF_List The URI of the node at which the RDF list starts.
% @arg Graph The atomic name of a graph or unbound.
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

%! rdf_assert_datatype(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +DatatypeName:oneof([]),
%!   +Value,
%!   +Graph:atom
%! ) is det.
% Asserts a float value for a resource.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg DatatypeName
% @arg Value
% @arg Graph The atomic name of an RDF graph.

rdf_assert_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  rdf_assert(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a literal value for a resource.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Literal An atom.
% @arg Graph The atomic name of an RDF graph.
% @see rdf_assert_literal/5 also specifies the language.

rdf_assert_literal(Subject, Predicate, Literal, Graph):-
  % Make sure that the literal is atomic.
  rdf_assert(Subject, Predicate, literal(Literal), Graph).

%! rdf_assert_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Language:atom,
%!   +Literal:atom,
%!   +Graph:atom
%! ) is det.
% Asserts a integer value for a resource.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Language The atomic name of a language.
% @arg Literal An atom.
% @arg Graph The atomic name of an RDF graph.

rdf_assert_literal(Subject, Predicate, Language, Literal, Graph):-
  % Make sure that the literal is atomic.
  rdf_assert(Subject, Predicate, literal(lang(Language, Literal)), Graph).

%! rdf_assert_xml_literal(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +XMLLiteral:atom,
%!   +Graph:atom
%! ) is det.

rdf_assert_xml_literal(Subject, Predicate, XMLLiteral, Graph):-
  rdf_assert_datatype(Subject, Predicate, rdf:'XMLLiteral', XMLLiteral, Graph).



% LITERAL RETRACTIONS %

%! rdf_retractall_datatype(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?DatatypeName:oneof([]),
%!   ?Value,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert an integer property.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg DatatypeName
% @arg Value
% @arg Graph The atomic name of an RDF graph.

rdf_retractall_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  forall(
    rdf_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph),
    (
      rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
      rdf_retractall(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph)
    )
  ).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Literal An atom.
% @arg Graph The atomic name of an RDF graph.
% @see rdf_retractall_literal/5 only retracts triples with literals of
%      a specific name.

rdf_retractall_literal(Subject, Predicate, Literal, Graph):-
  rdf_retractall_literal(Subject, Predicate, literal(Literal), Graph).

%! rdf_retractall_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Language:atom,
%!   ?Literal:atom,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that assert a literal property
% in a specific language.
%
% @arg Subject A resource.
% @arg Predicate A resource.
% @arg Language The atomic name of a language.
% @arg Literal An atom.
% @arg Graph The atomic name of an RDF graph.

rdf_retractall_literal(Subject, Predicate, Language, Literal, Graph):-
  rdf_retractall(Subject, Predicate, literal(lang(Language, Literal)), Graph).



% LITERAL UPDATES %

%! rdf_increment(+Link:uri, +Relation:uri, +Graph:atom) is det.
% Inrements an integer stored in RDF.

rdf_increment(Subject, Predicate, Graph):-
  once(rdf_datatype(Subject, Predicate, integer, OldValue, Graph)),
  NewValue is OldValue + 1,
  rdf_retractall_datatype(Link, Relation, integer, OldValue, Graph),
  rdf_assert_datatype(Link, Relation, integer, NewValue, Graph).

%! rdf_overwrite_datatype(
%!   +Subject:oneof([bnode,uri]),
%!   +Predicate:uri,
%!   +Datatype:oneof([atom,uri]),
%!   +NewValue,
%!   +Graph:atom
%! ) is det.
% The single new value is going to overwrite all old values, unless the new
% value is already asserted. In that case none of the other values gets
% retracted.

rdf_overwrite_datatype(Subject, Predicate, Datatype, NewValue, Graph):-
  % Type checking.
  % We need a completely qualified RDF triple.
  rdf_is_subject(Subject),
  rdf_is_predicate(Predicate),
  rdf_datatype(Datatype, NewValue, _CanonicalValue),
  rdf_graph(Graph),
  !,
  findall(
    OldValue,
    rdf_datatype(Subject, Predicate, Datatype, OldValue, Graph),
    OldValues
  ),
  (
    member(NewValue, OldValues)
  ;
    rdf_retractall_datatype(Subject, Predicate, Datatype, OldValue, Graph),
    rdf_assert_datatype(Subject, Predicate, Datatype, NewValue, Graph),
    debug(
      rdf_build,
      'Updated value <~w, ~w, ~w^^~w, ~w> --> <~w, ~w, ~w^^~w, ~w>\n',
      [Subject, Predicate, OldValues, Datatype, Graph, Subject, Predicate,
      NewValue, Datatype, Graph]
    )
  ),
  !.
