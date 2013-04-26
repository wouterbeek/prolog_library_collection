:- module(
  rdf_read,
  [
% LISTS
    rdf_list/3, % +O:list(nvpair)
                % +RDFList:uri
                % -List:list
    rdf_list_first/2, % +List:uri
                      % -FirstElement:uri
    rdf_list_last/2, % +List:uri
                     % -LastElement:uri
    rdf_list_length/2, % +List:uri
                       % -Length:number
    rdf_list_next/2, % ?Element:uri
                     % ?NextElement:uri
    rdf_list_occurs_after/2, % +After:uri
                             % +Before:uri
    rdf_list_occurs_before/2, % +Before:uri
                              % +After:uri
    rdf_list_previous/2, % ?Element:uri
                         % ?PreviousElement:uri

% LITERALS
    rdf_datatype/5, % ?Subject:uri
                    % ?Predicate:uri
                    % ?Datatype:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                    % ?Value:atomic
                    % ?Graph:graph
    rdf_literal/4, % ?Subject:uri
                   % ?Predicate:uri
                   % ?Literal:atom
                   % ?Graph:graph
    rdf_literal/5, % ?Subject:uri
                   % ?Predicate:uri
                   % ?Language:atom
                   % ?Literal:atom
                   % ?Graph:graph

% MEMBERSHIP
    rdf_member/2, % ?Member:uri
                  % ?Members:list(uri)
    rdf_memberchk/2, % ?Member:uri
                     % ?Members:list(uri)

% RDF HAS
    rdf_has_datatype/4, % ?Subject:uri
                        % ?Predicate:uri
                        % ?Datatype:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer])
                        % ?Value:atomic

% STRUCTURE-BASED READS
    rdf_index/5, % ?Subject:uri
                 % ?Predicate:uri
                 % ?Object:uri
                 % ?Graph:graph
                 % ?Index:term
    rdf_node/2, % ?Graph:graph
                % ?Node:uri
    rdf_random/5, % -Subject:uri
                  % -Predicate:uri
                  % -Object:uri
                  % +Graph:graph
                  % -Index:integer
    rdf_term/2, % ?Graph:graph
                % ?Term:uri
    rdf_valuetype/2 % ?Graph:graph
                    % ?Type:uri
  ]
).

/** <module> RDF read

Predicates for reading from RDF, customized for specific datatypes and
literals.

@author Wouter Beek
@version 2011/08, 2012/01, 2012/03, 2012/09, 2012/11-2013/04
*/

:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph)).

% LISTS %
:- rdf_meta(rdf_list(+,r,-)).
:- rdf_meta(rdf_list_first(r,r)).
:- rdf_meta(rdf_list_last(r,r)).
:- rdf_meta(rdf_list_length(r,-)).
:- rdf_meta(rdf_list_next(r,r)).
:- rdf_meta(rdf_list_occurs_after(r,r)).
:- rdf_meta(rdf_list_occurs_before(r,r)).
:- rdf_meta(rdf_list_previous(r,r)).
% LITERALS %
:- rdf_meta(rdf_datatype(r,r,?,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?)).
:- rdf_meta(rdf_literal(r,r,?,?,?)).
% MEMBERSHIP %
:- rdf_meta(rdf_member(r,+)).
:- rdf_meta(rdf_memberchk(r,+)).
% RDF HAS %
:- rdf_meta(rdf_has_datatype(r,r,?,?)).
% STRUCTURE-BASED READS %
:- rdf_meta(rdf_index(r,r,r,?,?)).
:- rdf_meta(rdf_node(?,r)).
:- rdf_meta(rdf_random(r,r,r,?,-)).
:- rdf_meta(rdf_term(?,r)).
:- rdf_meta(rdf_valuetype(?,r)).

:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#', [keep(true)]).



% LISTS %

%% rdf_list(+O:list(nvpair), +RDFList:uri, -List:list) is det
% Returns the list that starts at the given node.
%
% @param Options Thw following options are supported:
%        1. =|recursive(boolean)|=
% @param StartNode The URI of a node that starts the RDF list.
% @param List A prolog list.
%
% @author Wouter Beek
% @author Sander Latour

rdf_list(_O, RDFList, []):-
  rdf_global_id(rdf:nil, RDFList),
  !.
rdf_list(O, RDFList, [H1 | T]):-
  rdf(RDFList, rdf:first, H),
  (
    option(recursive(true), O, true),
    rdf(H, rdf:type, rdf:'List')
  ->
    rdf_list(O, H, H1)
  ;
    H1 = H
  ),
  rdf(RDFList, rdf:rest, RDFTail),
  !,
  rdf_list(O, RDFTail, T).

%% rdf_list_first(?List:uri, ?First:uri) is nondet.
% Pairs of lists and their first element.
%
% @param List an RDF list.
% @param First The first element of an RDF list.

rdf_list_first(List, First):-
  rdf(List, rdf:first, First).

%% rdf_list_first(?List:uri, ?Last:uri) is nondet.
% Pairs of lists and their last element.
%
% @param List an RDF list.
% @param Last The last element of an RDF list.

rdf_list_last(List, Last):-
  rdf(List, rdf:rest, rdf:nil),
  !,
  rdf(List, rdf:first, Last).
rdf_list_last(List, Last):-
  rdf(List, rdf:rest, NextList),
  rdf_list_last(NextList, Last).

%% rdf_list_length(+List:uri, -Length:integer) is det.
% Returns the number of elements in the given list.
%
% @param List An RDF list.
% @param Length An integer.

rdf_list_length(List, Length):-
  rdf_list_length(List, 0, Length).

rdf_list_length(List, Length, Length):-
  rdf(List, rdf:rest, rdf:nil),
  !.
rdf_list_length(List, Length, Length):-
  rdf(List, rdf:rest, PartialList),
  rdf_list_length(PartialList, PartialLength, Length),
  succ(PartialLength, Length).

%% rdf_list_next(Element, NextElement) is nondet.
% Returns pairs of consecutive elements in a list.
%
% @param Element A resource that is an element in an RDF list.
% @param NextElement A resource that is an element in an RDF list.

rdf_list_next(Element, NextElement):-
  rdf(List, rdf:first, Element),
  rdf(List, rdf:rest, NextList),
  \+ rdf_global_id(rdf:nil, NextList),
  rdf(NextList, rdf:first, NextElement).

rdf_list_occurs_after(After, Before):-
  After \== Before,
  rdf_list_occurs_after0(After, Before).
rdf_list_occurs_after0(X, X).
rdf_list_occurs_after0(After1, Before):-
  rdf_list_previous(After1, After2),
  rdf_list_occurs_after0(After2, Before).

rdf_list_occurs_before(Before, After):-
  Before \== After,
  rdf_list_occurs_before0(Before, After).
rdf_list_occurs_before0(X, X).
rdf_list_occurs_before0(Before1, After):-
  rdf_list_next(Before1, Before2),
  rdf_list_occurs_before0(Before2, After).

%% rdf_list_previous(Element, PreviousElement) is nondet.
% Returns pairs of inverted consecutive elements in a list.
%
% @param Element A resource that is an element in an RDF list.
% @param PreviousElement A resource that is an element in an RDF list.

rdf_list_previous(Element, PreviousElement):-
  rdf_list_next(PreviousElement, Element).



% LITERALS %

%% rdf_datatype(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,image,integer]),
%%   ?LexicalValue,
%%   ?Graph:atom
%% ) is nondet.

rdf_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  nonvar(LexicalValue),
  !,
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  rdf(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph).
rdf_datatype(Subject, Predicate, DatatypeName, LexicalValue, Graph):-
  rdf_datatype(DatatypeName, Datatype),
  rdf(Subject, Predicate, literal(type(Datatype, CanonicalValue)), Graph),
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue).

%% rdf_literal(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?Literal:atom,
%%   ?Graph:graph
%% ) is nondet.
% The RDF triple for a literal valued property.
%
% @see rdf_literal/5.

rdf_literal(Subject, Predicate, Literal, Graph):-
  rdf(Subject, Predicate, literal(Literal), Graph).
rdf_literal(Subject, Predicate, Literal, Graph):-
  rdf_literal(Subject, Predicate, en, Literal, Graph).

%% rdf_literal(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?Language:atom,
%%   ?Literal:atom,
%%   ?Graph:graph
%% ) is nondet.
% The RDF triple for a literal valued property, encoded in a certain language.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Language The atomic name of a language.
% @param Literal An atom.
% @param Graph The atomic name of an RDF graph.

rdf_literal(Subject, Predicate, Language, Literal, Graph):-
  rdf(Subject, Predicate, literal(lang(Language, Literal)), Graph).



% MEMBERSHIP %

rdf_member(Member, List):-
  member(Member0, List),
  rdf_global_id(Member0, Member).

rdf_memberchk(Member, List):-
  once(rdf_member(Member, List)).



% RDF HAS %

rdf_has_datatype(Subject, Predicate, DatatypeName, LexicalValue):-
  nonvar(LexicalValue),
  !,
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  rdf_has(Subject, Predicate, literal(type(Datatype, CanonicalValue))).
rdf_has_datatype(Subject, Predicate, DatatypeName, LexicalValue):-
  rdf_has(Subject, Predicate, literal(type(Datatype, CanonicalValue))),
  rdf_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue).



% STRUCTURE-BASED READS %

%% rdf_index(
%%   ?Subject:uri,
%%   ?Predicate:uri,
%%   ?Object:uri,
%%   ?Graph:graph,
%%   ?Index:integer
%% ) is nondet.
% Returns the rdf triple that has the given index in the arbitrary sequence
% in which SWI-Prolog returns its triples.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Object A resource.
% @param Index A compound term of the form =Graph:Line= with =Graph= the
%        atomic name of an RDF graph and =Line= an integer.

rdf_index(Subject, Predicate, Object, Graph, Index):-
  rdf_triples(Graph, Triples),
  nth0(Index, Triples, rdf(Subject, Predicate, Object)).

rdf_node(Graph, Node):-
  nonvar_det(rdf_node0(Graph, Node)).
rdf_node0(Graph, Node):-
  rdf_subject(Graph, Node).
rdf_node0(Graph, Node):-
  rdf_object(Graph, Node).

%% rdf_random(
%%   -Subject:uri,
%%   -Predicate:uri,
%%   -Object:uri,
%%   ?Graph:graph,
%%   -Index:integer
%% ) is det.
% Returns a random triple from the given graph.
%
% @param Subject A resource.
% @param Predicate A resource.
% @param Object A resource.
% @param Graph The atomic name of a graph.
% @param Index An integer representating the index of the randomly selected
%        triple.

rdf_random(Subject, Predicate, Object, Graph, RandomIndex):-
  rdf_graph_property(Graph, triples(NumberOfTriples)),
  succ(UpperIndex, NumberOfTriples),
  random_betwixt(UpperIndex, RandomIndex),
  rdf_index(Subject, Predicate, Object, Graph, RandomIndex).

%% rdf_term(?Graph:graph, ?Term:uri) is nondet.
% Pairs of graphs and terms that occur in that graph.
% A term is either a subject, predicate or object term
% in an RDF triple.
%
% @param Graph The atomic name of a graph.
% @param Term A resource.

rdf_term(Graph, Term):-
  rdf_node(Graph, Term).
rdf_term(Graph, Term):-
  rdf_predicate(Graph, Term).

rdf_valuetype(Graph, Type):-
  rdf(_Subject, _Predicate, literal(type(Type, _Value)), Graph).

