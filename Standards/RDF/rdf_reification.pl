:- module(
  rdf_reification,
  [
% READING
    rdf_object/3, % ?Stmt:statement
                  % ?Object:onef([literal,resource])
                  % +Graph:graph
    rdf_predicate/3, % ?Stmt:statement
                     % ?Predicate:resource
                     % +Graph:graph
    rdf_statement/5, % ?Subject:onef([bnode,literal,resource])
                     % ?Predicate:resource
                     % ?Object:onef([literal,resource])
                     % +Graph:graph
                     % ?Stmt:statement
    rdf_subject/3, % ?Stmt:statement
                   % ?Subject:onef([bnode,literal,resource])
                   % +Graph:graph

% WRITING
    rdf_assert_object/3, % +Stmt:statement
                         % +Object:oneof([literal,resource])
                         % +Graph:graph
    rdf_assert_predicate/3, % +Stmt:statement
                            % +Predicate:resource
                            % +Graph:graph
    rdf_assert_statement/5, % +Subject:resource
                            % +Predicate:resource
                            % +Object:resource
                            % +Graph:graph
                            % -Stmt:statement
    rdf_assert_subject/3 % +Stmt:statement
                         % +Subject:resource
                         % +Graph:graph
  ]
).

/** <module> RDF reification

Reification for RDF. Both reading and writing.

@author Wouter Beek
@tbd Assess this module after reading the semantics standard for reification.
@version 2013/02
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_object(r,r,?)).
:- rdf_meta(rdf_predicate(r,r,?)).
:- rdf_meta(rdf_statement(r,r,r,?,r)).
:- rdf_meta(rdf_subject(r,r,?)).

:- rdf_meta(rdf_assert_object(r,r,+)).
:- rdf_meta(rdf_assert_predicate(r,r,+)).
:- rdf_meta(rdf_assert_statement(r,r,r,+,r)).
:- rdf_meta(rdf_assert_subject(r,r,+)).



% READING %

rdf_object(Stmt, Object, Graph):-
  rdf(Stmt, rdf:object, Object, Graph).

rdf_predicate(Stmt, Predicate, Graph):-
  rdf(Stmt, rdf:predicate, Predicate, Graph).

rdf_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_subject(Stmt, Subject, Graph),
  rdf_predicate(Stmt, Predicate, Graph),
  rdf_object(Stmt, Object, Graph).

rdf_subject(Stmt, Subject, Graph):-
  rdf(Stmt, rdf:subject, Subject, Graph).



% WRITING %

rdf_assert_object(Stmt, Object, Graph):-
  rdf_assert(Stmt, rdf:object, Object, Graph).

rdf_assert_predicate(Stmt, Predicate, Graph):-
  rdf_assert(Stmt, rdf:predicate, Predicate, Graph).

rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_statement(Subject, Predicate, Object, Graph, Stmt),
  !.
rdf_assert_statement(Subject, Predicate, Object, Graph, Stmt):-
  rdf_bnode(Stmt),
  rdf_assert(Stmt, rdf:type, rdf:'Statement', Graph),
  rdf_assert_subject(Stmt, Subject, Graph),
  rdf_assert_predicate(Stmt, Predicate, Graph),
  rdf_assert_object(Stmt, Object, Graph),
  rdf_assert(Subject, Predicate, Object, Graph).

rdf_assert_subject(Stmt, Subject, Graph):-
  rdf_assert(Stmt, rdf:subject, Subject, Graph).

