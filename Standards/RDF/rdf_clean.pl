:- module(
  rdf_clean,
  [
    rdf_clean/4, % ?Subject:oneof([bnode,uri])
                 % ?Predicate:uri
                 % ?Object:oneof([bnode,literal,uri])
                 % ?Graph1:atom
    rdf_clean_datatype/5, % ?Subject:oneof([bnode,uri])
                          % ?Predicate:uri
                          % ?Datatype:onceof([boolean,double,float,integer,string])
                          % ?Object:oneof([bnode,literal,uri])
                          % ?Graph1:atom
    rdf_duplicate/5, % ?Subject:oneof([bnode,uri])
                     % ?Predicate:uri
                     % ?Object:oneof([bnode,literal,uri])
                     % ?Graph1:atom
                     % ?Graph2:atom
    rdf_strip_strings/1 % +Graph:atom
  ]
).

/** <module> RDF clean

Predicates that allow RDF graphs to be cleaned in a controlled way.

@author Wouter Beek
@version 2013/03-2013/04
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_clean(r,r,r,?)).
:- rdf_meta(rdf_clean_datatype(r,r,r,?,?)).
:- rdf_meta(rdf_duplicate(r,r,r,?,?)).
:- rdf_meta(rdf_strip_string(r,r,+,+)).



rdf_clean(Subject, Predicate, Object, Graph):-
  findall(
    [Subject, Predicate, Object, Graph],
    rdf(Subject, Predicate, Object, Graph),
    Tuples
  ),
  user_interaction(
    'REMOVE-RDF-TRIPLE',
    rdf_retractall,
    ['Subject', 'Predicate', 'Object', 'Graph'],
    Tuples
  ).

rdf_clean_datatype(Subject, Predicate, Datatype, Object, Graph):-
  findall(
    [Subject, Predicate, Datatype, Object, Graph],
    rdf_datatype(Subject, Predicate, Datatype, Object, Graph),
    Tuples
  ),
  user_interaction(
    'REMOVE-RDF-DATATYPE-TRIPLE',
    rdf_retractall_datatype,
    ['Subject', 'Predicate', 'Datatype', 'Value', 'Graph'],
    Tuples
  ).

rdf_duplicate(Subject, Predicate, Object, Graph1, Graph2):-
  rdf(Subject, Predicate, Object, Graph1:_),
  rdf(Subject, Predicate, Object, Graph2:_),
  Graph1 \== Graph2.

rdf_strip_string(Subject, Predicate, UnstrippedString, Graph):-
  strip([' '], UnstrippedString, StrippedString),
  rdf_overwrite_datatype(Subject, Predicate, string, StrippedString, Graph).

go:-
  forall(
    (
      rdf_datatype(Subject, Predicate, string, UnstrippedString, Graph),
      strip([' '], UnstrippedString, StrippedString),
      UnstrippedString \= StrippedString
    ),
    (
      rdf_assert_datatype(Subject, Predicate, string, StrippedString, Graph),
      rdf_retractall_datatype(Subject, Predicate, string, UnstrippedString, Graph)
    )
  ).

rdf_strip_strings(Graph):-
  findall(
    [Subject, Predicate, UnstrippedString, Graph],
    (
      rdf_datatype(Subject, Predicate, string, UnstrippedString, Graph),
      strip([' '], UnstrippedString, StrippedString),
      UnstrippedString \= StrippedString
    ),
    Tuples
  ),
  user_interaction(
    'STRIP-RDF-DATATYPE-STRING',
    rdf_strip_string,
    ['Subject', 'Predicate', 'UnstrippedString', 'Graph'],
    Tuples
  ).
