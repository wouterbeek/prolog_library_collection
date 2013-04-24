:- module(
  rdf_clean,
  [
    rdf_clean/4, % ?Subject:oneof([bnode,uri])
                 % ?Predicate:uri
                 % ?Object:oneof([bnode,literal,uri])
                 % ?Graph1:atom
    rdf_clean_datatype/5, % ?Subject:oneof([bnode,uri])
                          % ?Predicate:uri
                          % ?Datatype:atom
                          % ?Value
                          % ?Graph1:atom
    rdf_convert_datatype/6, % +Subject:oneof([bnode,uri])
                            % +Predicate:uri
                            % +FromDatatype:atom
                            % +FromValue
                            % +ToDatatype:atom
                            % +Graph:atom
    rdf_duplicate/5, % ?Subject:oneof([bnode,uri])
                     % ?Predicate:uri
                     % ?Object:oneof([bnode,literal,uri])
                     % ?Graph1:atom
                     % ?Graph2:atom
    rdf_split_string/4, % ?Subject:onef([bnode,uri])
                        % ?Predicate:uri
                        % ?Graph:atom
                        % +Split:atom
    rdf_strip_string/3 % ?Subject:onef([bnode,uri])
                       % ?Predicate:uri
                       % ?Graph:atom
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
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_clean(r,r,r,?)).
:- rdf_meta(rdf_clean_datatype(r,r,r,?,?)).
:- rdf_meta(rdf_convert_datatype(r,r,+,+,+,+)).
:- rdf_meta(rdf_duplicate(r,r,r,?,?)).
:- rdf_meta(rdf_split_string(r,r,?,+)).
:- rdf_meta(rdf_strip_string(r,r,?)).



%% rdf_clean(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri]),
%%   ?Graph:atom
%% ) is det.
% Clean RDF triples with explicit user-consent.

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

%% rdf_clean_datatype(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Datatype:atom,
%%   ?Value,
%%   ?Graph:atom
%% ) is det.
% Clean RDF datatype triples with explicit user-consent.

rdf_clean_datatype(Subject, Predicate, Datatype, Value, Graph):-
  findall(
    [Subject, Predicate, Datatype, Value, Graph],
    rdf_datatype(Subject, Predicate, Datatype, Value, Graph),
    Tuples
  ),
  user_interaction(
    'REMOVE-RDF-DATATYPE-TRIPLE',
    rdf_retractall_datatype,
    ['Subject', 'Predicate', 'Datatype', 'Value', 'Graph'],
    Tuples
  ).

rdf_convert_datatype(
  Subject,
  Predicate,
  FromDatatype,
  FromValue,
  ToDatatype,
  Graph
):-
  forall(
    rdf_datatype(Subject, Predicate, FromDatatype, FromValue, Graph),
    (
      rdf_convert_datatype(FromDatatype, FromValue, ToDatatype, ToValue),
      rdf_assert_datatype(Subject, Predicate, ToDatatype, ToValue, Graph)
    )
  ).

%% rdf_duplicate(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri]),
%%   ?Graph1:atom,
%%   ?Graph2:atom\
%% ) is nondet.
% Duplicate triples, that occur in at least two graphs.

rdf_duplicate(Subject, Predicate, Object, Graph1, Graph2):-
  rdf(Subject, Predicate, Object, Graph1:_),
  rdf(Subject, Predicate, Object, Graph2:_),
  Graph1 \== Graph2.

rdf_split_string(Subject, Predicate, Graph, Split):-
  findall(
    [Subject, Predicate, String, Graph],
    (
      rdf_datatype(Subject, Predicate, string, String, Graph),
      split_atom_exclusive(String, Split, Splits),
      length(Splits, Length),
      Length > 1
    ),
    Tuples
  ),
  user_interaction(
    'SPLIT-RDF-DATATYPE-STRING',
    rdf_split_string0(Split),
    ['Subject', 'Predicate', 'String', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_split_string0(+,r,r,+,+)).
rdf_split_string0(Split, Subject, Predicate, OldString, Graph):-
  split_atom_exclusive(OldString, Split, NewStrings),
  forall(
    member(NewString, NewStrings),
    rdf_assert_datatype(Subject, Predicate, string, NewString, Graph)
  ),
  rdf_retractall_datatype(Subject, Predicate, string, OldString, Graph).

%% rdf_strip_string(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Graph:atom
%% ) is det.
% Strip RDF string datatypes.

rdf_strip_string(Subject, Predicate, Graph):-
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
    rdf_overwrite_datatype0(string),
    ['Subject', 'Predicate', 'UnstrippedString', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_overwrite_datatype0(?,r,r,?,?)).
rdf_overwrite_datatype0(Datatype, Subject, Predicate, Value, Graph):-
  rdf_overwrite_datatype(Subject, Predicate, Datatype, Value, Graph).

