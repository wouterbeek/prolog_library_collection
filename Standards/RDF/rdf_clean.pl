:- module(
  rdf_clean,
  [
    rdf_duplicate/5, % ?Subject:oneof([bnode,uri])
                     % ?Predicate:uri
                     % ?Object:oneof([bnode,literal,uri])
                     % ?Graph1:atom
                     % ?Graph2:atom
    rdf_expand_namespace/4, % ?Subject:oneof([atom,bnode,uri])
                            % ?Predicate:oneof([atom,uri])
                            % ?Object:oneof([atom,bnode,literal,uri])
                            % ?Graph:atom
% DATATYPES %
    rdf_convert_datatype/6, % +Subject:oneof([bnode,uri])
                            % +Predicate:uri
                            % +FromDatatype:atom
                            % +FromValue
                            % +ToDatatype:atom
                            % +Graph:atom
% LITERALS %
    rdf_split_literal/4, % ?Subject:onef([bnode,uri])
                         % ?Predicate:uri
                         % ?Graph:atom
                         % +Split:atom
    rdf_strip_literal/3, % ?Subject:oneof([bnode,uri])
                         % ?Predicate:uri
                         % ?Graph:atom
% REMOVAL %
    rdf_remove/4, % ?Subject:oneof([bnode,uri])
                  % ?Predicate:uri
                  % ?Object:oneof([bnode,literal,uri])
                  % ?Graph:atom
    rdf_remove_datatype/5 % ?Subject:oneof([bnode,uri])
                          % ?Predicate:uri
                          % ?Datatype:atom
                          % ?Value
                          % ?Graph:atom
  ]
).

/** <module> RDF clean

Predicates that allow RDF graphs to be cleaned in a controlled way.

@author Wouter Beek
@version 2013/03-2013/04, 2013/06
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_duplicate(r,r,r,?,?)).
:- rdf_meta(rdf_expand_namespace(r,r,r,?)).
% DATATYPES %
:- rdf_meta(rdf_convert_datatype(r,r,+,+,+,+)).
% LITERALS %
:- rdf_meta(rdf_split_literal(r,r,?,+)).
:- rdf_meta(rdf_strip_literal(r,r,?)).
% REMOVAL %
:- rdf_meta(rdf_remove(r,r,r,?)).
:- rdf_meta(rdf_remove_datatype(r,r,r,?,?)).



%! rdf_duplicate(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?Graph1:atom,
%!   ?Graph2:atom
%! ) is nondet.
% Duplicate triples, that occur in at least two graphs.

rdf_duplicate(Subject, Predicate, Object, Graph1, Graph2):-
  rdf(Subject, Predicate, Object, Graph1:_),
  rdf(Subject, Predicate, Object, Graph2:_),
  Graph1 \== Graph2.

rdf_expand_namespace(BNode, BNode):-
  rdf_is_bnode(BNode),
  !.
rdf_expand_namespace(
  literal(lang(Language, Literal)),
  literal(lang(Language, Literal))
):-
  !.
% Datatypes in typed literals are treaded in a special way.
rdf_expand_namespace(literal(type(Atom, Value)), literal(type(URI, Value))):-
  rdf_expand_namespace(Atom, URI).
% No namespace.
rdf_expand_namespace(literal(Literal), literal(Literal)):-
  !.
% Already a URI.
rdf_expand_namespace(URI, URI):-
  is_uri(URI),
  !.
% An atom that can be converted to a URI.
rdf_expand_namespace(Atom, URI):-
  split_atom_exclusive(':', Atom, [Namespace, LocalName]),
  rdf_global_id(Namespace:LocalName, URI).

%! rdf_expand_namespace(
%!   ?S1:oneof([bnode,uri]),
%!   ?P:uri,
%!   ?O:oneof([bnode,literal,uri]),
%!   ?G:atom
%! ) is det.
% Expands namespaces that currently occur as atoms.
%
% This was used for several RDF files from the OAEI where the datatypes
% of typed literals would sometimes be 'xsd:float' instea of 'xsd':'float'.

rdf_expand_namespace(S1, P1, O1, G):-
  findall(
    [S1, P1, O1, G],
    (
      rdf(S1, P1, O1, G),
      (
        rdf_expand_namespace(S1, S2),
        S1 \== S2
      ;
        rdf_expand_namespace(P1, P2),
        P1 \== P2
      ;
        rdf_expand_namespace(O1, O2),
        O1 \== O2
      )
    ),
    Tuples
  ),
  user_interaction(
    'EXPAND-NAMESPACE',
    rdf_expand_namespace0,
    ['Subject', 'Predicate', 'Object', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_expand_namespace0(r,r,r,?)).
rdf_expand_namespace0(Subject1, Predicate1, Object1, Graph):-
  maplist(
    rdf_expand_namespace,
    [Subject1, Predicate1, Object1],
    [Subject2, Predicate2, Object2]
  ),
  rdf_retractall(Subject1, Predicate1, Object1, Graph),
  rdf_assert(Subject2, Predicate2, Object2, Graph).



% DATATYPES %

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



% LITERALS %

rdf_split_literal(Subject, Predicate, Graph, Split):-
  findall(
    [Subject, Predicate, Literal, Graph],
    (
      rdf_literal(Subject, Predicate, Literal, Graph),
      split_atom_exclusive(Split, Literal, Splits),
      length(Splits, Length),
      Length > 1
    ),
    Tuples
  ),
  user_interaction(
    'SPLIT-RDF-DATATYPE-STRING',
    rdf_split_string0(Split),
    ['Subject', 'Predicate', 'Literal', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_split_string0(+,r,r,+,+)).
rdf_split_string0(Split, Subject, Predicate, OldLiteral, Graph):-
  split_atom_exclusive(Split, OldLiteral, NewLiterals),
  forall(
    member(NewLiteral, NewLiterals),
    rdf_assert_literal(Subject, Predicate, NewLiteral, Graph)
  ),
  rdf_retractall_literal(Subject, Predicate, OldLiteral, Graph).

%! rdf_strip_literal(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Graph:atom
%! ) is det.
% Strip RDF string datatypes.

rdf_strip_literal(Subject, Predicate, Graph):-
  findall(
    [Subject, Predicate, Literal1, Graph],
    (
      rdf_literal(Subject, Predicate, Literal1, Graph),
      strip_atom([' '], Literal1, Literal2),
      Literal1 \= Literal2
    ),
    Tuples
  ),
  user_interaction(
    'STRIP-RDF-DATATYPE-STRING',
    rdf_strip_literal0,
    ['Subject', 'Predicate', 'Literal', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_strip_literal0(r,r,+,+)).
rdf_strip_literal0(Subject, Predicate, OldLiteral, Graph):-
  strip_atom([' '], OldLiteral, NewLiteral),
  rdf_assert_literal(Subject, Predicate, NewLiteral, Graph),
  rdf_retractall_literal(Subject, Predicate, OldLiteral, Graph).



% REMOVAL %

%! rdf_remove(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Object:oneof([bnode,literal,uri]),
%!   ?Graph:atom
%! ) is det.
% Clean RDF triples with explicit user-consent.

rdf_remove(Subject, Predicate, Object, Graph):-
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

%! rdf_remove_datatype(
%!   ?Subject:oneof([bnode,uri]),
%!   ?Predicate:uri,
%!   ?Datatype:atom,
%!   ?Value,
%!   ?Graph:atom
%! ) is det.
% Clean RDF datatype triples with explicit user-consent.

rdf_remove_datatype(Subject, Predicate, Datatype, Value, Graph):-
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

