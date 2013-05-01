:- module(
  rdf_entailment,
  [
    rdf_rdf/3, % ?S:oneof([bnode,uri])
               % ?P:uri
               % ?O:oneof([bnode,literal,uri])
    rdf_rdf/4 % ?S:oneof([bnode,uri])
              % ?P:uri
              % ?O:oneof([bnode,literal,uri])
              % ?G:atom
  ]
).

/** <module> RDF ENTAILMENT

RDF entailment extensions of simple entailment, as defined in RDF-SEMANTICS.

---+ Q&A

Q: Why does rdf_meta/1 argument 'r' not expand datatype uriRefs?
   (rdf_global_object/2 must be used for this explicitly).

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(logic(model_theory)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_rdf(r,r,r)).
:- rdf_meta(rdf_rdf(r,r,r,?)).



rdf_rdf(S, P, O):-
  rdf_rdf(S, P, O, _).

% If a resource appears as a predicate term in some triple, then it
% is an instance of class RDF-Property.
rdf_rdf(S, rdf:type, rdf:'Property', Gs):-
  (nonvar(S) -> once(rdf_rdf0(_, S, _, Gs))
  ; setoff(S0, rdf_rdf0(_, S0, _, Gs), S0s), member(S, S0s)).
rdf_rdf(RDF_V, rdf:type, rdf:'Property', _Gs):-
  rdf_member(RDF_V,
    [rdf:first,rdf:nil,rdf:object,rdf:predicate,rdf:rest,rdf:subject,rdf:type,
     rdf:value]).
rdf_rdf(RDF_LI, rdf:type, rdf:'Property', _Gs):-
  between(1, 3, I),
  format(atom(J), '_~w', [I]),
  rdf_global_id(rdf:J, RDF_LI).
rdf_rdf(rdf:nil, rdf:type, rdf:'List', _Gs).
% If a literal is of a well-typed XML literal string, then it is an
% instance of class RDF-XML-Literal.
rdf_rdf(Lit, rdf:type, rdf:'XMLLiteral', Gs):-
  (nonvar(Lit) -> once(rdf_rdf0(_, _, literal(type(rdf:'XMLLiteral', Lit)), Gs))
  ; setoff(Lit0, rdf_rdf0(_, _, literal(type(rdf:'XMLLiteral', Lit0)), Gs), Lit0s),
    member(Lit, Lit0s)).
% 'Simple' triples.
rdf_rdf(S, P, O, Gs):-
  rdf_rdf0(S, P, O, Gs),
  % Predicate terms must be instances of class RDF-Property.
  rdf_rdf(P, rdf:type, rdf:'Property', Gs),
  % Type XML literal strings must be instances of class RDF-XML-Literal.
  (O = literal(type(rdf:'XMLLiteral', Lit))
  -> rdf_rdf(Lit, rdf:type, rdf:'XMLLiteral', Gs)
  ; true).

% Allow the graph list argument to be completely neglected.
rdf_rdf0(S, P, O, Gs):-
  var(Gs),
  !,
  % Perform typed literal expansion.
  rdf_global_object(O, O0),
  rdf(S, P, O0).
rdf_rdf0(S, P, O, Gs):-
  member(G, Gs),
  % Perform typed literal expansion.
  rdf_global_object(O, O0),
  rdf(S, P, O0, G).
