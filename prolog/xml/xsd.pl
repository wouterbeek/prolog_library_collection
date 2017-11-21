:- module(
  xsd,
  [
    xsd_lexical_value/3, % +D, ?Lex, ?Val
    xsd_subtype_of/2     % ?Subtype, ?Supertype
  ]
).

/** <module> XML Schema 1.1 Datatypes

@author Wouter Beek
@version 2017/08-2017/11
*/

:- use_module(library(arithmetic)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dif)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf11), [op(650, xfx, ^^)]).
:- use_module(library(xml/xsd_number)).
:- use_module(library(xsdp_types)).

:- arithmetic_function(xsd_div/2).

:- op(400, yfx, xsd_div).

% xsd_div(+M, +N, -Z) is det.
%
% # Definition
%
% If `M` and `N` are numbers, then `M div N` is the greatest integer
% less than or equal to `M / N`.

xsd_div(X, Y, Z):-
  Z is floor(X rdiv Y).

:- rdf_meta
   xsd_lexical_value(r, ?, ?),
   xsd_strict_subtype_of(r, r),
   xsd_subtype_of(r, r).





%! xsd_lexical_value(+D:atom, +Lex:atom, -Value:term) is det.
%! xsd_lexical_value(+D:atom, -Lex:atom, +Value:term) is det.

xsd_lexical_value(xsd:decimal, Lex, N) :- !,
  (   atom(Lex)
  ->  atom_phrase(decimalLexicalMap(N), Lex)
  ;   atom_phrase(decimalCanonicalMap(N), Lex)
  ).
xsd_lexical_value(D, Lex, Value) :-
  (   ground(Lex)
  ->  rdf11:out_type(D, Value, Lex)
  ;   rdf11:pre_ground_object(Value^^D, literal(type(D,Lex)))
  ).



%! xsd_strict_subtype_of(?Subtype:atom, ?Supertype:atom) is nondet.

xsd_strict_subtype_of(X, Y) :-
  dif(X, Y),
  xsd_subtype_of(X, Y).



%! xsd_subtype_of(?Subtype:atom, ?Supertype:atom) is nondet.

xsd_subtype_of(SubGlobal, SuperGlobal) :-
  xsd_global_local_(SubGlobal, SubLocal),
  xsd_global_local_(SuperGlobal, SuperLocal),
  xsdp_subtype_of(SubLocal, SuperLocal),
  xsd_global_local_(SubGlobal, SubLocal),
  xsd_global_local_(SuperGlobal, SuperLocal).

xsd_global_local_(Global, Local) :-
  var(Global),
  var(Local), !.
xsd_global_local_(Global, Local) :-
  rdf_prefix_iri(xsd:Local, Global).
