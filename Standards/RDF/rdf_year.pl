:- module(
  rdf_year,
  [
    rdf_assert_year/6, % +Subject:oneof([bnode,uri])
                       % +IntervalPredicate1:uri
                       % +IntervalPredicate2:uri
                       % +PointPredicate:uri
                       % +Year:oneof([integer,pair(integer)])
                       % +Graph:atom
    rdf_clean_year/6 % ?Subject:oneof([bnode,uri])
                     % ?Predicate:uri
                     % ?Graph:atom
                     % +IntervalPredicate1:uri
                     % +IntervalPredicate2:uri
                     % +PointPredicate:uri
  ]
).

/** <module> RDF_YEAR

Support for year data in RDF graphs.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_year)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).

:- rdf_meta(rdf_assert_year(r,r,r,r,+,+)).
:- rdf_meta(rdf_clean_year(r,r,?,r,r,r)).



rdf_assert_year(S, IntervalP1, IntervalP2, _PointP, Year1-Year2, G):-
  !,
  rdf_assert_datatype(S, IntervalP1, gYear, Year1, G),
  rdf_assert_datatype(S, IntervalP2, gYear, Year2, G).
rdf_assert_year(S, _IntervalP1, _IntervalP2, PointP, Year, G):-
  rdf_assert_datatype(S, PointP, gYear, Year, G).

rdf_clean_year(S, P, G, IntervalP1, IntervalP2, PointP):-
  findall(
    [S, P, Lit, G],
    (
      rdf_literal(S, P, Lit, G),
      phrase(year(_Lang, _Year), Lit)
    ),
    Tuples
  ),
  user_interaction(
    'RDF-DATATYPE-GYEAR',
    rdf_clean_year0(IntervalP1, IntervalP2, PointP),
    ['Subject', 'Predicate', 'Literal', 'Graph'],
    Tuples
  ).
:- rdf_meta(rdf_clean_year0(r,r,r,r,r,+,+)).
rdf_clean_year0(IntervalP1, IntervalP2, PointP, S, P, Lit, G):-
  rdf_retractall_literal(S, P, Lit, G),
  dcg_phrase(year(_Lang, Year), Lit),
  rdf_assert_year(S, IntervalP1, IntervalP2, PointP, Year, G).

