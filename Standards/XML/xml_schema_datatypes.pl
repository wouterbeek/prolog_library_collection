:- module(
  xmls,
  [
    xmls_datatype/2, % ?DatatypeName:atom
                     % ?Datatype:uri
    xmls_datatype/4 % ?DatatypeName:atom
                    % ?LexicalValue
                    % ?Datatype:uri
                    % ?CanonicalValue
  ]
).

/** <module> XML Schema

XML Schema 2: Datatypes (Second Edition)


---+ Datatype

A triple consisting of:
  * *|Value space|*
    A set of distinct values.
  * *|Lexical space|*
    A set of lexical representations.
  * *Facets*
    Characterizing properties of the value space, individual values, or the
    lexical space.

---+ Value space

The set of values for a given datatype.

Each value in the value space of a datatype is denoted by at least one
literal in the lexical space of the same datatype.

Value space definitions:
  * *Intensional*: axiomatically from fundamental notions.
  * *Extensional*: enumeration.
  * *Restriction* of the value space of an already defined datatype.
  * * Combination* of value from different value spaces, according to some
    construction procedure (XMLS list, XMLS union).

---+ Lexical space

The set of valid literals for a datatype.

Characteristics:
  * Interoperability: minimum number of literals for the same value.
  * Readability: non-binary; text.
  * Parsing and serialization: taken from common languages and libraries.

---++ Canonical lexical representation

A subset of the lexical space for which there is a one-to-one mapping to
the value space.

---+ Facet

A single defining aspect of a value space.

---++ Fundamental facet

---++ Constraining facet


@author Wouter Beek
@compat XML Schema 2: Datatypes (Second Edition)
@see http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/
@tbd
@version 2013/01, 2013/03-2013/05
*/

:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(xmls_convert_datatype(r,+,r,-)).
:- rdf_meta(xmls_datatype(?,r)).
:- rdf_meta(xmls_datatype0(?,r)).
:- rdf_meta(xmls_datatype(?,?,r,?)).
:- rdf_meta(xmls_datatype0(?,?,r,?)).
:- rdf_meta(xmls_datatype_check(r,+)).

:- xml_register_namespace(iso, 'http://www.iso.org/').
:- xml_register_namespace(stdc, 'http://www.example.org/standards/').
:- xml_register_namespace(w3c, 'http://www.w3.org/').

init:-
  Graph = w3c,
  rdf_global_id(w3c:'TR/2004/REC-xmlschema-2-20041028/', This),
  rdfs_assert_individual(This, w3c:'Recommendation', Graph),
  rdf_assert_datatype(This, w3c:year, gYear, 2004, Graph),
  rdf_assert_literal(
    This,
    std:title,
    'XML Schema Part 2: Datatypes Second Edition',
    Graph
  ),
  rdf_assert_literal(This, w3c:author, 'Paul V. Biron', Graph),
  rdf_assert_literal(This, w3c:author, 'Ashok Malhotra', Graph),
  % Language-independent datatypes.
  rdf_assert(This, w3c:mentions, iso:'11404', Graph),
  % SQL datatypes.
  rdf_assert(This, w3c:mentions, std:'SQL', Graph),
  true.
:- init.



%% xmls_datatype(?DatatypeName:atom, ?Datatype:uri) is nondet.
% Translations between datatype names and datatype URIs.
%
% @param DatatypeName The atomic name of an XML Schema datatype.
% @param Datatype The URI of an XML Schema datatype.

xmls_datatype(DatatypeName, Datatype):-
  var(DatatypeName),
  var(Datatype),
  !,
  xmls_datatype0(DatatypeName, Datatype).
xmls_datatype(DatatypeName, Datatype):-
  once(xmls_datatype0(DatatypeName, Datatype)).

xmls_datatype0(boolean,  xsd:boolean ).
xmls_datatype0(date,     xsd:date    ).
xmls_datatype0(dateTime, xsd:dateTime).
xmls_datatype0(double,   xsd:double  ).
xmls_datatype0(float,    xsd:float   ).
xmls_datatype0(gDay,     xsd:gDay    ).
xmls_datatype0(gMonth,   xsd:gMonth  ).
xmls_datatype0(gYear,    xsd:gYear   ).
xmls_datatype0(int,      xsd:int     ).
xmls_datatype0(string,   xsd:string  ).

%% xmls_datatype(
%%   ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,int]),
%%   ?LexicalValue,
%%   ?Datatype:uri,
%%   ?CanonicalValue
%% ) is nondet.

xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  (
    nonvar(DatatypeName)
  ;
    nonvar(Datatype)
  ),
  !,
  xmls_datatype0(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  !.
xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  xmls_datatype0(DatatypeName, LexicalValue, Datatype, CanonicalValue).

%% xmls_datatype0(
%%   ?DatatypeName:atom,
%%   ?LexicalValue,
%%   ?Datatype:uri,
%%   ?CanonicalValue
%% )

% Boolean
xmls_datatype0(boolean, 0, xsd:boolean, false).
xmls_datatype0(boolean, 1, xsd:boolean, true).
xmls_datatype0(boolean, false, xsd:boolean, false).
xmls_datatype0(boolean, true, xsd:boolean, true).
% Date
xmls_datatype0(date, Date, xsd:date, Atom):-
  (
    nonvar(Date)
  ->
    xmls_datatype_check(xsd:date, Date),
    format_time(atom(Atom), '%F%:z', Date)
  ;
    nonvar(Atom)
  ->
    parse_time(Atom, iso_8601, Date)
  ).
% Date & time
xmls_datatype0(dateTime, DateTime, xsd:dateTime, Atom):-
  (
    nonvar(DateTime)
  ->
    xmls_datatype_check(xsd:dateTime, DateTime),
    format_time(atom(Atom), '%FT%T%:z', DateTime)
  ;
    nonvar(Atom)
  ->
    parse_time(Atom, iso_8601, DateTime)
  ).
% Double
xmls_datatype0(double, Double, xsd:double, Atom):-
  (
    nonvar(Double)
  ->
    atom_number(Atom, Double)
  ;
    atom_number(Atom, Number),
    Double is float(Number)
  ),
  xmls_datatype_check(xsd:double, Double).
% Float
xmls_datatype0(float, Float, xsd:float, Atom):-
  (
    nonvar(Float)
  ->
    float(Float),
    atom_number(Atom, Float)
  ;
    atom_number(Atom, Number),
    Float is float(Number)
  ),
  xmls_datatype_check(xsd:float, Float).
% Day
xmls_datatype0(gDay, Day, xsd:gDay, Atom):-
  atom_number(Atom, Day),
  xmls_datatype_check(xsd:gDay, Day).
% Month
xmls_datatype0(gMonth, Month, xsd:gMonth, Atom):-
  atom_number(Atom, Month),
  xmls_datatype_check(xsd:gMonth, Month).
% Year
xmls_datatype0(gYear, Year, xsd:gYear, Atom):-
  atom_number(Atom, Year),
  xmls_datatype_check(xsd:gYear, Year).
% Integer
xmls_datatype0(int, Integer, xsd:int, Atom):-
  atom_number(Atom, Integer),
  xmls_datatype_check(xsd:int, Integer).
% String
xmls_datatype0(string, String, xsd:string, Atom):-
  (
    nonvar(String)
  ->
    term_to_atom(String, Atom)
  ;
    nonvar(Atom)
  ->
    String = Atom
  ).

%% xmls_datatype_check(+Datatype:uri, +Value) is semidet.
% Succeeds if the given value is of the given XML Schema datatype.

xmls_datatype_check(xsd:boolean, Boolean):-
  memberchk(Boolean, [0, 1, false, true]),
  !.
xmls_datatype_check(Datatype, TimeStamp):-
  rdf_memberchk(Datatype, [xsd:date, xsd:dateTime]),
  stamp_date_time(TimeStamp, _DateTime, 'UTC'),
  !.
xmls_datatype_check(Datatype, DateTime):-
  rdf_memberchk(Datatype, [xsd:date, xsd:dateTime]),
  date_time_stamp(DateTime, _TimeStamp),
  !.
xmls_datatype_check(xsd:double, Double):-
  float(Double),
  !.
xmls_datatype_check(xsd:float, Float):-
  float(Float),
  !.
xmls_datatype_check(xsd:gDay, Day):-
  integer(Day),
  between(1, 31, Day),
  !.
xmls_datatype_check(xsd:gMonth, Month):-
  integer(Month),
  between(1, 12, Month),
  !.
xmls_datatype_check(xsd:gYear, Year):-
  integer(Year),
  !.
xmls_datatype_check(xsd:int, Integer):-
  integer(Integer),
  (
    current_prolog_flag(bounded, true)
  ->
    current_prolog_flag(max_integer, Max),
    Integer =< Max,
    current_prolog_flag(min_integer, Min),
    Integer >= Min
  ;
    true
  ).

