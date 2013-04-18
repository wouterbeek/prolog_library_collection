:- module(
  xmls,
  [
    xmls_datatype/2, % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,integer])
                     % ?Datatype:uri
    xmls_datatype/4 % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,integer])
                    % ?LexicalValue
                    % ?Datatype:uri
                    % ?CanonicalValue
  ]
).

/** <module> XML Schema

Standard support for XML Schema.

@author Wouter Beek
@version 2013/01, 2013/03
*/

:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#', [keep(true)]).

:- rdf_meta(xmls_datatype(?,r)).
:- rdf_meta(xmls_datatype(?,?,r,?)).
:- rdf_meta(xmls_datatype0(?,?,r,?)).



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

%% datatype(
%%   ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,integer]),
%%   ?LexicalValue,
%%   ?Datatype:uri,
%%   ?CanonicalValue
%% ) is nondet.

xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  nonvar(DatatypeName),
  !,
  xmls_datatype0(DatatypeName, LexicalValue, Datatype, CanonicalValue),
  !.
xmls_datatype(DatatypeName, LexicalValue, Datatype, CanonicalValue):-
  xmls_datatype0(DatatypeName, LexicalValue, Datatype, CanonicalValue).

xmls_datatype0(boolean, 0,     xsd:boolean, false).
xmls_datatype0(boolean, 1,     xsd:boolean, true ).
xmls_datatype0(boolean, false, xsd:boolean, false).
xmls_datatype0(boolean, true,  xsd:boolean, true ).

xmls_datatype0(date, LexicalValue, xsd:date, CanonicalValue):-
  format_time(atom(CanonicalValue), '%F%:z', LexicalValue).

xmls_datatype0(dateTime, LexicalValue, xsd:dateTime, CanonicalValue):-
  nonvar(LexicalValue),
  !,
  format_time(atom(CanonicalValue), '%FT%T%:z', LexicalValue).
xmls_datatype0(dateTime, LexicalValue, xsd:dateTime, CanonicalValue):-
  nonvar(CanonicalValue),
  !,
  parse_time(CanonicalValue, iso_8601, LexicalValue).

xmls_datatype0(double, Value, xsd:double, Value1):-
  atom_or_number_number(Value, Value1),
  float(Value1).

xmls_datatype0(float, Value, xsd:float, Value):-
  atom_or_number_number(Value, Value1),
  float(Value1).

xmls_datatype0(gDay, Value, xsd:gDay, Value1):-
  atom_or_number_number(Value, Value1),
  integer(Value1),
  atom_length(Value, 2).

xmls_datatype0(gMonth, Value, xsd:gMonth, Value1):-
  atom_or_number_number(Value, Value1),
  integer(Value1),
  atom_length(Value, 2).

xmls_datatype0(gYear, LexicalValue, xsd:gYear, CanonicalValue):-
  atom_or_number_number(CanonicalValue, LexicalValue),
  integer(LexicalValue),
  atom_length(CanonicalValue, Length),
  Length >= 4.

xmls_datatype0(int, LexicalValue, xsd:int, CanonicalValue):-
  current_prolog_flag(bounded, false),
  !,
  atom_or_number_number(CanonicalValue, LexicalValue),
  integer(LexicalValue).
xmls_datatype0(int, LexicalValue, xsd:int, CanonicalValue):-
  current_prolog_flag(bounded, false),
  !,
  atom_or_number_number(CanonicalValue, LexicalValue),
  integer(LexicalValue),
  current_prolog_flag(max_integer, Max),
  LexicalValue =< Max,
  current_prolog_flag(min_integer, Min),
  LexicalValue >= Min.

xmls_datatype0(string, LexicalValue, xsd:string, CanonicalValue):-
  term_atom(LexicalValue, CanonicalValue).

