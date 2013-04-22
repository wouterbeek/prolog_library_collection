:- module(
  xmls,
  [
    xmls_datatype/2, % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,int])
                     % ?Datatype:uri
    xmls_datatype/4 % ?DatatypeName:oneof([boolean,dateTime,double,float,gDay,gMonth,gYear,int])
                    % ?LexicalValue
                    % ?Datatype:uri
                    % ?CanonicalValue
  ]
).

/** <module> XML Schema

Standards support for XML Schema.

@author Wouter Beek
@version 2013/01, 2013/03-2013/04
*/

:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).

:- rdf_register_namespace(xsd).

:- rdf_meta(xmls_convert_datatype(r,+,r,-)).
:- rdf_meta(xmls_datatype(?,r)).
:- rdf_meta(xmls_datatype(?,?,r,?)).
:- rdf_meta(xmls_datatype0(?,?,r,?)).
:- rdf_meta(xmls_datatype_check(r,+)).



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
    term_atom(String, Atom)
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

