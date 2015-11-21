:- module(
  date_ext,
  [
    dateTime_date/2, % +DateTime:compound
                     % -Date:compound
    get_date/1 % -Date:compound
  ]
).

/** <module> Date-time extensions

Support predicates for dealing with date and time representations.

@author Wouter Beek
@version 2015/08, 2015/11
*/

:- use_module(library(apply)).





%! dateTime_date(+DateTime:compound, -Date:compound) is det.
% Conversion from XSD-inspired dateTime representation to
% the three Prolog date-time compound term representations.

dateTime_date(dateTime(Y,Mo,D,H,Mi,S0,Off), time(H,Mi,S)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S is float(S0).
dateTime_date(dateTime(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
dateTime_date(dateTime(Y,Mo,D,H,Mi,S0,Off0), date(Y,Mo,D,H,Mi,S,Off,-,-)):-
  (var(Off0) -> true ; Off is Off0 * 60),
  S is float(S0).



%! get_date(-Date:compound) is det.

get_date(Date):-
  get_time(Timestamp),
  stamp_date_time(Timestamp, Date, local).
