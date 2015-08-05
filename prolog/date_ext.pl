:- module(
  date_ext,
  [
    get_date/1 % -Date:compound
  ]
).

/** <module> Date-time extensions

Support predicates for dealing with date and time representations.

@author Wouter Beek
@version 2015/08
*/





%! get_date(-Date:compound) is det.

get_date(Date):-
  get_time(Timestamp),
  stamp_date_time(Timestamp, Date, local).
