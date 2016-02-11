:- module(
  datetime,
  [
    call_time/2,        % :Goal_0, -Seconds
    datetime_mask/3,    % +Mask, +DT, -MaskedDT
    date_to_datetime/2, % +D, -DT
    datetime_to_date/2, % +DT, -D
    get_datetime/1,     % -DT
    is_datetime/1       % +DT
  ]
).

/** <module> Date-time support

Support predicates for dealing with date and time representations.

@author Wouter Beek
@version 2015/08, 2015/11-2015/12, 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate(call_time(0,-)).

% Prolog date/3
error:has_type(date, date(Y,Mo,D)):-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% Prolog date/9
error:has_type(date, date(Y,Mo,D,H,Mi,S,Off,_,_)):-
  error:has_type(date, date(Y,Mo,D)),
  error:has_type(date, time(H,Mi,S)),
  (var(Off) -> true ; error:has_type(between(-50400,50400), Off)).
% XSD-inspired 7-value model.
error:has_type(datetime, datetime(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(rational, S)),
  (var(Off) -> true ; error:has_type(between(-840,840), Off)).
% Prolog time/3
error:has_type(date, time(H,Mi,S)):-
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)).





%! call_time(:Goal_0, -Seconds:nonneg) is det.

call_time(Goal_0, N):- get_time(X), Goal_0, get_time(Y), N is Y - X.



%! datetime_mask(
%!   +Mask:oneof([none,year,month,day,hour,minute,second,offset]),
%!   +Datetime:compound,
%!   -MaskedDatetime:compound
%! ) is det.

datetime_mask(none,   datetime(Y,Mo,D,H,Mi,S,Off), datetime(Y,Mo,D,H,Mi,S,Off)).
datetime_mask(year,   datetime(_,Mo,D,H,Mi,S,Off), datetime(_,Mo,D,H,Mi,S,Off)).
datetime_mask(month,  datetime(Y,_, D,H,Mi,S,Off), datetime(Y,_, D,H,Mi,S,Off)).
datetime_mask(day,    datetime(Y,Mo,_,H,Mi,S,Off), datetime(Y,Mo,_,H,Mi,S,Off)).
datetime_mask(hour,   datetime(Y,Mo,D,_,Mi,S,Off), datetime(Y,Mo,D,_,Mi,S,Off)).
datetime_mask(minute, datetime(Y,Mo,D,H,_, S,Off), datetime(Y,Mo,D,H,_, S,Off)).
datetime_mask(second, datetime(Y,Mo,D,H,Mi,_,Off), datetime(Y,Mo,D,H,Mi,_,Off)).
datetime_mask(offset, datetime(Y,Mo,D,H,Mi,S,_  ), datetime(Y,Mo,D,H,Mi,S,_  )).



%! date_to_datetime(+Date:compound, -Datetime:compound) is det.
% Converts the three Prolog date-time compound term representations to
% the one XSD datetime representation.
%
% The Prolog term: date(Y,Mo,D,H,Mi,S,Off,TZ,DST)
% The XSD term:    date(Y,Mo,D,H,Mi,S,Off)
%
% There are two main differences:
%   1. In Prolog `S` represents the seconds as a *floating point number*
%      between 0.0 and 60.0.
%      In XSD `S` represents the seconds as a *decimal number*
%      greater than or equal to 0 and less than 60.
%   2. In Prolog `Off` represents the offset relative to UTC in *seconds*
%      as an integer, where positive values are west of Greenwich.
%      In XSD `Off` represents the offset relative to UTC in *minutes*
%      as an integer between -840 and 840 inclusive.

date_to_datetime(time(H,Mi,S1), datetime(_,_,_,H,Mi,S2,_)):- !,
  S2 is rationalize(S1).
date_to_datetime(date(Y,Mo,D), datetime(Y,Mo,D,_,_,_,_)):- !.
date_to_datetime(date(Y,Mo,D,H,Mi,S1,Off1,_,_), datetime(Y,Mo,D,H,Mi,S2,Off2)):-
  S2 is rationalize(S1),
  Off2 is Off1 // 60.



%! datetime_to_date(+DateTime:compound, -Date:compound) is det.
% Conversion from XSD-inspired date-time representation to
% the three Prolog date-time compound term representations.

datetime_to_date(datetime(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
datetime_to_date(datetime(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
datetime_to_date(datetime(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! get_datetime(-Datetime:compound) is det.

get_datetime(DT):-
  get_time(TS),
  stamp_date_time(TS, D, local),
  date_to_datetime(D, DT).



%! is_datetime(@Term) is semidet.

is_datetime(T) :-
  is_of_type(datetime, T).
