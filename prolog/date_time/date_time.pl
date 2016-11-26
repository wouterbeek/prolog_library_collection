:- module(
  date_time,
  [
    call_statistics/3,        % :Goal_0, +Key, -Delta
    call_statistics/4,        % :Goal_0, +Key, +NumCalls, -AvgDelta
    date_time_masks/3,        % +Masks, +DT, -MaskedDT
    'dt-pl_to_date_time'/2,   % +Pl, -DT
    'date_time_to_dt-pl'/2,   % +DT, -Pl
    'date_time_to_dt-rdf'/3,  % +DT, +D, -Rdf
    'dt-rdf_to_date_time'/2,  % +Rdf, -DT
    get_date_time/1,          % -DT
    'is_dt-pl'/1,             % @Term
    'is_dt-rdf'/1,            % @Term
    is_date_time/1,           % @Term
    something_to_date_time/2, % +Something, -DT
    'something_to_dt-rdf'/3,  % +Something, -DT
    timestamp_to_date_time/2  % +TS, -DT
  ]
).

/** <module> Date-time support

Support predicates for dealing with date and time representations.

Prolog uses multiple representations for date/time values.  This
module converts all these representations into one that is consistent
with the XSD date/time model.

There are -- at least -- the following date/time representations:

  * Prolog float/timestamp (`time`)
  * The main Prolog date/time compounds (`dt-pl`):
    * time/3
    * date/3
    * date/9
  * From Semweb library `rdf11` (`dt-rdf`):
    * date/3
    * date_time/6
    * month_day/2
    * time/3
    * year_month/2
  * XSD (`date_time`):
    * date_time/7

The purpose of this module is to allow the programmer to write
predicates that only work with date_time/7.  This is a huge
time-saver!

@author Wouter Beek
@version 2015/08, 2015/11-2015/12, 2016/02, 2016/07-2016/08, 2016/10-2016/11
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf11)).

:- meta_predicate
    call_statistics(0, +, -),
    call_statistics(0, +, +, -).

:- rdf_meta
   'date_time_to_dt-rdf'(+, r, -).

% XSD-inspired 7-value model, except for seconds,
% which is a float or integer rather than a rational,
error:has_type(date_time, date_time(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)),
  (var(Off) -> true ; error:has_type(between(-840,840), Off)).
% Prolog date/3
error:has_type('dt-pl', date(Y,Mo,D)):-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% Prolog date/9
error:has_type('dt-pl', date(Y,Mo,D,H,Mi,S,Off,_,_)):-
  error:has_type(date, date(Y,Mo,D)),
  error:has_type(date, time(H,Mi,S)),
  (var(Off) -> true ; error:has_type(between(-50400,50400), Off)).
% Prolog time/3
error:has_type('dt-pl', time(H,Mi,S)):-
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)).
% RDF11 date/3
error:has_type('dt-rdf', date(Y,Mo,D)) :-
  error:has_type('dt-pl', date(Y,Mo,D)).
% RDF11 date_time/6
error:has_type('dt-rdf', date_time(Y,Mo,D,H,Mi,S)) :-
  error:has_type('dt-rdf', date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; (error:has_type(integer, S) ; error:has_type(float, S))).
% RDF11 month_day/2
error:has_type('dt-rdf', month_day(Mo,D)) :-
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% RDF11 year_month/2
error:has_type('dt-rdf', year_month(Y,Mo)) :-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)).  
% RDF11 time/3
error:has_type('dt-rdf', time(H,Mi,S)) :-
  error:has_type('dt-pl', time(H,Mi,S)).





%! call_statistics(:Goal_0, +Key, -Delta) is det.
%! call_statistics(:Goal_0, +Key, +NumCalls, -AvgDelta) is det.
%
% call_statistics/3 preserves variable bindings but call_statistics/4
% does not.

call_statistics(Goal_0, Key, Delta):-
  statistics(Key, Val1),
  call(Goal_0),
  statistics(Key, Val2),
  Delta is Val2 - Val1.


call_statistics(Goal_0, Key, NumCalls, AvgDelta):-
  statistics(Key, Val1),
  forall(
    between(1, NumCalls, _),
    (copy_term(Goal_0, GoalCopy_0), call(GoalCopy_0))
  ),
  statistics(Key, Val2),
  AvgDelta is (Val2 - Val1) / NumCalls.



%! date_time_mask(
%!   +Mask:oneof([none,year,month,day,hour,minute,second,offset]),
%!   +DT,
%!   -MaskedDT
%! ) is det.

date_time_mask(none,   DT,                           DT                          ) :- !.
date_time_mask(year,   date(_,Mo,D),                 date(_,Mo,D)                ) :- !.
date_time_mask(year,   date_time(_,Mo,D,H,Mi,S,Off), date_time(_,Mo,D,H,Mi,S,Off)) :- !.
date_time_mask(year,   time(H,Mi,S),                 time(H,Mi,S)                ) :- !.
date_time_mask(month,  date(Y,_, D),                 date(Y,_, D)                ) :- !.
date_time_mask(month,  date_time(Y,_, D,H,Mi,S,Off), date_time(Y,_, D,H,Mi,S,Off)) :- !.
date_time_mask(month,  time(H,Mi,S),                 time(H,Mi,S)                ) :- !.
date_time_mask(day,    date(Y,Mo,_),                 date(Y,Mo,_)                ) :- !.
date_time_mask(day,    date_time(Y,Mo,_,H,Mi,S,Off), date_time(Y,Mo,_,H,Mi,S,Off)) :- !.
date_time_mask(day,    time(H,Mi,S),                 time(H,Mi,S)                ) :- !.
date_time_mask(hour,   date(Y,Mo,D),                 date(Y,Mo,D)                ) :- !.
date_time_mask(hour,   date_time(Y,Mo,D,_,Mi,S,Off), date_time(Y,Mo,D,_,Mi,S,Off)) :- !.
date_time_mask(hour,   time(_,Mi,S),                 time(_,Mi,S)                ) :- !.
date_time_mask(minute, date(Y,Mo,D),                 date(Y,Mo,D)                ) :- !.
date_time_mask(minute, date_time(Y,Mo,D,H,_, S,Off), date_time(Y,Mo,D,H,_, S,Off)) :- !.
date_time_mask(minute, time(H,_, S),                 time(H,_, S)                ) :- !.
date_time_mask(second, date(Y,Mo,D),                 date(Y,Mo,D)                ) :- !.
date_time_mask(second, date_time(Y,Mo,D,H,Mi,_,Off), date_time(Y,Mo,D,H,Mi,_,Off)) :- !.
date_time_mask(second, time(H,Mi,_),                 time(H,Mi,_)                ) :- !.
date_time_mask(offset, date(Y,Mo,D),                 date(Y,Mo,D)                ) :- !.
date_time_mask(offset, date_time(Y,Mo,D,H,Mi,S,_  ), date_time(Y,Mo,D,H,Mi,S,_  )) :- !.
date_time_mask(_,      DT,                           DT                          ).



%! date_time_masks(+Masks:list(atom), +DT, -MaskedDT) is det.

date_time_masks([], DT, DT) :- !.
date_time_masks([H|T], DT1, DT3) :-
  date_time_mask(H, DT1, DT2),
  date_time_masks(T, DT2, DT3).



%! 'date_time_to_dt-pl'(+DT, -D) is det.
%
% Conversion from XSD-inspired date/time representation to
% the three Prolog date/time compound term representations.

'date_time_to_dt-pl'(date_time(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
'date_time_to_dt-pl'(date_time(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
'date_time_to_dt-pl'(date_time(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! 'date_time_to_dt-rdf'(+DT, +D, -Rdf) is det.

'date_time_to_dt-rdf'(date_time(Y,Mo,D,_,_,_,_), xsd:date, date(Y,Mo,D)) :- !.
'date_time_to_dt-rdf'(date_time(Y,Mo,D,H,Mi,S,_), xsd:dateTime, date_time(Y,Mo,D,H,Mi,S)) :- !.
'date_time_to_dt-rdf'(date_time(_,_,D,_,_,_,_), xsd:gDay, D) :- !.
'date_time_to_dt-rdf'(date_time(_,Mo,_,_,_,_,_), xsd:gMonth, Mo) :- !.
'date_time_to_dt-rdf'(date_time(_,Mo,D,_,_,_,_), xsd:gMonthDay, month_day(Mo,D)) :- !.
'date_time_to_dt-rdf'(date_time(Y,_,_,_,_,_,_), xsd:gYear, Y) :- !.
'date_time_to_dt-rdf'(date_time(Y,Mo,_,_,_,_,_), xsd:gYearMonth, year_month(Y,Mo)) :- !.
'date_time_to_dt-rdf'(date_time(_,_,_,H,Mi,S,_), xsd:time, time(H,Mi,S)).



%! 'dt-pl_to_date_time'(+Pl, -DT) is det.
%
% Converts the two Prolog date representations to the one XSD
% date/time representation.
%
% The two Prolog date representations:
%
%   * `date(Y,Mo,D)`
%
%   * `date(Y,Mo,D,H,Mi,S,Off,TZ,DST)`
%
% The one XSD date/time representation:
%
%   * `date(Y,Mo,D,H,Mi,S,Off)`
%
% Apart from a difference in compound structure there are also two
% differences in value semantics:
%
%   1. In Prolog `S` represents the seconds as a *floating point
%      number* between 0.0 and 60.0.  In XSD `S` represents the
%      seconds as a *decimal number* greater than or equal to 0 and
%      less than 60.
%
%   2. In Prolog `Off` represents the offset relative to UTC in
%      *seconds* as an integer, where positive values are west of
%      Greenwich.  In XSD `Off` represents the offset relative to UTC
%      in *minutes* as an integer between -840 and 840 inclusive.

'dt-pl_to_date_time'(date(Y,Mo,D), date_time(Y,Mo,D,_,_,_,0)):- !.
'dt-pl_to_date_time'(date(Y,Mo,D,H,Mi,S,Off1,_,_), date_time(Y,Mo,D,H,Mi,S,Off2)):- !,
  Off2 is Off1 // 60.
'dt-pl_to_date_time'(time(H,Mi,S), date_time(_,_,_,H,Mi,S,0)).



%! 'dt-rdf_to_date_time'(+DT1, -DT2) is det.

'dt-rdf_to_date_time'(date(Y,Mo,D), date_time(Y,Mo,D,_,_,_,0)) :- !.
'dt-rdf_to_date_time'(date_time(Y,Mo,D,H,Mi,S), date_time(Y,Mo,D,H,Mi,S,0)) :- !.
'dt-rdf_to_date_time'(month_day(Mo,D), date_time(_,Mo,D,_,_,_,0)) :- !.
'dt-rdf_to_date_time'(time(H,Mi,S), date_time(_,_,_,H,Mi,S,0)) :- !.
'dt-rdf_to_date_time'(year_month(Y,Mo), date_time(Y,Mo,_,_,_,_,0)).



%! get_date_time(-DT) is det.

get_date_time(DT):-
  get_time(TS),
  timestamp_to_date_time(TS, DT).



%! is_date_time(@Term) is semidet.

is_date_time(T) :-
  is_of_type(date_time, T).



%! 'is_dt-pl'(@Term) is semidet.

'is_dt-pl'(T) :-
  is_of_type('dt-pl', T).



%! 'is_dt-rdf'(@Term) is semidet.

'is_dt-rdf'(T) :-
  is_of_type('dt-rdf', T).



%! something_to_date_time(+Something, -DT) is det.

% Full date/time notation.
something_to_date_time(
  date_time(Y,Mo,D,H,Mi,S,Off),
  date_time(Y,Mo,D,H,Mi,S,Off)
) :- !.
something_to_date_time(Rdf, DT) :-
  'is_dt-rdf'(Rdf), !,
  'dt-rdf_to_date_time'(Rdf, DT).
something_to_date_time(Pl, DT) :-
  'is_dt-pl'(Pl), !,
  'dt-pl_to_date_time'(Pl, DT).
something_to_date_time(TS, DT) :-
  float(TS), !,
  timestamp_to_date_time(TS, DT).



%! 'something_to_dt-rdf'(+Something, +D, -DT) is det.

'something_to_dt-rdf'(Something, D, Rdf) :-
  something_to_date_time(Something, DT),
  'date_time_to_dt-rdf'(DT, D, Rdf).



%! timestamp_to_date_time(+TS, -DT) is det.

timestamp_to_date_time(TS, DT) :-
  stamp_date_time(TS, D, local),
  'dt-pl_to_date_time'(D, DT).
