:- module(
  date_time,
  [
    call_statistics/4,        % :Goal_0, +Key, +NumCalls, -AvgDelta
    date_time_masks/3,        % +Masks, +DateTime, -MaskedDateTime
    debug_epoch/1,            % +Flag
    prolog_to_date_time/2,    % +Prolog, -DateTime
    date_time_to_prolog/2,    % +DateTime, -Prolog
    date_time_to_semweb/3,    % +DateTime, +Datatype, -Semweb
    semweb_to_date_time/2,    % +Semweb, -DateTime
    get_date_time/1,          % -DateTime
    is_prolog_date_time/1,    % @Term
    is_semweb_date_time/1,    % @Term
    is_date_time/1,           % @Term
    something_to_date_time/2, % +Something, -DateTime
    something_to_semweb/3,    % +Something, -DateTime
    something_to_stamp/2,     % +Something:term, -Stamp:float
    timestamp_to_date_time/2  % +Timestamp, -DateTime
  ]
).

/** <module> Date-time support

Support predicates for dealing with date and time representations.

Prolog uses multiple representations for date/time values.  This
module converts all these representations into one that is consistent
with the XSD date/time model.

There are -- at least -- the following date/time representations:

  * Prolog float/timestamp (type: `time`)

  * The main Prolog date/time compounds (type: `prolog_date_time`,
    variable: `Prolog'):

    * time/3
    * date/3
    * date/9

  * From library `semweb/rdf11` (type: `semweb_date_time', variable:
    `Semweb`):

    * date/3
    * date_time/6
    * month_day/2
    * time/3
    * year_month/2

  * XSD (type: `date_time`, variable: `DateTime`):

    * date_time/7

The purpose of this module is to allow the programmer to write
predicates that only work with date_time/7.  This is a huge
date/time-saver!

@author Wouter Beek
@version 2015/08-2017/01, 2017/05-2017/07
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(semweb/rdf11) ,[
     (rdf_meta)/1,
     op(1150, fx, rdf_meta)
   ]).

:- meta_predicate
    call_statistics(0, +, +, -).

:- rdf_meta
   'date_time_to_semweb'(+, r, -).

% XSD-inspired 7-value model, except for seconds,
% which is a float or integer rather than a rational,
error:has_type(date_time, date_time(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)),
  (var(Off) -> true ; error:has_type(between(-840,840), Off)).
% Prolog date/3
error:has_type(prolog_date_time, date(Y,Mo,D)):-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% Prolog date/9
error:has_type(prolog_date_time, date(Y,Mo,D,H,Mi,S,Off,_,_)):-
  error:has_type(date, date(Y,Mo,D)),
  error:has_type(date, time(H,Mi,S)),
  (var(Off) -> true ; error:has_type(between(-50400,50400), Off)).
% Prolog time/3
error:has_type(prolog_date_time, time(H,Mi,S)):-
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)).
% RDF11 date/3
error:has_type(semweb_date_time, date(Y,Mo,D)) :-
  error:has_type(prolog_date_time, date(Y,Mo,D)).
% RDF11 date_time/6
error:has_type(semweb_date_time, date_time(Y,Mo,D,H,Mi,S)) :-
  error:has_type(semweb_date_time, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; (error:has_type(integer, S) ; error:has_type(float, S))).
% RDF11 month_day/2
error:has_type(semweb_date_time, month_day(Mo,D)) :-
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% RDF11 year_month/2
error:has_type(semweb_date_time, year_month(Y,Mo)) :-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)).  
% RDF11 time/3
error:has_type(semweb_date_time, time(H,Mi,S)) :-
  error:has_type(prolog_date_time, time(H,Mi,S)).





%! call_statistics(:Goal_0, +Key, +NumCalls, -AvgDelta) is det.
%
% call_statistics/3 preserves variable bindings but call_statistics/4
% does not.

call_statistics(Goal_0, Key, NumCalls, AvgDelta):-
  statistics(Key, Val1a),
  fix_value0(Val1a, Val1b),
  forall(
    between(1, NumCalls, _),
    (copy_term(Goal_0, GoalCopy_0), call(GoalCopy_0))
  ),
  statistics(Key, Val2a),
  fix_value0(Val2a, Val2b),
  AvgDelta is (Val2b - Val1b) / NumCalls.

fix_value0([X,_], X) :- !.
fix_value0(X, X).



%! date_time_mask(+Mask, +DateTime, -MaskedDateTime) is det.
%
% @arg Mask is one of the following atoms: `year', `month', `day',
%      `hour', `minute', `second', and `offset'.

date_time_mask(none,   DateTime,                     DateTime                          ) :- !.
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
date_time_mask(_,      DateTime,                     DateTime                          ).



%! date_time_masks(+Masks:list, +DateTime, -MaskedDateTime) is det.

date_time_masks([], DateTime, DateTime) :- !.
date_time_masks([H|T], DateTime1, DateTime3) :-
  date_time_mask(H, DateTime1, DateTime2),
  date_time_masks(T, DateTime2, DateTime3).



%! date_time_to_prolog(+DateTime, -Prolog) is det.
%
% Conversion from XSD-inspired date/time representation to
% the three Prolog date/time compound term representations.

date_time_to_prolog(date_time(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
date_time_to_prolog(date_time(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
date_time_to_prolog(date_time(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! date_time_to_semweb(+DateTime, +Datatype, -Semweb) is det.

date_time_to_semweb(date_time(Y,Mo,D,_,_,_,_), xsd:date, date(Y,Mo,D)) :- !.
date_time_to_semweb(date_time(Y,Mo,D,H,Mi,S,_), xsd:dateTime, date_time(Y,Mo,D,H,Mi,S)) :- !.
date_time_to_semweb(date_time(_,_,D,_,_,_,_), xsd:gDay, D) :- !.
date_time_to_semweb(date_time(_,Mo,_,_,_,_,_), xsd:gMonth, Mo) :- !.
date_time_to_semweb(date_time(_,Mo,D,_,_,_,_), xsd:gMonthDay, month_day(Mo,D)) :- !.
date_time_to_semweb(date_time(Y,_,_,_,_,_,_), xsd:gYear, Y) :- !.
date_time_to_semweb(date_time(Y,Mo,_,_,_,_,_), xsd:gYearMonth, year_month(Y,Mo)) :- !.
date_time_to_semweb(date_time(_,_,_,H,Mi,S,_), xsd:time, time(H,Mi,S)).



%! debug_epoch(+Flag:term) is det.

debug_epoch(Flag) :-
  get_time(Epoch),
  debug(Flag, "~w", [Epoch]).



%! prolog_to_date_time(+Prolog, -DateTime) is det.
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

prolog_to_date_time(date(Y,Mo,D), date_time(Y,Mo,D,_,_,_,0)):- !.
prolog_to_date_time(
  date(Y,Mo,D,H,Mi,S,Off1,_,_),
  date_time(Y,Mo,D,H,Mi,S,Off2)
):- !,
  Off2 is Off1 // 60.
prolog_to_date_time(time(H,Mi,S), date_time(_,_,_,H,Mi,S,0)).



%! semweb_to_date_time(+Semweb, -DateTime) is det.

semweb_to_date_time(date(Y,Mo,D), date_time(Y,Mo,D,_,_,_,0)) :- !.
semweb_to_date_time(date_time(Y,Mo,D,H,Mi,S), date_time(Y,Mo,D,H,Mi,S,0)) :- !.
semweb_to_date_time(month_day(Mo,D), date_time(_,Mo,D,_,_,_,0)) :- !.
semweb_to_date_time(time(H,Mi,S), date_time(_,_,_,H,Mi,S,0)) :- !.
semweb_to_date_time(year_month(Y,Mo), date_time(Y,Mo,_,_,_,_,0)).



%! get_date_time(-DateTime) is det.

get_date_time(DateTime):-
  get_time(Timestamp),
  timestamp_to_date_time(Timestamp, DateTime).



%! is_date_time(@Term) is semidet.

is_date_time(T) :-
  is_of_type(date_time, T).



%! is_prolog_date_time(@Term) is semidet.

is_prolog_date_time(T) :-
  is_of_type(prolog_date_time, T).



%! is_semweb_date_time(@Term) is semidet.

is_semweb_date_time(T) :-
  is_of_type(semweb_date_time, T).



%! something_to_date_time(+Something, -DateTime) is det.

% Full date/time notation.
something_to_date_time(
  date_time(Y,Mo,D,H,Mi,S,Off),
  date_time(Y,Mo,D,H,Mi,S,Off)
) :- !.
something_to_date_time(Semweb, DateTime) :-
  is_semweb_date_time(Semweb), !,
  semweb_to_date_time(Semweb, DateTime).
something_to_date_time(Prolog, DateTime) :-
  is_prolog_date_time(Prolog), !,
  prolog_to_date_time(Prolog, DateTime).
something_to_date_time(Timestamp, DateTime) :-
  float(Timestamp), !,
  timestamp_to_date_time(Timestamp, DateTime).



%! something_to_semweb(+Something, +D, -DateTime) is det.

something_to_semweb(Something, D, Semweb) :-
  something_to_date_time(Something, DateTime),
  date_time_to_semweb(DateTime, D, Semweb).



%! something_to_stamp(+Something:term, -Stamp:float) is det.

something_to_stamp(Something, Stamp) :-
  something_to_date_time(Something, DateTime),
  date_time_stamp(DateTime, Stamp).



%! timestamp_to_date_time(+Timestamp, -DateTime) is det.

timestamp_to_date_time(Timestamp, DateTime) :-
  stamp_date_time(Timestamp, Prolog, local),
  prolog_to_date_time(Prolog, DateTime).
