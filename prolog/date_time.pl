:- module(
  date_time,
  [
  % CONVERSIONS
    timestamp_to_dt/2, % +Timestamp, -DT
    date_time_to_dt/2, % +DateTime, -DT
    dt_to_date_time/2, % +DT, -DateTime
    is_date_time/1,    % @Term
    is_dt/1,           % @Term
  % OPERATIONS
    date_time_masks/3, % +Masks, +DT1, -DT2
    now/1              % -DT
  ]
).

/** <module> Date-time support

Support predicates for dealing with date/time representations.

Prolog uses multiple representations for date/time values.  This
module converts all these representations into one that is consistent
with the XSD 7-property model.

Prolog uses the following date/time representations:

  * float (timestamp)
  * time/3
  * date/3
  * date/9

We use the following date/time representation:

    * dt/7

The purpose of this module is to allow the programmer to write all
predicates that use date/time representations to only work with dt/7.
This is a huge date/time-saver!

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(error)).
:- use_module(library(solution_sequences)).

% XSD-inspired 7-value model, except for seconds,
% which is a float or integer rather than a rational,
error:has_type(dt, dt(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)),
  (var(Off) -> true ; error:has_type(between(-840,840), Off)).
% Prolog date/3
error:has_type(date_time, date(Y,Mo,D)):-
  (var(Y) -> true ; error:has_type(integer, Y)),
  (var(Mo) -> true ; error:has_type(between(1,12), Mo)),
  (var(D) -> true ; error:has_type(between(1,31), D)).
% Prolog date/9
error:has_type(date_time, date(Y,Mo,D,H,Mi,S,Off,_,_)):-
  error:has_type(date, date(Y,Mo,D)),
  error:has_type(date, time(H,Mi,S)),
  (var(Off) -> true ; error:has_type(between(-50400,50400), Off)).
% Prolog time/3
error:has_type(date_time, time(H,Mi,S)):-
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)).





% CONVERSIONS %

%! timestamp_to_dt(+Timestamp:float, -DT:dt) is det.

timestamp_to_dt(Timestamp, DT) :-
  date_time_stamp(DateTime, Timestamp),
  date_time_to_dt(DateTime, DT).



%! date_time_to_dt(+DateTime:date_time, -DT:dt) is det.
%
% Converts the three Prolog date/time representations to the one
% XSD-inspired 7-property model representation (type `dt`).
%
% Prolog uses the following three date/time representations (type
% `date_time`):
%
%   * `date(Y,Mo,D)`
%   * `date(Y,Mo,D,H,Mi,S,Off,TZ,DST)`
%   * `time(H,Mi,S)`
%
% The one `dt` representation:
%
%   * `dt(Y,Mo,D,H,Mi,S,Off)`
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

date_time_to_dt(date(Y,Mo,D), dt(Y,Mo,D,_,_,_,0)):- !.
date_time_to_dt(date(Y,Mo,D,H,Mi,S,Off1,_,_), dt(Y,Mo,D,H,Mi,S,Off2)):- !,
  Off2 is Off1 // 60.
date_time_to_dt(time(H,Mi,S), dt(_,_,_,H,Mi,S,0)).



%! dt_to_date_time(+DT:dt, -DateTime:date_time) is det.
%
% Conversion from the XSD-inspired 7-property model to the three
% Prolog date/time compound term representations.

dt_to_date_time(dt(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
dt_to_date_time(dt(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
dt_to_date_time(dt(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! is_dt(@Term) is semidet.

is_dt(Term) :-
  is_of_type(dt, Term).



%! is_date_time(@Term) is semidet.

is_date_time(Term) :-
  is_of_type(date_time, Term).





% OPERATIONS %

%! date_time_mask(+Mask:atom) is semidet.
%! date_time_mask(-Mask:atom) is multi.

date_time_mask(Mask) :-
  distinct(Mask, date_time_mask_(Mask, _, _)).



%! date_time_mask(+Mask:atom, +DateTime:dt, -MaskedDateTime:dt) is det.
%
% @arg Mask is one of the values of date_time_mask/1.
%
% TBD: Support Mask=none?

date_time_mask(Mask, DT1, DT2) :-
  call_must_be(date_time_mask, Mask),
  must_be(dt, DT1),
  once(date_time_mask_(Mask, DT1, DT2)).

date_time_mask_(none,   DateTime,              DateTime             ).
date_time_mask_(year,   dt(_,Mo,D,H,Mi,S,Off), dt(_,Mo,D,H,Mi,S,Off)).
date_time_mask_(month,  dt(Y,_, D,H,Mi,S,Off), dt(Y,_, D,H,Mi,S,Off)).
date_time_mask_(day,    dt(Y,Mo,_,H,Mi,S,Off), dt(Y,Mo,_,H,Mi,S,Off)).
date_time_mask_(hour,   dt(Y,Mo,D,_,Mi,S,Off), dt(Y,Mo,D,_,Mi,S,Off)).
date_time_mask_(minute, dt(Y,Mo,D,H,_, S,Off), dt(Y,Mo,D,H,_, S,Off)).
date_time_mask_(second, dt(Y,Mo,D,H,Mi,_,Off), dt(Y,Mo,D,H,Mi,_,Off)).
date_time_mask_(offset, dt(Y,Mo,D,H,Mi,S,_  ), dt(Y,Mo,D,H,Mi,S,_  )).



%! date_time_masks(+Masks:list(atom), +DateTime:dt, -MaskedDateTime:dt) is det.
%
% Apply an arbitrary number of date/time masks.
%
% @see date_time_mask/3

date_time_masks([], DT, DT) :- !.
date_time_masks([H|T], DT1, DT3) :-
  date_time_mask(H, DT1, DT2),
  date_time_masks(T, DT2, DT3).



%! now(-DT:dt) is det.
%
% Return the current date/time as a `dt`-typed compound term.

now(DT):-
  get_time(Timestamp),
  timestamp_to_dt(Timestamp, DT).
