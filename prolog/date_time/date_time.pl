:- module(
  date_time,
  [
    call_time/2,             % :Goal_0, -Seconds
    date_time_masks/3,       % +Masks, +DT, -MaskedDT
    date_to_date_time/2,     % +D, -DT
    date_time_to_date/2,     % +DT, -D
    get_date_time/1,         % -DT
    is_date/1,               % +D
    is_date_time/1,          % +DT
    stamp_to_date_time/2,    % +TS, -DT
    something_to_date_time/2 % +Something, -DT
  ]
).

/** <module> Date-time support

Support predicates for dealing with date and time representations.

@author Wouter Beek
@version 2015/08, 2015/11-2015/12, 2016/02
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate
    call_time(0, -).

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
% XSD-inspired 7-value model, except for seconds,
% which is a float or integer rather than a rational,
error:has_type(date_time, date_time(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date, date(Y,Mo,D)),
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)),
  (var(Off) -> true ; error:has_type(between(-840,840), Off)).
% Prolog time/3
error:has_type(date, time(H,Mi,S)):-
  (var(H) -> true ; error:has_type(between(0,24), H)),
  (var(Mi) -> true ; error:has_type(between(0,59), Mi)),
  (var(S) -> true ; error:has_type(float, S)).





%! call_time(:Goal_0, -Seconds:float) is det.

call_time(Goal_0, N):- get_time(X), Goal_0, get_time(Y), N is Y - X.



%! date_time_mask(
%!   +Mask:oneof([none,year,month,day,hour,minute,second,offset]),
%!   +Datetime:compound,
%!   -MaskedDatetime:compound
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



%! date_to_date_time(+D, -DT) is det.
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

date_to_date_time(time(H,Mi,S), date_time(_,_,_,H,Mi,S,_)):- !.
date_to_date_time(date(Y,Mo,D), date_time(Y,Mo,D,_,_,_,_)):- !.
date_to_date_time(date(Y,Mo,D,H,Mi,S,Off1,_,_), date_time(Y,Mo,D,H,Mi,S,Off2)):-
  Off2 is Off1 // 60.



%! date_time_to_date(+DT, -D) is det.
% Conversion from XSD-inspired date-time representation to
% the three Prolog date-time compound term representations.

date_time_to_date(date_time(Y,Mo,D,H,Mi,S1,Off), time(H,Mi,S2)):-
  maplist(var, [Y,Mo,D,Off]), !,
  S2 is float(S1).
date_time_to_date(date_time(Y,Mo,D,H,Mi,S,Off), date(Y,Mo,D)):-
  maplist(var, [H,Mi,S,Off]), !.
date_time_to_date(date_time(Y,Mo,D,H,Mi,S1,Off1), date(Y,Mo,D,H,Mi,S2,Off2,-,-)):-
  (var(Off1) -> true ; Off2 is Off1 * 60),
  S2 is float(S1).



%! get_date_time(-DT) is det.

get_date_time(DT):-
  get_time(TS),
  stamp_to_date_time(TS, DT).



%! is_date(@Term) is semidet.

is_date(T) :-
  is_of_type(date, T).



%! is_date_time(@Term) is semidet.

is_date_time(T) :-
  is_of_type(date_time, T).



%! stamp_to_date_time(+TS, -DT) is det.

stamp_to_date_time(TS, DT) :-
  stamp_date_time(TS, D, local),
  date_to_date_time(D, DT).



%! something_to_date_time(+Something, -DT) is det.

something_to_date_time(DT, DT) :-
  is_date_time(DT), !.
something_to_date_time(D, DT) :-
  is_date(D), !,
  date_to_date_time(D, DT).
something_to_date_time(Stamp, DT) :-
  float(Stamp), !,
  stamp_to_date_time(Stamp, DT).
