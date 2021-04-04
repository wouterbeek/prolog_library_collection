:- module(
  date_time,
  [
    cpu_time/2,        % :Goal_0, -Delta
  % CONVERSIONS
    date_time_to_dt/2, % +SwiDateTime, -RdfDateTime
    dt_to_timestamp/2, % +RdfDateTime, -Timestamp
    dt_to_date_time/2, % +RdfDateTime, -SwiDateTime
    is_date_time/1,    % @Term
    is_dt/1,           % @Term
    timestamp_to_dt/2, % +Timestamp, -RdfDateTime
  % OPERATIONS
    date_time_masks/3, % +Masks, +DateTime1, -DateTime2
    now/1,             % -DateTime
    number_of_days_in_month_of_year/3, % ?Year, ?Month, ?MaxDay
  % PP
    date_time_label/2, % +SwiDateTime, -Label
    date_time_label/3, % +SwiDateTime, -Label, +Options
    dt_label/2,        % +RdfDateTime, -Label
    dt_label/3,        % +RdfDateTime, -Label, +Options
  % HELPERS
    generate_as_digits//2, % +N, +NumberOfDigits
    generate_as_digits//3  % +N, +Base, +NumberOfDigits
  ]
).

/** <module> Date-time support

Support predicates for dealing with date/time representations.

Prolog uses multiple representations for date/time values.  This
module converts all these representations into one that is consistent
with the XSD 7-property model.

Prolog uses the following date/time representations, indicated with
`SwiDateTime':

  * floating-point (timestamp)
  * time/3
  * date/3
  * date/9

We use the following date/time representation, indicated with
`RdfDateTime':

  * dt/7

The SWI standard library `sgml' uses the following date/time
representation, indicated with `XsdDateTime':

  * floating-point (timestamp)
  * date/3
  * date/9
  * time/3

The purpose of this module is to allow the programmer to write all
predicates that use date/time representations to only work with dt/7.
This is a huge date/time-saver!

*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(solution_sequences)).

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(nlp_lang)).

:- meta_predicate
    cpu_time(0, -).

% XSD-inspired 7-value model, except for seconds,
% which is a float or integer rather than a rational,
error:has_type(dt, dt(Y,Mo,D,H,Mi,S,Off)):-
  error:has_type(date_time, date(Y,Mo,D)),
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





%! cpu_time(:Goal_0, -Delta:double) is det.

cpu_time(Goal_0, Delta) :-
  statistics(cputime, Cpu1),
  call(Goal_0),
  statistics(cputime, Cpu2),
  Delta is Cpu2 - Cpu1.





% CONVERSIONS %

%! date_time_to_dt(+SwiDateTime:compound, -RdfDateTime:compound) is det.
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



%! dt_to_date_time(+RdfDateTime:compound, -SwiDateTime:compound) is det.
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



%! dt_to_timestamp(+RdfDateTime:compound, -Timestamp:float) is det.

dt_to_timestamp(RdfDateTime, Timestamp) :-
  dt_to_date_time(RdfDateTime, SwiDateTime),
  date_time_stamp(SwiDateTime, Timestamp).



%! is_dt(@Term) is semidet.

is_dt(Term) :-
  is_of_type(dt, Term).



%! is_date_time(@Term) is semidet.

is_date_time(Term) :-
  is_of_type(date_time, Term).



%! timestamp_to_dt(+Timestamp:float, -RdfDateTime:compound) is det.

timestamp_to_dt(Timestamp, RdfDateTime) :-
  stamp_date_time(Timestamp, SwiDateTime, local),
  date_time_to_dt(SwiDateTime, RdfDateTime).




% OPERATIONS %

%! date_time_mask(+Mask:atom) is semidet.
%! date_time_mask(-Mask:atom) is multi.

date_time_mask(Mask) :-
  distinct(Mask, date_time_mask_(Mask, _, _)).



%! date_time_mask(+Mask:atom, +RdfDateTime1:compound, -RdfDateTime2:compound) is det.
%
% @param Mask is one of the values of date_time_mask/1.
%
% @todo Support Mask=none?

date_time_mask(Mask, RdfDateTime1, RdfDateTime2) :-
  call_must_be(date_time_mask, Mask),
  once(date_time_mask_(Mask, RdfDateTime1, RdfDateTime2)).

date_time_mask_(none,   RdfDateTime,           RdfDateTime          ).
date_time_mask_(year,   dt(_,Mo,D,H,Mi,S,Off), dt(_,Mo,D,H,Mi,S,Off)).
date_time_mask_(month,  dt(Y,_, D,H,Mi,S,Off), dt(Y,_, D,H,Mi,S,Off)).
date_time_mask_(day,    dt(Y,Mo,_,H,Mi,S,Off), dt(Y,Mo,_,H,Mi,S,Off)).
date_time_mask_(hour,   dt(Y,Mo,D,_,Mi,S,Off), dt(Y,Mo,D,_,Mi,S,Off)).
date_time_mask_(minute, dt(Y,Mo,D,H,_, S,Off), dt(Y,Mo,D,H,_, S,Off)).
date_time_mask_(second, dt(Y,Mo,D,H,Mi,_,Off), dt(Y,Mo,D,H,Mi,_,Off)).
date_time_mask_(offset, dt(Y,Mo,D,H,Mi,S,_  ), dt(Y,Mo,D,H,Mi,S,_  )).



%! date_time_masks(+Masks:list(atom),
%!                 +SwiDateTime1:compound,
%!                 -SwiDateTime2:compound) is det.
%
% Apply an arbitrary number of date/time masks.
%
% @see date_time_mask/3

date_time_masks([], SwiDateTime, SwiDateTime) :- !.
date_time_masks([H|T], SwiDateTime1, SwiDateTime3) :-
  date_time_mask(H, SwiDateTime1, SwiDateTime2),
  date_time_masks(T, SwiDateTime2, SwiDateTime3).



%! now(-RdfDateTime:compound) is det.
%
% Return the current date/time as a `dt`-typed compound term.

now(RdfDateTime):-
  get_time(Timestamp),
  timestamp_to_dt(Timestamp, RdfDateTime).



%! number_of_days_in_month_of_year(?Year:integer,
%!                                 ?Month:between(1,12),
%!                                 ?MaxDay:between(28,31)) is nondet.
%
% The number of days in month of year is:
%
%   * 31 if month is 1, 3, 5, 7, 8, 10, or 12;
%
%   * 30 if month is 4, 6, 9, or 11;
%
%   * 29 if month is 2 and year is a number divisible by 400, or if
%     year is a number divisible by 4 but not by 100;
%
%   * 28 otherwise.

number_of_days_in_month_of_year(_, Mo, 31):-
  memberchk(Mo, [1,3,5,7,8,10,12]), !.
number_of_days_in_month_of_year(_, Mo, 30):-
  memberchk(Mo, [4,6,9,11]), !.
number_of_days_in_month_of_year(Y, 2, 29):-
  (Y mod 400 =:= 0 ; (Y mod 4 =:= 0, Y mod 100 =\= 0)), !.
number_of_days_in_month_of_year(_, _, 28).



% PP %

%! date_time_label(+SwiDateTime:compound, -Label:string) is det.
%! date_time_label(+SwiDateTime:compound, -Label:string, +Options:options) is det.

date_time_label(SwiDateTime, Label) :-
  date_time_to_dt(SwiDateTime, RdfDateTime),
  dt_label(RdfDateTime, Label).


date_time_label(SwiDateTime, Label, Options) :-
  date_time_to_dt(SwiDateTime, RdfDateTime),
  dt_label(RdfDateTime, Label, Options).



%! dt_label(+RdfDateTime:compound, -Label:string) is det.
%! dt_label(+RdfDateTime:compound, -Label:string, +Options:options) is det.

dt_label(RdfDateTime, Label) :-
  dt_label(RdfDateTime, Label, options{}).


dt_label(RdfDateTime, Label, Options) :-
  string_phrase(dt_(RdfDateTime, Options), Label).

dt_(dt(Y,Mo,Da,H,Mi,S,Off), Options) -->
  (   {ground(date(Y,Mo,Da,H,Mi,S,Off))}
  ->  global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Options)
  ;   {ground(date(Y,Mo,Da,H,Mi,S))}
  ->  floating_date_and_time(Y, Mo, Da, H, Mi, S, Options)
  ;   {ground(date(Y,Mo,Da))}
  ->  date(Y, Mo, Da, Options)
  ;   {ground(date(H,Mi,S))}
  ->  time(H, Mi, S, Options)
  ;   {ground(date(Mo,Da))}
  ->  yearless_date(Mo, Da, Options)
  ;   {ground(date(Y,Mo))}
  ->  month(Y, Mo, Options)
  ;   {ground(date(Y))}
  ->  year(Y, Options)
  ;   {ground(date(Off))}
  ->  timezone_offset(Off)
  ).

%! date(+Year:integer,
%!      +Month:between(1,12),
%!      +Day:between(1,31),
%!      +Options:options)// is det.

date(Y, Mo, Da, Options) -->
  month_day(Da, Options),
  " ",
  month(Y, Mo, Options).

%! floating_date_and_time(+Year:integer,
%!                        +Month:between(1,12),
%!                        +Day:between(1,31),
%!                        +Hour:between(0,24),
%!                        +Minute:between(0,59),
%!                        +Second:float,
%!                        +Options:options)// is det.

floating_date_and_time(Y, Mo, Da, H, Mi, S, Options) -->
  date(Y, Mo, Da, Options),
  " ",
  time(H, Mi, S, Options).

%! global_date_and_time(+Year:integer,
%!                      +Month:between(1,12),
%!                      +Day:between(1,31),
%!                      +Hour:between(0,24),
%!                      +Minute:between(0,59),
%!                      +Second:float,
%!                      +Offset:between(-840,840),
%!                      +Options:options)// is det.

global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Options) -->
  floating_date_and_time(Y, Mo, Da, H, Mi, S, Options),
  timezone_offset(Off).

%! hour(+Hour:between(0,24), +Options:options)// is det.

hour(H, _) -->
  padding_zero(H),
  integer(H).

%! minute(+Minute:between(0,59), +Options:options)// is det.

minute(Mi, _) -->
  padding_zero(Mi),
  integer(Mi).

%! month(+Month:between(1,12), +Options:options)// is det.

month(Mo, Options) -->
  {
    dict_get(ltag, Options, en, LTag),
    dict_get(month_abbr, Options, false, IsAbbr),
    once(month_name(Mo, LTag, Abbr, Full)),
    (IsAbbr == true -> Month = Abbr ; Month = Full)
  },
  atom(Month).

%! month(+Year:integer, +Month:between(1,12), +Options:options)// is det.

month(Y, Mo, Options) -->
  month(Mo, Options),
  " ",
  year(Y, Options).

%! month_day(+Day:between(1,31), +Options:options)// is det.

month_day(Da, Options) -->
  (   {dict_get(ltag, Options, nl)}
  ->  integer(Da)
  ;   ordinal(Da, Options)
  ).

%! ordinal(+N:nonneg, +Options:options)// is det.

ordinal(N, Options) -->
  {
    dict_get(ltag, Options, en, LTag),
    ordinal_suffix(N, LTag, Suffix)
  },
  integer(N),
  atom(Suffix).

%! second(+Second:float, +Options:options)// is det.

second(S0, _) -->
  {S is floor(S0)},
  padding_zero(S),
  integer(S).

%! sign(+N:number)// is det.

sign(N) -->
  {N < 0}, !,
  "-".
sign(_) -->
  "+".

%! time(+Hour:between(0,24),
%!      +Minute:between(0,59),
%!      +Second:float,
%!      +Options:options)// is det.

time(H, Mi, S, Options) -->
  hour(H, Options),
  ":",
  minute(Mi, Options),
  ":",
  second(S, Options).

%! timezone_offset(+Offset:between(-840,840))// is det.

timezone_offset(Off) -->
  {Off =:= 0}, !,
  "Z".
timezone_offset(Off) -->
  sign(Off),
  {H is Off // 60},
  generate_as_digits(H, 2),
  ":",
  {Mi is Off mod 60},
  generate_as_digits(Mi, 2).

%! year(+Year:integer, +Options:options)// is det.

year(Y, _) -->
  integer(Y).

%! yearless_date(+Month:between(1,12), +Day:between(1,31), +Options:options)// is det.

yearless_date(Mo, Da, Options) -->
  month(Mo, Options),
  month_day(Da, Options).





% HELPERS %

%! generate_as_digits(+N:nonneg, +NumberOfDigits:nonneg)// is det.
%! generate_as_digits(+N:nonneg, +Base:positive_integer, +NumberOfDigits:nonneg)// is det.
%
% Generate the non-negative integer N using exactly NumberOfDigits
% digits, using `0' as padding if needed.

generate_as_digits(N, M) -->
  generate_as_digits(N, 10, M).


generate_as_digits(_, _, 0) --> !, "".
generate_as_digits(N1, Base, M1) -->
  {M2 is M1 - 1},
  {D is N1 // Base ^ M2},
  digit_weight(D),
  {N2 is N1 mod Base ^ M2},
  generate_as_digits(N2, Base, M2).



%! padding_zero(+N:between(0,9))// is det.

padding_zero(N) -->
  {N =< 9}, !,
  "0".
padding_zero(_) -->
  "".
