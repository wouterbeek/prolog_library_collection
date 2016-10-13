:- module(
  html_date_time_machine,
  [
    html_machine_date_time/2 % +DT, -MachineString
  ]
).

/** <module> Machine-readable HTML date/time formats

Both human- and machine-readable.

@author Wouter Beek
@compat HTML 5
@see Grammar http://www.w3.org/TR/html5/infrastructure.html#dates-and-times
@see Examples http://www.w3.org/TR/html5/text-level-semantics.html#the-time-element
% @tbd We cannot expect rational representations for seconds yet.
@version 2015/08-2015/12, 2016/02, 2016/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(plunit)).





%! date(+DT)// is det.

date(date_time(Y,Mo,D,H,Mi,S,Off)) -->
  (   {ground(date(Y,Mo,D,H,Mi,S,Off))}
  ->  global_date_and_time(Y, Mo, D, H, Mi, S, Off)
  ;   {ground(date(Y,Mo,D,H,Mi,S))}
  ->  floating_date_and_time(Y, Mo, D, H, Mi, S)
  ;   {ground(date(Y,Mo,D))}
  ->  date(Y, Mo, D)
  ;   {ground(date(H,Mi,S))}
  ->  time(H, Mi, S)
  ;   {ground(date(Mo,D))}
  ->  yearless_date(_, Mo, D)
  ;   {ground(date(Y,Mo))}
  ->  month(Y, Mo)
  ;   {ground(date(Y))}
  ->  year(Y)
  ;   {ground(date(Off))}
  ->  timezone_offset(Off)
  ).



%! date(
%!   +Year:nonneg,
%!   +Month:between(1,12),
%!   +Day:between(1,31)
%! )// is det.
%
% A string is a **valid date string** representing a year, month, and
% day if it consists of the following components in the given order:
%
%   * A valid month string, representing year and month
%
%   * A "-" (U+002D) character
%
%   * Two ASCII digits, representing a day in the range [1,maxday]
%     where maxday is the number of days in the month and year.
%
% @throws type_error If Day is not in range [1,maxday].

date(Y, Mo, D) -->
  month(Y, Mo),
  "-",
  {
    % Check that day is within range for the given year and month.
    number_of_days_in_month_of_year(Y, Mo, MaxD),
    must_be(between(1,MaxD), D)
  },
  generate_as_digits(D, 2).

:- begin_tests(date).

test(date, [forall(date(X,Y,Mo,D)),nondet]):-
  string_phrase(date(Y,Mo,D), X0), X0 = X.

date("2011-11-12", 2011, 11, 12).

:- end_tests(date).



%! floating_date_and_time(
%!   +Year:nonneg,
%!   +Month:between(1,12),
%!   +Day:between(1,31),
%!   +Hour:between(0,23),
%!   +Minute:between(0,59),
%!   +Second:rational
%! )// is det.
%
% A **floating date and time** consists of a specific
% proleptic-Gregorian date, consisting of a year, a month, and a day,
% and a time, consisting of an hour, a minute, a second, and a
% fraction of a second, but expressed without a time zone. [GREGORIAN]
%
% A string is a **valid floating date and time string** representing a
% date and time if it consists of the following components in the
% given order:
%
%   1. A valid date string representing the date
%
%   2. A "T" (U+0054) character or a U+0020 SPACE character
%
%   3. A valid time string representing the time
%
% A string is a **valid normalized floating date and time string**
% representing a date and time if it consists of the following
% components in the given order:
%
%   1. A valid date string representing the date
%
%   2. A "T" (U+0054) character
%
%   3. A valid time string representing the time, expressed as the
%      shortest possible string for the given time (e.g. omitting the
%      seconds component entirely if the given time is zero seconds
%      past the minute)

floating_date_and_time(Y, Mo, D, H, Mi, S) -->
  date(Y, Mo, D),
  "T",
  time(H, Mi, S).

:- begin_tests(floating_date_and_time).

test(
  floating_date_and_time,
  [forall(floating_date_and_time(X,Y,Mo,D,H,Mi,S)),nondet]
):-
  string_phrase(floating_date_and_time(Y,Mo,D,H,Mi,S), X0), X0 = X.

floating_date_and_time("2011-11-12T14:54", 2011, 11, 12, 14, 54, 0).
floating_date_and_time("2011-11-12T14:54:39", 2011, 11, 12, 14, 54, 39).
%floating_date_and_time("2011-11-12T14:54:39.929", 2011, 11, 12, 14, 54, 39929 rdiv 1000).

:- end_tests(floating_date_and_time).



%! global_date_and_time(
%!   +Year:nonneg,
%!   +Month:between(1,12),
%!   +Day:between(1,31),
%!   +Hour:between(0,23),
%!   +Minute:between(0,59),
%!   +Second:rational,
%!   +Offset:between(-840,840)
%! )// is det.
%
% A **global date and time** consists of a specific
% proleptic-Gregorian date, consisting of a year, a month, and a day,
% and a time, consisting of an hour, a minute, a second, and a
% fraction of a second, expressed with a time-zone offset, consisting
% of a signed number of hours and minutes. [GREGORIAN]
%
% A string is a **valid global date and time** string representing a
% date, time, and a time-zone offset if it consists of the following
% components in the given order:
%
%   * A valid date string representing the date
%
%   * A "T" (U+0054) character or a U+0020 SPACE character
%
%   * A valid time string representing the time
%
%   * A valid time-zone offset string representing the time-zone
%     offset

global_date_and_time(Y, Mo, D, H, Mi, S, Off) -->
  date(Y, Mo, D),
  "T",
  time(H, Mi, S),
  timezone_offset(Off).

:- begin_tests(global_date_and_time).

test(
  global_date_and_time,
  [forall(global_date_and_time(X,Y,Mo,D,H,Mi,S,Off)),nondet]
):-
  string_phrase(global_date_and_time(Y,Mo,D,H,Mi,S,Off), X0), X0 = X.

global_date_and_time("2011-11-12T14:54Z", 2011, 11, 12, 14, 54, 0, 0).
global_date_and_time("2011-11-12T14:54:39Z", 2011, 11, 12, 14, 54, 39, _).
%global_date_and_time("2011-11-12T14:54:39.929Z", 2011, 11, 12, 14, 54, 39929 rdiv 1000, 0).
global_date_and_time("2011-11-12T06:54-08:00", 2011, 11, 12, 6, 54, 0, -480).
global_date_and_time("2011-11-12T06:54:39-08:00", 2011, 11, 12, 6, 54, 39, -480).
%global_date_and_time("2011-11-12T06:54:39.929-08:00", 2011, 11, 12, 6, 54, 39929 rdiv 1000, -480).

:- end_tests(global_date_and_time).



%! html_machine_date_time(+DT, -MachineString) is det.

html_machine_date_time(DT, MachineString):-
  once(string_phrase(date(DT), MachineString)).



%! month(+Year:nonneg, +Month:between(1,12))// is det.
%
% A string is a **valid month string** representing a year year and
% month if it consists of the following components in the given order:
%
%   * Four or more ASCII digits, representing year, where year > 0
%
%   * A "-" (U+002D) character
%
%   * Two ASCII digits, representing the month, in the range [1,12].
%
% @throws type_error If Month is not in range [1,12].

month(Y, Mo) -->
  year(Y),
  "-",
  {must_be(between(1,12), Mo)},
  generate_as_digits(Mo, 2).

:- begin_tests(month).

test(month, [forall(month(X,Y,Mo)),nondet]):-
  string_phrase(month(Y,Mo), X0), X0 = X.

month("2011-11", 2011, 11).

:- end_tests(month).



%! time(
%!   +Hour:between(0,23),
%!   +Minute:between(0,59),
%!   +Second:rational
%! )// is det.
%
% A **time** consists of a specific time with no time-zone
% information, consisting of an hour, a minute, a second, and a
% fraction of a second.
%
% A string is a **valid time string** representing an hour, a minute,
% and a second if it consists of the following components in the given
% order:
%
%   * Two ASCII digits, representing hour, in the range [0,23]
%
%   * A ":" (U+003A) character
%
%   * Two ASCII digits, representing minute, in the range [0,59]
%
%   * Optionally (required if second is non-zero):
%
%     * A ":" (U+003A) character
%
%     * Two ASCII digits, representing the integer part of second, in
%       the range [0,59]
%
%     * Optionally (required if second is not an integer):
%
%       * A 002E FULL STOP character (.)
%
%       * One, two, or three ASCII digits, representing the fractional
%         part of second
%
% @throws type_error If Hour is not in range [0,23].
%
% @throws type_error If Minute is not in range [0,59].
%
% @throws type_error If the integer part of Second is not in range
%         [0,59].
%
% @tbd Seconds is currently assumed to be float i.o. rational.

time(H, Mi, S) -->
  {must_be(between(0,23), H)},
  generate_as_digits(H, 2),
  ":",
  {must_be(between(0,59), Mi)},
  generate_as_digits(Mi, 2),
  ({S =:= 0} -> "" ; ":", seconds_non_zero(S)).


seconds_non_zero(S) -->
  {
    decimal_parts(S, SI, SFrac),
    must_be(between(0,59), SI)
  },
  generate_as_digits(SI, 2),
  (   {SFrac =:= 0}
  ->  ""
  ;   ".",
      {
        pos(SFrac, Ds0),
        list_truncate(Ds0, 3, Ds)
      },
      '*'(digit, Ds)
  ).

:- begin_tests(time).

test(time, [forall(time(X,H,Mi,S)),nondet]):-
  string_phrase(time(H,Mi,S), X0), X0 = X.

time("14:54", 14, 54, 0).
time("14:54:39", 14, 54, 39).
%time("14:54:39.929", 14, 54, 39929 rdiv 1000).

:- end_tests(time).



%! timezone_offset(+Offset:between(-840,840))// is det.
%
% A **time-zone offset** consists of a signed number of hours and
% minutes.
%
% A string is a **valid time-zone offset string** representing a
% time-zone offset if it consists of either:
%
%   * A "Z" (U+005A) character, allowed only if the time zone is UTC
%
%   * Or, the following components, in the given order:
%
%     * Either a "+" (U+002B) character or, if the time-zone offset is
%       not zero, a "-" (U+002D) character, representing the sign of
%       the time-zone offset
%
%     * Two ASCII digits, representing the hours component hour of the
%       time-zone offset, in the range [0,23]
%
%     * Optionally, a ":" (U+003A) character
%
%     * Two ASCII digits, representing the minutes component minute of
%       the time-zone offset, in the range [0,59]
%
% @throws type_error If the hour component of Offset is not in range
%         [0,23].
%
% @throws type_error If the minute component of Offset is not in range
%         [0,59].

timezone_offset(0) --> "Z", !.
timezone_offset(Off) -->
  ({Off < 0} -> "-" ; {Off > 0} -> "+"),
  {
    AbsOff is abs(Off),
    H is AbsOff // 60,
    must_be(between(0,23), H)
  },
  generate_as_digits(H, 2),
  ":",
  {
    Mi is AbsOff mod 60,
    must_be(between(0,59), Mi)
  },
  generate_as_digits(Mi, 2).

:- begin_tests(timezone_offset).

test(timezone_offset, [forall(timezone_offset(X,Off)),nondet]):-
  string_phrase(timezone_offset(Off), X0), X0 = X.

timezone_offset("Z", 0).
timezone_offset("-08:00", -480).

:- end_tests(timezone_offset).



%! week(+Year:nonneg, +Week:between(1,53))// is det.
%
% A string is a **valid week string** representing a week-year and
% week if it consists of the following components in the given order:
%
%   1. Four or more ASCII digits, representing year, where year > 0.
%
%   2. A "-" (U+002D) character.
%
%   3. A "W" (U+0057) character.
%
%   4. Two ASCII digits, representing the week, in the range
%      [1,maxweek], where maxweek is the week number of the last day
%      of week-year.
%
% @tbd Implement `maxweek' check.

week(Y, W) -->
  year(Y),
  "-W",
  generate_as_digits(W, 2).



%! yearless_date(
%!   +Year:nonneg,
%!   +Month:between(1,12),
%!   +Day:between(1,31)
%! )// is det.
%
% A **yearless date** consists of a Gregorian month and a day within
% that month, but with no associated year. [GREGORIAN]
%
% A string is a **valid yearless date** string representing a month
% and a day if it consists of the following components in the given
% order:
%
%   * Optionally, two "-" (U+002D) characters
%
%   * Two ASCII digits, representing a month in the range [1,12]
%
%   * A "-" (U+002D) character
%
%   * Two ASCII digits, representing a day in the range [1,maxday]
%     where maxday is the number of days in the month and any
%     arbitrary leap year (e.g. 4 or 2000)
%
% @throws type_check If Month is not in range [1,12].
%
% @throws type_check If Day is not in range [1,maxday].

yearless_date(Y, Mo, D) -->
  {must_be(between(1,12), Mo)},
  generate_as_digits(Mo, 2),
  "-",
  {number_of_days_in_month_of_year(Y, Mo, MaxD), between(1, MaxD, D)},
  generate_as_digits(D, 2).

:- begin_tests(yearless_date).

test(yearless_date, [forall(yearless_date(X,Y,Mo,D)),nondet]):-
  string_phrase(yearless_date(Y,Mo,D), X0), X0 = X.

yearless_date("11-12", _, 11, 12).

:- end_tests(yearless_date).





% HELPERS %

%! number_of_days_in_month_of_year(
%!   ?Year:nonneg,
%!   ?Month:between(1,12),
%!   ?MaxDay:between(28,31)
%! ) is nondet.
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



%! year(+Year:nonneg)// is det.

year(Y) --> {Y > 9999}, !, integer(Y).
year(Y) --> generate_as_digits(Y, 4).
