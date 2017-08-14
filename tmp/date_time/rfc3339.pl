:- module(
  rfc3339,
  [
    'date-fullyear'//1,  % ?Year
    'date-mday'//1,      % ?Day
    'date-month'//1,     % ?Month
    'date-time'//7,      % ?Year, ?Month, ?Day, ?Hour, ?Minute, ?Second, ?Offset
    'full-date'//3,      % ?Year, ?Month, ?Day
    'full-time'//4,      % ?Hour, ?Minute, ?Second, ?Offset
    'partial-time'//3,   % ?Hour, ?Minute, ?Second
    'time-hour'//1,      % ?Hour
    'time-minute'//1,    % ?Minute
    'time-numoffset'//1, % ?Offset
    'time-offset'//1,    % ?Offset
    'time-second'//1     % ?Second
  ]
).

/** <module> RFC 3339: Date and Time on the Internet: Timestamps

@author Wouter Beek
@compat RFC 3339
@see https://tools.ietf.org/html/rfc3339
@version 2015/07, 2015/11, 2017/05, 2017/08
*/

:- use_module(library(clpfd)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(plunit)).





%! 'date-fullyear'(?Year:between(0,9999))// is det.
%
% ```abnf
% date-fullyear = 4DIGIT
% ```

'date-fullyear'(Year) -->
  dcg_integer(#(4, digit), Year).

:- begin_tests('date-fullyear').

test('date-fullyear', [forall('date-fullyear'(S,Y)),nondet]):-
  string_phrase('date-fullyear'(Y0), S), Y0 = Y.

'date-fullyear'("2015", 2015).
'date-fullyear'("0015", 15).

:- end_tests('date-fullyear').



%! 'date-mday'(?Day:between(1,31))// is det.
%
% ```abnf
% date-mday = 2DIGIT   ; 01-28, 01-29, 01-30, 01-31 based on month/year
% ```
%
% @bug The comment is stricter than the grammar rule.

'date-mday'(Day) -->
  dcg_integer(#(2, digit), Day).
  {between(1, 31, Day)}.



%! 'date-month'(?Month:between(1,12))// is det.
%
% ```abnf
% date-month = 2DIGIT   ; 01-12
% ```
%
% @bug The comment is stricter than the grammar rule.

'date-month'(Month) -->
  dcg_integer(#(2, digit), Month),
  {between(1, 12, Month)}.



%! 'date-time'(?Year:between(0,9999), ?Month:between(1,12), ?Day:between(1,31),
%!             ?Hour:between(0,23), ?Minute:between(0,59), ?Second:rational,
%!             ?Offset:between(-86340,86340))// is det.
%
% ```abnf
% date-time = full-date "T" full-time
% ```

'date-time'(Year, Month, Day, Hour, Minute, Second, Offset) -->
  'full-date'(Year, Month, Day),
  "T",
  'full-time'(Hour, Minute, Second, Offset).



%! 'full-date'(?Year:between(0,9999), ?Month:between(1,12),
%!             ?Day:between(1,31))// is det.
%
% ```abnf
% full-date = date-fullyear "-" date-month "-" date-mday
% ```

'full-date'(Year, Month, Day) -->
  'date-fullyear'(Year),
  "-",
  'date-month'(Month),
  "-",
  'date-mday'(Day).



%! 'full-time'(?Hour:between(0,23), ?Minute:between(0,59), ?Second:rational,
%!             ?Offset:between(-86340,86340))// is det.
%
% ```abnf
% full-time = partial-time time-offset
% ```

'full-time'(Hour, Minute, Second, Offset) -->
  'partial-time'(Hour, Minute, Second),
  'time-offset'(Offset).



%! 'partial-time'(?Hour:between(0,23), ?Minute:between(0,59),
%!                ?Second:rational)// is det.
%
% ```abnf
% partial-time = time-hour ":" time-minute ":" time-second [time-secfrac]
% ```

'partial-time'(Hour, Minute, Second) -->
  'time-hour'(Hour),
  ":",
  'time-minute'(Minute),
  ":",
  'time-second'(Second1),
  def('time-secfrac', Second2, 0),
  {Second #= Second1 + Second2}.



%! 'time-hour'(?Hour:between(0,23))// is det.
%
% ```abnf
% time-hour = 2DIGIT   ; 00-23
% ```

'time-hour'(Hour) -->
  dcg_integer(#(2, digit), Hour).
  {between(0, 23, Hour)}.



%! 'time-minute'(?Minute:between(0,59))// is det.
%
% ```abnf
% time-minute = 2DIGIT   ; 00-59
% ```

'time-minute'(Minute) -->
  dcg_integer(#(2, digit), Minute).
  {between(0, 59, Minute)}.



%! 'time-numoffset'(?Offset:between(-86340,86340))// is det.
%
% ```abnf
% time-numoffset = ("+" / "-") time-hour ":" time-minute
% ```

'time-numoffset'(Offset) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  'time-hour'(H), ":", 'time-minute'(Mi),
  {Offset is Sg * (H * 60 + Mi) * 60}.



%! 'time-offset'(?Offset:between(-86340,86340))// is det.
%
% ```abnf
% time-offset = "Z" / time-numoffset
% ```

'time-offset'(0) --> "Z", !.
'time-offset'(Offset) --> 'time-numoffset'(Offset).



%! 'time-secfrac'(?Fraction:rational)// is det.
%
% ```abnf
% time-secfrac = "." 1*DIGIT
% ```

'time-secfrac'(SFrac) -->
  ".",
  +(digit, Weights),
  {fractional_weights(SFrac, Weights)}.



%! 'time-second'(?Second:between(0,60))// is det.
%
% ```abnf
% time-second = 2DIGIT   ; 00-58, 00-59, 00-60 based on leap second rules
% ```
%
% @bug The comment is sticter than the grammar rule.

'time-second'(Second) -->
  dcg_integer(#(2, digit), Second),
  {between(0, 60, Second)}.
