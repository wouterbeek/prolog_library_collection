:- module(
  rfc3339,
  [
    'date-fullyear'//1, % ?Year:between(0,9999)
    'date-mday'//1, % ?Day:between(1,31)
    'date-month'//1, % ?Month:between(1,12)
    'date-time'//7, % ?Year:between(0,9999)
                    % ?Month:between(1,12)
                    % ?Day:between(1,31)
                    % ?Hour:between(0,23)
                    % ?Minute:between(0,59)
                    % ?Second:rational
                    % ?Offset:between(-86340,86340)
    'full-date'//3, % ?Year:between(0,9999)
                    % ?Month:between(1,12)
                    % ?Day:between(1,31)
    'full-time'//4, % ?Hour:between(0,23)
                    % ?Minute:between(0,59)
                    % ?Second:rational
                    % ?Offset:between(-86340,86340)
    'partial-time'//3, % ?Hour:between(0,23)
                       % ?Minute:between(0,59)
                       % ?Second:rational
    'time-hour'//1, % ?Hour:between(0,23)
    'time-minute'//1, % ?Minute:between(0,59)
    'time-numoffset'//1, % ?Offset:between(-86340,86340)
    'time-offset'//1, % ?Offset:between(-86340,86340)
    'time-second'//1 % ?Second:between(0,99)
  ]
).

/** <module> RFC 3339: Date and Time on the Internet: Timestamps

@author Wouter Beek
@compat RFC 3339
@see https://tools.ietf.org/html/rfc3339
@version 2015/07, 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(plunit)).





%! 'date-fullyear'(?Year:between(0,9999))// is det.
% ```abnf
% date-fullyear = 4DIGIT
% ```

'date-fullyear'(Y) --> #(4, digit, Ds), {pos_sum(Ds, Y)}.

:- begin_tests('date-fullyear').

test('date-fullyear', [forall('date-fullyear'(S,Y)),nondet]):-
  string_phrase('date-fullyear'(Y0), S), Y0 = Y.

'date-fullyear'("2015", 2015).
'date-fullyear'("0015", 15).

:- end_tests('date-fullyear').



%! 'date-mday'(?Day:between(1,31))// is det.
% ```abnf
% date-mday = 2DIGIT   ; 01-28, 01-29, 01-30, 01-31 based on month/year
% ```
%
% @bug The comment is stricter than the grammar rule.

'date-mday'(D) --> #(2, digit, Ds), {pos_sum(Ds, D), between(1, 31, D)}.



%! 'date-month'(?Month:between(1,12))// is det.
% ```abnf
% date-month = 2DIGIT   ; 01-12
% ```
%
% @bug The comment is stricter than the grammar rule.

'date-month'(Mo) --> #(2, digit, Ds), {pos_sum(Ds, Mo), between(1, 12, Mo)}.



%! 'date-time'(
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(1,31),
%!   ?Hour:between(0,23),
%!   ?Minute:between(0,59),
%!   ?Second:rational,
%!   ?Offset:between(-86340,86340)
%! )// is det.
% ```abnf
% date-time = full-date "T" full-time
% ```

'date-time'(Y, Mo, D, H, Mi, S, Offset) -->
  'full-date'(Y, Mo, D), "T", 'full-time'(H, Mi, S, Offset).



%! 'full-date'(
%!   ?Year:between(0,9999),
%!   ?Month:between(1,12),
%!   ?Day:between(1,31)
%! )// is det.
% ```abnf
% full-date = date-fullyear "-" date-month "-" date-mday
% ```

'full-date'(Y, Mo, D) -->
  'date-fullyear'(Y), "-", 'date-month'(Mo), "-", 'date-mday'(D).



%! 'full-time'(
%!   ?Hour:between(0,23),
%!   ?Minute:between(0,59),
%!   ?Second:rational,
%!   ?Offset:between(-86340,86340)
%! )// is det.
% ```abnf
% full-time = partial-time time-offset
% ```

'full-time'(H, Mi, S, Offset) -->
  'partial-time'(H, Mi, S),
  'time-offset'(Offset).



%! 'partial-time'(
%!    ?Hour:between(0,23),
%!    ?Minute:between(0,59),
%!    ?Second:rational
%! )// is det.
% ```abnf
% partial-time = time-hour ":" time-minute ":" time-second [time-secfrac]
% ```

'partial-time'(H, Mi, S) -->
  'time-hour'(H), ":", 'time-minute'(Mi), ":", 'time-second'(S1),
  def('time-secfrac', S2, 0),
  {S is S1 + S2}.



%! 'time-hour'(?Hour:between(0,23))// is det.
% ```abnf
% time-hour = 2DIGIT   ; 00-23
% ```

'time-hour'(H) --> #(2, digit, Ds), {pos_sum(Ds, H), between(0, 23, H)}.



%! 'time-minute'(?Minute:between(0,59))// is det.
% ```abnf
% time-minute = 2DIGIT   ; 00-59
% ```

'time-minute'(Mi) --> #(2, digit, Ds), {pos_sum(Ds, Mi), between(0, 59, Mi)}.



%! 'time-numoffset'(?Offset:between(-86340,86340))// is det.
% ```abnf
% time-numoffset = ("+" / "-") time-hour ":" time-minute
% ```

'time-numoffset'(Offset) -->
  ("+" -> {Sg = 1} ; "-" -> {Sg = -1}),
  'time-hour'(H), ":", 'time-minute'(Mi),
  {Offset is Sg * (H * 60 + Mi) * 60}.



%! 'time-offset'(?Offset:between(-86340,86340))// is det.
% ```abnf
% time-offset = "Z" / time-numoffset
% ```

'time-offset'(0) --> "Z", !.
'time-offset'(Offset) --> 'time-numoffset'(Offset).



%! 'time-secfrac'(?Fraction:rational)// is det.
% ```abnf
% time-secfrac = "." 1*DIGIT
% ```

'time-secfrac'(SFrac) --> ".", +(digit, Ds), {pos_frac(Ds, SFrac)}.



%! 'time-second'(?Second:between(0,60))// is det.
% ```abnf
% time-second = 2DIGIT   ; 00-58, 00-59, 00-60 based on leap second rules
% ```
%
% @bug The comment is sticter than the grammar rule.

'time-second'(S) --> '#'(2, digit, Ds), {pos_sum(Ds, S), between(0, 60, S)}.
