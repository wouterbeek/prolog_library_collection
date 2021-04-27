:- encoding(utf8).
:- module(
  xsd_grammar,
  [
    dayTimeDurationCanonicalMap//1, % +Duration
    dayTimeDurationMap//1,          % -Duration
    decimalLexicalMap//1,           % -Decimal
    decimalCanonicalMap//1,         % +Decimal
    durationCanonicalMap//1,        % +Duration
    durationMap//1                  % -Duration
  ]
).

/** <module> XSD grammar

XSD grammar rules for parsing decimals and durations.

@compat XML Schema 1.1 Part 2 ― Datatypes

*/

:- use_module(library(arithmetic)).

:- use_module(library(abnf)).
:- use_module(library(dcg)).
:- use_module(library(default)).
:- use_module(library(list_ext)).
:- use_module(library(math_ext)).

:- arithmetic_function(xsd_div/2).
:- arithmetic_function(xsd_mod/2).

:- op(400, yfx, xsd_div).
:- op(400, yfx, xsd_mod).

% xsd_div(+M, +N, -Z) is det.
%
% If `M` and `N` are numbers, then `M div N` is the greatest integer
% less than or equal to `M / N`.

xsd_div(X, Y, Z):-
  Z is floor(X rdiv Y).

%! xsd_mod(+M, +N, -X) is det.
%
% If `M` and `N` are numbers, then `M mod N` is `m-n * (m div n)`.

xsd_mod(X, Y, Z):-
  Z is X - Y * (X xsd_div Y).





%! dateCanonicalMap(+DT)// is det.
%
% Maps a date value to a dateLexicalRep//1.
%
% # Arguments
%
% @param Date A complete date value.
%
% # Algorithm
%
% Let D be:
%
%   - yearCanonicalFragmentMap(da's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(da's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(da's day)
%
% Return:
%
%   - D, when da's timezoneOffset is absent
%
%   - D & timezoneCanonicalFragmentMap(da's timezoneOffset), otherwise

dateCanonicalMap(date_time(Y,Mo,D,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! dateLexicalMap(-DT)// is det.
%
% Maps a dateLexicalRep//1 to a date value.
%
% Arguments
%
% @param Date A complete date value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - hyphen-separated:
%
%     - an instance Y of yearFrag//1,
%
%     - an instance M of monthFrag//1, and
%
%     - an instance D of dayFrag//1,
%
%   - and optionally followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), monthFragValue(M),
% dayFragValue(D), absent, absent, absent, tz).

dateLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  ?(timezoneFragValue, Off),
  {newDateTime(Y, Mo, D, _, _, _, Off, DT)}.



%! dateTimeCanonicalMap(+DT)// is det.
%
% Maps a datetime value to a datetimeLexicalRep//1.
%
% # Arguments
%
% @param Date A complete datetime value.
%
% # Algorithm
%
% Let DT be:
%
%   - yearCanonicalFragmentMap(dt's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(dt's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(dt's day) &
%
%   - 'T' &
%
%   - hourCanonicalFragmentMap(dt's hour) &
%
%   - ':' &
%
%   - minuteCanonicalFragmentMap(dt's minute) &
%
%   - ':' &
%
%   - secondCanonicalFragmentMap(dt's second)
%
% Return:
%
%   - DT, when dt's timezoneOffset is absent
%
%   - DT & timezoneCanonicalFragmentMap(dt's timezoneOffset),
%     otherwise

dateTimeCanonicalMap(date_time(Y,Mo,D,H,Mi,S,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  "T",
  hourCanonicalFragmentMap(H),
  ":",
  minuteCanonicalFragmentMap(Mi),
  ":",
  secondCanonicalFragmentMap(S),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! dateTimeLexicalMap(-DT)// is det.
%
% Maps a dateTimeLexicalRep//1 to a dateTime value.
%
% # Arguments
%
% @param Date A complete dateTime value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - substrings that are instances of yearFrag//1, monthFrag//1, and
%     dayFrag//1 (below referred to as Y, Mo, and D respectively)
%
%   - it also contains either:
%
%     - instances of hourFrag//1, minuteFrag//1, and secondFrag//1
%       (Y, MI, and S)
%
%     - or else an instance of endOfDayFrag//1
%
%   - finally, it may optionally contain an instance oftimezoneFrag//1
%     (T)
%
% Let tz be:
%
%   - timezoneFragValue(T), when T is present
%
%   - otherwise absent.
%
% Return:
%
%   - newDateTime(yearFragValue(Y), monthFragValue(MO),
%     dayFragValue(D), 24, 0, 0, tz), when endOfDayFrag//1 is present
%
%   - newDateTime(yearFragValue(Y), monthFragValue(MO),
%     dayFragValue(D), hourFragValue(H), minuteFragValue(MI),
%     secondFragValue(S), tz), otherwise

dateTimeLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  "T",
  (   hourFragValue(H),
      ":",
      minuteFragValue(Mi),
      ":",
      secondFragValue(S)
  ;   endOfDayFrag(Y, Mi, S)
  ), !,
  ?(timezoneFragValue, Off),
  {newDateTime(Y, Mo, D, H, Mi, S, Off, DT)}.



%! dateTimePlusDuration(+Duration, +DT1, -DT2) is det.
%
% Adds a du to a dt value, producing another date/time value.
%
% # Arguments
%
% @param du A duration value.
%
% @param dt A date/time value.
%
% # Result
%
% A date/time value.
%
% # Algorithm
%
% Let:
%
%   - yr be dt's year
%
%   - mo be dt's month
%
%   - da be dt's day
%
%   - hr be dt's hour
%
%   - mi be dt's minute
%
%   - se be dt's second
%
%   - tz be dt's timezoneOffset
%
% Steps:
%
%   - Add du's months to mo
%
%   - normalizeMonth(yr, mo), i.e., carry any over- or underflow,
%     adjust month.
%
%   - Set da to min(da, daysInMonth(yr, mo)), i.e., pin the value if
%     necessary.
%
%   - Add du's seconds to se.
%
%   - normalizeSecond(yr, mo, da, hr, mi, se), i.e., carry over- or
%     underflow of seconds up to minutes, hours, etc.
%
%   - Return newDateTime(yr, mo, da, hr, mi, se, tz)

dateTimePlusDuration(duration(Mo0,S0), date_time(Y1,Mo1,D1,H1,Mi1,S1,Off), DT) :-
  Mo2_ is Mo1 + Mo0,
  normalizeMonth(Y1, Mo2_, Y2, Mo2),
  daysInMonth(Y2, Mo2, DaysInMonth),
  D2 is min(D1, DaysInMonth),
  S2 is S1 + S0,
  normalizeSecond(Y2, Mo2, D2, H1, Mi1, S2, Y3, Mo3, D3, H3, Mi3, S3),
  newDateTime(Y3, Mo3, D3, H3, Mi3, S3, Off, DT).



%! dayCanonicalFragmentMap(+D:between(0,99))// is det.
%
% Maps an integer, presumably the day property of a
% date/timeSevenPropertyModel value, onto a dayFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Day An integer between 1 and 31 inclusive (may be limited
%            further depending on associated year and month).
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(d).

dayCanonicalFragmentMap(D) -->
  {between(1, 31, D)},
  unsTwoDigitCanonicalFragmentMap(D).



%! dayFragValue(-Day:between(1,31))// .
%
% Maps a dayFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the day property of a
% date/timeSevenPropertyModel value.
%
% ```abnf
% [58] dayFrag ::= ('0' [1-9]) | ([12] digit) | ('3' [01])
% ```
%
% # Arguments
%
% @param Day An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(DA).

dayFragValue(Day) -->
  #(2, digit_weight, Ns),
  {
    integer_weights(Day, Ns),
    must_be(between(1, 31), Day)
  }.



%! daysInMonth(?Year:integer, +Month:between(1,12),
%!             -Days:between(28,31)) is det.
%
% Returns the number of the last day of the month for any combination
% of year and month.
%
%
% # Algorithm
%
% Return:
%
%   - 28, when m is 2 and y is not evenly divisible by 4, or is evenly
%         divisible by 100 but not by 400, or is absent
%
%   - 29, when m is 2 and y is evenly divisible by 400, or is evenly
%         divisible by 4 but not by 100
%
%   - 30, when m is 4, 6, 9, or 11
%
%   - 31, otherwise (m is 1, 3, 5, 7, 8, 10, or 12)
%
%
% @compat /XML Schema 1.1: Datatypes/, appendix E.3.2 “Auxiliary
%         Functions”.

% 28 when y is absent.
daysInMonth(Y, 2, 28):-
  var(Y), !.
% 28 when m is 2 and y is not evenly divisible by 4.
daysInMonth(Y, 2, 28):-
  Y rem 4 =\= 0, !.
% 28 when m is 2 and y is evenly divisible by 100 but not by 400.
daysInMonth(Y, 2, 28):-
  Y rem 100 =:= 0,
  Y rem 400 =\= 0, !.
% 29 when m is 2 and y is evenly divisible by 400.
daysInMonth(Y, 2, 29):-
  Y rem 400 =:= 0, !.
% 29 when m is 2 and y is evenly divisible by 4 but not by 100.
daysInMonth(Y, 2, 29):-
  Y rem 4 =:= 0,
  Y rem 100 =\= 0, !.
% 30 when m is 4, 6, 9, or 11.
daysInMonth(_, Month, 30):-
  memberchk(Month, [4,6,9,11]), !.
% 31 otherwise (m is 1, 3, 5, 7, 8, 10, or 12).
daysInMonth(_, _, 31).



%! dayTimeDurationCanonicalMap(+Duration)// is det.
%
% Maps a dayTimeDuration's seconds value to a
% dayTimeDurationLexicalRep//1.  (The months value is necessarily zero
% and is ignored.)  dayTimeDurationCanonicalMap//1 is a restriction of
% durationCanonicalMap//1.
%
% # Algorithm
%
% Let:
%
%   - s be dt's months
%
%   - sgn be '-' if s is negative and the empty string ('') otherwise
%
% Return sgn & 'P' & duYearMonthCanonicalFragmentMap(|s|)
%
% @bug s' should be dt''s seconds rather than months.
%
% @bug duYearMonthCanonicalFragmentMap should be
%      duDayTimeCanonicalFragmentMap.

dayTimeDurationCanonicalMap(duration(0,S)) -->
  ({S < 0} -> "-", {SAbs is abs(S)} ; {SAbs is abs(S)}),
  "P",
  duDayTimeCanonicalFragmentMap(SAbs).



%! dayTimeDurationMap(+Duration)// is det.
%
% Maps the lexical representation into the seconds of a
% dayTimeDuration value.  (A dayTimeDuration's months is always zero.)
% dayTimeDurationMap//1 is a restriction of durationMap//1.
%
% # Arguments
%
% @param Duration A dayTimeDuration value.
%
% # Algorithm
%
% DT necessarily consists of possibly a leading '-', followed by 'P'
% and then an instance D of duDayTimeFrag//1.
%
% Return a dayTimeDuration whose:
%
%   - months value is (necessarily) 0
%
%   - seconds value is
%
%     - -duDayTimeFragmentMap(D), if '-' is present in DT
%
%     - duDayTimeFragmentMap(D), otherwise

dayTimeDurationMap(duration(0,S)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P", duDayTimeFragmentMap(Sabs),
  {S is Sg * Sabs}.



%! decimalCanonicalMap(+Decimal:rational)// is det.
%
% Maps a Decimal to its canonical representation, a
% decimalLexicalRep//1.
%
% # Arguments
%
% @param Decimal A decimal value.
%
% # Algorithm
%
% If `d` is an integer, then return `noDecimalPtCanonicalMap(d)`.
% Otherwise, return `decimalPtCanonicalMap(d)`.

decimalCanonicalMap(N) -->
  {integer(N)}, !,
  noDecimalPtCanonicalMap(N).
decimalCanonicalMap(N) -->
  decimalPtCanonicalMap(N).



%! decimalLexicalMap(-Decimal:rational)// is det.
%
% Maps a decimalLexicalRep//1 onto a decimal value.
%
% # Arguments
%
% @param Decimal A decimal value.
%
% # Algorithm
%
% Let `d` be a decimal value.
%
% Set `d` to:
%
%   - `noDecimalMap(LEX)`, when `LEX` is an instance of
%     noDecimalPtNumeral//1
%
%   - `decimalPtMap(LEX)`, when `LEX` is an instance of
%     decimalPtNumeral//1 Return `d`.

decimalLexicalMap(N) -->
  decimalPtMap(N), !.
decimalLexicalMap(N) -->
  noDecimalMap(N).



%! decimalPtCanonicalMap(+Decimal:rational)// is det.
%
% Maps a decimal number to a decimalPtNumeral//1, its canonical
% representation.
%
% # Arguments
%
% @param Decimal A decimal number.
%
% # Algorithm
%
% Return:
%   - `'-' & unsignedDecimalPtCanonicalMap(-i)`, when `i` is negative
%   - `unsignedDecimalPtCanonicalMap(i)`, otherwise

decimalPtCanonicalMap(N) -->
  {N < 0}, !,
  "-",
  {N0 is abs(N)},
  unsignedDecimalPtCanonicalMap(N0).
decimalPtCanonicalMap(N) -->
  unsignedDecimalPtCanonicalMap(N).



%! decimalPtMap(-Decimal:rational)// is det.
%
% Maps a decimalPtNumeral//1 to its numerical value.
%
% # Arguments
%
% @param Decimal A decimal number.
%
% # Algorithm
%
% `N` necessarily consists of an optional sign (`'+'` or `'-'`) and
% then an instance `U` of unsignedDecimalPtNumeral//1.
%
% Return:
%   - `-unsignedDecimalPtMap(U)`, when `'-'` is present
%   - `unsignedDecimalPtMap(U)`, otherwise

decimalPtMap(N) -->
  ("-" -> {Sg = -1} ; "+" -> {Sg = 1} ; {Sg = 1}),
  unsignedDecimalPtMap(N0),
  {N is copysign(N0, Sg)}.



%! digit(+Integer:between(0,9))// is det.
%
% Maps each integer between 0 and 9 to the corresponding digit.
%
% In the XSD 1.1 specification this mapping is named `digit`, thus
% conflicting with the name of the grammar rule to which it is
% related.
%
% # Arguments
%
% @param Integer Between 0 and 9 inclusive.
%
% # Algorithm:
%
% Return:
%   - `'0'`, when `i = 0`
%   - `'1'`, when `i = 1`
%   - `'2'`, when `i = 2`
%   - etc.



%! digitRemainderSeq(+I:nonneg, -Sequence:list(nonneg)) is det.
%
% Maps each nonnegative integer to a sequence of integers used by
% digitSeq/2 to ultimately create an unsignedNoDecimalPtNumeral//1.
%
% # Arguments
%
% @param I A nonnegative integer.
%
% @param Seq A infinite sequence of nonnegative integers
%
% # Algorithm
%
% Return that sequence s for which
%   - `s_0 = i` and
%   - `s_{j+1} = s_j div 10`
%
% According to this algorithm, Sequence ends in an inifinite number of
% zeros.  This is no problem in Prolog:
%
% ```prolog
% ?- digitRemainderSeq(123, Seq), append([X,Y,Z,A,B,C,D,E,F|_], _, Seq).
% Seq = [123, 12, 1, 0, 0, 0, 0, 0, 0|...],
% X = 123,
% Y = 12,
% Z = 1,
% A = B, B = C, C = D, D = E, E = F, F = 0,
% freeze(_G29912399, xsd_numer_aux: (_G29912399=[0|_G29912477], inflist(0, _G29912477))) .
% ```

digitRemainderSeq(0, L):- !,
  inflist(0, L).
digitRemainderSeq(I1, [I1|T]):-
  I2 is I1 xsd_div 10,
  digitRemainderSeq(I2, T).



%! digitSeq(+I:nonneg, -Seq:list(nonneg)) is det.
%
% Maps each nonnegative integer to a sequence of integers used by
% unsignedNoDecimalPtCanonicalMap//1 to create an
% unsignedNoDecimalPtNumeral//1.
%
% # Arguments
%
% @param I A nonnegative integer.
%
% @param Sequence A sequence of integers where each term is between 0
%        and 9 inclusive.
%
% # Algorithm:
%
% Return that sequence `s` for which `s_j = digitRemainderSeq(i)_j mod
% 10`.

digitSeq(0, L):- !,
  inflist(0, L).
digitSeq(I1, [I|T]):-
  I is I1 xsd_mod 10,
  I2 is I1 xsd_div 10,
  digitSeq(I2, T).



%! digitSequenceValue(+Digits:list(between(0,9)), -Integer:nonneg) is det.
%
% Maps a sequence of digits to the position-weighted sum of the terms
% numerical values.
%
% # Arguments
%
% @param Digits A finite sequence of literals, each term matching
%        `digit//1`.
%
% @param Integer A nonnegative integer.
%
% # Algorithm
%
% Return the sum of `digitValue(S_i) × 10^{length(S)-i}`, where `i`
% runs over the domain of `S`.

digitSequenceValue(Ds, N) :-
  length(Ds, Len),
  aggregate(
    sum(D * 10 ^ (Len - I)),
    nth1(I, Ds, D),
    N
  ).



%! digitValue(-Integer:between(0,9))// is det.
%
% Maps each digit to its numerical value.
%
% # Arguments
%
% @param Integer A nonnegative integer less than ten.
%
% # Algorithm
%
% Return
%   - `0` when `d = '0'`
%   - `1` when `d = '1'`
%   - `2` when `d = '2'`
%   - etc.

digitValue(N) -->
  digit_weight(N).



%! duDayFragmentMap(-D)// is det.
%
% Maps a duDayFrag//1 to an integer, intended as part of the value of
% the seconds property of a duration value.
%
% # Arguments
%
% @param D A nonnegative integer.
%
% # Algorithm
%
% D is necessarily the letter 'D' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duDayFragmentMap(D) -->
  noDecimalMap(D),
  "D".



%! duDayCanonicalFragmentMap(+Day)// is det.
%
% Maps a nonnegative integer, presumably the day normalized value from
% the seconds of a duration value, to a duDayFrag//1, a fragment of a
% duration lexical representation.
%
% # Arguments
%
% @param Day A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(d) & 'D', when d is not zero
%
%   - the empty string (''), when d is zero

duDayCanonicalFragmentMap(0) --> !, [].
duDayCanonicalFragmentMap(D) -->
  unsignedNoDecimalPtCanonicalMap(D),
  "D".



%! duDayTimeCanonicalFragmentMap(+Second:rational)// is det.
%
% Maps a nonnegative decimal number, presumably the absolute value of
% the seconds of a duration value, to a duDayTimeFrag//1, a fragment
% of a duration lexical representation.
%
% # Arguments
%
% @param Second A nonnegative decimal number.
%
% # Algorithm
%
% Let:
%
%   - d is ss div 86400
%
%   - h is (ss mod 86400) div 3600
%
%   - m is (ss mod 3600) div 60
%
%   - s is ss mod 60
%
% Return:
%
%   - duDayCanonicalFragmentMap(d) & duTimeCanonicalFragmentMap(h, m,
%     s), when ss is not zero
%
%   - 'T0S', when ss is zero

duDayTimeCanonicalFragmentMap(0) --> !,
  "T0S".
duDayTimeCanonicalFragmentMap(S0) -->
  {
    % Days
    xsd_div(S0, 86400, D),

    % Hours
    xsd_mod(S0, 86400, H0),
    xsd_div(H0, 3600, H),

    % Minutes
    xsd_mod(S0, 3600, Mi0),
    xsd_div(Mi0, 60, Mi),

    % Seconds
    xsd_mod(S0, 60, S)
  },
  duDayCanonicalFragmentMap(D),
  duTimeCanonicalFragmentMap(H, Mi, S).



%! duDayTimeFragmentMap(-Second:rational)// is det.
%
% Maps a duDayTimeFrag//1 into a decimal number, which is the
% potential value of the seconds property of a duration value.
%
% # Arguments
%
% Second A nonnegative decimal number.
%
% # Algorithm
%
% DT necessarily consists of an instance D of duDayFrag//1 and/or an
% instance T of duTimeFrag//1.
%
% Let:
%
%   - d be duDayFragmentMap(D) (or 0 if D is not present)
%
%   - t be duTimeFragmentMap(T) (or 0 if T is not present)
%
% Return 86400 × d + t

duDayTimeFragmentMap(S) -->
  (   duDayFragmentMap(D0)
  ->  (duTimeFragmentMap(S0) -> "" ; {S0 = 0})
  ;   {D0 = 0},
      duTimeFragmentMap(S0)
  ),
  {S is 86400 * D0 + S0}.



%! duHourCanonicalFragmentMap(+Hour:nonneg)// is det.
%
% Maps a nonnegative integer, presumably the hour normalized value
% from the seconds of a duration value, to a duHourFrag//1, a fragment
% of a duration lexical representation.
%
% # Arguments
%
% @param Hour A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(h) & 'H', when h is not zero
%
%   - the empty string (''), when h is zero

duHourCanonicalFragmentMap(0) --> !, [].
duHourCanonicalFragmentMap(H) -->
  unsignedNoDecimalPtCanonicalMap(H),
  "H".



%! duHourFragmentMap(-H)// is det.
%
% Maps a duHourFrag//1 to an integer, intended as part of the value of
% the seconds property of a duration value.
%
% # Arguments
%
% @param H A nonnegative integer.
%
% # Algorithm
%
%   - D is necessarily the letter 'D' followed by a numeral N.
%
%   - Return noDecimalMap(N).
%
% @bug Swap letter and numeral.
%
% @bug D' should be H'.

duHourFragmentMap(H) -->
  noDecimalMap(H),
  "H".



%! duMinuteCanonicalFragmentMap(+Minute:nonneg)// is det.
%
% Maps a nonnegative integer, presumably the minute normalized value
% from the seconds of a duration value, to a duMinuteFrag//1, a
% fragment of a duration lexical representation.
%
% # Arguments
%
% @param Minute A nonnegative integer.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(m) & 'M', when m is not zero
%
%   - the empty string (''), when m is zero

duMinuteCanonicalFragmentMap(0) --> !, [].
duMinuteCanonicalFragmentMap(M) -->
  unsignedNoDecimalPtCanonicalMap(M),
  "M".



%! duMinuteFragmentMap(-Mi)// is det.
%
% Maps a duMinuteFrag//1 to an integer, intended as part of the value
% of the seconds property of a duration value.
%
% # Arguments
%
% @param Mi A nonnegative integer.
%
% # Algorithm
%
%   - M is necessarily the letter 'M' followed by a numeral N.
%
%   - Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duMinuteFragmentMap(Mi) -->
  noDecimalMap(Mi),
  "M".



%! duMonthFragmentMap(-Mo)// is det.
%
% Maps a duMonthFrag//1 to an integer, intended as part of the value
% of the months property of a duration value.
%
% # Arguments
%
% @param Mo A nonnegative integer.
%
% # Algorithm
%
% M is necessarily the letter 'M' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duMonthFragmentMap(Mo) -->
  noDecimalMap(Mo),
  "M".



%! durationCanonicalMap(+Duration)// is det.
%
% Maps a duration's property values to durationLexicalRep//1 fragments
% and combines the fragments into a complete durationLexicalRep//1.
%
% # Arguments
%
% @param Duration A complete duration value.
%
% # Algorithm
%
% Let:
%
%   - m be v's months
%
%   - s be v's seconds
%
%   - sgn be '-' if m or s is negative and the empty string ('')
%     otherwise
%
% Return:
%
%   - sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|) &
%     duDayTimeCanonicalFragmentMap(|s|), when neither m nor s is zero
%
%   - sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|), when m is not
%     zero but s is
%
%   - sgn & 'P' & duDayTimeCanonicalFragmentMap(|s|), when m is zero

durationCanonicalMap(duration(Mo,S)) -->
  ({(Mo < 0 ; S < 0.0)} -> "-" ; ""),
  "P",
  (   {Mo =:= 0}
  ->  {SAbs is abs(S)},
      duDayTimeCanonicalFragmentMap(SAbs)
  ;   {S =:= 0}
  ->  {MoAbs is abs(Mo)},
      duYearMonthCanonicalFragmentMap(MoAbs)
  ;   {
        MoAbs is abs(Mo),
        SAbs is abs(S)
      },
      duYearMonthCanonicalFragmentMap(MoAbs),
      duDayTimeCanonicalFragmentMap(SAbs)
  ).



%! durationMap(-Duration:compound)// is det.
%
% Separates the durationLexicalRep//1 into the month part and the
% seconds part, then maps them into the months and seconds of the
% duration value.
%
% # Arguments
%
% @param Duration A complete duration value.
%
% # Algorithm
%
% DUR consists of:
%
%   - possibly a leading '-',
%
%   - followed by 'P'
%
%   - and then an instance Y of duYearMonthFrag//1 and/or an instance
%     D of duDayTimeFrag//1
%
% Return a duration whose:
%
%   - months value is:
%
%     - 0, if Y is not present
%
%     - -duYearMonthFragmentMap(Y), if both '-' and Y are present
%
%     - duYearMonthFragmentMap(Y), otherwise
%
%   - seconds value is:
%
%     - 0, if D is not present
%
%     - -duDayTimeFragmentMap(D), if both '-' and D are present
%
%     - duDayTimeFragmentMap(D), otherwise

durationMap(duration(Mo,S)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P",
  (   duYearMonthFragmentMap(MoAbs)
  ->  (duDayTimeFragmentMap(SAbs) -> "" ; {SAbs = 0})
  ;   {MoAbs = 0},
      duDayTimeFragmentMap(SAbs)
  ),
  {
    Mo is copysign(MoAbs, Sg),
    S is copysign(SAbs, Sg)
  }.



%! duSecondCanonicalFragmentMap(+Second:rational)// is det.
%
% Maps a nonnegative decimal number, presumably the second normalized
% value from the seconds of a duration value, to a duSecondFrag//1, a
% fragment of a duration lexical representation.
%
% # Arguments
%
% @param Second A nonnegative decimal number.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(s) & 'S', when s is a non-zero
%     integer
%
%   - unsignedDecimalPtCanonicalMap(s) & 'S', when s is not an integer
%
%   - the empty string (''), when s is zero

duSecondCanonicalFragmentMap(S) -->
  {S =:= 0}, !, [].
duSecondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsignedNoDecimalPtCanonicalMap(S),
  "S".
duSecondCanonicalFragmentMap(S) -->
  unsignedDecimalPtCanonicalMap(S),
  "S".



%! duSecondFragmentMap(-S:rational)// is det.
%
% Maps a duSecondFrag//1 to a decimal number, intended as part of the
% value of the seconds property of a duration value.
%
% # Arguments
%
% @param Second A nonnegative decimal number.
%
% # Algorithm
%
%   - S is necessarily 'S' followed by a numeral N.
%
%   - Return:
%
%     - decimalPtMap(N), when '.' occurs in N
%
%     - noDecimalMap(N), otherwise

duSecondFragmentMap(S) -->
  (decimalPtMap(S) ; noDecimalMap(S)),
  "S".



%! duTimeCanonicalFragmentMap(
%!   +Hour:nonneg,
%!   +Minute:nonneg,
%!   +Second:rational
%! )// is det.
%
% Maps three nonnegative numbers, presumably the hour, minute, and
% second normalized values from a duration's seconds, to a
% duTimeFrag//1, a fragment of a duration lexical representation.
%
% # Arguments
%
% @param Hour A nonnegative integer.
%
% @param Minute A nonnegative integer.
%
% @param Seconds A nonnegative decimal number.
%
% # Algorithm
%
% Return:
%
%   - 'T' & duHourCanonicalFragmentMap(h) &
%     duMinuteCanonicalFragmentMap(m) &
%     duSecondCanonicalFragmentMap(s), when h, m, and s are not all
%     zero
%
%   - the empty string ('') when all arguments are zero

duTimeCanonicalFragmentMap(0, 0, 0) --> !, [].
duTimeCanonicalFragmentMap(H, Mi, S) -->
  "T",
  duHourCanonicalFragmentMap(H),
  duMinuteCanonicalFragmentMap(Mi),
  duSecondCanonicalFragmentMap(S).



%! duTimeFragmentMap(-Seconds:rational)// is det.
%
% Maps a duTimeFrag//1 into a decimal number, intended as part of the
% seconds property of a duration value.
%
% # Arguments
%
% @param Seconds A nonnegative decimal number
%
% # Algorithm
%
% T necessarily consists of an instance H of duHourFrag//1, and/or an
% instance M of duMinuteFrag//1, and/or an instance S of
% duSecondFrag//1.
%
% Let:
%
%   - h be duDayFragmentMap(H) (or 0 if H is not present)
%
%   - m be duMinuteFragmentMap(M) (or 0 if M is not present)
%
%   - s be duSecondFragmentMap(S) (or 0 if S is not present)
%
% Return 3600 × h + 60 × m + s
%
% @tbd duDayFragmentMap should be duHourFragmentMap.

duTimeFragmentMap(S) -->
  "T",
  (   duHourFragmentMap(H0)
  ->  (duMinuteFragmentMap(Mi0) -> "" ; {Mi0 = 0}),
      (duSecondFragmentMap(S0)  -> "" ; {S0  = 0})
  ;   duMinuteFragmentMap(Mi0)
  ->  {H0 = 0},
      (duSecondFragmentMap(S0)  -> "" ; {S0  = 0})
  ;   {H0 = 0, Mi0 = 0},
      duSecondFragmentMap(S0)
  ),
  {S is 3600 * H0 + 60 * Mi0 + S0}.



%! duYearMonthCanonicalFragmentMap(+Mo)// is det.
%
% Maps a nonnegative integer, presumably the absolute value of the
% months of a duration value, to a duYearMonthFrag//1, a fragment of a
% duration lexical representation.
%
% # Arguments
%
% @param Mo A nonnegative integer.
%
% # Algorithm
%
% Let:
%
%   - y be ym div 12
%
%   - m be ym mod 12
%
% Return:
%
%   - unsignedNoDecimalPtCanonicalMap(y) & 'Y' &
%     unsignedNoDecimalPtCanonicalMap(m) & 'M', when neither y nor m
%     is zero
%
%   - unsignedNoDecimalPtCanonicalMap(y) & 'Y', when y is not zero but
%     m is
%
%   - unsignedNoDecimalPtCanonicalMap(m) & 'M', when y is zero

duYearMonthCanonicalFragmentMap(YM) -->
  {
    Y is YM xsd_div 12,
    Mo is YM xsd_mod 12
  },
  (   {Y =:= 0}
  ->  unsignedNoDecimalPtCanonicalMap(Mo),
      "M"
  ;   {Mo =:= 0}
  ->  unsignedNoDecimalPtCanonicalMap(Y),
      "Y"
  ;   unsignedNoDecimalPtCanonicalMap(Y),
      "Y",
      unsignedNoDecimalPtCanonicalMap(Mo),
      "M"
  ).



%! duYearFragmentMap(-Y)// is det.
%
% Maps a duYearFrag//1 to an integer, intended as part of the value of
% the months property of a duration value.
%
% # Arguments
%
% @param Y A nonnegative integer.
%
% # Algorithm
%
% Y is necessarily the letter 'Y' followed by a numeral N.
%
% Return noDecimalMap(N)
%
% @bug Swap letter and numeral.

duYearFragmentMap(Y) -->
  noDecimalMap(Y),
  "Y".



%! duYearMonthFragmentMap(-Month:nonneg)// is det.
%
% Maps a duYearMonthFrag//1 into an integer, intended as part of the
% months property of a duration value.
%
% # Arguments
%
% @param Month A nonnegative integer.
%
% # Algorithm
%
%   - YM necessarily consists of an instance Y of duYearFrag//1 and/or
%     an instance M of duMonthFrag//1.
%
%   - Let:
%
%     - y be duYearFragmentMap(Y) (or 0 if Y is not present)
%
%     - m be duMonthFragmentMap(M) (or 0 if M is not present)
%
%   - Return  12 × y + m .

duYearMonthFragmentMap(Mo) -->
  (   duYearFragmentMap(Y0)
  ->  (duMonthFragmentMap(Mo0) -> "" ; {Mo0 = 0})
  ;   {Y0 = 0},
      duMonthFragmentMap(Mo0)
  ),
  {Mo is 12 * Y0 + Mo0}.



%! endOfDayFrag(-H, -Mi, -S:rational)// is det.
%
% ```ebnf
% endOfDayFrag ::= '24:00:00' ('.' '0'+)?
% ```
%
% @bug Missing from the standard.

endOfDayFrag(24, 0, 0) -->
  "24:00:00",
  ("." -> +("0") ; "").



%! fourDigitCanonicalFragmentMap(+Integer:between(-9999,9999))// is det.
%
% Maps an integer between -10000 and 10000 onto an always-four-digit
% numeral.
%
% # Arguments
%
% @param Integer An integer whose absolute value is less than 10000.
%
% # Algorithm
%
% Return:
%
%   - '-' & unsTwoDigitCanonicalFragmentMap(-i div 100) &
%     unsTwoDigitCanonicalFragmentMap(-i mod 100), when i is negative
%
%   - unsTwoDigitCanonicalFragmentMap(i div 100) &
%     unsTwoDigitCanonicalFragmentMap(i mod 100), otherwise

fourDigitCanonicalFragmentMap(N) -->
  ({N < 0} -> "-", {N0 is -N} ; {N0 = N}),
  {N1 is N0 xsd_div 100},
  unsTwoDigitCanonicalFragmentMap(N1),
  {N2 is N0 xsd_mod 100},
  unsTwoDigitCanonicalFragmentMap(N2).



%! 'FractionDigitRemainderSeq'(+Fraction:rational, -Sequence:list(compound)) is det.
%
% Maps each nonnegative decimal number less than 1 to a sequence of
% decimal numbers used by fractionDigitSeq/2 to ultimately create an
% unsignedNoDecimalPtNumeral//1.
%
% # Arguments:
%
% @param Fraction A nonnegative rational number smaller than 1.
%
% @param Sequence A sequence of nonnegative rational numbers.
%
% # Algorithm
%
% Return that sequence `s` for which
%
%   - `s_0 = f - 10`
%
%   - `s_{j+1} = (s_j mod 1) - 10`

'FractionDigitRemainderSeq'(0, L):- !,
  inflist(0, L).
'FractionDigitRemainderSeq'(F1, [F0|T]):-
  F0 is F1 * 10,
  F2 is F0 xsd_mod 1,
  'FractionDigitRemainderSeq'(F2, T).



%! fractionDigitSeq(+Fraction:rational, -Sequence:list(between(0,9))) is det.
%
% Maps each nonnegative decimal number less than 1 to a sequence of
% integers used by fractionDigitsCanonicalFragmentMap//1 to ultimately
% create an unsignedNoDecimalPtNumeral//1.
%
% # Arguments
%
% @param Fraction A nonnegative rational number smaller than 1.
%
% @param Sequence A sequence of integers where each term is between 0
%        and 9 inclusive.
%
% # Algorithm
%
% For a given fraction `f`, return that sequence `s` for which `s_j =
% FractionDigitRemainderSeq(f)_j div 1`.

fractionDigitSeq(0, L):- !,
  inflist(0, L).
fractionDigitSeq(F1, [F0|T]):-
  F_ is F1 * 10,
  F0 is F_ xsd_div 1,
  F2 is F_ xsd_mod 1,
  fractionDigitSeq(F2, T).



%! fractionDigitsCanonicalFragmentMap(+Fraction:rational)// is det.
%
% Maps each nonnegative decimal number less than 1 to a literal used
% by unsignedDecimalPtCanonicalMap//1 to create an
% unsignedDecimalPtNumeral//1.
%
% # Arguments
%
% @param Fraction A nonnegative rational number smaller than 1.
%
% # Algorithm
%
% For a given fraction `f`, return:
%
%   - `digit(fractionDigitSeq(f)_0)` &
%
%   - ... &
%
%   - `digit(fractionDigitSeq(f)_{lastSignificantDigit(FractionDigitRemainderSeq(f))})`

fractionDigitsCanonicalFragmentMap(Frac) -->
  {
    fractionDigitSeq(Frac, Seq),
    'FractionDigitRemainderSeq'(Frac, RemSeq),
    lastSignificantDigit(RemSeq, Last),
    length(Ds, Last),
    prefix(Ds, Seq)
  },
  '*!'(digit_weight, Ds), !.



%! fractionDigitSequenceValue(+Digits:list(between(0,9)),
%!                            -Fraction:rational) is det.
%
% Maps a sequence of digits to the position-weighted sum of the terms
% numerical values, weighted appropriately for fractional digits.
%
% # Arguments
%
% @param Digits A finite sequence of literals, each term matching digit.
%
% @param Integer A nonnegative integer.
%
% # Algorithm
%
% Return the sum of `digitValue(S_i) * 10^{-i}`, where `i` runs over
% the domain of `S`.

fractionDigitSequenceValue(Ds, F):-
  aggregate(
    % @bug The brackets are needed in the exponent.
    sum(rdiv(D,10^I)),
    nth1(I, Ds, D),
    F
  ).



%! fractionFragValue(-Fraction:rational)// is det.
%
% Maps a fracFrag//1 to the appropriate fractional decimal number.
%
% # Arguments
%
% @param Fraction A nonnegative decimal number.
%
% # Algorithm
%
% The parsed string is necessarily the left-to-right concatenation of
% a finite sequence `S` of literals, each term matching digit//1.
%
% Return fractionDigitSequenceValue(S).

fractionFragValue(Frac) -->
  '*!'(digit_weight, Ds), !,
  {fractionDigitSequenceValue(Ds, Frac)}.



%! gDayCanonicalMap(+DT)// is det.
%
% Maps a gDay value to a gDayLexicalRep//1.
%
% # Arguments
%
% @param Datetime A complete gDay value.
%
% # Algorithm
%
% Return:
%
%   - '---' & dayCanonicalFragmentMap(gD's day), when gD's
%     timezoneOffset is absent
%
%   - '---' & dayCanonicalFragmentMap(gD's day) &
%     timezoneCanonicalFragmentMap(gD's timezoneOffset), otherwise.

gDayCanonicalMap(date_time(_,_,D,_,_,_,Off)) -->
  "---",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gDayLexicalMap(-DT)// is det.
%
% Maps a gDayLexicalRep//1 to a gDay value.
%
% # Arguments
%
% @param DT A complete gDay value.
%
% #Algorithm
%
% LEX necessarily includes an instance D of dayFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(gD, absent, absent, dayFragValue(D), absent,
% absent, absent, tz)
%
% Return newDateTime(absent, absent, dayFragValue(D), absent, absent,
% absent, tz)
%
% @bug First return line is erroneous.

gDayLexicalMap(DT) -->
  "---",
  dayFragValue(D),
  ?(timezoneFragValue, Off),
  {newDateTime(_, _, D, _, _, _, Off, DT)}.



%! gMonthCanonicalMap(+DT)// is det.
%
% Maps a gMonth value to a gMonthLexicalRep//1.
%
% # Arguments
%
% @param DT A complete gMonth value.
%
% # Algorithm
%
% Return:
%
%   - '--' & monthCanonicalFragmentMap(gM's day), when gM's
%     timezoneOffset is absent
%
%   - '--' & monthCanonicalFragmentMap(gM's day) &
%     timezoneCanonicalFragmentMap(gM's timezoneOffset), otherwise.

gMonthCanonicalMap(date_time(_,Mo,_,_,_,_,Off)) -->
  "--",
  monthCanonicalFragmentMap(Mo),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gMonthDayCanonicalMap(+DT)// is det.
%
% Maps a gMonthDay value to a gMonthDayLexicalRep//1.
%
% # Arguments
%
% @param DT A complete gMonthDay value.
%
% # Algorithm
%
% Let MD be:
%
%   - '--' &
%
%   - monthCanonicalFragmentMap(md's month) &
%
%   - '-' &
%
%   - dayCanonicalFragmentMap(md's day)
%
% Return:
%
%   - MD, when md's timezoneOffset is absent
%
%   - MD & timezoneCanonicalFragmentMap(md's timezoneOffset),
%     otherwise

gMonthDayCanonicalMap(date_time(_,Mo,D,_,_,_,Off)) -->
  "--",
  monthCanonicalFragmentMap(Mo),
  "-",
  dayCanonicalFragmentMap(D),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gMonthDayLexicalMap(-DT)// is det.
%
% Maps a gMonthDayLexicalRep//1 to a gMonthDay value.
%
% # Arguments
%
% @param DT A complete gMonthDay value.
%
% # Algorithm
%
% LEX necessarily includes an instance M of monthFrag//1 and an
% instance D of dayFrag//1, hyphen-separated and optionally followed
% by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(absent, monthFragValue(M), dayFragValue(D),
% absent, absent, absent, tz)

gMonthDayLexicalMap(DT) -->
  "--",
  monthFragValue(Mo),
  "-",
  dayFragValue(D),
  ?(timezoneFragValue, Off),
  {newDateTime(_, Mo, D, _, _, _, Off, DT)}.



%! gMonthLexicalMap(-DT)// is det.
%
% Maps a gMonthLexicalRep//1 to a gMonth value.
%
% # Arguments
%
% @param Date A complete gMonth value.
%
% # Algorithm
%
% LEX necessarily includes an instance M of monthFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(absent, monthFragValue(M), absent, absent,
% absent, absent, tz)

gMonthLexicalMap(DT) -->
  "--",
  monthFragValue(Mo),
  ?(timezoneFragValue, Off),
  {newDateTime(_, Mo, _, _, _, _, Off, DT)}.



%! gYearCanonicalMap(+DT)// is det.
%
% Maps a gYear value to a gYearLexicalRep//1.
%
% # Arguments
%
% @param DT A complete gYear value.
%
% # Algorithm
%
% Return:
%
%   - yearCanonicalFragmentMap(gY's year), when gY's timezoneOffset is
%     absent
%
%   - yearCanonicalFragmentMap(gY's year) &
%     timezoneCanonicalFragmentMap(gY's timezoneOffset), otherwise

gYearCanonicalMap(date_time(Y,_,_,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gYearLexicalMap(-DT)// is det.
%
% Maps a gYearLexicalRep//1 to a gYear value.
%
% # Arguments
%
% @param DT A complete gYear value.
%
% # Algorithm
%
% LEX necessarily includes an instance Y of yearFrag//1, optionally
% followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), absent, absent, absent, absent,
% absent, tz)

gYearLexicalMap(DT) -->
  yearFragValue(Y),
  ?(timezoneFragValue, Off),
  {newDateTime(Y, _, _, _, _, _, Off, DT)}.



%! gYearMonthCanonicalMap(+DT)// is det.
%
% Maps a gYearMonth value to a gYearMonthLexicalRep//1.
%
% # Arguments
%
% @param YM A complete gYearMonth value.
%
% # Algorithm
%
% Let YM be
%
%   - yearCanonicalFragmentMap(ym's year) &
%
%   - '-' &
%
%   - monthCanonicalFragmentMap(ym's month)
%
% Return:
%
%   - YM, when ym's timezoneOffset is absent
%
%   - YM & timezoneCanonicalFragmentMap(ym's timezoneOffset),
%     otherwise

gYearMonthCanonicalMap(date_time(Y,Mo,_,_,_,_,Off)) -->
  yearCanonicalFragmentMap(Y),
  "-",
  monthCanonicalFragmentMap(Mo),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! gYearMonthLexicalMap(-DT)// is det.
%
% Maps a gYearMonthLexicalRep//1 to a gYearMonth value.
%
% # Arguments
%
% @param Date A complete gYearMonth value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - an instance Y of yearFrag//1
%
%   - an instance M of monthFrag//1, hyphen-separated and
%
%   - optionally followed by an instance T of timezoneFrag//1.
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return newDateTime(yearFragValue(Y), monthFragValue(M), absent,
% absent, absent, absent, tz)

gYearMonthLexicalMap(DT) -->
  yearFragValue(Y),
  "-",
  monthFragValue(Mo),
  ?(timezoneFragValue, Off),
  {newDateTime(Y, Mo, _, _, _, _, Off, DT)}.



%! hourCanonicalFragmentMap(+H:between(0,99))// is det.
%
% Maps an integer, presumably the hour property of a
% date/timeSevenPropertyModel value, onto a hourFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Hour An integer between 0 and 23 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(h).

hourCanonicalFragmentMap(H) -->
  {between(0, 23, H)},
  unsTwoDigitCanonicalFragmentMap(H).



%! hourFragValue(-Hour:between(0,23))// is det.
%
% Maps a hourFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the hour property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Hour An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(HR).
%
% @bug This is more generic than the grammar, which requires the hour
%      to consist of exactly two digits.

hourFragValue(Hour) -->
  #(2, digit_weight, Ns),
  {
    integer_weights(Hour, Ns),
    must_be(between(0, 23), Hour)
  }.



%! lastSignificantDigit(+Sequence:list(nonneg), -Last:nonneg) is det.
%
% Maps a sequence of nonnegative integers to the index of the last
% non-zero term (when reading from left to right).
%
% This is zero iff the sequence consists of only zeros.  This is a
% non-zero, count-by-1 index into Seq otherwise.
%
% # Arguments
%
% @param Sequence Aa sequence of nonnegative integers.
%
% @param Index A nonnegative integer.
%
% # Algorithm
%
% For a sequence of nonnegative integers `s`, return the smallest
% nonnegative integer `j` such that `s(i)_{j+1} = 0`.

lastSignificantDigit(Seq, J):-
  nth0(J, Seq, N),
  N =:= 0, !.



%! minuteCanonicalFragmentMap(+Mi:between(0,99))// is det.
%
% Maps an integer, presumably the minute property of a
% date/timeSevenPropertyModel value, onto a minuteFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Minute An integer between 0 and 59 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(m).

minuteCanonicalFragmentMap(Mi) -->
  {between(0, 59, Mi)},
  unsTwoDigitCanonicalFragmentMap(Mi).



%! minuteFragValue(-Minute:between(0,59))// is det.
%
% Maps a minuteFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto an integer, presumably the minute
% property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Minute An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(MI).

minuteFragValue(Minute) -->
  #(2, digit_weight, Ns),
  {
    integer_weights(Minute, Ns),
    must_be(between(0, 59), Minute)
  }.



%! monthCanonicalFragmentMap(+Mo:between(0,99))// is det.
%
% Maps an integer, presumably the month property of a
% date/timeSevenPropertyModel value, onto a monthFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Month An integer between 1 and 12 inclusive.
%
% # Algorithm
%
% Return unsTwoDigitCanonicalFragmentMap(m).

monthCanonicalFragmentMap(Mo) -->
  {between(1, 12, Mo)},
  unsTwoDigitCanonicalFragmentMap(Mo).



%! monthFragValue(-Month:between(1,12))// is det.
%
% Maps a monthFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the month property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Month An integer.
%
% # Algorithm
%
% Return unsignedNoDecimalMap(MO).

monthFragValue(Month) -->
  #(2, digit_weight, Ns),
  {
    integer_weights(Month, Ns),
    must_be(between(1, 12), Month)
  }.



%! newDateTime(
%!   ?Y,
%!   ?Mo:between(1,12),
%!   ?D:between(1,31),
%!   ?H:between(0,24),
%!   ?Mi:between(0,59),
%!   ?S:rational,
%!   ?Off:between(-840,840),
%!   -DT
%! ) is det.
%
% Returns an instance of the date/timeSevenPropertyModel with property
% values as specified in the arguments. If an argument is omitted, the
% corresponding property is set to absent.
%
% # Arguments
%
% @param Y An optional integer.
%
% @param Mo An optional integer between 1 and 12 inclusive.
%
% @param D An optional integer between 1 and 31 inclusive.
%
% @param H An optional integer between 0 and 24 inclusive.
%
% @param Mi An optional integer between 0 and 59 inclusive.
%
% @param S An optional decimal number greater than or equal to 0 and
%        less than 60.
%
% @param Off An optional integer between -840 and 840 inclusive.
%
% # Algorithm
%
% Let:
%
%   - dt be an instance of the date/timeSevenPropertyModel.
%
%   - yr be Yr when Yr is not absent, otherwise 1.
%
%   - mo be Mo when Mo is not absent, otherwise 1.
%
%   - da be Da when Da is not absent, otherwise 1.
%
%   - hr be Hr when Hr is not absent, otherwise 0.
%
%   - mi be Mi when Mi is not absent, otherwise 0.
%
%   - se be Se when Se is not absent, otherwise 0.
%
% Steps:
%
%   - normalizeSecond(yr, mo, da, hr, mi, se)
%
%   - Set the year property of dt to absent when Yr is absent,
%     otherwise yr.
%
%   - Set the month property of dt to absent when Mo is absent,
%     otherwise mo.
%
%   - Set the day property of dt to absent when Da is absent,
%     otherwise da.
%
%   - Set the hour property of dt to absent when Hr is absent,
%     otherwise hr.
%
%   - Set the minute property of dt to absent when Mi is absent,
%     otherwise mi.
%
%   - Set the second property of dt to absent when Se is absent,
%     otherwise se.
%
%   - Set the timezoneOffset property of dt to Tz.
%
%   - Return dt.
%
% @tbd Add type checking.

newDateTime(
  Y1, Mo1, D1, H1, Mi1, S1, Off,
  date_time(Y4,Mo4,D4,H4,Mi4,S4,Off)
) :-
  % Set the values that are used for performing the normalization.
  default_value(Y1, 1, Y2),
  default_value(Mo1, 1, Mo2),
  default_value(D1, 1, D2),
  default_value(H1, 0, H2),
  default_value(Mi1, 0, Mi2),
  default_value(S1, 0, S2),
  normalizeSecond(Y2, Mo2, D2, H2, Mi2, S2, Y3, Mo3, D3, H3, Mi3, S3),
  % Variables stay variable.  Non-variables get the normalized value.
  var_or_val(Y1, Y3, Y4),
  var_or_val(Mo1, Mo3, Mo4),
  var_or_val(D1, D3, D4),
  var_or_val(H1, H3, H4),
  var_or_val(Mi1, Mi3, Mi4),
  var_or_val(S1, S3, S4).



%! noDecimalMap(-Integer:integer)// .
%
% Maps an noDecimalPtNumeral//1 to its numerical value.
%
% Arguments
%
% @param Integer An integer.
%
% # Algorithm
%
% `N` necessarily consists of an optional sign (`'+'` or `'-'`) and then
% a literal `U` that matches unsignedNoDecimalPtNumeral//1.
%
% Return:
%   - `-1 × unsignedNoDecimalMap(U)`, when `'-'` is present
%   - `unsignedNoDecimalMap(U)`, otherwise.

noDecimalMap(N) -->
  ("-" -> {Sg = -1} ; "+" -> {Sg = 1} ; {Sg = 1}),
  unsignedNoDecimalMap(N0),
  {N is copysign(N0, Sg)}.



%! noDecimalPtCanonicalMap(+Integer:integer)// is det.
%
% Maps an integer to a noDecimalPtNumeral//1, its canonical
% representation.
%
% # Arguments
%
% @param Integer An integer.
%
% # Algorithm
%
% For a given integer `i`, return:
%
%   - `'-'` & `unsignedNoDecimalPtCanonicalMap(-i)`, when `i` is
%     negative
%
%   - `unsignedNoDecimalPtCanonicalMap(i)`, otherwise.

noDecimalPtCanonicalMap(N) -->
  {N < 0}, !,
  "-",
  {N0 is abs(N)},
  unsignedNoDecimalPtCanonicalMap(N0).
noDecimalPtCanonicalMap(N) -->
  unsignedNoDecimalPtCanonicalMap(N).



%! normalizeDay(+Y, +Mo, +D, -NormY, -NormMo, -NormD) is det.
%
% If month (mo) is out of range, or day (da) is out of range for the
% appropriate month, then adjust values accordingly, otherwise make no
% change.
%
% # Algorithm
%
%  - normalizeMonth(yr, mo)
%
%  - Repeat until da is positive and not greater than daysInMonth(yr,
%    mo):
%
%    - If da exceeds daysInMonth(yr, mo) then:
%
%      - Subtract that limit from da.
%
%      - Add 1 to mo.
%
%      - normalizeMonth(yr, mo)
%
%    - If da is not positive then:
%
%      - Subtract 1 from mo.
%
%      - normalizeMonth(yr, mo)
%
%      - Add the new upper limit from the table to da.
%
% @tbd It is unclear what "from the table to" means.

normalizeDay(Y1, Mo1, D1, Y3, Mo3, D3):-
  normalizeMonth(Y1, Mo1, Y2, Mo2),
  normalizeDay0(Y2, Mo2, D1, Y3, Mo3, D3).


normalizeDay0(Y1, Mo1, D1, Y3, Mo3, D3):-
  daysInMonth(Y1, Mo1, D1_max),
  (   D1 > D1_max
  ->  D2 is D1 - D1_max,
      Mo1_succ is Mo1 + 1,
      normalizeMonth(Y1, Mo1_succ, Y2, Mo2),
      normalizeDay0(Y2, Mo2, D2, Y3, Mo3, D3)
  ;   D1 < 0
  ->  Mo1_pred is Mo1 - 1,
      normalizeMonth(Y1, Mo1_pred, Y2, Mo2),
      daysInMonth(Y2, Mo2, D1_max),
      D2 is D1 + D1_max,
      normalizeDay0(Y2, Mo2, D2, Y3, Mo3, D3)
  ;   Y3 = Y1,
      Mo3 = Mo1,
      D3 = D1
  ).



%! normalizeMinute(
%!   +Y,     +Mo,     +D,     +H,     +Mi,
%!   -NormY, -NormMo, -NormD, -NormH, -NormMi
%! ) is det.
%
% Normalizes minute, hour, month, and year values to values that obey
% the appropriate constraints.
%
% Algorithm:
%
% - Add mi div 60 to hr.
%
% - Set mi to mi mod 60.
%
% - Add hr div 24 to da.
%
% - Set hr to  hr mod 24.
%
% - normalizeDay(yr, mo, da)

normalizeMinute(Y1, Mo1, D1, H1, Mi1, Y2, Mo2, D2, H2, Mi2):-
  H1a is H1 + Mi1 xsd_div 60,
  Mi2 is      Mi1 xsd_mod 60,
  D1a is D1 + H1a xsd_div 24,
  H2  is      H1a xsd_mod 24,
  normalizeDay(Y1, Mo1, D1a, Y2, Mo2, D2).



%! normalizeMonth(+Y, +Mo, -NormY, -NormMo) is det.
%
% If month (mo) is out of range, adjust month and year (yr)
% accordingly; otherwise, make no change.
%
% # Algorithm
%
%   - Add (mo - 1) div 12 to yr
%
%   - Set mo to (mo - 1) mod 12 + 1

normalizeMonth(Y1, Mo1, Y2, Mo2):-
  % Add (mo - 1) div 12 to yr.
  Y2 is Y1 + (Mo1 - 1) xsd_div 12,
  % Set mo to (mo - 1) mod 12 + 1.
  Mo2 is (Mo1 - 1) xsd_mod 12 + 1.



%! normalizeSecond(
%!   +Y,     +Mo,     +D,     +H,     +Mi,     +S:rational,
%!   -NormY, -NormMo, -NormD, -NormH, -NormMi, -NormS:rational
%! ) is det.
%
% Normalizes second, minute, hour, month, and year values to values
% that obey the appropriate constraints (ignoring leap seconds).
%
% Algorithm:
%
% - Add se div 60 to mi
%
% - Set se to se mod 60
%
% - normalizeMinute(yr, mo, da, hr, mi)

normalizeSecond(Y1, Mo1, D1, H1, Mi1, S1, Y2, Mo2, D2, H2, Mi2, S2):-
  Mi0 is Mi1 + S1 xsd_div 60,
  S2 is S1 xsd_mod 60,
  normalizeMinute(Y1, Mo1, D1, H1, Mi0, Y2, Mo2, D2, H2, Mi2).





%! scientificCanonicalMap(+Decimal:rational)// is det.
%
% Maps a decimal number to a scientificNotationNumeral//1, its
% canonical representation.
%
% # Arguments
%
% @param Decimal A decimal number.
%
% # Algorithm
%
% Return:
%
%   - `'-'` & `unsignedScientificCanonicalMap(-n)`, when `n` is
%     negative
%
%   - `unsignedScientificCanonicalMap(i)`, otherwise
%
% @tbd `i` should be `n`.

scientificCanonicalMap(N) -->
  {N < 0}, !,
  "-",
  {N0 is abs(N)},
  unsignedScientificCanonicalMap(N0).
scientificCanonicalMap(N) -->
  unsignedScientificCanonicalMap(N).



%! scientificMap(-Decimal:rational)// is det.
%
% Maps a scientificNotationNumeral//1 to its numerical value.
%
% # Arguments
%
% @param Decimal A decimal number.
%
% # Algorithm
%
% `N` necessarily consists of an instance `C` of:
%
%   - either noDecimalPtNumeral//1 or decimalPtNumeral//1,
%
%   - either an `'e'` or an `'E'`, and then
%
%   - an instance `E` of noDecimalPtNumeral//1.
%
% Return:
%
%   - `decimalPtMap(C) × 10 ^ unsignedDecimalPtMap(E)`, when a `'.'`
%     is present in `N`, and
%
%   - `noDecimalMap(C) × 10 ^ unsignedDecimalPtMap(E)`, otherwise.

scientificMap(N) -->
  (decimalPtMap(C) -> "" ; noDecimalMap(C)),
  ("e" -> "" ; "E"),
  noDecimalMap(E),
  {N is C * 10 ^ E}.



%! secondCanonicalFragmentMap(+S:rational)// is det.
%
% Maps a decimal number, presumably the second property of a
% date/timeSevenPropertyModel value, onto a secondFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Second A nonnegative decimal number less than 70.
%
% # Algorithm
%
% Return:
%
%   - unsTwoDigitCanonicalFragmentMap(s), when s is an integer
%
%   - unsTwoDigitCanonicalFragmentMap(s div 1) & '.' &
%     fractionDigitsCanonicalFragmentMap(s mod 1), otherwise

secondCanonicalFragmentMap(S) -->
  {integer(S)}, !,
  unsTwoDigitCanonicalFragmentMap(S).
secondCanonicalFragmentMap(S) -->
  {
    I is S xsd_div 1,
    between(0, 59, I)
  },
  unsTwoDigitCanonicalFragmentMap(I),
  ".",
  {Frac is S xsd_mod 1},
  fractionDigitsCanonicalFragmentMap(Frac).



%! secondFragValue(-S:rational)// is det.
%
% Maps a secondFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto a decimal number, presumably the second
% property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Second A decimal number.
%
% # Algorithm
%
% Return:
%
%   - unsignedNoDecimalMap(SE), when no decimal point occurs in SE
%
%   - unsignedDecimalPtMap(SE)  otherwise

secondFragValue(S) -->
  unsignedDecimalPtMap(S), !,
  {
    0 =< S,
    S < 60
  }.
secondFragValue(S) -->
  unsignedNoDecimalMap(S),
  {between(0, 59, S)}.



%! specialRepCanonicalMap(+SpecialValue:atom)// is det.
%
% Maps the special values used with some numerical datatypes to their
% canonical representations.
%
% # Arguments
%
% @param SpecialValue One of `positiveInfinity`, `negativeInfinity`,
%        and `notANumber`.
%
% # Algorithm
%
% Return:
%
%   - `'INF'`, when `c` is `positiveInfinity`
%
%   - `'-INF'`, when `c` is `negativeInfinity`
%
%   - `'NaN'`, when `c` is `notANumber`

specialRepCanonicalMap(positiveInfinity) --> !, "INF".
specialRepCanonicalMap(negativeInfinity) --> !, "-INF".
specialRepCanonicalMap(notANumber) --> "NaN".



%! specialRepValue(-SpecialValue:atom)// is det.
%
% Maps the lexical representations of special values used with some
% numerical datatypes to those special values.
%
% # Arguments
%
% @param SpecialValue One of `positiveInfinity`, `negativeInfinity`,
%        or `notANumber`.
%
% # Algorithm
%
% Return:
%   - `positiveInfinity`, when `S` is `'INF'` or `'+INF'`
%   - `negativeInfinity`, when `S` is `'-INF'`
%   - `notANumber`, when `S` is `'NaN'`

specialRepValue(positiveInfinity) --> "INF",  !.
specialRepValue(positiveInfinity) --> "+INF", !.
specialRepValue(negativeInfinity) --> "-INF", !.
specialRepValue(notANumber) --> "NaN".



%! timeCanonicalMap(+DT)// is det.
%
% Maps a time value to a timeLexicalRep//1.
%
% # Arguments
%
% @param Time A complete time value.
%
% # Algorithm
%
% Let T be:
%
%   - hourCanonicalFragmentMap(ti's hour) &
%
%   - ':' &
%
%   - minuteCanonicalFragmentMap(ti's minute) &
%
%   - ':' &
%
%   - secondCanonicalFragmentMap(ti's second)
%
% Return:
%
%   - T, when ti's timezoneOffset is absent
%
%   - T & timezoneCanonicalFragmentMap(ti's timezoneOffset), otherwise

timeCanonicalMap(date_time(_,_,_,H,Mi,S,Off)) -->
  hourCanonicalFragmentMap(H),
  ":",
  minuteCanonicalFragmentMap(Mi),
  ":",
  secondCanonicalFragmentMap(S),
  ({var(Off)} -> "" ; timezoneCanonicalFragmentMap(Off)).



%! timeLexicalMap(-DT)// is det.
%
% Maps a timeLexicalRep//1 to a time value.
%
% # Arguments
%
% @param DT A complete time value.
%
% # Algorithm
%
% LEX necessarily includes:
%
%   - either:
%
%     - substrings that are instances of hourFrag//1, minuteFrag//1,
%       and secondFrag//1, (H, M, and S), or else
%
%     - an instance of endOfDayFrag//1; and
%
%   - optionally an instance of timezoneFrag//1 (T).
%
% Let tz be timezoneFragValue(T) when T is present, otherwise absent.
%
% Return:
%
%   - newDateTime(absent, absent, absent, 0, 0, 0, tz), when
%     endOfDayFrag//1 is present
%
%   - newDateTime(absent, absent, absent, hourFragValue(H),
%     minuteFragValue(MI), secondFragValue(S), tz), otherwise

timeLexicalMap(DT) -->
  (   hourFragValue(H),
      ":",
      minuteFragValue(Mi),
      ":",
      secondFragValue(S)
  ;   endOfDayFrag(H, Mi, S)
  ), !,
  ?(timezoneFragValue, Off),
  {newDateTime(_, _, _, H, Mi, S, Off, DT)}.



%! timeOnTimeline(+DT:dt, -Seconds:rational) is det.
%
% Maps an XSD-compatible 7-property model date/time representation
% (type `dt`) to the decimal number denoting its position on the time
% line in seconds.
%
%
% # Algorithm
%
% Let:
%
%   - yr be 1971, when dt's year is absent, and dt's year - 1
%     otherwise
%
%   - mo be 12 or dt's month, similarly
%
%   - da be daysInMonth(yr + 1, mo) - 1 or (dt's day) - 1,
%     similarly
%
%   - hr be 0 or dt's hour, similarly
%
%   - mi be 0 or dt's minute, similarly
%
%   - se be 0 or dt's second, similarly
%
% Steps:
%
%   - Subtract timezoneOffset from mi, when timezoneOffset is not
%     absent.
%
%   - (year)
%
%     - Set ToTl to 31536000 × yr
%
%   - (Leap-year Days, month, and day)
%
%     - Add 86400 × (yr div 400 - yr div 100 + yr div 4) to ToTl
%
%     - Add 86400 × Sum_{m < mo} daysInMonth(yr + 1, m) to ToTl
%
%     - Add 86400 × da to ToTl
%
%   - (hour, minute, and second)
%
%     - Add 3600 × hr + 60 × mi + se to ToTl
%
%   - Return ToTl
%
%
% @compat /XML Schema 1.1: Datatypes/, appendix E.3.4 “Time on
%         Timeline”.

timeOnTimeline(dt(Y1,Mo1,D1,H,Mi1,S,Off), ToTl5) :-
  % Let ‘yr’ be 1971 when dt's year is absent, and (dt's year)-1
  % otherwise.
  (var(Y1) -> Y2 = 1971 ; Y2 is Y1 - 1),
  % Let ‘mo’ be 12 or (dt's month), similarly.
  default_value(Mo1, Mo2, 12),
  % Let ‘da’ be daysInMonth(yr+1,mo)-1 or (dt's day)-1, similarly.
  Y3 is Y2 + 1,
  (   var(D1)
  ->  daysInMonth(Y3, Mo2, D3),
      D2 is D3 - 1
  ;   D2 is D1 - 1
  ),
  % Let ‘hr’ be 0 or (dt's hour), similarly.
  default_value(H, 0),
  % Let ‘mi’ be 0 or (dt's minute), similarly.
  default_value(Mi1, 0),
  % Let ‘se’ be 0 or (dt's second), similarly.
  default_value(S, 0),
  % Subtract ‘timezoneOffset’ from ‘mi’ when ‘timezoneOffset’ is not
  % absent.
  (var(Off) -> Mi2 = Mi1 ; Mi2 is Mi1 - Off),
  % Set ToTl to 31536000 × yr.
  ToTl1 is 31536000 * Y2,
  % Leap-year, month, and day.
  % Add 86400 ⨯ (yr div 400 - yr div 100 + yr div 4) to ToTl.
  ToTl2 is ToTl1 + 86400 * ((Y2 xsd_div 400) - (Y2 xsd_div 100) + (Y2 xsd_div 4)),
  % Add 86400 × Sum_{m < mo} daysInMonth(yr+1,m) to ToTl.
  Mo3 is Mo2 - 1,
  aggregate_all(
    sum(D0),
    (
      between(1, Mo3, Mo0),
      daysInMonth(Y3, Mo0, D0)
    ),
    DaysInMonth
  ),
  ToTl3 is ToTl2 + 86400 * DaysInMonth,
  % Add 86400 ⨯ ‘da’ to ToTl.
  ToTl4 is ToTl3 + 86400 * D2,
  % Hour, minute, and second.
  % Add 3600 ⨯ hr + 60 ⨯ mi + se to ToTl.
  ToTl5 is ToTl4 + 3600 * H + 60 * Mi2 + S.



%! timezoneCanonicalFragmentMap(+Off:between(-840,840))// is det.
%
% Maps an integer, presumably the timezoneOffset property of a
% date/timeSevenPropertyModel value, onto a timezoneFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Off An integer between -840 and 840 inclusive.
%
% # Algorithm
%
% Return:
%
%   - 'Z', when t is zero
%
%   - '-' & unsTwoDigitCanonicalFragmentMap(-t div 60) & ':' &
%     unsTwoDigitCanonicalFragmentMap(-t mod 60), when t is negative
%
%   - '+' & unsTwoDigitCanonicalFragmentMap(t div 60) & ':' &
%     unsTwoDigitCanonicalFragmentMap(t mod 60), otherwise

timezoneCanonicalFragmentMap(0) --> !,
  "Z".
timezoneCanonicalFragmentMap(Off) -->
  ({Off < 0} -> "-", {OffAbs is abs(Off)} ; "+", {OffAbs = Off}),
  {H is OffAbs xsd_div 60},
  unsTwoDigitCanonicalFragmentMap(H),
  ":",
  {Mi is OffAbs xsd_mod 60},
  unsTwoDigitCanonicalFragmentMap(Mi).



%! timezoneFragValue(-Off:integer)// is det.
%
% Maps a timezoneFrag//1, part of a date/timeSevenPropertyModel's
% lexical representation, onto an integer, presumably the
% timezoneOffset property of a date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Timezone An integer.
%
% # Algorithm
%
% TZ necessarily consists of either:
%
%    - just 'Z', or
%
%    - a sign ('+' or '-') followed by an instance H of hourFrag//1, a
%      colon, and an instance M of minuteFrag//1.
%
% Return:
%
%   - 0, when TZ is 'Z'
%
%   - -(unsignedDecimalPtMap(H) × 60 + unsignedDecimalPtMap(M)), when
%     the sign is '-'
%
%   - unsignedDecimalPtMap(H) × 60 + unsignedDecimalPtMap(M),
%     otherwise

timezoneFragValue(0) -->
  "Z", !.
timezoneFragValue(Off) -->
  ("-" -> {Sg = -1} ; "+" -> {Sg = 1}),
  hourFragValue(H),
  ":",
  minuteFragValue(Mi),
  {Off is copysign(H * 60 + Mi, Sg)}.



%! unsignedDecimalPtCanonicalMap(+Decimal:rational)// is det.
%
% Maps a nonnegative decimal number to a unsignedDecimalPtNumeral//1,
% its canonical representation.
%
% # Arguments
%
% @param Decimal A nonnegative decimal number.
%
% # Algorithm
%
% Return:
%   - `unsignedNoDecimalPtCanonicalMap(n div 1)` &
%   - `'.'` &
%   - `fractionDigitsCanonicalFragmentMap(n mod 1)`

unsignedDecimalPtCanonicalMap(N) -->
  {N1 is N xsd_div 1},
  unsignedNoDecimalPtCanonicalMap(N1),
  ".",
  {N2 is N xsd_mod 1},
  ({N2 =:= 0} -> "" ; fractionDigitsCanonicalFragmentMap(N2)).



%! unsignedDecimalPtMap(-Decimal:rational)// is det.
%
% Maps an unsignedDecimalPtNumeral//1 to its numerical value.
%
% # Arguments
%
% @param Decimal A nonnegative decimal number.
%
% # Algorithm
%
% `D` necessarily consists of an optional literal `N` matching:
%
%   - unsignedNoDecimalPtNumeral//1
%
%   - a decimal point
%
%   - an optional literal `F` matching fracFrag//1.
%
% Return:
%
%   - `unsignedNoDecimalMap(N)`, when `F` is not present.
%
%   - `fractionFragValue(F)`, when `N` is not present.
%
%   - `unsignedNoDecimalMap(N) + fractionFragValue(F)`, otherwise.

unsignedDecimalPtMap(N) -->
  unsignedNoDecimalMap(I), !,
  ".",
  (fractionFragValue(F) -> {N is I + F} ; {N is I}).
unsignedDecimalPtMap(N) -->
  ".",
  fractionFragValue(N).



%! unsignedNoDecimalMap(-Integer:nonneg)// is det.
%
% The parser for unsignedNoDecimalPtNumeral//1
%
% Maps an unsignedNoDecimalPtNumeral//1 to its numerical value.
%
% # Arguments
%
% @param Integer A nonnegative integer.
%
% # Algorithm
%
% `N` is the left-to-right concatenation of a finite sequence `S` of
% literals, each term matching digit//1.
%
% Return `digitSequenceValue(S)`.

unsignedNoDecimalMap(N) -->
  '*!'(digit_weight, Ds), !,
  {digitSequenceValue(Ds, N)}.



%! unsignedNoDecimalPtCanonicalMap(+Integer:nonneg)// is det.
%
% Maps a nonnegative integer to a unsignedNoDecimalPtNumeral//1,
% its canonical representation.
%
% # Arguments
%
% @param Integer A nonnegative integer.
%
% # Algorithm
%
% Given an integer `i`, return:
%   - `digit(digitSeq(i)_{lastSignificantDigit(digitRemainderSeq(i))})`
%   - & ... &
%   - `digit(digitSeq(i)_0)`
%
% Note that the concatenation is in reverse order.

unsignedNoDecimalPtCanonicalMap(N) -->
  {
    digitRemainderSeq(N, RemainderSeq),
    lastSignificantDigit(RemainderSeq, Last),
    digitSeq(N, Seq),
    % Count-by-1.
    length(Ds0, Last),
    prefix(Ds0, Seq),
    reverse(Ds0, Ds)
  },
  ({Ds == []} -> "0" ; '+!'(digit_weight, Ds), !).



%! unsignedScientificCanonicalMap(+Decimal:rational)// is det.
%
% Maps a nonnegative decimal number to a
% unsignedScientificNotationNumeral//1, its canonical representation.
%
% # Arguments
%
% @param Decimal A nonnegative decimal number.
%
% # Algorithm
%
% Return
%   - `unsignedDecimalPtCanonicalMap(n / 10^{log(n) div 1})` &
%   - `'E'` &
%   - `noDecimalPtCanonicalMap(log(n) div 1)`

unsignedScientificCanonicalMap(N) -->
  {(  N =:= 0
  ->  N1 = 0
  ;   N1 is rationalize(N / 10 ^ (log10(N) xsd_div 1))
  )},
  unsignedDecimalPtCanonicalMap(N1),
  "E",
  {(N =:= 0 -> N2 = 0 ; N2 is rationalize(log10(N) xsd_div 1))},
  noDecimalPtCanonicalMap(N2).



%! unsTwoDigitCanonicalFragmentMap(+Integer:between(0,99))// is det.
%
% Maps a nonnegative integer less than 100 onto an unsigned
% always-two-digit numeral.
%
% # Arguments
%
% @param Integer A nonnegative integer less than 100
%
% # Algorithm
%
% Return digit(i div 10) & digit(i mod 10).

unsTwoDigitCanonicalFragmentMap(N) -->
  {N1 is N xsd_div 10},
  digit_weight(N1),
  {N2 is N xsd_mod 10},
  digit_weight(N2).



%! yearCanonicalFragmentMap(+Y)// is det.
%
% Maps an integer, presumably the year property of a
% date/timeSevenPropertyModel value, onto a yearFrag//1, part of a
% date/timeSevenPropertyModel's lexical representation.
%
% # Arguments
%
% @param Year An integer.
%
% # Algorithm
%
% Return:
%
%   - noDecimalPtCanonicalMap(y), when |y| > 9999
%
%   - fourDigitCanonicalFragmentMap(y), otherwise

yearCanonicalFragmentMap(Y) -->
  {abs(Y) > 9999}, !,
  noDecimalPtCanonicalMap(Y).
yearCanonicalFragmentMap(Y) -->
  fourDigitCanonicalFragmentMap(Y).



%! yearFragValue(-Y)// is det.
%
% Maps a yearFrag//1, part of a date/timeSevenPropertyModel's lexical
% representation, onto an integer, presumably the year property of a
% date/timeSevenPropertyModel value.
%
% # Arguments
%
% @param Year An integer.
%
% # Algorithm
%
% Return noDecimalMap(YR).

yearFragValue(YR) -->
  noDecimalMap(YR).



%! yearMonthDurationCanonicalMap(+Duration)// is det.
%
% Maps a yearMonthDuration's months value to a
% yearMonthDurationLexicalRep//1.  (The seconds value is necessarily
% zero and is ignored.)  yearMonthDurationCanonicalMap//1 is a
% restriction of durationCanonicalMap//1.
%
% # Arguments
%
% @param Duration A complete yearMonthDuration value.
%
% # Algorithm
%
% Let:
%
%   - m be ym's months
%
%   - sgn be '-' if m is negative and the empty string ('') otherwise.
%
% Return sgn & 'P' & duYearMonthCanonicalFragmentMap(|m|)

yearMonthDurationCanonicalMap(duration(Mo,0)) -->
  ({Mo < 0} -> "-", {MoAbs = -Mo} ; {MoAbs = Mo}),
  "P",
  duYearMonthCanonicalFragmentMap(MoAbs).



%! yearMonthDurationMap(-Duration)// is det.
%
% Maps the lexical representation into the months of a
% yearMonthDuration value.  (A yearMonthDuration's seconds is always
% zero.)  yearMonthDurationMap//1 is a restriction of durationMap//1.
%
% # Arguments
%
% @param Duration A complete yearMonthDuration value.
%
% # Algorithm
%
% YM necessarily consists of
%
%   - an optional leading '-'
%
%   - followed by 'P'
%
%   - and then an instance Y of duYearMonthFrag//1
%
% Return a yearMonthDuration whose:
%
%   - months value is:
%
%     - -duYearMonthFragmentMap(Y), if '-' is present in YM
%
%     - duYearMonthFragmentMap(Y), otherwise
%
%   - seconds value is (necessarily) 0

yearMonthDurationMap(duration(Mo,0)) -->
  ("-" -> {Sg = -1} ; {Sg = 1}),
  "P", duYearMonthFragmentMap(Moabs),
  {Mo is Sg * Moabs}.





% HELPERS %

%! var_or_val(+Arg, +Val, -VarOrVal) is det.
%
% Makes sure that a variable Arg returns a fresh variable, and that a
% non-variable Arg returns Val.

var_or_val(Arg, _, _):-
  var(Arg), !.
var_or_val(_, Val, Val).
