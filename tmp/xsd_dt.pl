:- module(
  xsd_dt,
  [
    daysInMonth/3,        % +Year, +Month, -Days
    timeOnTimeline/2,     % +DT, -Seconds
    xsd_date_time_to_dt/3 % +DateTime, +D, -DT
  ]
).

/** <module> XSD date/time

@author Wouter Beek
@version 2017/08-2017/11
*/

:- use_module(library(aggregate)).
:- use_module(library(arithmetic)).
:- use_module(library(default)).

:- arithmetic_function(xsd_div/2).

:- op(400, yfx, xsd_div).

% xsd_div(+M, +N, -Z) is det.
%
% # Definition
%
% If `M` and `N` are numbers, then `M div N` is the greatest integer
% less than or equal to `M / N`.

xsd_div(X, Y, Z):-
  Z is floor(X rdiv Y).





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
  defval(Mo1, 12, Mo2),
  % Let ‘da’ be daysInMonth(yr+1,mo)-1 or (dt's day)-1, similarly.
  Y3 is Y2 + 1,
  (   var(D1)
  ->  daysInMonth(Y3, Mo2, D3),
      D2 is D3 - 1
  ;   D2 is D1 - 1
  ),
  % Let ‘hr’ be 0 or (dt's hour), similarly.
  defval(0, H),
  % Let ‘mi’ be 0 or (dt's minute), similarly.
  defval(0, Mi1),
  % Let ‘se’ be 0 or (dt's second), similarly.
  defval(0, S),
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



%! xsd_date_time_to_dt(+DateTime:compound, +D:atom, -DT:compound) is det.

% xsd:date
xsd_date_time_to_dt(date(Y,Mo,D), xsd:date, dt(Y,Mo,D,_,_,_,0)).
% xsd:dateTime
xsd_date_time_to_dt(date_time(Y,Mo,D,H,Mi,S), xsd:dateTime, dt(Y,Mo,D,H,Mi,S,0)).
xsd_date_time_to_dt(date_time(Y,Mo,D,H,Mi,S,TZ), xsd:dateTime, dt(Y,Mo,D,H,Mi,S,TZ)).
% xsd:gDay
xsd_date_time_to_dt(D, xsd:gDay, dt(_,_,D,_,_,_,0)).
% xsd:gMonth
xsd_date_time_to_dt(Mo, xsd:gMonth, dt(_,Mo,_,_,_,_,0)).
% xsd:gMonthDay
xsd_date_time_to_dt(month_day(Mo,D), xsd:gMonthDay, dt(_,Mo,D,_,_,_,0)).
% xsd:gYear
xsd_date_time_to_dt(Y, xsd:gYear, dt(Y,_,_,_,_,_,0)).
% xsd:gYearMonth
xsd_date_time_to_dt(year_month(Y,Mo), xsd:gYearMonth, dt(Y,Mo,_,_,_,_,0)).
% xsd:time
xsd_date_time_to_dt(time(H,Mi,S), xsd:time, dt(_,_,_,H,Mi,S,0)).





% HELPERS %

%! defval(?FromValue, +DefaultValue, -ToValue) is det.
%
% Returns the given value, unless the given value is a variable.  In
% the latter case, the default value is returned instead.
%
% @note We are sometimes using this predicate instead of defval/2 from
%       `library(default)' because we do not want variable occurrences
%       of `FromValue' to get instantiated.

defval(X, Y, Y):-
  var(X), !.
defval(X, _, X).
