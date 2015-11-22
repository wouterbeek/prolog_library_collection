:- module(
  rfc2616_date,
  [
    'asctime-date'//1, % ?DateTime:compound
    date1//3, % ?Day:between(0,99)
              % ?Month:between(1,12)
              % ?Year:between(0,9999)
    date2//3, % ?Day:between(0,99)
              % ?Month:between(1,12)
              % ?Year:between(0,9999)
    date3//2, % ?Day:between(0,99)
              % ?Month:between(0,12)
    'delta-seconds'//1, % ?Second:nonneg
    'HTTP-date'//1, % ?DateTime:compound
    month//1, % ?Month:between(1,12)
    'rfc850-date'//1, % ?DateTime:compound
    'rfc1123-date'//1, % ?DateTime:compound
    time//3, % ?Hour:between(0,99)
             % ?Minute:between(0,99)
             % ?Second:between(0,99)
    wkday//1, % ?Weekday:between(1,7)
    weekday//1 % ?Weekday:between(1,7)
  ]
).

/** <module> RFC 2616: Date

@author Wouter Beek
@compat RFC 2616
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/rfc2234_re)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(math/positional)).





%! 'asctime-date'(?DateTime:compound)// .
% ```abnf
% asctime-date = wkday SP date3 SP time SP 4DIGIT
% ```

'asctime-date'(dateTime(_,Mo,D,H,Mi,S,Off)) -->
  wkday(_WKD), !, 'SP',
  date3(D, Mo), 'SP',
  time(H, Mi, S), 'SP',
  '#DIGIT'(4, Off).



%! date1(?Day:between(0,99), ?Month:between(1,12), ?Year:between(0,9999))// .
% ```abnf
% date1 = 2DIGIT SP month SP 4DIGIT
% ```

date1(D, Mo, Y) -->
  '#DIGIT'(2, D), 'SP',
  month(Mo), !, 'SP',
  '#DIGIT'(4, Y).



%! date2(?Day:between(0,99), ?Month:between(1,12), ?Year:between(0,99))// .
% ```abnf
% date2 = 2DIGIT "-" month "-" 2DIGIT
% ```

date2(D, Mo, Y) -->
  '#DIGIT'(2, D), "-",
  month(Mo), !, "-",
  '#DIGIT'(2, Y).



%! date3(?Day:between(0,99), ?Month:between(1,12))// .
% ```abnf
% date3 = month SP ( 2DIGIT | ( SP 1DIGIT ))
% ```

date3(D, Mo) -->
  month(Mo), !,
  'SP',
  ('#DIGIT'(2, D) ; ('SP', 'DIGIT'(D))).



%! 'delta-seconds'(?Second:nonneg)// .
% ```abnf
% delta-seconds  = 1*DIGIT
% ```

'delta-seconds'(N) --> '+DIGIT'(N).



%! 'HTTP-date'(?DateTime:compound)// .
% ```abnf
% HTTP-date = rfc1123-date | rfc850-date | asctime-date
% ```

'HTTP-date'(DT) --> 'rfc1123-date'(DT), !.
'HTTP-date'(DT) --> 'rfc850-date'(DT),  !.
'HTTP-date'(DT) --> 'asctime-date'(DT).



%! month(?Month:between(1,12))// .
% ```abnf
% month = "Jan" | "Feb" | "Mar" | "Apr"
%       | "May" | "Jun" | "Jul" | "Aug"
%       | "Sep" | "Oct" | "Nov" | "Dec"
% ```

month(1) --> "Jan".
month(2) --> "Feb".
month(3) --> "Mar".
month(4) --> "Apr".
month(5) --> "May".
month(6) --> "Jun".
month(7) --> "Jul".
month(8) --> "Aug".
month(9) --> "Sep".
month(10) --> "Oct".
month(11) --> "Nov".
month(12) --> "Dec".



%! 'rfc850-date'(?DateTime:compound)// .
% ```abnf
% rfc850-date = weekday "," SP date2 SP time SP "GMT"
% ```

'rfc850-date'(dateTime(Y,Mo,D,H,Mi,S,_Off)) -->
  weekday(_WKD), ",", 'SP',
  date2(D, Mo, Y), 'SP',
  time(H, Mi, S), 'SP',
  "GMT".



%! 'rfc1123-date'(?DateTime:compound)// .
% ```abnf
% rfc1123-date = wkday "," SP date1 SP time SP "GMT"
% ```

'rfc1123-date'(dateTime(Y,Mo,D,H,Mi,S,_Off)) -->
  wkday(_WKD), !, ",", 'SP',
  date1(D, Mo, Y), 'SP',
  time(H, Mi, S), 'SP',
  "GMT".



%! time(?Hour:between(0,99), ?Minute:between(0,99), ?Second:between(0,99))// .
% ```abnf
% time = 2DIGIT ":" 2DIGIT ":" 2DIGIT
% ```

time(H, Mi, S) --> '#DIGIT'(2, H), ":", '#DIGIT'(2, Mi), ":", '#DIGIT'(2, S).



%! wkday(?Weekday:between(1,7))// .
% ```abnf
% wkday = "Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun"
% ```

wkday(1) --> "Mon".
wkday(2) --> "Tue".
wkday(3) --> "Wed".
wkday(4) --> "Thu".
wkday(5) --> "Fri".
wkday(6) --> "Sat".
wkday(7) --> "Sun".



%! weekday(?Weekday:between(1,7))// .
% ```abnf
% weekday = "Monday" | "Tuesday" | "Wednesday"
%         | "Thursday" | "Friday" | "Saturday" | "Sunday"
% ```

weekday(1) --> "Monday".
weekday(2) --> "Tuesday".
weekday(3) --> "Wednesday".
weekday(4) --> "Thursday".
weekday(5) --> "Friday".
weekday(6) --> "Saturday".
weekday(7) --> "Sunday".
