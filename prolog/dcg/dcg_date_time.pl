:- module(
  dcg_date_time,
  [
    date//3,      % +Y, +Mo, +D
    date_time//1, % +DT
    time//3       % +H, +Mi, +S
  ]
).

/** <module> DCG date time

Parse/generate date/time strings.

@author Wouter Beek
@version 2017/01, 2017/04-2017/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(nlp/nlp_ext)).





%! date(+Y, +Mo, +D)// .

date(Y, Mo, D) -->
  month_day(D),
  " ",
  month(Y, Mo).



%! date_time(+DT)// .

date_time(date_time(Y,Mo,D,H,Mi,S,TZ)) -->
  date(Y, Mo, D),
  " ",
  time(H, Mi, S),
  timezone(TZ).



%! hour(+H)// .

hour(H) -->
  padding_zero(H),
  integer(H).



%! minute(+Mi)// .

minute(Mi) -->
  padding_zero(Mi),
  integer(Mi).



%! month(+Mo)// .

month(Mo) -->
  {once(month_name(Mo, en, Abbr, _))},
  atom(Abbr).


%! month(+Y, +Mo)// .

month(Y, Mo) -->
  month(Mo),
  " ",
  year(Y).



%! month_day(+D)// .

month_day(D) -->
  ordinal(D).



%! second(+S)// .

second(S0) -->
  {S is floor(S0)},
  padding_zero(S),
  integer(S).



%! time(+H, +Mi, +S)// .

time(H, Mi, S) -->
  hour(H),
  ":",
  minute(Mi),
  ":",
  second(S).



%! timezone(+Off)// .

timezone(0) --> !,
  "Z".
timezone(Off) -->
  sign(Off),
  {H is Off // 60},
  generate_as_digits(H, 2),
  ":",
  {Mi is Off mod 60},
  generate_as_digits(Mi, 2).



%! year(+Y)// .

year(Y) -->
  integer(Y).





% HELPERS %

%! padding_zero(+N)// .

padding_zero(N) -->
  {N =< 9}, !,
  "0".
padding_zero(_) --> "".



%! sign(+Sg)// .

sign(Sg) -->
  {Sg < 0}, !,
  "-".
sign(_) -->
  "+".
