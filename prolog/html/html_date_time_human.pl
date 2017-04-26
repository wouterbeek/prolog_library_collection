:- module(
  html_date_time_human,
  [
    html_human_date_time//2, % +DT, +Opts
    html_human_today//1      % +Opts
  ]
).

/** <module> Human-readable HTML date/time formats

DCG rules for parsing/generating human-readable HTML5 dates.

@author Wouter Beek
@version 2015/08-2015/12, 2016/02, 2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(date_time/date_time)).
:- use_module(library(dict_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(nlp/nlp_ext)).





%! html_human_date_time(+DT, +Opts)// is det.

html_human_date_time(date_time(Y,Mo,Da,H,Mi,S,Off), Opts) -->
  (   {ground(date(Y,Mo,Da,H,Mi,S,Off))}
  ->  global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Opts)
  ;   {ground(date(Y,Mo,Da,H,Mi,S))}
  ->  floating_date_and_time(Y, Mo, Da, H, Mi, S, Opts)
  ;   {ground(date(Y,Mo,Da))}
  ->  date(Y, Mo, Da, Opts)
  ;   {ground(date(H,Mi,S))}
  ->  time(H, Mi, S, Opts)
  ;   {ground(date(Mo,Da))}
  ->  yearless_date(_, Mo, Da, Opts)
  ;   {ground(date(Y,Mo))}
  ->  month(Y, Mo, Opts)
  ;   {ground(date(Y))}
  ->  year(Y, Opts)
  ;   {ground(date(Off))}
  ->  timezone_offset(Off)
  ).



%! html_human_today(+Opts)// is det.

html_human_today(Opts) -->
  {get_date_time(DT)},
  html_human_date_time(DT, Opts).



%! date(+Y, +Mo, +Da, +Opts)// is det.

date(Y, Mo, Da, Opts) -->
  html([\month_day(Da, Opts)," ",\month(Y, Mo, Opts)]).



%! floating_date_and_time(+Y, +Mo, +Da, +H, +Mi, +S, +Opts)// is det.

floating_date_and_time(Y, Mo, Da, H, Mi, S, Opts) -->
  html([\date(Y, Mo, Da, Opts)," ",\time(H, Mi, S, Opts)]).



%! global_date_and_time(+Y, +Mo, +Da, +H, +Mi, +S, +Off, +Opts)// is det.

global_date_and_time(Y, Mo, Da, H, Mi, S, Off, Opts) -->
  floating_date_and_time(Y, Mo, Da, H, Mi, S, Opts),
  timezone_offset(Off).



%! hour(+H, +Opts)// is det.

hour(H, _) -->
  html(span(class=hour, [\padding_zero(H),H])).



%! minute(+Mi, +Opts)// is det.

minute(Mi, _) -->
  html(span(class=minute, [\padding_zero(Mi),Mi])).



%! month(+Mo, +Opts)// is det.

month(Mo, Opts) -->
  {
    get_dict(ltag, Opts, en, LTag),
    get_dict(month_abbr, Opts, false, IsAbbr),
    once(month_name(Mo, LTag, Abbr, Full)),
    (IsAbbr == true -> Month = Abbr ; Month = Full)
  },
  html(span(class=month, Month)).



%! month(+Y, +Mo, +Opts)// is det.

month(Y, Mo, Opts) -->
  html([\month(Mo, Opts)," ",\year(Y, Opts)]).



%! month_day(+Day, +Opts) is det.

month_day(Da, Opts) -->
  html(span(class='month-day', \month_day_inner(Da, Opts))).

month_day_inner(Da, Opts) -->
  {get_dict(ltag, Opts, nl)}, !,
  html(Da).
month_day_inner(Da, Opts) -->
  ordinal(Da, Opts).



%! ordinal(+N, +Opts)// is det.

ordinal(N, Opts) -->
  {
    get_dict(ltag, Opts, en, LTag),
    ordinal_suffix(N, LTag, Suffix)
  },
  html([N, sup([], Suffix)]).



%! second(+S, +Opts)// is det.

second(S0, _) -->
  {S is floor(S0)},
  html(span(class=second, [\padding_zero(S),S])).



%! sign(+Sg)// is det.

sign(Sg) -->
  {Sg < 0}, !,
  html("-").
sign(_)  -->
  html("+").



%! time(
%!   +Hour:between(0,23),
%!   +Minute:between(0,59),
%!   +Second:float,
%!   +Opts
%! )// is det.

time(H, Mi, S, Opts) -->
  html(
    span(class=time, [
      \hour(H, Opts),
      ":",
      \minute(Mi, Opts),
      ":",
      \second(S, Opts)
    ])
  ).



%! timezone_offset(+Off)// is det.

timezone_offset(Off) -->
  html(span(class='timezone-offset', \timezone_offset_inner(Off))).


timezone_offset_inner(0) --> !,
  html("Z").
timezone_offset_inner(Off) -->
  {
    H is Off // 60,
    dcg_with_output_to(string(H0), generate_as_digits(H, 2)),
    Mi is Off mod 60,
    dcg_with_output_to(string(Mi0), generate_as_digits(Mi, 2))
  },
  html([\sign(Off),H0,":",Mi0]).



%! year(+Y, +Opts)// is det.

year(Y, _) -->
  html(span(class=year, Y)).



%! yearless_date(+Y, +Mo, +Da, +Opts)// is det.

yearless_date(_, Mo, Da, Opts) -->
  html(
    span(class='yearless-date', [
      \month(Mo, Opts),
      \month_day(Da, Opts)
    ])
  ).





% HELPERS %

%! padding_zero(+N:between(0,9))// is det.

padding_zero(N) -->
  {N =< 9}, !,
  html("0").
padding_zero(_) --> [].
