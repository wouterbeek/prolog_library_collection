%! date_time(+DT)// .

date_time(date_time(Y,Mo,D,H,Mi,S,TZ)) -->
  date(Y, Mo, D),
  " ",
  time(H, Mi, S),
  timezone(TZ).
