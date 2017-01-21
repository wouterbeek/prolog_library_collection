:- module(
  nlp_ext,
  [
    month_name/4,    % ?Mo, ?LTag, ?Abbr, ?Full
    ordinal_suffix/3 % +N, +LTag, -Suffix
  ]
).

/** <module> Natural Language Processing

Simple resources for NLP.

@author Wouter Beek
@version 2017/01
*/





%! month_name(?Mo, ?LTag, ?Abbr, ?Full) is nondet.

month_name(1,  en, "Jan", "January").
month_name(2,  en, "Feb", "February").
month_name(3,  en, "Mar", "March").
month_name(4,  en, "Apr", "April").
month_name(5,  en, "May", "May").
month_name(6,  en, "Jun", "June").
month_name(7,  en, "Jul", "July").
month_name(8,  en, "Aug", "Augustus").
month_name(9,  en, "Sep", "September").
month_name(10, en, "Oct", "October").
month_name(11, en, "Nov", "November").
month_name(12, en, "Dec", "December").
month_name(1,  nl, "jan", "januari").
month_name(2,  nl, "feb", "februari").
month_name(3,  nl, "mrt", "maart").
month_name(4,  nl, "apr", "april").
month_name(5,  nl, "mei", "mei").
month_name(6,  nl, "jun", "juni").
month_name(7,  nl, "jul", "juli").
month_name(8,  nl, "aug", "augustus").
month_name(9,  nl, "sep", "september").
month_name(10, nl, "okt", "oktober").
month_name(11, nl, "nov", "november").
month_name(12, nl, "dec", "december").



%! ordinal_suffix(+N, +LTag, -W) is det.

ordinal_suffix(1, en, "st").
ordinal_suffix(2, en, "nd").
ordinal_suffix(3, en, "rd").
ordinal_suffix(_, en, "th").
