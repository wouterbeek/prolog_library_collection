:- module(
  nlp_lang,
  [
    current_lrange/1, % -LRange
    current_ltag/1,   % -LTag
    current_ltag/2,   % +LTags, -LTag
    month_name/4,     % ?Month, ?LTag, ?Abbr, ?Full
    nlp_string/2,     % +Name, -Str
    nlp_string/3,     % +Name, +Args, -Str
    ordinal_suffix/3  % +N, +LTag, -Suffix
  ]
).

/** <module> Natural language support

*/

:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- dynamic
    nlp:nlp_string0/3.

:- setting(
     nlp:lrange,
     list(atom),
     [en],
     "The default langage range."
   ).

:- multifile
    nlp:nlp_string0/3.





%! current_lrange(-LRange) is det.

current_lrange(LRange) :-
  nlp:setting(lrange, LRange).



%! current_ltag(-LTag) is det.
%! current_ltag(+LTags, -LTag) is det.

current_ltag(LTag) :-
  current_lrange(LRange),
  lrange_to_ltag(LRange, LTag).


current_ltag(LTags, LTag) :-
  current_ltag(LTag),
  memberchk(LTag, LTags).



%! lrange_to_ltag(+LRange, -LTag) is nondet.

lrange_to_ltag(LRange, LTag2) :-
  member(LTag1, LRange),
  atomic_list_concat(Subtags, -, LTag1),
  longest_to_shortest_prefix0(PrefixSubtags, Subtags),
  PrefixSubtags \== [],
  atomic_list_concat(PrefixSubtags, -, LTag2).

longest_to_shortest_prefix0(L, L).
longest_to_shortest_prefix0(Prefix, L1) :-
  append(L2, [_], L1),
  longest_to_shortest_prefix0(Prefix, L2).



%! month_name(?Month, ?LTag, ?Abbr, ?Full) is nondet.

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



%! nlp_string(+Name, -String) is det.
%! nlp_string(+Name, +Args, -String) is det.

nlp_string(Name, String) :-
  nlp_string(Name, [], String).


nlp_string(Name, Args, String) :-
  current_ltag(LTag),
  nlp:nlp_string0(LTag, Name, Format),
  format(string(String), Format, Args).



%! ordinal_suffix(+N, +LTag, -W) is det.

ordinal_suffix(1, en, "st").
ordinal_suffix(2, en, "nd").
ordinal_suffix(3, en, "rd").
ordinal_suffix(_, en, "th").
