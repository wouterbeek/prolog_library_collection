:- module(
  dcg_print,
  [
    publication_print//3 % ?Lang:atom
                         % ?Number:integer
                         % ?Changes:boolean
  ]
).

/** <module> DCG_PRINT

DCGs for the print of a publication.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_ordinal)).
:- use_module(dcg(dcg_year)).
:- use_module(logic(boolean_logic)).
:- use_module(library(dcg/basics)).



adj(Lang, Print, _B) -->
  ordinal(Lang, Print).
adj(Lang, _Print, B) -->
  adj0(Lang, B).
% Example: "naar het Latijn herziene".
adj(Lang, Print, B) -->
  pp(Lang, Print, B1),
  blank,
  adj0(Lang, B2),
  {boolean_or(B1, B2, B)}.

adj0(nl, true) --> "bewerkte".
adj0(nl, true) --> "bijgewerkte".
% @tbd This could be relevant to assert as well.
adj0(nl, fail) --> "fotomechanische".
adj0(nl, _) --> "geheel".
% @tbd This is a relevant aspect that should be asserted!
adj0(nl, fail) --> "goedkoope".
% Abbreviation of 'gewijzigde'.
adj0(nl, true) --> "gew.".
adj0(nl, true) --> "gewijzigde".
% Abbreviation of 'herziene'.
adj0(nl, true) --> "herz.".
adj0(nl, true) --> "herziene".
adj0(nl, _) --> "ingrijpend".
% Meaningless adjective.
adj0(nl, fail) --> "nieuwe".
% Abbreviation for 'omgewerkte'?
adj0(nl, true) --> "omgew.".
adj0(nl, true) --> "omgewerkte".
adj0(nl, fail) --> "onveranderde".
% Strange word.
adj0(nl, fail) --> "veelveranderde".
% Abbreviated version of 'verbeterde'.
adj0(nl, true) --> "verb.".
adj0(nl, true) --> "verbeterde".
% Abbreviation for 'vermeerderde'.
adj0(nl, fail) --> "verm.".
adj0(nl, fail) --> "vermeerderde".
% Probably a typo.
adj0(nl, fail) --> "vermeerde".
% Meaningless adjective.
adj0(nl, fail) --> "volledige".

adjs(_Lang, _Print, _Boolean) --> "".
% Consecutive adjectives.
adjs(Lang, Print, B) -->
  adj(Lang, Print, B1), blank,
  adjs(Lang, Print, B2),
  {boolean_or(B1, B2, B)}.
% Conjunction of adjectives.
adjs(Lang, Print, B) -->
  adj(Lang, Print, B1), blanks,
  conj(Lang), blank,
  adjs(Lang, Print, B2),
  {boolean_or(B1, B2, B)}.

commercial_edition(Lang) -->
  opening_round_bracket,
  commercial_edition(Lang),
  closing_round_bracket.
commercial_edition(nl) --> "handelseditie".

det(en) --> "the".
det(nl) --> "het".

facsimile(nl) --> "facsimile".

n(de) --> "Auflage".
n(nl) --> "druk".
n(nl) --> "herdruk".
n(nl) --> "uitgave".
n(Lang) --> language(Lang).
n(Lang) --> year(Lang, _Year).

np(Lang, Print, B) -->
  (det(Lang), blank ; ""),
  adjs(Lang, Print, B1), blanks,
  n(Lang),
  (
    blank, pp(Lang, Print, B2),
    {boolean_or(B1, B2, B)}
  ;
    "",
    {B = B1}
  ).

pp(Lang, Print, B) -->
  pre(Lang), blank,
  np(Lang, Print, B).

pre(nl) --> "naar".
pre(nl) --> "van".

publication_print(Lang, Print, Changes) -->
  s(Lang, Print, Changes).

s(Lang, Print, Changes) -->
  opening_round_bracket,
  s(Lang, Print, Changes),
  (uncertainty(Lang) ; ""),
  (blank, commercial_edition(Lang) ; ""),
  closing_round_bracket.
s(Lang, Print, Changes) -->
  (facsimile(Lang), blank ; ""),
  np(Lang, Print, Changes).
% Information for different volumes is sometimes given.
% @tbd This information is currently skipped.
s(Lang, Print, Changes) -->
  volumes(Lang, Print, Changes).
% No number-of-print, only a conjunction of adjectives.
s(Lang, Print, Changes) -->
  np(Lang, Print, Changes).
% No number-of-print informat occurs.
s(_Lang, fail, fail) --> "".

volume(Lang, Print, Changes) -->
  volume_noun(Lang), blank,
  integer(_Volume),
  colon, blank,
  s(Lang, Print, Changes).

volume_noun(en) --> "volume".
volume_noun(nl) --> "deel".

%! volumes(Lang, Print, Changes)//
% @tbd The print number and change boolean are only returned if
%      they are the same for all volumes.

volumes(Lang, Print, Changes) -->
  volume(Lang, Print, Changes).
volumes(Lang, Print, Changes) -->
  volume(Lang, Print1, Changes1), blank,
  forward_slash, blank,
  volume(Lang, Print2, Changes2),
  {if_then_else(
    Print1 == Print2,
    Print = Print1,
    Print = _
  )},
  {if_then_else(
    Changes1 == Changes2,
    Changes = Changes1,
    Changes = _
  )}.

