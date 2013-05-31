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

---+ Sentences

  * "naar het Latijn herziene druk"
  * "fotomechanische herdruk van uitgave 1870"
  * "ongewijzigde herdruk van de 2de, herziene druk"
  * "15de, met zorg herziene druk"
  * "1ste druk voor Meulenhoff"

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(dcg(dcg_ordinal)).
:- use_module(dcg(dcg_volume)).
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
  adj(Lang, Print, B2),
  {boolean_or(B1, B2, B)}.

adj0(nl, true) --> "bewerkte".
adj0(nl, true) --> "bijgewerkte".
adj0(nl, _) --> "fotografische".
adj0(nl, _) --> "fotomechanische".
adj0(nl, _) --> "geheel".
% @tbd This is a relevant aspect that should be asserted!
adj0(nl, _) --> "goedkoope".
% @tbd The presence of illustrations are not yet asserted.
adj0(nl, _) --> "geïllustreerde".
% Abbreviation of 'gewijzigde'.
adj0(nl, true) --> "gew.".
adj0(nl, true) --> "gewijzigde".
% Abbreviation of 'herziene'.
adj0(nl, true) --> "herz.".
adj0(nl, true) --> "herziene".
% Maybe a typo of 'herziende'.
adj0(nl, true) --> "herzienste".
adj0(nl, _) --> "laatste".
adj0(nl, _) --> "ingrijpend".
% Meaningless adjective.
adj0(nl, _) --> "nieuwe".
adj0(nl, _) --> "nieuwste".
% Abbreviation for 'omgewerkte'?
adj0(nl, true) --> "omgew.".
adj0(nl, true) --> "omgewerkte".
% Maybe a typo of 'omgewerkte'?
adj0(nl, true) --> "omwerkte".
adj0(nl, fail) --> "ongewijzigde".
adj0(nl, fail) --> "onveranderde".
adj0(nl, _) --> "sterk".
adj0(nl, _) --> "veel".
% Strange word.
adj0(nl, fail) --> "veelveranderde".
% Abbreviated version of 'verbeterde'.
adj0(nl, true) --> "verb.".
adj0(nl, true) --> "verbeterde".
% Abbreviation for 'vermeerderde'.
adj0(nl, fail) --> "verm.".
adj0(nl, fail) --> "vermeerderde".
% Probably a typo of 'vermeerderde'.
adj0(nl, fail) --> "vermeerde".
% Meaningless adjective.
adj0(nl, _) --> "volledige".
adj0(nl, _) --> "Vlaamse".
adj0(nl, _) --> "zeer".

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

commercial_edition(nl) --> "handelseditie".

det(en) --> "the".
det(nl) --> "de".
det(nl) --> "het".

facsimile(nl) --> "facsimile".

n(Lang) --> n0(Lang).
n(Lang) --> language(Lang).
n(Lang) --> year(Lang, _Year).
% @tbd The connection to the previous edtion is not yet asserted.
n(Lang) --> n0(Lang), blank, year(Lang, _Year).
n(Lang) --> name(Lang).

n0(de) --> "Auflage".
n0(nl) --> "druk".
n0(nl) --> "herdruk".
n0(nl) --> "oplaag".
n0(nl) --> "Pockets".
n0(nl) --> "uitgaaf".
n0(nl) --> "uitgave".
n0(nl) --> "zorg".

name(_Lang) --> "Meulenhoff".

np(Lang, Print, B) -->
  np0(Lang, Print, B).
np(Lang, Print, B) -->
  np0(Lang, Print, B1), blank,
  pp(Lang, Print, B2),
  {boolean_or(B1, B2, B)}.
np(Lang, Print, B) -->
  adjs(Lang, Print, B),
  n(Lang).

np0(Lang, Print, B) -->
  (det(Lang), blank ; ""),
  adjs(Lang, Print, B), blanks,
  n(Lang).

popular_edition(nl) --> "Volksuitgaaf".

pp(Lang, Print, B) -->
  pre(Lang), blank,
  np(Lang, Print, B).

pre(nl) --> "in".
pre(nl) --> "met".
pre(nl) --> "naar".
pre(nl) --> "van".
pre(nl) --> "voor".

publication_print(Lang, Print, Changes) -->
  s(Lang, Print, Changes).

s(Lang, Print, Changes) -->
  opening_round_bracket,
  s(Lang, Print, Changes),
  (uncertainty(Lang) ; ""),
  (blank, special_modifier(Lang) ; ""),
  closing_round_bracket.
s(Lang, Print, B) -->
  (facsimile(Lang), blank ; ""),
  np(Lang, Print, B1),
  (blank, pp(Lang, Print, B2), {boolean_or(B1, B2, B)} ; "").
s(Lang, Print, B) -->
  np(Lang, Print, B1), blank,
  np(Lang, Print, B2),
  {boolean_or(B1, B2, B)}.
s(Lang, Print, B) -->
  voltooid_deelwoord, blank,
  pp(Lang, Print, B).
% Information for different volumes is sometimes given.
% @tbd This information is currently skipped.
s(Lang, Print, Changes) -->
  volumes(Lang, Print, Changes).
% No number-of-print, only a conjunction of adjectives.
s(Lang, Print, Changes) -->
  np(Lang, Print, Changes).
% No number-of-print informat occurs.
s(_Lang, fail, fail) --> "".

special_modifier(Lang) -->
  opening_round_bracket,
  special_modifier(Lang),
  closing_round_bracket.
special_modifier(Lang) -->
  commercial_edition(Lang).
special_modifier(Lang) -->
  popular_edition(Lang).

voltooid_deelwoord --> "uitgegeven".

volume_s(Lang, Print, Changes) -->
  volume(Lang, _Volume),
  colon, blank,
  s(Lang, Print, Changes).

%! volumes(Lang, Print, Changes)//
% @tbd The print number and change boolean are only returned if
%      they are the same for all volumes.

volumes(Lang, Print, Changes) -->
  volume_s(Lang, Print, Changes).
volumes(Lang, Print, Changes) -->
  volume_s(Lang, Print1, Changes1), blank,
  forward_slash, blank,
  volume_s(Lang, Print2, Changes2),
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

