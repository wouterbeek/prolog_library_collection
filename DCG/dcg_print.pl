:- module(
  dcg_print,
  [
    
  ]
).

/** <module> DCG_PRINT

DCGs for the print of a publication.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_ordinal)).
:- use_module(logic(boolean_logic)).
:- use_module(library(dcg/basics)).



% Can occur between spaces.
print_number(Lang, Print, Changes) -->
  opening_round_bracket,
  print_number(Lang, Print, Changes),
  closing_round_bracket.
print_number(Lang, Print, Changes) -->
  facsimile(Lang),
  integer(Print),
  ordinal(Lang, Ordinal),
  skip,
  print_changes(Lang, Changes).
% Information for different volumes are given.
% @tbd This information is now skipped.
print_number(Lang, Print, Changes) -->
  print_number_volumes(Lang, Print, Changes).
% No number-of-print, only a conjunction of adjectives.
print_number(Lang, fail, Changes) -->
  print_changes(Lang, Changes).
% No number-of-print informat occurs.
print_number(_Lang, fail, fail) -->
  [].

print_number_volume(Lang, Print, Changes) -->
  part_word(Lang),
  blank,
  integer(_Volume),
  colon,
  blank,
  print_number(Lang, Print, Changes).

%! print_number_volumes(Lang, Print, Changes)//
% @tbd The print number and change boolean are only returned if
%      they are the same for all volumes.

print_number_volumes(Lang, Print, Changes) -->
  print_number_volume(Lang, Print, Changes).
print_number_volumes(Lang, Print, Changes) -->
  print_number_volume(Lang, Print1, Changes1),
  blank,
  forward_slash,
  blank,
  print_number_volume(Lang, Print2, Changes2),
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

% Indicators that a print contains changes w.r.t. a previous print.
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  uncertainty(Lang).
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  blank,
  commercial_edition(Lang).
print_changes(Lang, Boolean) -->
  print_changes_conj(Lang, Boolean),
  blank,
  print_word(Lang),
  blank,
  atom(van),
  blank,
  print_word(Lang),
  blank,
  year(Lang, _Year).

print_changes_adj(Lang, Boolean) -->
  print_changes_adj0(Lang, Boolean).
% Consecutive adjectives.
print_changes_adj(Lang, Boolean) -->
  print_changes_adj0(Lang, Boolean),
  blank,
  print_changes_adj(Lang, Boolean).
% Language preposition.
print_changes_adj(Lang, Boolean) -->
  {Lang = nl},
  atom('naar het'),
  blank,
  language(Lang),
  blank,
  print_changes_adj0(Lang, Boolean).

print_changes_adj0(nl, true) -->
  atom(bewerkte).
print_changes_adj0(nl, true) -->
  atom(bijgewerkte).
% @tbd This could be relevant to assert as well.
print_changes_adj0(nl, fail) -->
  atom(fotomechanische).
print_changes_adj0(nl, _) -->
  atom(geheel).
% @tbd This is a relevant aspect that should be asserted!
print_changes_adj0(nl, fail) -->
  atom(goedkoope).
% Abbreviation of 'gewijzigde'.
print_changes_adj0(nl, true) -->
  atom('gew.').
print_changes_adj0(nl, true) -->
  atom(gewijzigde).
% Abbreviation of 'herziene'.
print_changes_adj0(nl, true) -->
  atom('herz.').
print_changes_adj0(nl, true) -->
  atom(herziene).
print_changes_adj0(nl, _) -->
  atom(ingrijpend).
% Meaningless adjective.
print_changes_adj0(nl, fail) -->
  atom(nieuwe).
% Abbreviation for 'omgewerkte'?
print_changes_adj0(nl, true) -->
  atom('omgew.').
print_changes_adj0(nl, true) -->
  atom(omgewerkte).
print_changes_adj0(nl, fail) -->
  atom(onveranderde).
% Strange word.
print_changes_adj0(nl, fail) -->
  atom(veelveranderde).
% Abbreviated version of 'verbeterde'.
print_changes_adj0(nl, true) -->
  atom('verb.').
print_changes_adj0(nl, true) -->
  atom(verbeterde).
% Abbreviation for 'vermeerderde'.
print_changes_adj0(nl, fail) -->
  atom('verm.').
print_changes_adj0(nl, fail) -->
  atom(vermeerderde).
% Probably a typo.
print_changes_adj0(nl, fail) -->
  atom(vermeerde).
% Meaningless adjective.
print_changes_adj0(nl, fail) -->
  atom(volledige).

print_changes_conj(_Lang, _Boolean) -->
  [].
print_changes_conj(Lang, Boolean) -->
  print_changes_adj(Lang, Boolean).
print_changes_conj(Lang, Boolean) -->
  print_changes_adj(Lang, Boolean1),
  conjunct(Lang),
  print_changes_conj(Lang, Boolean2),
  {boolean_or(Boolean1, Boolean2, Boolean)}.

print_word(de) -->
  atom('Auflage').
print_word(nl) -->
  atom(druk).
print_word(nl) -->
  atom(herdruk).
print_word(nl) -->
  atom(uitgave).

