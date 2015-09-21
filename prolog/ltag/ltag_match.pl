:- module(
  ltag_match,
  [
    basic_filtering/2, % +LanguagePriorityList:list(atom)
                       % +LanguageTag:atom
    extended_filtering/2 % +LanguagePriorityList:list(atom)
                         % +LanguageTag:atom
  ]
).

/** <module> Language tag: Matching

Algorithms for matching language ranges against language tags.

# Concepts

  - **Filtering**
    Return a set of results.
  - **Lookup**
    Return a single result.

# Semantics

Applications, protocols, and specifications are not required to validate
or understand any of the semantics of the language tags or ranges or of
the subtags in them, nor do they require access to the IANA Language Subtag
Registry.

---

@author Wouter Beek
@compat RFC 4647
@version 2015/08-2015/09
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(list_ext)).





%! basic_filtering(
%!   +LanguagePriorityList:list(atom),
%!   +LanguageTag:atom
%! ) is semidet.
% Succeeds if the LanguagePriorityList matches the LanguageTag according to
% the basic filtering algorithm described in RFC 4647,
% i.e., if the former is a case-insensitive prefix of the latter,
% while also treating the `*` sign as a wildcard.
%
% @arg LanguagePriorityList A list of atoms that parse according to
%      'language-range'//1.
% @arg LanguageTag ...
%
% @compat RFC 4647

% Allow language priority lists of length 1 to be specified as atoms.
basic_filtering(Ranges, Tag):-
  % NONDET
  member(Range, Ranges),
  atomic_list_concat(Subtags1, -, Range),
  atomic_list_concat(Subtags2, -, Tag),
  basic_filtering0(Subtags1, Subtags2), !.

basic_filtering0([], _).
basic_filtering0([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  basic_filtering0(T1, T2).



%! extended_filtering(
%!   +LanguagePriorityList:list(atom),
%!   +LanguageTag:atom
%! ) is semidet.
% Compares extended language ranges to language tags.
%
% Subtags not specified, including those at the end of the language range,
% are treated as if assigned the wildcard value `*`.
% Much like basic filtering, extended filtering selects content with
% arbitrarily long tags that share the same initial subtags as the language
% range.
% In addition, extended filtering selects language tags that contain any
% intermediate subtags not specified in the language range.
%
% ### Example
%
% `de-*-DE` or `de-DE` matches all of the following tags:
%
% ```rfc5646
% de-DE
% de-de
% de-Latn-DE
% de-Latf-DE
% de-DE-x-goethe
% de-Latn-DE-1996
% de-Deva-DE
% ```
%
% and does not match any of the following tags:
%
% ```rfc5646
% de
% de-x-DE
% de-Deva
% ```
%
% @compat RFC 4647

% [2] Begin with the first subtag in each list.
%     If the first subtag in the range does not match the first subtag in
% the tag, the overall match fails.
% Otherwise, move to the next subtag in both the range and the tag.
extended_filtering([H1|T1], [H2|T2]):-
  subtag_match(H1, H2),
  extended_filtering_tail(T1, T2).

% [4] When the language range's list has no more subtags, the match succeeds.
extended_filtering_tail([], _):- !.
% [3a] If the subtag currently being examined in the range is the wildcard
%      `*`, move to the next subtag in the range and continue with the loop.
extended_filtering_tail([*|T1], [_|T2]):- !,
  extended_filtering_tail(T1, T2).
% [3b] Else, if there are no more subtags in the language tag's list,
%      the match fails.
extended_filtering_tail(_, []):- !,
  fail.
% [3c] Else, if the current subtag in the range's list matches the current
%      subtag in the language tag's list, move to the next subtag in both
%      lists and continue with the loop.
extended_filtering_tail([H1|T1], [H2|T2]):-
  subtag_match(H1, H2), !,
  extended_filtering_tail(T1, T2).
% [3d] Else, if the language tag's subtag is a "singleton" (a single letter
%      or digit, which includes the private-use subtag `x`) the match fails.
extended_filtering_tail(_, [H2|_]):-
  atom_length(H2, 1), !.
% [3e] Else, move to the next subtag in the language tag's list and continue
%      with the loop.
extended_filtering_tail(L1, [_|T2]):-
  extended_filtering_tail(L1, T2).



%! extended_to_basic(
%!   +ExtendedLanguageRange:list(atom)
%!   -BasicLanguageRange:list(atom)
%! ) is det.
% Maps extended language ranges to basic language ranges.
%
% @compat RFC 4647

extended_to_basic([*|_], [*]):- !.
extended_to_basic(Extended, Basic):-
  exclude(=(*), Extended, Basic).



%! lookup(+LanguageRange:list(atom), +LanguageTag:list(atom))
% In contrast to filtering, each language range represents the most specific
% tag that is an acceptable match.

lookup(L, L):- !.
lookup(L1a, L2):-
  append(L1b, [_], L1a),
  lookup(L1b, L2).





% HELPERS %

%! subtag_match(+RangeSubtag:atom, +Subtag:atom) is semidet.
% Two subtags match if either they are the same when compared
% case-insensitively or the language range's subtag is the wildcard `*`

subtag_match(*, _):- !.
subtag_match(X1, X2):-
  downcase_atom(X1, X),
  downcase_atom(X2, X).
