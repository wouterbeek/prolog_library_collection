:- module(
  language_range,
  [
    alphanum//1, % ?Code:code
    'extended-language-range'//1, % -ExtendedLanguageRange:atom
    'language-range'//1 % -LanguageRange:atom
  ]
).

/** <module> Language range

Grammar for parsing BCP 47 language ranges.

# Concepts

  * Extended language range
  * Language priority list
  * Language range

--

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_abnf_rules)).





%! alphanum(?Code:code)// .
% ```abnf
% alphanum = ALPHA / DIGIT
% ```

alphanum(C) --> 'ALPHA'(C).
alphanum(C) --> 'DIGIT'(C).



%! 'extended-language-range'(-ExtendedLanguageRange:atom)// is det.
% ```abnf
% extended-language-range = (1*8ALPHA / "*")
%                           *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'(ELRange) -->
  ("*" --> H = * ; first_tag(H)),
  'extended-language-range_tail'(T),
  {atomic_list_concat([H|T], -, ELRange).

'extended-language-range_tail'([H|T]) -->
  "-", !,
  ("*" --> H = * ; nonfirst_tag(H)),
  'extended-language-range_tail'(T).
'extended-language-range_tail'([]) --> "".



%! 'language-range'(-LanguageRange:atom)// is det.
% Parses a basic language range.
%
% ```abnf
% language-range = (1*8ALPHA *("-" 1*8alphanum)) / "*"
% ```

'language-range'(*) --> "*", !.
'language-range'(LRange) -->
  first_tag(H),
  'language-range_tail'(T),
  {atomic_list_concat([H|T], -, LRange)}.

'language-range_tail'([H|T]) -->
  "-", !,
  nonfirst_tag(H),
  'language-range_tail'(T).
'language-range_tail'([]) --> "".





% HELPERS %

first_tag(Tag) -->
  'm*n'(1, 8, 'ALPHA', H, [convert(1,atom)]).

nonfirst_tag(Tag) -->
  'm*n'(1, 8, alphanum, H, [convert(1,atom)]).
