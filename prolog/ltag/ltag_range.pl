:- module(
  ltag_range,
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

:- use_module(library(ltag/ltag_generics)).





%! 'extended-language-range'(-ExtendedLanguageRange:atom)// is det.
% ```abnf
% extended-language-range = (1*8ALPHA / "*")
%                           *("-" (1*8alphanum / "*"))
% ```

'extended-language-range'(ELRange) -->
  ("*" --> H = * ; 'primary-subtag'(H)),
  'extended-language-range_tail'(T),
  {atomic_list_concat([H|T], -, ELRange).

'extended-language-range_tail'([H|T]) -->
  "-", !,
  ("*" --> H = * ; subtag(H)),
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
  'primary-subtag'(H),
  'language-range_tail'(T),
  {atomic_list_concat([H|T], -, LRange)}.

'language-range_tail'([H|T]) -->
  "-", !,
  'subtag'(H),
  'language-range_tail'(T).
'language-range_tail'([]) --> "".
