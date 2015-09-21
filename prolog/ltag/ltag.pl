:- module(
  ltag,
  [
    'obs-language-tag'//1 % -LanguageTag:atom
  ]
).

/** <module> Language tag

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(ltag/ltag_generics)).





%! 'obs-language-tag'// is det.
% ```abnf
% obs-language-tag = primary-subtag *( "-" subtag )
% ```

'obs-language-tag'(LTag) -->
  'primary-subtag'(H),
  'obs-language-tag_tail'(T),
  {atomic_list_concat([H|T], -, LTag)}.

'obs-language-tag_tail'([H|T]) -->
  "-", !,
  subtag(H),
  'obs-language-tag_tail'(T).
'obs-language-tag_tail'([]) --> "".
