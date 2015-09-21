:- module(
  ltag_generics,
  [
    'primary-subtag'//1, % -Subtag:atom
    subtag//1 % -Subtag:atom
  ]
).

/** <module> Language tags: Generics

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



%! 'primary-subtag'(-Subtag:atom)// is det.
% ```abnf
% primary-subtag = 1*8ALPHA
% ```

'primary-subtag'(Subtag) -->
  'm*n'(1, 8, 'ALPHA', Subtag, [convert(1-atom_lower)]).



%! subtag(-Subtag:atom)// is det.
% ```abnf
% subtag = 1*8(ALPHA / DIGIT)
% ```

subtag(Subtag) -->
  'm*n'(1, 8, alphanum, Subtag,[convert(1-atom_lower)]).
