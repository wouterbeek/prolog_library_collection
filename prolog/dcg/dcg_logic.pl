:- module(
  dcg_logic,
  [
    entails//0,
    entails//1, % ?Code:code
    equivalence//0,
    equivalence//1, % ?Code:code
    falsum//0,
    falsum//1, % ?Code:code
    provable//0,
    provable//1, % ?Code:code
    set_membership//0,
    set_membership//1, % ?Code:code
    subclass//0,
    subclass//1 % ?Code:code
  ]
).

/** <module> DCG logic

Grammar snippets for logic expression.

@author Wouter Beek
@version 2015/08
*/



%! entails// .
%! entails(?Code:code)// .
% ⊨

entails --> entails(_).
entails(8658) --> [8658].


%! equivalence// .
%! equivalence(?Code:code)// .
% ≡

equivalence --> equivalence(_).
equivalence(8801) --> [8801].



%! falsum// .
%! falsum(?Code:code)// .
% ⊥

falsum --> falsum(_).
falsum(8869) --> [8869].



%! provable// .
%! provable(?Code:code)// .
% ⊢

provable --> provable(_).
provable(8866) --> [8866].


%! set_membership// .
%! set_membership(?Code:code)// .
% ∊

set_membership --> set_membership(_).
set_membership(8714) --> [8714].


%! subclass// .
%! subclass(?Code:code)// .
% ⊆

subclass --> subclass(_).
subclass(8838) --> [8838].
