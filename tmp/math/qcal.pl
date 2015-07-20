:- module(
  qcal,
  [
    sign_multiply/3 % +Sign1:oneof([min,zero,plus])
                    % +Sign2:oneof([min,zero,plus])
                    % -Sign3:oneof([min,zero,plus])
  ]
).

/** <module> QCAL

Predicates implementing a qualitative calculus.

@author Wouter Beek
@version 2013/07
*/



%! sign_multiply(
%!   +Sign1:oneof([min,zero,plus]),
%!   +Sign2:oneof([min,zero,plus]),
%!   -Sign3:oneof([min,zero,plus])
%! ) is det.
% Performs sign multiplication according to the following table:
%
% |   | + | 0 | - |
% | + | + | 0 | - |
% | 0 | 0 | 0 | 0 |
% | - | - | 0 | + |

sign_multiply(zero, _Sign, zero):- !.
sign_multiply(_Sign, zero, zero):- !.
sign_multiply(Sign, Sign, plus):- !.
sign_multiply(Sign1, Sign2, min):-
  Sign1 \== Sign2.
