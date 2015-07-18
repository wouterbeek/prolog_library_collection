:- module(
  typecheck,
  [
    boolean/1, % @Term
    char/1, % @Term
    chars/1, % @Term
    code/1, % @Term
    codes/1, % @Term
    negative_float/1, % @Term
    negative_integer/1, % @Term
    nonneg/1, % @Term
    positive_float/1, % @Term
    positive_integer/1, % @Term
    text/1 % @Term
  ]
).
:- reexport(
  library(error),
  [
    is_of_type/2, % +Type
                  % @Term
    must_be/2 % +Type
              % @Term
  ]
).

/** <module> Typecheck

Predicates used for parsing and checking value-type conformance.

| *Type*               | *|Defined here|* | *|Atom-conversion|* |
| atom                 |                  | Yes                 |
| atomic               |                  |                     |
| between/2            |                  | Yes                 |
| boolean              |                  | Yes                 |
| callable             |                  |                     |
| char                 |                  | Yes                 |
| chars                |                  | Yes                 |
| code                 |                  | Yes                 |
| codes                |                  | Yes                 |
| compound             |                  |                     |
| constant             |                  |                     |
| email                |                  |                     |
| encoding             |                  |                     |
| float                |                  | Yes                 |
| ground               |                  |                     |
| integer              |                  | Yes                 |
| list                 |                  |                     |
| list/1               | Yes              |                     |
| list_or_partial_list |                  |                     |
| negative_integer     |                  | Yes                 |
| nonneg               |                  | Yes                 |
| nonvar               |                  |                     |
| number               |                  | Yes                 |
| oneof/1              |                  | Yes                 |
| or/1                 | Yes              |                     |
| positive_integer     |                  | Yes                 |
| rational             |                  |                     |
| string               |                  | Yes                 |
| symbol               |                  |                     |
| term                 | Yes              |                     |
| text                 |                  |                     |
| uri                  | Yes              |                     |
| iri                  | Yes              |                     |
| var                  |                  |                     |

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(error)).

:- multifile(error:has_type/2).

% char/0
error:has_type(char, Term):-
  is_char(Term).
% code/0
error:has_type(code, Term):-
  once(code_type(Term, _)).
% between_float/2, extension of between/2 for floats
% allowing uninstiated upper and lower bounds.
error:has_type(between_float(L,U), X):-
  number(X),
  (number(L) -> X >= L ; true),
  (number(U) -> X =< L ; true).
% or/1
error:has_type(or(Types), Term):-
  member(Type, Types),
  error:has_type(Type, Term), !.
% term
error:has_type(term, _).



%! boolean(@Term) is semidet.

boolean(Term):-
  error:has_type(boolean, Term).



%! char(@Term) is semidet.

char(Term):-
  error:has_type(char, Term).



%! chars(@Term) is semidet

chars(Term):-
  error:has_type(chars, Term).



%! code(@Term) is semidet.

code(Term):-
  error:has_type(code, Term).



%! codes(@Term) is semidet

codes(Term):-
  error:has_type(codes, Term).



%! negative_float(@Term) is semidet.
% Fails silently when no negative integer.

negative_float(I):-
  float(I),
  I > 0.



%! negative_integer(@Term) is semidet.
% Fails silently when no negative integer.

negative_integer(I):-
  integer(I),
  I < 0.



%! nonneg(@Term) is semidet.
% Fails silently when no positive integer or zero.

nonneg(I):-
  integer(I),
  I >= 0.



%! positive_float(@Term) is semidet.
% Fails silently when no negative integer.

positive_float(I):-
  float(I),
  I > 0.0.



%! positive_integer(@Term) is semidet.
% Fails silently when no negative integer.

positive_integer(I):-
  integer(I),
  I > 0.



%! text(@Term) is semidet.
% Text is one of atom, string, chars or codes.

text(Term):-
  error:has_type(text, Term).
