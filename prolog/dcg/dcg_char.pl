:- module(
  dcg_char,
  [
    char//1, % ?Char:char
    char_case//2, % ?Char:char
                  % ?Case:oneof([lower,upper])
    char_ci//1, % ?Char:char
    char_lower//1, % ?Char:char
    char_upper//1 % ?Char:char
  ]
).

/** <module> DCG character

Grammar rules for processing characters.

# Parsing

Read letters in either case, prefering lowercase
 (i.e., the first solution is all-lowercase).

```prolog
?- atom_phrase(char_upper(Char), 'Q').
Char = 'q';
Char = 'Q'.
```

# Generating

Write letters according to the predicate's semantics,
 e.g., code_upper//1 writes uppercase letters.
It does not matter in which case the letters are supplied
 in the instantiated argument.

```prolog
?- atom_phrase(char_upper(q), Atom).
Atom = 'Q'.
```

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_code)).

:- meta_predicate(char_code_metacall(3,?,?,?)).





%! char(?Char:char)// .

char(Char) -->
  char_code_metacall(code, Char).



%! char_case(?Char:char, ?Case:oneof([lower,upper]))// .
% Allows the calling context to set the case on a per-char basis.
%
% Also allows the calling context to apply the same case consistently across
% chracters without worrying which case.

char_case(Char, lower) -->
  char_lower(Char).
char_case(Char, upper) -->
  char_upper(Char).



%! char_ci(?Char:char)// .

char_ci(Char) -->
  char_code_metacall(code_ci, Char).



%! char_lower(?Char:char)// .

char_lower(Char) -->
  char_code_metacall(code_lower, Char).



%! char_upper(?Char:char)// .

char_upper(Char) -->
  char_code_metacall(code_upper, Char).





% HELPERS %

char_code_metacall(Goal, Char) -->
  {var(Char)}, !,
  dcg_call(Goal, Code),
  {char_code(Char, Code)}.
char_code_metacall(Goal, Char) -->
  {char_code(Char, Code)},
  dcg_call(Goal, Code).
