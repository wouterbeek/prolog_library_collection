:- module(
  string_ext,
  [
    capitalize_string/2,  % +Str1, -Str2
    lowercase_string/2,   % +Str1, -Str2
    string_atom/2,        % ?Str, ?A
    string_to_term/2,     % +Str, -Term
    string_replace/4,     % +Str1, +SubStr1, -SubStr2, -Str2
    uppercase_string/2    % +Str1, -Str2
  ]
).

/** <module> String extensions

Additional support for native strings in SWI-Prolog.

Non-native string representations in Prolog:
  - List of character codes
    Strings cannot be distinguished from lists of (non-negative) integers.
  - List of characters

@author Wouter Beek
@version 2015/08, 2016/02, 2016/05-2016/09, 2016/12
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(error)).





%! capitalize_string(+Str1, -Str2) is det.

capitalize_string("", "").
capitalize_string(Str1, Str2) :-
  string_codes(Str1, [H1|T]),
  to_upper(H1, H2),
  string_codes(Str2, [H2|T]).



%! lowercase_string(+Str1, -Str2) is det.

lowercase_string(Str1, Str2) :-
  string_atom(Str1, A1),
  lowercase_atom(A1, A2),
  atom_string(A2, Str2).



%! string_atom(+Str, -A) is det.
%! string_atom(-Str, +A) is det.

string_atom(Str, A) :-
  atom_string(A, Str).



%! string_replace(+Str1, +SubStr1, -SubStr2, -Str2) is det.

string_replace(Str1, SubStr1, SubStr2, Str2) :-
  string_list_concat(L, SubStr1, Str1),
  string_list_concat(L, SubStr2, Str2).



%! string_to_term(+Str, -Term) is det.

string_to_term(Str, Term) :-
  atom_string(A, Str),
  atom_to_term(A, Term).



%! uppercase_string(+Str1, -Str2) is det.

uppercase_string("", "").
uppercase_string(Str1, Str2) :-
  string_codes(Str1, Cs1),
  maplist(to_upper, Cs1, Cs2),
  string_codes(Str2, Cs2).
