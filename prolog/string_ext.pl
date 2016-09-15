:- module(
  string_ext,
  [
    capitalize_string/2,  % +Str1, -Str2
    codes_string/2,       % ?Cs, ?Str
    lowercase_string/2,   % +Str1, -Str2
    string_atom/2,        % ?Str, ?A
    string_list_concat/2, % +Strs, -Str
    string_list_concat/3, % ?Strs, ?Sep, ?Str
    string_to_term/2,     % +Str, -Term
    string_replace/4,     % +Str1, +SubStr1, -SubStr2, -Str2
    string_truncate/3,    % +Str, +Max, -TruncatedStr
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
@version 2015/08, 2016/02, 2016/05-2016/09
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



%! codes_string(+Cs, +Str) is semidet.
%! codes_string(+Cs, -Str) is det.
%! codes_string(-Cs, +Str) is det.
% Variant of the built-in string_codes/2.

codes_string(Cs, Str):-
  string_codes(Str, Cs).



%! lowercase_string(+Str1, -Str2) is det.

lowercase_string(Str1, Str2) :-
  string_atom(Str1, A1),
  lowercase_atom(A1, A2),
  atom_string(A2, Str2).



%! string_atom(+Str, -A) is det.
%! string_atom(-Str, +A) is det.

string_atom(Str, A) :-
  atom_string(A, Str).



%! string_list_concat(+Strs, +Str) is semidet.
%! string_list_concat(+Strs, -Str) is det.
%! string_list_concat(+Strs, +Sep, +Str) is semidet.
%! string_list_concat(+Strs, +Sep, -Str) is det.
%! string_list_concat(-Strs, +Sep, +Str) is det.

string_list_concat(Strs, Str) :-
  atomics_to_string(Strs, Str).


string_list_concat(Strs, Sep, Str):-
  ground(Strs),
  ground(Sep), !,
  atomics_to_string(Strs, Sep, Str).
string_list_concat(Strs, Sep, Str):-
  maplist(atom_string, [Sep0,A], [Sep,Str]),
  atomic_list_concat(As, Sep0, A),
  maplist(atom_string, As, Strs).



%! string_replace(+Str1, +SubStr1, -SubStr2, -Str2) is det.

string_replace(Str1, SubStr1, SubStr2, Str2) :-
  string_list_concat(L, SubStr1, Str1),
  string_list_concat(L, SubStr2, Str2).



%! string_to_term(+Str, -Term) is det.

string_to_term(Str, Term) :-
  atom_string(A, Str),
  atom_to_term(A, Term).



%! string_truncate(+Str, +Max, -TruncatedStr) is det.

string_truncate(Str, inf, Str):- !.
string_truncate(Str, Max, Str):-
  must_be(positive_integer, Max),
  Max =< 5, !.
% The string does not have to be truncated, it is not that long.
string_truncate(Str, Max, Str):-
  string_length(Str, Len),
  Len =< Max, !.
% The string exceeds the maximum length, it is truncated.
% For this purpose the displayed length of the string is
%  the maximum length minus 4 (but never less than 3).
string_truncate(Str1, Max, Str3):-
  TruncatedLen is Max - 3,
  sub_string(Str1, 0, TruncatedLen, _, Str2),
  string_concat(Str2, "...", Str3).



%! uppercase_string(+Str1, -Str2) is det.

uppercase_string("", "").
uppercase_string(Str1, Str2) :-
  string_codes(Str1, Cs1),
  maplist(to_upper, Cs1, Cs2),
  string_codes(Str2, Cs2).
