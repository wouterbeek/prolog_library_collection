:- module(
  string_ext,
  [
    codes_string/2,       % ?Cs, ?S
    string_list_concat/3, % ?Ss, ?Sep, ?S
    string_to_term/2,     % +S, -Term
    string_truncate/3     % +S, +Max, -TruncatedS
  ]
).

/** <module> String extensions

Additional support for native strings in SWI-Prolog.

Non-native string representations in Prolog:
  - List of character codes
    Strings cannot be distinguished from lists of (non-negative) integers.
  - List of characters

@author Wouter Beek
@version 2015/08, 2016/02, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(error)).





%! codes_string(+Cs, +S) is semidet.
%! codes_string(+Cs, -S) is det.
%! codes_string(-Cs, +S) is det.
% Variant of the built-in string_codes/2.

codes_string(Cs, S):-
  string_codes(S, Cs).



%! string_list_concat(+Ss, +Sep, +S) is semidet.
%! string_list_concat(+Ss, +Sep, -S) is det.
%! string_list_concat(-Ss, +Sep, +S) is det.

string_list_concat(Ss, Sep, S):-
  var(S), !,
  maplist(atom_string, [Sep0|As], [Sep|Ss]),
  atomic_list_concat(As, Sep0, A),
  atom_string(A, S).
string_list_concat(Ss, Sep, S):-
  maplist(atom_string, [Sep0,A], [Sep,S]),
  atomic_list_concat(As, Sep0, A),
  maplist(atom_string, As, Ss).



%! string_to_term(+S, -Term) is det.

string_to_term(S, Term) :-
  atom_string(A, S),
  atom_to_term(A, Term).



%! string_truncate(+S, +Max, -TruncatedS) is det.

string_truncate(S, inf, S):- !.
string_truncate(S, Max, S):-
  must_be(positive_integer, Max),
  Max =< 5, !.
% The string does not have to be truncated, it is not that long.
string_truncate(S, Max, S):-
  string_length(S, Len),
  Len =< Max, !.
% The string exceeds the maximum length, it is truncated.
% For this purpose the displayed length of the string is
%  the maximum length minus 4 (but never less than 3).
string_truncate(S1, Max, S3):-
  TruncatedLen is Max - 3,
  sub_string(S1, 0, TruncatedLen, _, S2),
  string_concat(S2, "...", S3).
