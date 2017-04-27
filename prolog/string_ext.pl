:- module(
  string_ext,
  [
    capitalize_string/2,  % +Str1, -Str2
    lowercase_string/2,   % +Str1, -Str2
    read_string/2,        % +In, -Str
    string_atom/2,        % ?Str, ?A
    string_ellipsis/3,    % +Str, ?MaxLen, -Ellipsis
    string_list_concat/2, % +Strs, -Str
    string_list_concat/3, % ?Strs, ?Sep, ?Str
    string_prefix/3,      % +Str, ?Len, ?Sub
    string_to_term/2,     % +Str, -Term
    string_replace/4,     % +Str1, +SubStr1, -SubStr2, -Str2
    string_truncate/3,    % +Str,  +Max, -Truncated
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



%! read_string(+In, -Str) is det.

read_string(In, Str) :-
  read_string(In, _, Str).



%! string_atom(+Str, -A) is det.
%! string_atom(-Str, +A) is det.

string_atom(Str, A) :-
  atom_string(A, Str).



%! string_ellipsis(+String, +Len, +Ellipsis) is semidet.
%! string_ellipsis(+String, +Len, -Ellipsis) is semidet.
%! string_ellipsis(+String, -Len, -Ellipsis) is nondet.
%
% ```
% ?- string_ellipsis("monkey", N, X).
% N = 2,
% X = "m…" ;
% N = 3,
% X = "mo…" ;
% N = 4,
% X = "mon…" ;
% N = 5,
% X = "monk…" ;
% N = 6,
% X = "monkey".
% ```

string_ellipsis(Str, ELen, Ellipsis) :-
  string_length(Str, Len),
  (   ELen == inf
  ->  Ellipsis = Str
  ;   between(2, Len, ELen)
  *-> (   ELen =:= Len
      ->  Ellipsis = Str
      ;   TLen is ELen - 1,
          string_truncate(Str, TLen, Truncated),
          string_concat(Truncated, "…", Ellipsis)
      )
  ;   Ellipsis = Str
  ).



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



%! string_prefix(+Str, +Len, +Sub) is semidet.
%! string_prefix(+Str, +Len, -Sub) is semidet.
%! string_prefix(+Str, -Len, +Sub) is semidet.
%! string_prefix(+Str, -Len, -Sub) is multi.
%
% Sub is the prefix of string Str that has length Len.
%
% Fails in case Len is higher than the length of string Str.

string_prefix(Str, Len, Sub) :-
  sub_string(Str, 0, Len, _, Sub).



%! string_replace(+Str1, +SubStr1, -SubStr2, -Str2) is det.

string_replace(Str1, SubStr1, SubStr2, Str2) :-
  string_list_concat(L, SubStr1, Str1),
  string_list_concat(L, SubStr2, Str2).



%! string_to_term(+Str, -Term) is det.

string_to_term(Str, Term) :-
  atom_string(A, Str),
  atom_to_term(A, Term).



%! string_truncate(+Str, +Max, -Truncated) is det.
%
% @see string_ellipsis

string_truncate(Str, inf, Str) :- !.
string_truncate(Str, MaxLen, Str) :-
  must_be(nonneg, MaxLen),
  string_length(Str, Len),
  Len =< MaxLen, !.
string_truncate(Str, MaxLen, Prefix) :-
  string_prefix(Str, MaxLen, Prefix).



%! uppercase_string(+Str1, -Str2) is det.

uppercase_string("", "").
uppercase_string(Str1, Str2) :-
  string_codes(Str1, Cs1),
  maplist(to_upper, Cs1, Cs2),
  string_codes(Str2, Cs2).
