:- module(
  string_ext,
  [
    read_string/2,        % +In, -String
    string_ellipsis/3,    % +String, ?MaxLength, -Ellipsis
    string_list_concat/2, % +Strings, -String
    string_list_concat/3, % ?Strings, ?Separator, ?String
    string_prefix/3,      % +String, ?Length, ?SubString
    string_truncate/3     % +String, +Max, -Truncated
  ]
).

/** <module> String extensions

@author Wouter Beek
@version 2017/05-2017/06
*/

:- use_module(library(error)).





%! read_string(+In, -String) is det.

read_string(In, String) :-
  read_string(In, _, String).



%! string_ellipsis(+String, +Length, +Ellipsis) is semidet.
%! string_ellipsis(+String, +Length, -Ellipsis) is semidet.
%! string_ellipsis(+String, -Length, -Ellipsis) is nondet.
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

string_ellipsis(String, ELength, Ellipsis) :-
  string_length(String, Length),
  (   ELength == inf
  ->  Ellipsis = String
  ;   between(2, Length, ELength)
  *-> (   ELength =:= Length
      ->  Ellipsis = String
      ;   TLength is ELength - 1,
          string_truncate(String, TLength, Truncated),
          string_concat(Truncated, "…", Ellipsis)
      )
  ;   Ellipsis = String
  ).



%! string_list_concat(+Strings, +String) is semidet.
%! string_list_concat(+Strings, -String) is det.
%! string_list_concat(+Strings, +Separator, +String) is semidet.
%! string_list_concat(+Strings, +Separator, -String) is det.
%! string_list_concat(-Strings, +Separator, +String) is det.

string_list_concat(Strings, String) :-
  atomics_to_string(Strings, String).


string_list_concat(Strings, Separator, String):-
  ground(Strings),
  ground(Separator), !,
  atomics_to_string(Strings, Separator, String).
string_list_concat(Strings, Separator, String):-
  maplist(atom_string, [Separator0,A], [Separator,String]),
  atomic_list_concat(As, Separator0, A),
  maplist(atom_string, As, Strings).



%! string_prefix(+String, +Length, +SubString) is semidet.
%! string_prefix(+String, +Length, -SubString) is semidet.
%! string_prefix(+String, -Length, +SubString) is semidet.
%! string_prefix(+String, -Length, -SubString) is multi.
%
% SubString is the prefix of string String that has length Length.
%
% Fails in case Length is higher than the length of string String.

string_prefix(String, Length, SubString) :-
  sub_string(String, 0, Length, _, SubString).



%! string_truncate(+String, +Max, -Truncated) is det.
%
% @see string_ellipsis

string_truncate(String, inf, String) :- !.
string_truncate(String, MaxLength, String) :-
  must_be(nonneg, MaxLength),
  string_length(String, Length),
  Length =< MaxLength, !.
string_truncate(String, MaxLength, Prefix) :-
  string_prefix(String, MaxLength, Prefix).
