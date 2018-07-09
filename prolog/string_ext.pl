:- encoding(utf8).
:- module(
  string_ext,
  [
    read_string/2,        % +In, -String
    string_ellipsis/3,    % +Original, ?MaxLength, -Ellipsed
    string_list_concat/2, % +Strings, -String
    string_list_concat/3, % ?Strings, ?Separator, ?String
    string_postfix/3,     % +Original, ?Length, ?Postfix
    string_prefix/3,      % +Original, ?Length, ?Prefix
    string_strip/3,       % +Original, +Chars, -Stripped
    string_truncate/3     % +Original, +MaxLength, -Truncated
  ]
).

/** <module> String extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(error)).





%! read_string(+In:stream, -String:string) is det.
%
% Wrapper for read_string/3 when the number of read characters does
% not matter.

read_string(In, String) :-
  read_string(In, _, String).



%! string_ellipsis(+Original:string, +MaxLength:nonneg, +Ellipsed:string) is semidet.
%! string_ellipsis(+Original:string, +MaxLength:nonneg, -Ellipsed:string) is semidet.
%! string_ellipsis(+Original:string, -MaxLength:nonneg, -Ellipsed:string) is nondet.
%
% Succeeds if Ellipsed is like Orginal, but has ellipsis applied in
% order to have MaxLength.  If Original is not longer than MaxLength,
% Orignal and Ellipsed are the same.
%
% ```
% ?- string_ellipsis("monkey", Length, Ellipsed).
% Length = 2,
% Ellipsed = "m…" ;
% Length = 3,
% Ellipsed = "mo…" ;
% Length = 4,
% Ellipsed = "mon…" ;
% Length = 5,
% Ellipsed = "monk…" ;
% Length = 6,
% Ellipsed = "monkey".
% ```
%
% @see atom_ellipsis/3 provides the same functionality for atoms.

string_ellipsis(Original, MaxLength, Ellipsed) :-
  string_length(Original, Length),
  (   between(2, Length, MaxLength)
  *-> (   MaxLength =:= Length
      ->  Ellipsed = Original
      ;   PrefixLength is MaxLength - 1,
          string_prefix(Original, PrefixLength, Prefix),
          string_concat(Prefix, "…", Ellipsed)
      )
  ;   Ellipsed = Original
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



%! string_postfix(+Original:string, +Length:nonneg, +Postfix:string) is semidet.
%! string_postfix(+Original:string, +Length:nonneg, -Postfix:string) is semidet.
%! string_postfix(+Original:string, -Length:nonneg, +Postfix:string) is semidet.
%! string_postfix(+Original:string, -Length:nonneg, -Postfix:string) is multi.
%
% @arg Length is the number of characters in the Postfix string.
%
% @arg Postfix is the postfix of the Original string that has Length
%      characters.
%
% Fails in case Length is higher than the length of string String.

string_postfix(Original, Length, Postfix) :-
  sub_string(Original, _, Length, 0, Postfix).



%! string_prefix(+Original:string, +Length:nonneg, +Prefix:string) is semidet.
%! string_prefix(+Original:string, +Length:nonneg, -Prefix:string) is semidet.
%! string_prefix(+Original:string, -Length:nonneg, +Prefix:string) is semidet.
%! string_prefix(+Original:string, -Length:nonneg, -Prefix:string) is multi.
%
% @arg Length is the number of characters in the Prefix string.
%
% @arg Prefix is the prefix of the Original string that has Length
%      characters.
%
% Fails in case Length exceeds the Original string length.

string_prefix(Original, Length, Prefix) :-
  sub_string(Original, 0, Length, _, Prefix).



%! string_strip(+Original:string, +Chars:string, -Stripped:string) is det.
%
% @arg Chars is a string of charaters that will be stripped from the
%      Original string.
%
% @arg Stripped is the Original string, but without any of the leading
%      and/or trailing characters that appear in Chars.

string_strip(Original, Chars, Stripped) :-
  split_string(Original, "", Chars, [Stripped]).



%! string_truncate(+Original:string, +MaxLength:nonneg, -Truncated:string) is det.
%
% Like string_prefix/3, but the Truncated string is the Original
% string in case MaxLength exceeds the Original string length.

string_truncate(Original, MaxLength, Prefix) :-
  string_prefix(Original, MaxLength, Prefix).
string_truncate(Original, _, Original).
