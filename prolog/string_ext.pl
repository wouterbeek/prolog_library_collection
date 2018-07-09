:- encoding(utf8).
:- module(
  string_ext,
  [
    read_string/2,        % +In, -String
    string_ellipsis/3,    % +Original, ?MaxLength, ?Ellipsed
    string_list_concat/2, % +Strings, ?String
    string_list_concat/3, % ?Strings, ?Separator, ?String
    string_postfix/2,     % +Original, ?Postfix
    string_postfix/3,     % +Original, ?Length, ?Postfix
    string_prefix/2,      % +Original, ?Prefix
    string_prefix/3,      % +Original, ?Length, ?Prefix
    string_strip/3,       % +Original, +Chars, ?Stripped
    string_truncate/3     % +Original, +MaxLength, ?Truncated
  ]
).

/** <module> String extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).





%! read_string(+In:stream, -String:string) is det.
%
% Wrapper for read_string/3 when the number of read characters does
% not matter.

read_string(In, String) :-
  read_string(In, _, String).



%! string_ellipsis(+Original:string, +MaxLength:between(2,inf), +Ellipsed:string) is semidet.
%! string_ellipsis(+Original:string, +MaxLength:between(2,inf), -Ellipsed:string) is semidet.
%! string_ellipsis(+Original:string, -MaxLength:between(2,inf), -Ellipsed:string) is nondet.
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
  ;   must_be(between(2,inf), MaxLength),
      Ellipsed = Original
  ).

:- begin_tests(string_ellipsis).

test('string_ellipsis(+,+,+)', [forall(string_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  string_ellipsis(Original, MaxLength, Ellipsed).
test('string_ellipsis(+,+,-) err', [error(type_error(between(2,inf),_MaxLength))]) :-
  string_ellipsis(monkey, 1, _).
test('string_ellipsis(+,+,-)', [forall(string_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  string_ellipsis(Original, MaxLength, Ellipsed0),
  assertion(Ellipsed == Ellipsed0).
test('string_ellipsis(+,-,-)', [all(MaxLength-Ellipsed == [2-"a…",3-"ab…",4-"abcd"])]) :-
  string_ellipsis("abcd", MaxLength, Ellipsed).

string_ellipsis_test("monkey", 3, "mo…").

:- end_tests(string_ellipsis).



%! string_list_concat(+Strings:list(string), +String:string) is semidet.
%! string_list_concat(+Strings:list(string), -String:string) is det.
%! string_list_concat(+Strings:list(string), +Separator:string, +String:string) is semidet.
%! string_list_concat(+Strings:list(string), +Separator:string, -String:string) is det.
%! string_list_concat(-Strings:list(string), +Separator:string, +String:string) is semidet.
%
% @see Like atomic_list_concat/3, but for strings.

string_list_concat(Strings, String) :-
  atomics_to_string(Strings, String).


string_list_concat(Strings, Separator, String):-
  (   ground(Strings-Separator)
  ->  atomics_to_string(Strings, Separator, String)
  ;   maplist(atom_string, [Separator0,String0], [Separator,String]),
      atomic_list_concat(Strings0, Separator0, String0),
      maplist(atom_string, Strings0, Strings)
  ).

:- begin_tests(string_list_concat).

test(ambiguïty) :-
  string_list_concat([], "a", "").
test('string_list_concat(+,+,+)', [forall(test_string_list_concat(Strings,Separator,String))]) :-
  string_list_concat(Strings, Separator, String).
test('string_list_concat(+,+,-)', [forall(test_string_list_concat(Strings,Separator,String))]) :-
  string_list_concat(Strings, Separator, String0),
  assertion(String == String0).
test('string_list_concat(-,+,+)', [forall(test_string_list_concat(Strings,Separator,String))]) :-
  string_list_concat(Strings0, Separator, String),
  assertion(Strings == Strings0).

test_string_list_concat([""], "a", "").
test_string_list_concat(["","","",""], "a", "aaa").

:- end_tests(string_list_concat).



%! string_postfix(+Original:string, +Postfix:string) is semidet.
%! string_postfix(+Original:string, -Postfix:string) is multi.
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

string_postfix(Original, Postfix) :-
  string_postfix(Original, _, Postfix).


string_postfix(Original, Length, Postfix) :-
  sub_string(Original, _, Length, 0, Postfix).

:- begin_tests(string_postfix).

test('string_postfix(+,+,+)', [forall(test_string_postfix(Original,Length,Postfix))]) :-
  string_postfix(Original, Length, Postfix).
test('string_postfix(+,+,-)', [forall(test_string_postfix(Original,Length,Postfix))]) :-
  string_postfix(Original, Length, Postfix0),
  assertion(Postfix == Postfix0).
test('string_postfix(+,-,+)', [forall(test_string_postfix(Original,Length,Postfix))]) :-
  string_postfix(Original, Length0, Postfix),
  assertion(Length == Length0).
test('string_postfix(+,-,-)', [all(Length-Postfix == [4-"abcd",3-"bcd",2-"cd",1-"d",0-""])]) :-
  string_postfix("abcd", Length, Postfix).

test_string_postfix("abcd", 4, "abcd").
test_string_postfix("abcd", 3, "bcd").
test_string_postfix("abcd", 2, "cd").
test_string_postfix("abcd", 1, "d").
test_string_postfix("abcd", 0, "").

:- end_tests(string_postfix).



%! string_prefix(+Original:string, +Prefix:string) is semidet.
%! string_prefix(+Original:string, -Prefix:string) is multi.
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

string_prefix(Original, Prefix) :-
  string_prefix(Original, _, Prefix).


string_prefix(Original, Length, Prefix) :-
  sub_string(Original, 0, Length, _, Prefix).

:- begin_tests(string_prefix).

test('string_prefix(+,+,+)', [forall(test_string_prefix(Original,Length,Prefix))]) :-
  string_prefix(Original, Length, Prefix).
test('string_prefix(+,+,-)', [forall(test_string_prefix(Original,Length,Prefix))]) :-
  string_prefix(Original, Length, Prefix0),
  assertion(Prefix == Prefix0).
test('string_prefix(+,-,+)', [forall(test_string_prefix(Original,Length,Prefix))]) :-
  string_prefix(Original, Length0, Prefix),
  assertion(Length == Length0).
test('string_prefix(+,-,-)', [all(Length-Prefix == [0-"",1-"a",2-"ab",3-"abc",4-"abcd"])]) :-
  string_prefix("abcd", Length, Prefix).

test_string_prefix("abcd", 0, "").
test_string_prefix("abcd", 1, "a").
test_string_prefix("abcd", 2, "ab").
test_string_prefix("abcd", 3, "abc").
test_string_prefix("abcd", 4, "abcd").

:- end_tests(string_prefix).



%! string_strip(+Original:string, +Chars:list(char), +Stripped:string) is semidet.
%! string_strip(+Original:string, +Chars:list(char), -Stripped:string) is det.
%
% @arg Chars is a string of charaters that will be stripped from the
%      Original string.
%
% @arg Stripped is the Original string, but without any of the leading
%      and/or trailing characters that appear in Chars.

string_strip(Original, Chars, Stripped) :-
  string_chars(String, Chars),
  split_string(Original, "", String, [Stripped]).

:- begin_tests(string_strip).

test('string_strip(+,+,+)', [forall(test_string_strip(Original,Strip,Stripped))]) :-
  string_strip(Original, Strip, Stripped).
test('string_strip(+,+,-)', [forall(test_string_strip(Original,Strip,Stripped))]) :-
  string_strip(Original, Strip, Stripped0),
  assertion(Stripped == Stripped0).

test_string_strip(" a ", [' '], "a").
test_string_strip(" a ", [' ',a], "").
test_string_strip("", [' '], "").
test_string_strip(" ", [], " ").

:- end_tests(string_strip).



%! string_truncate(+Original:string, +MaxLength:nonneg, +Truncated:string) is semidet.
%! string_truncate(+Original:string, +MaxLength:nonneg, -Truncated:string) is det.
%
% @see Like string_prefix/3, but the Truncated string is the Original
%      string in case MaxLength exceeds the Original string length.

string_truncate(Original, MaxLength, Truncated) :-
  string_length(Original, Length),
  (   Length > MaxLength
  ->  string_prefix(Original, MaxLength, Truncated)
  ;   Truncated = Original
  ).

:- begin_tests(string_truncate).

test('string_truncate(+,+,+)', [forall(string_truncate_test(Original,MaxLength,Truncated))]) :-
  string_truncate(Original, MaxLength, Truncated).
test('string_truncate(+,+,-)', [forall(string_truncate_test(Original,MaxLength,Truncated))]) :-
  string_truncate(Original, MaxLength, Truncated0),
  assertion(Truncated == Truncated0).
test('string_truncate(+,-,-)', [error(instantiation_error,_Context)]) :-
  string_truncate("abcd", _MaxLength, "abcd").

string_truncate_test("monkey", 3, "mon").
string_truncate_test("monkey", 1 000, "monkey").

:- end_tests(string_truncate).
