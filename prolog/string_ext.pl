:- encoding(utf8).
:- module(
  string_ext,
  [
    max_string_length/2,  % +Strings, -Max
    message_lines/3,      % +Message, +MaxLength, -Lines
    read_string/2,        % +In, -String
    split_string/3,       % +String, +SepChars, -SubStrings
    string_code/2,        % ?String, ?Code
    string_ellipsis/3,    % +Original, ?MaxLength, ?Ellipsed
    string_list_concat/2, % +Strings, ?String
    string_list_concat/3, % ?Strings, ?Separator, ?String
    string_postfix/2,     % +Original, ?Postfix
    string_postfix/3,     % +Original, ?Length, ?Postfix
    string_prefix/2,      % +Original, ?Prefix
    string_prefix/3,      % +Original, ?Length, ?Prefix
    string_strip/2,       % +Original, ?Stripped
    string_strip/3,       % +Original, +Strip, ?Stripped
    string_truncate/3,    % +Original, +MaxLength, ?Truncated
    words_lines/3,        % +Words, +MaxLength, -Lines
    words_lines/4         % +Words, +MaxLength, +Separator, -Lines
  ]
).

/** <module> Extended support for strings

Extends the string support in the SWI-Prolog standard library.

*/

:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).



%! max_string_length(+Strings:list(string), -Max:nonneg) is det.

max_string_length([], 0) :- !.
max_string_length(Strings, Len) :-
  aggregate_all(
    max(Len0),
    (
      member(String, Strings),
      string_length(String, Len0)
    ),
    Len
  ).



%! message_lines(+Message:string,
%!               +MaxLength:positive_integer,
%!               -Lines:list(string)) is det.

message_lines(Message, Max, Lines) :-
  string_list_concat(Words, ' ', Message),
  words_lines(Words, Max, Lines).



%! read_string(+In:istream, -String:string) is det.
%
% Wrapper for read_string/3 when the number of read characters does
% not matter.

read_string(In, String) :-
  read_string(In, _, String).



%! split_string(+String:string, +SepChars:string, +SubStrings:list(string)) is semidet.
%! split_string(+String:string, +SepChars:string, -SubStrings:list(string)) is det.

split_string(String, SepChars, SubStrings) :-
  split_string(String, SepChars, "", SubStrings).



%! string_code(+String:string, +Code:atom) is semidet.
%! string_code(+String:string, -Code:atom) is det.
%! string_code(-String:string, +Code:atom) is det.

string_code(String, Code) :-
  ground(String), !,
  atom_string(Char, String),
  char_code(Char, Code).
string_code(String, Code) :-
  ground(Code), !,
  char_code(Char, Code),
  atom_string(Char, String).
string_code(String, Code) :-
  instantiation_error([String,Code]).



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

string_ellipsis(String, MaxLength, String) :-
  MaxLength == inf, !.
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
test('string_ellipsis(+,+,-)', [forall(string_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  string_ellipsis(Original, MaxLength, Ellipsed0),
  assertion(Ellipsed == Ellipsed0).
test('string_ellipsis(+,+,-) err_1', [error(type_error(between(2,inf),MaxLength))]) :-
  member(MaxLength, [-1,0,1,'0']),
  string_ellipsis("monkey", MaxLength, "").
test('string_ellipsis(+,-,-)', [forall(string_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  string_ellipsis(Original, MaxLength, Ellipsed).

string_ellipsis_test("monkey", 2, "m…").
string_ellipsis_test("monkey", 3, "mo…").
string_ellipsis_test("monkey", 4, "mon…").
string_ellipsis_test("monkey", 5, "monk…").
string_ellipsis_test("monkey", 6, "monkey").
string_ellipsis_test("monkey", 7, "monkey").
string_ellipsis_test("monkey", inf, "monkey").

:- end_tests(string_ellipsis).



%! string_list_concat(+Strings:list(string), +String:string) is semidet.
%! string_list_concat(+Strings:list(string), -String:string) is det.
%! string_list_concat(+Strings:list(string), +Separator:string, +String:string) is semidet.
%! string_list_concat(+Strings:list(string), +Separator:string, -String:string) is det.
%! string_list_concat(-Strings:list(string), +Separator:string, +String:string) is semidet.
%
% @see atomic_list_concat/3 provides the same functionality for atoms.

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
%
% @see atom_postfix/[2,3] provides the same functionality for atoms.

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
% Succeeds if Prefix is a prefix of Original consisting of Length
% characters.
%
% Fails in case Length exceeds the Original string length.
%
% @arg Length is the number of characters in the Prefix string.
%
% @arg Prefix is the prefix of the Original string that has Length
%      characters.
%
% @see atom_prefix/[2,3] provides the same functionality for atoms.

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



%! string_strip(+Original:string, +Stripped:string) is semidet.
%! string_strip(+Original:string, -Stripped:string) is det.
%! string_strip(+Original:string, +Strip:list(char), +Stripped:string) is semidet.
%! string_strip(+Original:string, +Strip:list(char), -Stripped:string) is det.
%
% Succeeds if Stripped is a copy of Original where leading and
% trailing characters in Strip have been removed.
%
% Notice that the order in which the characters in Strip are specified
% is significant.
%
% The default Strip characters are space, newline and horizontal tab.
%
% @arg Strip is a list of charaters that will be stripped from the
%      Original string.  The default includes: horizontal tab,
%      newline, space, NO-BREAK SPACE (0xa0).
%
% @see atom_strip/[2,3] provides the same functionality for atoms.

string_strip(Original, Stripped) :-
  string_strip(Original, ['\t','\n',' ','\u00a0'], Stripped).


string_strip(Original, Strip0, Stripped) :-
  string_chars(Strip, Strip0),
  split_string(Original, "", Strip, [Stripped]).

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
%
% @see atom_truncate/3 provides the same functionality for atoms.

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



%! words_lines(+Words:list(string),
%!             +MaxLength:positive_integer,
%!             -Lines:list(string)) is det.
%! words_lines(+Words:list(string),
%!             +MaxLength:positive_integer,
%!             +Separator:string,
%!             -Lines:list(string)) is det.
%
% Splits the given list of words into lines that do not exceed
% MaxLength.

words_lines(Words, Max, Lines) :-
  words_lines(Words, Max, ' ', Lines).


words_lines(Words, Max, Sep, Lines) :-
  string_length(Sep, SepLen),
  words_lines_(Words, SepLen, Max, Wordss),
  maplist(
    {Sep}/[Strings,String]>>string_list_concat(Strings, Sep, String),
    Wordss,
    Lines
  ).

words_lines_([], _, _, []) :- !.
words_lines_(Words1, SepLen, Max, [Line|Lines]) :-
  words_line_(Words1, SepLen, Max, Max, Line, Words2),
  words_lines_(Words2, SepLen, Max, Lines).

words_line_([Word|Words], _, _, Max, _, [Word], Words) :-
  string_length(Word, Length),
  Length >= Max, !.
words_line_([Word|Words], SepLen, Remaining1, Max, [Word|Line], WordsSol) :-
  string_length(Word, Length),
  Length =< Remaining1, !,
  Remaining2 #= Remaining1 - Length - SepLen,
  words_line_(Words, SepLen, Remaining2, Max, Line, WordsSol).
words_line_(Words, _, _, _, [], Words).
