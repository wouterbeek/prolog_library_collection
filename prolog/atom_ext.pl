:- encoding(utf8).
:- module(
  atom_ext,
  [
    atom_capitalize/2, % +Original, ?Capitalized
    atom_ellipsis/3,   % +Original, ?MaxLength, ?Ellipsed
    atom_postfix/2,    % +Original, ?Postfix
    atom_postfix/3,    % +Original, ?Length, ?Postfix
   %atom_prefix/2,     % +Original, ?Prefix
    atom_prefix/3,     % +Original, ?Length, ?Prefix
    atom_strip/2,      % +Original, ?Stripped
    atom_strip/3,      % +Original, +Strip, ?Stripped
    atom_terminator/3, % +Original, +Terminator, ?Terminated
    atom_truncate/3,   % +Original, +MaxLength, ?Truncated
    sub_atom/2         % ?Atom, ?Subatom
  ]
).

/** <module> Atom extensions

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(plunit)).





%! atom_capitalize(+Original:atom, +Capitalized:atom) is semidet.
%! atom_capitalize(+Original:atom, -Capitalized:atom) is det.
%
% Succeeds if Capitalized is a copy of Original where the first
% character is in upper case (if applicable).
%
% If the first character of Atom is already in upper case then
% Capitalized is a plain copy of Atom.

atom_capitalize('', '').
atom_capitalize(Atom, Capitalized) :-
  atom_codes(Atom, [H1|T]),
  to_upper(H1, H2),
  atom_codes(Capitalized, [H2|T]).

:- begin_tests(atom_capitalize).

test('atom_capitalize(+,+)', [forall(atom_capitalize_test(Original,Capitalized))]) :-
  atom_capitalize(Original, Capitalized).
test('atom_capitalize(+,-)', [forall(atom_capitalize_test(Original,Capitalized))]) :-
  atom_capitalize(Original, Capitalized0),
  assertion(Capitalized == Capitalized0).

atom_capitalize_test(monkey, 'Monkey').
% Atoms with a first character that does not have a capital variant.
atom_capitalize_test('123', '123').
atom_capitalize_test(' a ', ' a ').
% Atom with a first character this is non-ASCII.
atom_capitalize_test('ὰ', 'Ὰ').

:- end_tests(atom_capitalize).



%! atom_ellipsis(+Original:atom, +MaxLength:between(2,inf), +Ellipsed:atom) is semidet.
%! atom_ellipsis(+Original:atom, +MaxLength:between(2,inf), -Ellipsed:atom) is semidet.
%! atom_ellipsis(+Original:atom, -MaxLength:between(2,inf), -Ellipsed:atom) is nondet.
%
% Succeeds if Ellipsed is like Orginal, but has ellipsis applied in
% order to have MaxLength.  If Original is not longer than MaxLength,
% Orignal and Ellipsed are the same.
%
% For mode `(+,-,-)' the enumeration order prioritizes shorter atoms:
%
% ```
% ?- atom_ellipsis(monkey, Length, Ellipsed).
% Length = 2,
% Ellipsed = 'm…' ;
% Length = 3,
% Ellipsed = 'mo…' ;
% Length = 4,
% Ellipsed = 'mon…' ;
% Length = 5,
% Ellipsed = 'monk…' ;
% Length = 6,
% Ellipsed = monkey.
% ```
%
% @see string_ellipsis/3 provides the same functionality for strings.

atom_ellipsis(Original, MaxLength, Ellipsed) :-
  atom_length(Original, Length),
  (   between(2, Length, MaxLength)
  *-> (   MaxLength =:= Length
      ->  Ellipsed = Original
      ;   TruncateLength is MaxLength - 1,
          atom_truncate(Original, TruncateLength, Truncated),
          atomic_concat(Truncated, "…", Ellipsed)
      )
  ;   must_be(between(2,inf), MaxLength),
      Ellipsed = Original
  ).

:- begin_tests(atom_ellipsis).

test('atom_ellipsis(+,+,+)', [forall(atom_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  atom_ellipsis(Original, MaxLength, Ellipsed).
test('atom_ellipsis(+,+,-) err', [error(type_error(between(2,inf),_MaxLength))]) :-
  atom_ellipsis(monkey, 1, _).
test('atom_ellipsis(+,+,-)', [forall(atom_ellipsis_test(Original,MaxLength,Ellipsed))]) :-
  atom_ellipsis(Original, MaxLength, Ellipsed0),
  assertion(Ellipsed == Ellipsed0).
test('atom_ellipsis(+,-,-)', [all(MaxLength-Ellipsed == [2-'a…',3-'ab…',4-abcd])]) :-
  atom_ellipsis(abcd, MaxLength, Ellipsed).

atom_ellipsis_test(monkey, 3, 'mo…').

:- end_tests(atom_ellipsis).



%! atom_postfix(+Original:atom, +Postfix:atom) is semidet.
%! atom_postfix(+Original:atom, -Postfix:atom) is multi.
%! atom_postfix(+Original:atom, +Length:nonneg, +Postfix:atom) is semidet.
%! atom_postfix(+Original:atom, +Length:nonneg, -Postfix:atom) is semidet.
%! atom_postfix(+Original:atom, -Length:nonneg, +Postfix:atom) is semidet.
%! atom_postfix(+Original:atom, -Length:nonneg, -Postfix:atom) is multi.
%
% Succeeds if Postfix is a postfix of Original with consisting of
% Length characters.
%
% For mode `(+,-,-)' the enumeration order prioritizes longer atoms.
%
% @see string_postfix/[2,3] provides the same functionality for strings.

atom_postfix(Atom, Postfix) :-
  atom_postfix(Atom, _, Postfix).


atom_postfix(Atom, Length, Postfix) :-
  sub_atom(Atom, _, Length, 0, Postfix).

:- begin_tests(atom_postfix).

test('atom_postfix(+,+,+)', [forall(test_atom_postfix(Original,Length,Postfix))]) :-
  atom_postfix(Original, Length, Postfix).
test('atom_postfix(+,+,-)', [forall(test_atom_postfix(Original,Length,Postfix))]) :-
  atom_postfix(Original, Length, Postfix0),
  assertion(Postfix == Postfix0).
test('atom_postfix(+,-,+)', [forall(test_atom_postfix(Original,Length,Postfix))]) :-
  atom_postfix(Original, Length0, Postfix),
  assertion(Length == Length0).
test('atom_postfix(+,-,-)', [all(Length-Postfix == [4-abcd,3-bcd,2-cd,1-d,0-''])]) :-
  atom_postfix(abcd, Length, Postfix).

test_atom_postfix(abcd, 4, abcd).
test_atom_postfix(abcd, 3, bcd).
test_atom_postfix(abcd, 2, cd).
test_atom_postfix(abcd, 1, d).
test_atom_postfix(abcd, 0, '').

:- end_tests(atom_postfix).



%! atom_prefix(+Original:atom, +Length:nonneg, +Prefix:atom) is semidet.
%! atom_prefix(+Original:atom, +Length:nonneg, -Prefix:atom) is semidet.
%! atom_prefix(+Original:atom, -Length:nonneg, +Prefix:atom) is semidet.
%! atom_prefix(+Original:atom, -Length:nonneg, -Prefix:atom) is multi.
%
% Succeeds if Prefix is a prefix of Original consisting of Length
% characters.
%
% Fails when Length is greater than the length of Original.
%
% For mode `(+,-,-)' the enumeration order prioritizes shorter atoms.
%
% @see string_prefix/[2,3] provides the same functionality for strings.

atom_prefix(Atom, Length, Prefix) :-
  sub_atom(Atom, 0, Length, _, Prefix).

:- begin_tests(atom_prefix).

test('atom_prefix(+,+,+)', [forall(test_atom_prefix(Original,Length,Prefix))]) :-
  atom_prefix(Original, Length, Prefix).
test('atom_prefix(+,+,-)', [forall(test_atom_prefix(Original,Length,Prefix))]) :-
  atom_prefix(Original, Length, Prefix0),
  assertion(Prefix == Prefix0).
test('atom_prefix(+,-,+)', [forall(test_atom_prefix(Original,Length,Prefix))]) :-
  atom_prefix(Original, Length0, Prefix),
  assertion(Length == Length0).
test('atom_prefix(+,-,-)', [all(Length-Prefix == [0-'',1-a,2-ab,3-abc,4-abcd])]) :-
  atom_prefix(abcd, Length, Prefix).

test_atom_prefix(abcd, 0, '').
test_atom_prefix(abcd, 1, a).
test_atom_prefix(abcd, 2, ab).
test_atom_prefix(abcd, 3, abc).
test_atom_prefix(abcd, 4, abcd).

:- end_tests(atom_prefix).



%! atom_strip(+Original:atom, +Stripped:atom) is det.
%! atom_strip(+Original:atom, -Stripped:atom) is det.
%! atom_strip(+Original:atom, +Strip:list(char), +Stripped:atom) is semidet.
%! atom_strip(+Original:atom, +Strip:list(char), -Stripped:atom) is det.
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
%      Original atom.  The default includes: horizontal tab, newline,
%      space, NO-BREAK SPACE (0xa0).
%
% @see string_strip/[2,3] provides the same functionality for strings.

atom_strip(Original, Stripped) :-
  atom_strip(Original, ['\r','\t','\n',' ','\u00a0'], Stripped).


atom_strip(A1, Strip, A3) :-
  atom_strip_begin_(A1, Strip, A2),
  atom_strip_end_(A2, Strip, A3).

atom_strip_begin_(A1, Strip, A3) :-
  member(Char, Strip),
  atom_concat(Char, A2, A1), !,
  atom_strip_begin_(A2, Strip, A3).
atom_strip_begin_(A, _, A).

atom_strip_end_(A1, Strip, A3) :-
  member(Char, Strip),
  atom_concat(A2, Char, A1), !,
  atom_strip_end_(A2, Strip, A3).
atom_strip_end_(A, _, A).

:- begin_tests(atom_strip).

test('atom_strip(+,+,+)', [forall(test_atom_strip(Original,Strip,Stripped))]) :-
  atom_strip(Original, Strip, Stripped).
test('atom_strip(+,+,-)', [forall(test_atom_strip(Original,Strip,Stripped))]) :-
  atom_strip(Original, Strip, Stripped0),
  assertion(Stripped == Stripped0).

test_atom_strip(' a ', [' '], a).
test_atom_strip(' a ', [' ',a], '').
test_atom_strip('', [' '], '').
test_atom_strip(' ', [], ' ').

:- end_tests(atom_strip).



%! atom_terminator(+Original:atom, +Terminator:char, +Terminated:atom) is semidet.
%! atom_terminator(+Original:atom, +Terminator:char, -Terminated:atom) is det.
%
% Succeeds if Terminated is a copy of Original which is ensured to end
% with the Terminator character.

atom_terminator(Original, Terminator, Terminated) :-
  atom_chars(Original, Chars1),
  (Chars1 == [] -> true ; once(append(_, [Last], Chars1))),
  (   Last == Terminator
  ->  Terminated = Original
  ;   append(Chars1, [Terminator], Chars2),
      atom_chars(Terminated, Chars2)
  ).

:- begin_tests(atom_terminator).

test('atom_terminator(+,+,+)', [forall(test_atom_terminator(Original,Terminator,Terminated))]) :-
  atom_terminator(Original, Terminator, Terminated).
test('atom_terminator(+,+,-)', [forall(test_atom_terminator(Original,Terminator,Terminated))]) :-
  atom_terminator(Original, Terminator, Terminated0),
  assertion(Terminated == Terminated0).

test_atom_terminator('https://abc.com', /, 'https://abc.com/').
test_atom_terminator('https://abc.com/', /, 'https://abc.com/').
test_atom_terminator(/, /, /).
test_atom_terminator('', /, /).

:- end_tests(atom_terminator).



%! atom_truncate(+Original:atom, +MaxLength:nonneg, +Truncated:atom) is semidet.
%! atom_truncate(+Original:atom, +MaxLength:nonneg, -Truncated:atom) is det.
%
% @see Like atom_prefix/3, but the Truncated atom is the Original
%      atom in case MaxLength exceeds the Original atom length.
%
% @see string_truncate/3 provides the same functionality for strings.

atom_truncate(Original, MaxLength, Truncated) :-
  atom_length(Original, Length),
  (   Length > MaxLength
  ->  atom_prefix(Original, MaxLength, Truncated)
  ;   Truncated = Original
  ).

:- begin_tests(atom_truncate).

test('atom_truncate(+,+,+)', [forall(atom_truncate_test(Original,MaxLength,Truncated))]) :-
  atom_truncate(Original, MaxLength, Truncated).
test('atom_truncate(+,+,-)', [forall(atom_truncate_test(Original,MaxLength,Truncated))]) :-
  atom_truncate(Original, MaxLength, Truncated0),
  assertion(Truncated == Truncated0).
test('atom_truncate(+,-,-)', [error(instantiation_error,_Context)]) :-
  atom_truncate(abcd, _MaxLength, abcd).

atom_truncate_test(monkey, 3, mon).
atom_truncate_test(monkey, 1 000, monkey).

:- end_tests(atom_truncate).



%! sub_atom(+Atom:atom, +Subatom:atom) is semidet.
%! sub_atom(+Atom:atom, -Subatom:atom) is nondet.

sub_atom(Atom, Subatom) :-
  sub_atom(Atom, _, _, _, Subatom).
