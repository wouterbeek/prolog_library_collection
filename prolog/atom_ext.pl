:- module(
  atom_ext,
  [
    atom_capitalize/2, % +Atom,   -Capitalized
    atom_ellipsis/3,   % +Atom, ?Length, ?Ellipsis
   %atom_prefix/2,     % +Atom, ?Sub
    atom_prefix/3,     % +Atom, ?Length, ?Sub
    atom_strip/2,      % +Atom, -StrippedAtom
    atom_strip/3,      % +StripChars, +Atom, -StrippedAtom
    atom_to_file/2,    % +Atom, +FileSpec
    atom_truncate/3,   % +Atom, +MaxLength, -Truncated
    is_empty_atom/1    % @Term
  ]
).

/** <module> Atom extensions

@author Wouter Beek
@version 2017/04-2017/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(uri/uri_ext)).





%! atom_capitalize(+Atom, -Capitalized) is det.
%
% Succeeds if Capitalized is a copy of Atom where the first character
% is in upper case.
%
% If the first character of Atom is already in upper case then
% Capitalized is a plain copy of Atom.

atom_capitalize('', '').
atom_capitalize(A1, A2) :-
  atom_codes(A1, [H1|T]),
  to_upper(H1, H2),
  atom_codes(A2, [H2|T]).



%! atom_ellipsis(+Atom, +Length, +Ellipsis) is semidet.
%! atom_ellipsis(+Atom, +Length, -Ellipsis) is semidet.
%! atom_ellipsis(+Atom, -Length, -Ellipsis) is nondet.
%
% ```
% ?- atom_ellipsis(monkey, N, X).
% N = 2,
% X = 'm…' ;
% N = 3,
% X = 'mo…' ;
% N = 4,
% X = 'mon…' ;
% N = 5,
% X = 'monk…' ;
% N = 6,
% X = monkey.
% ```

atom_ellipsis(Atom, ELength, Ellipsis) :-
  atom_length(Atom, Length),
  (   ELength = inf
  ->  Ellipsis = Atom
  ;   between(2, Length, ELength)
  *-> (   ELength =:= Length
      ->  Ellipsis = Atom
      ;   TLength is ELength - 1,
          atom_truncate(Atom, TLength, Truncated),
          atomic_concat(Truncated, "…", Ellipsis)
      )
  ;   Ellipsis = Atom
  ).



%! atom_prefix(+Atom, +Length, +Sub) is semidet.
%! atom_prefix(+Atom, +Length, -Sub) is semidet.
%! atom_prefix(+Atom, -Length, +Sub) is semidet.
%! atom_prefix(+Atom, -Length, -Sub) is multi.
%
% Sub is the prefix of Atom that has length Length.
%
% Fails in case Length is higher than the length of Atom.

atom_prefix(Atom, Length, Sub) :-
  sub_atom(Atom, 0, Length, _, Sub).



%! atom_strip(+Atom, -StrippedAtom) is det.
%! atom_strip(+StripChars, +Atom, -StrippedAtom) is det.
%
% Return Atom with any occurrens of PadChars remove from the front
% and/or back.
%
% Notice that the order in which the PadChars occur is significant.
%
% The default PadChars are space, newline and horizontal tab.

atom_strip(A1, A2) :-
  atom_strip([' ','\n','\t'], A1, A2).


atom_strip(StripChars, A1, A3) :-
  atom_strip_begin0(StripChars, A1, A2),
  atom_strip_end0(StripChars, A2, A3).

atom_strip_begin0(StripChars, A1, A3) :-
  member(StripChar, StripChars),
  atom_concat(StripChar, A2, A1), !,
  atom_strip_begin0(StripChars, A2, A3).
atom_strip_begin0(_, A, A).

atom_strip_end0(StripChars, A1, A3) :-
  member(StripChar, StripChars),
  atom_concat(A2, StripChar, A1), !,
  atom_strip_end0(StripChars, A2, A3).
atom_strip_end0(_, A, A).



%! atom_to_file(+Content:atom, +FileSpec:term) is det.

atom_to_file(Content, FileSpec) :-
  setup_call_cleanup(
    uri_open(FileSpec, write, In),
    format(In, '~a', [Content]),
    stream_close(In)
  ).



%! atom_truncate(+Atom:atom, +MaxLenght:nonneg, -Truncated:atom) is det.
%
% Return a truncated version of the given atom.  MaxLength is the
% exact maximum lenght of the truncated atom.  Truncation will always
% result in an atom which has at most `MaxLength`.
%
% @param MaxLength must be a non-negative integer or `inf`.  When
%        `inf` the original atom is returned without truncation.
%
% @see atom_ellipsis/3 for returning a truncated atom with ellipsis
%      sign.
%
% @throws type_error

atom_truncate(A, inf, A) :- !.
atom_truncate(A, MaxLength, A) :-
  must_be(nonneg, MaxLength),
  atom_length(A, Length),
  Length =< MaxLength, !.
atom_truncate(A, MaxLength, Prefix) :-
  atom_prefix(A, MaxLength, Prefix).



%! is_empty_atom(@Term) is semidet.
%
% Succeeds only on the empty atom.

is_empty_atom(A) :-
  atom_codes(A, Cs),
  phrase(blanks, Cs).
