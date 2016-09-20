:- module(
  atom_ext,
  [
    atom_ellipsis/3,      % +Atom,   ?Len,     ?Ellipsis
    atom_ending_in/3,     % +Atom,   +Sub,     -NewAtom
    atom_postfix/2,       % +Atom,   ?Sub
    atom_postfix/3,       % +Atom,   ?Len,     ?Sub
    atom_prefix/3,        % +Atom,   ?Len,     ?Sub
    atom_to_term/2,       % +Atom,   -Term
    atom_truncate/3,      % +Atom,   +MaxLen,  -Truncated
    capitalize_atom/2,    % +Atom,   -Capitalized
    codes_atom/2,         % ?Cs,     ?Atom
    common_atom_prefix/3, % +Atom1,  +Atom2,   -Sub
    ensure_atom/2,        % +Term,   -Atom
    integer_padding/3,    % +I, +Len, -Atom
    integer_padding/4,    % +I, +Len, +PadChar, -Atom
    is_empty_atom/1,      % +Empty
    lowercase_atom/2,     % +Atom,   -Lowercased
    new_atom/2,           % +Old,    -New
    repeating_atom/3,     % +Sub,    +Repeats, -Atom
    split_atom_length/3,  % +Atom,   +Len,     -Subs
    strip_atom/2,         % +Atom,   -NewAtom
    strip_atom/3,         % +PadChars, +Atom,  -NewAtom
    strip_atom_begin/3,   % +PadChars, +Atom,  -NewAtom
    strip_atom_end/3      % +PadChars, +Atom,  -NewAtom
  ]
).
:- reexport(
  library(dialect/ifprolog),
  [
    lower_upper/2 % ?Lower, ?Upper
  ]).

/** <module> Atom extensions

Predicates for manipulating atoms.

We assume that atoms are encoded using ASCII (or an ASCII-compatible)
 encoding scheme.

# Replace

In-atom replacements can best be made using DCGs.
This requires the atom to be translated to/from a list of numeric codes.
For example, escaping spaces and grave accent (e.g. in URIs):

```prolog
:- use_module(plc(dcg/dcg_ext)).
:- use_module(plc(dcg/dcg_replace)).
% Escape space (SPACE to `%20`) and grave accent (GRAVE-ACCENT to `%60`).
atom_phrase(
  *(dcg_replace, [[32],[96]], [[37,50,48],[37,54,48]], []),
  AtomIn,
  AtomOut
)
```

# Strip

Stripping atoms of an arbitrary number of subatoms can be done using
strip_atom/3, strip_atom_begin/3, and strip_atom_end/3.

# Titlecase

Titlecase atoms can be created using upcase_atom/2.

---

@author Wouter Beek
@version 2015/07-2015/10, 2016/03-2016/06, 2016/08-2016/09
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(char_ext)).
:- use_module(library(typecheck)).





%! atom_ellipsis(+Atom, +Len, +Ellipsis) is semidet.
%! atom_ellipsis(+Atom, +Len, -Ellipsis) is semidet.
%! atom_ellipsis(+Atom, -Len, -Ellipsis) is nondet.
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

atom_ellipsis(Atom, ELen, Ellipsis) :-
  atom_length(Atom, Len),
  between(2, Len, ELen),
  (   ELen =:= Len
  ->  Ellipsis = Atom
  ;   TLen is ELen - 1,
      atom_truncate(Atom, TLen, Truncated),
      atomic_concat(Truncated, "…", Ellipsis)
  ).



%! atom_ending_in(+Atom, +Suffix, -NewAtom) is det.
% Make sure that NewAtom is similar to Atom and ends in Suffix.
%
% If Atom already ends in Suffix then NewAtom == Atom.
%
% This can for instance be used to ensure that an atom
% ends with a forward slash in the process of building file paths.

atom_ending_in(A, Suffix, A) :-
  atom_concat(_, Suffix, A), !.
atom_ending_in(A0, Suffix, A) :-
  atom_concat(A0, Suffix, A).



%! atom_postfix(+Atom,       -Sub) is multi.
%! atom_postfix(+Atom, ?Len, -Sub) is multi.

atom_postfix(Atom, Sub) :-
  atom_postfix(Atom, _, Sub).


atom_postfix(Atom, Len, Sub) :-
  atom_codes(Atom, Cs),
  append(_, SubCs, Cs),
  length(SubCs, Len),
  atom_codes(Sub, SubCs).



%! atom_prefix(+Atom, +Len, +Sub) is semidet.
%! atom_prefix(+Atom, +Len, -Sub) is semidet.
%! atom_prefix(+Atom, -Len, +Sub) is semidet.
%! atom_prefix(+Atom, -Len, -Sub) is multi.
%
% Sub is the prefix of Atom that has length Len.
%
% Fails in case Len is higher than the length of Atom.

atom_prefix(Atom, Len, Sub) :-
  sub_atom(Atom, 0, Len, _, Sub).



%! atom_to_term(+Atom, -Term) is det.
% Return the Term described by the Atom.
%
% @see Wrapper around atom_to_term/3 that omits the variable bindings.

atom_to_term(Atom, Term) :-
  atom_to_term(Atom, Term, _).



%! atom_truncate(+Atom, +MaxLen, -Truncated) is det.
%
% Return a truncated version of the given atom.  MaxLen is the exact
% maximum lenght of the truncated atom.  Truncation will always result
% in an atom which has at most `MaxLength`.
%
% @param MaxLen must be a non-negative integer or `inf`.  When `inf`
%        the original atom is returned without truncation.
%
% @see atom_ellipsis/3 for returning a truncated atom with ellipsis
%      sign.
%
% @throws type_error

atom_truncate(A, inf, A) :- !.
atom_truncate(A, MaxLen, A) :-
  must_be(nonneg, MaxLen),
  atom_length(A, Len),
  Len =< MaxLen, !.
atom_truncate(A, MaxLen, Prefix) :-
  atom_prefix(A, MaxLen, Prefix).



%! capitalize_atom(+Atom, -Capitalized) is det.
% Succeeds if Capitalized is a copy of Atom where the first character
% is in upper case.
%
% If the first character of Atom is already in upper case then
% Capitalized is a plain copy of Atom.

capitalize_atom('', '').
capitalize_atom(A1, A2) :-
  atom_codes(A1, [H1|T]),
  to_upper(H1, H2),
  atom_codes(A2, [H2|T]).



%! codes_atom(+Cs, +A) is semidet.
%! codes_atom(+Cs, -A) is det.
%! codes_atom(-Cs, +A) is det.
% Positional variant of atom_codes/2.

codes_atom(Cs, A) :-
  atom_codes(A, Cs).



%! common_atom_prefix(+Atom1, +Atom2, +Sub) is semidet.
%! common_atom_prefix(+Atom1, +Atom2, -Sub) is nondet.
% Returns the longest common prefix of the given two atoms.

common_atom_prefix(Atom1, Atom2, Sub) :-
  maplist(atom_codes, [Atom1,Atom2], [Cs1,Cs2]),
  common_list_prefix(Cs1, Cs2, SubCs),
  atom_codes(Sub, SubCs).



%! ensure_atom(+Term, -Atom) is det.

ensure_atom(A, A) :- atom(A), !.
ensure_atom(N, A) :- number(N), !, atom_number(A, N).
ensure_atom(S, A) :- string(S), !, atom_string(A, S).
ensure_atom(T, A) :- term_to_atom(T, A).



%! integer_padding(+I, +Len, -A) is det.
%! integer_padding(+I, +Len, +PadChar, -A) is det.
%
% Returns a formatted representation of the given integer that is
% exactly the given number of characters long.
%
% Fails in case the length of the formatted integer exceeds the given
% length.
%
% @tbd See whether this can be done using format/2 tab stops,
% http://www.swi-prolog.org/pldoc/doc_for?object=format/2.

integer_padding(I, L, Out) :-
  integer_padding(I, L, '0', Out).


integer_padding(I, L, PadChar, Out) :-
  atom_length(I, IL),
  ZeroLength is L - IL,
  repeating_atom(PadChar, ZeroLength, Zeros),
  atomic_concat(Zeros, I, Out).



%! is_empty_atom(+Empty) is semidet.
% Succeeds only on the empty atom.

is_empty_atom(A) :-
  atom(A),
  atom_phrase(blanks, A).



%! lowercase_atom(+Atom, -Lowercased) is det.
% Succeeds if Lowercased is a copy of Atom where the first character
% is in upper case.
%
% If the first character of Atom is already in upper case then
% Lowercased is a plain copy of Atom.

lowercase_atom('', '').
lowercase_atom(A1, A2) :-
  atom_codes(A1, [H1|T]),
  to_lower(H1, H2),
  atom_codes(A2, [H2|T]).



%! new_atom(+Old:atom, -New:atom) is det.
% Returns a new atom, based on the given atom
% either by incrementing its index,
% or by adding such an index.
%
% This predicate comes in handy when creating unique identifiers
% based on a given base name, e.g. for threads, RDF graphs, files, etc.

new_atom(A1, A2) :-
  atomic_list_concat(Comps, '_', A1), % split
  reverse(Comps, [LastSplit|RestComps]),
  (   atom_number(LastSplit, OldNumber)
  ->  NewNumber is OldNumber + 1,
      atom_number(NewLastSplit, NewNumber),
      reverse([NewLastSplit|RestComps], NewComps)
  ;   reverse(['1',LastSplit|RestComps], NewComps)
  ),
  atomic_list_concat(NewComps, '_', A2).



%! repeating_atom(+SubAtom, +Repeats, -Atom) is det.
% Returns the atom that is the repetition of the given subatom
% for the given number of times.

repeating_atom(_SubAtom, 0, '') :- !.
repeating_atom(SubAtom, 1, SubAtom) :- !.
repeating_atom(SubAtom, Repeats, Atom) :-
  Repeats > 1,
  NewRepeats is Repeats - 1,
  repeating_atom(SubAtom, NewRepeats, Atom1),
  atomic_concat(Atom1, SubAtom, Atom).



%! split_atom_length(+Atom, +Len, -Subs) is nondet.
% Split Atom by length.
% The last subatom is allowed to have a shorter length.
%
% If `Length` is zero this predicate does not terminate.
% This is the correct behavior, since there is an infinite number of
% empty subatoms in each atom.
%
% @throws domain_error When Len is less than zero.

split_atom_length('', _, []) :- !.
split_atom_length(A1, L, [H|T]) :-
  sub_atom(A1, 0, L, _, H), !,
  atom_concat(H, A2, A1),
  split_atom_length(A2, L, T).
split_atom_length(A, _, [A]).



%! strip_atom(+Atom, -Sub) is det.
%! strip_atom(+PadChars, +Atom, -Sub) is det.
%! strip_atom_begin(+PadChars, +Atom, -Sub) is det.
%! strip_atom_end(+PadChars, +Atom, -Sub) is det.
% Return Atom with any occurrens of PadChars remove from the front and/or back.
%
% Notice that the order in which the PadChars occur is significant.
%
% The default PadChars are space, newline and horizontal tab.

strip_atom(A1, A2) :-
  strip_atom([' ','\n','\t'], A1, A2).


strip_atom(PadChars, A1, A3) :-
  strip_atom_begin(PadChars, A1, A2),
  strip_atom_end(PadChars, A2, A3).


strip_atom_begin(PadChars, A1, A3) :-
  member(PadChar, PadChars),
  atom_concat(PadChar, A2, A1), !,
  strip_atom_begin(PadChars, A2, A3).
strip_atom_begin(_, A, A).


strip_atom_end(PadChars, A1, A3) :-
  member(PadChar, PadChars),
  atom_concat(A2, PadChar, A1), !,
  strip_atom_end(PadChars, A2, A3).
strip_atom_end(_, A, A).
