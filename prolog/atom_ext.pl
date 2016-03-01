:- module(
  atom_ext,
  [
    atom_ending_in/3, % +Atom:atom
                      % +Suffix:atom
                      % -NewAtom:atom
    atom_splits/3, % +Splits:list(atom)
                   % +Atom:atom
                   % -Subatoms:list(atom)
    atom_to_term/2, % +Atom:atom
                    % -Term:term
    atom_truncate/3, % +Atom:atom
                     % +MaximumLength:integer
                     % -TruncatedAtom:atom
    capitalize_atom/2, % +Atom:atom
                       % -Capitalized:atom
    codes_atom/2, % ?Codes:list(nonneg)
                  % ?Atom:atom
    common_atom_prefix/3, % +Atom1:atom
                          % +Atom2:atom
                          % -Prefix:atom
    empty_atom/1, % ?Empty:atom
    first_split/3, % +Atom:atom
                   % +Split:atom
                   % -FirstSubatom:atom
    format_integer/3, % +Integer:integer
                      % +Length:integer
                      % -Atom:atom
    new_atom/2, % +Old:atom
                % -New:atom
    repeating_atom/3, % +SubAtom:atom
                      % +Repeats:integer
                      % -Atom:atom
    split_atom_length/3, % +Atom:atom
                         % +Length:integer
                         % -Subatoms:list(atom)
    strip_atom/2, % +In:atom
                  % -Out:atom
    strip_atom/3, % +Strips:list(atom)
                  % +In:atom
                  % -Out:atom
    strip_atom_begin/3, % +Strips:list(atom)
                        % +In:atom
                        % -Out:atom
    strip_atom_end/3 % +Strips:list(atom)
                     % +In:atom
                     % -Out:atom
  ]
).

/** <module> Atom: Extensions

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

# Split

`atomic_list_concat(-,+,+)` performs atom splitting
 according to a single given separator.

For using multiple splits at once, use atom_splits/3.

For splitting by a set length, use atom_split_length/3.

# Strip

Stripping atoms of an arbitrary number of subatoms can be done using
 strip_atom/3, strip_atom_begin/3, and strip_atom_end/3.

# Titlecase

Titlecase atoms can be created using upcase_atom/2.

---

@author Wouter Beek
@version 2015/07-2018/10
*/

:- use_module(library(apply)).
:- use_module(library(list_ext)).
:- use_module(library(char_ext)).
:- use_module(library(typecheck)).





%! atom_ending_in(+Atom:atom, +Suffix:atom, -NewAtom:atom) is det.
% Succeeds if NewAtom is the same as Atom and has given Suffix.
% If Atom already ends in Suffix then NewAtom == Atom.
%
% This can for instance be used to ensure that an atom
% ends with a forward slash in the process of building file paths.

atom_ending_in(A, Suffix, A):-
  atom_concat(_, Suffix, A), !.
atom_ending_in(A0, Suffix, A):-
  atom_concat(A0, Suffix, A).



%! atom_splits(+Splits:list(atom), +Atom:atom, -Subatoms:list(atom)) is det.
% Returns the given atom split up in two, according to the given split.
% The first split does not include the split atom, making this method
% exclusive.
%
% @arg Splits Atoms where the main atom with be split.
%        Notice that the order in which the splits appear is significant.
% @arg Atom The original, unsplit atom.
% @arg Subatoms The results of splitting.

atom_splits(Splits, Atom1, [H|T]):-
  member(Split, Splits),
  atom_concat(H, Temp, Atom1),
  atom_concat(Split, Atom2, Temp),
  atom_splits(Splits, Atom2, T).
atom_splits(_, Subatom, [Subatom]).



%! atom_to_term(+Atom:atom, -Term:term) is det.
% Returns the term described by the atom.
%
% @arg Atom An atom.
% @arg Term A term.
% @see Wrapper around atom_to_term/3, omitting the bindings.

atom_to_term(Atom, Term):-
  atom_to_term(Atom, Term, _Bindings).



%! atom_truncate(
%!   +Atom:atom,
%!   +MaxLength:or([oneof([inf]),positive_integer]),
%!   -TruncatedAtom:atom
%! ) is det.
% Returns the truncated version of the given atom.
% Truncated atoms end in `...` to indicate its truncated nature.
% The maximum length indicates the exact maximum.
% Truncation will always result in an atom which has at most `MaxLength`.
%
% @arg Atom The original atom.
% @arg MaxLength The maximum allowed length of an atom.
%      For values smaller than or equal to 5 the original atom is returned.
%      With value `inf` the original atom is always returned.
% @arg TruncatedAtom The truncated atom.
%
% @throws type_error

% The maximum allowed length is too short to be used with truncation.
atom_truncate(A, inf, A):- !.
atom_truncate(A, Max, A):-
  must_be(positive_integer, Max),
  Max =< 5, !.
% The atom does not have to be truncated, it is not that long.
atom_truncate(A, Max, A):-
  atom_length(A, AL),
  AL =< Max, !.
% The atom exceeds the maximum length, it is truncated.
% For this purpose the displayed length of the atom is
%  the maximum length minus 4 (but never less than 3).
atom_truncate(A1, Max, A3):-
  TruncatedL is Max - 3,
  sub_atom(A1, 0, TruncatedL, _, A2),
  atom_concat(A2, ..., A3).



%! capitalize_atom(+Atom:atom, -Capitalized:atom) is det.
% Succeeds if Capitalized is a copy of Atom where the first character
% is in upper case.
%
% If the first character of Atom is already in upper case then
% Capitalized is a plain copy of Atom.

capitalize_atom('', '').
capitalize_atom(A1, A2):-
  atom_codes(A1, [H1|T]),
  to_upper(H1, H2),
  atom_codes(A2, [H2|T]).



%! codes_atom(+Codes:list(nonneg), +Atom:atom) is semidet.
%! codes_atom(+Codes:list(nonneg), -Atom:atom) is det.
%! codes_atom(-Codes:list(nonneg), +Atom:atom) is det.
% Variant of atom_codes/2.

codes_atom(Cs, A):-
  atom_codes(A, Cs).



%! common_atom_prefix(+Atom1:atom, +Atom2:atom, -Prefix:atom) is semidet.
%! common_atom_prefix(+Atom1:atom, +Atom2:atom, -Prefix:atom) is nondet.
% Returns the longest common prefix of the given two atoms.

common_atom_prefix(Atom1, Atom2, Prefix):-
  maplist(atom_codes, [Atom1,Atom2], [Codes1,Codes2]),
  common_list_prefix(Codes1, Codes2, PrefixCodes),
  atom_codes(Prefix, PrefixCodes).



%! empty_atom(+Empty:atom) is semidet.
%! empty_atom(-Empty:atom) is det.
% Succeeds only on the empty atom.

empty_atom('').



%! first_split(+Atom:atom, +Split:atom, -FirstSubatom:atom) is nondet.
% Returns the first split.
% For the first result this is behaviorally equivalent to:
% ```prolog
% atomic_list_concat(Subatoms, Split, Atom),
% Subatoms = [FirstSubatom|_]
% ```

first_split(Atom, Split, FirstSubatom):-
  atom_concat(Subatom, _, Atom),
  atom_concat(FirstSubatom, Split, Subatom).



%! format_integer(+Integer:integer, +Length:integer, -Atom:atom) is det.
% Returns a formatted representation of the given integer
%  that is exactly the given number of characters long.
%
% Fails in case the length of the formatted integer exceeds the given length.
%
% @arg Integer The integer value that is to be formatted.
% @arg Length The exact character length of the formatted integer atom.
% @arg Atom The formatted version of the integer value.
%
% @tbd See whether this can be done using format/2 tab stops,
%      http://www.swi-prolog.org/pldoc/doc_for?object=format/2.

format_integer(I, L, Out):-
  atom_length(I, IL),
  ZeroLength is L - IL,
  repeating_atom('0', ZeroLength, Zeros),
  atomic_concat(Zeros, I, Out).



%! new_atom(+Old:atom, -New:atom) is det.
% Returns a new atom, based on the given atom
% either by incrementing its index,
% or by adding such an index.
%
% This predicate comes in handy when creating unique identifiers
% based on a given base name, e.g. for threads, RDF graphs, files, etc.

new_atom(A1, A2):-
  atomic_list_concat(Splits, '_', A1), % split
  reverse(Splits, [LastSplit|RestSplits]),
  (   atom_number(LastSplit, OldNumber)
  ->  NewNumber is OldNumber + 1,
      atom_number(NewLastSplit, NewNumber),
      reverse([NewLastSplit|RestSplits], NewSplits)
  ;   reverse(['1',LastSplit|RestSplits], NewSplits)
  ),
  atomic_list_concat(NewSplits, '_', A2).



%! repeating_atom(+SubAtom:atom, +Repeats:integer, -Atom:atom) is det.
% Returns the atom that is the repetition of the given subatom
%  for the given number of times.
%
% @arg SubAtom An atom, the element that gets repeated.
% @arg Repeats A integer, the number of repeats of the subatom.
% @arg Atom An atom, the result of repeating the given atom.

repeating_atom(_SubAtom, 0, ''):- !.
repeating_atom(SubAtom, 1, SubAtom):- !.
repeating_atom(SubAtom, Repeats, Atom):-
  Repeats > 1,
  NewRepeats is Repeats - 1,
  repeating_atom(SubAtom, NewRepeats, Atom1),
  atomic_concat(Atom1, SubAtom, Atom).



%! split_atom_length(
%!   +Atom:atom,
%!   +Length:nonneg,
%!   -Subatoms:list(atom)
%! ) is nondet.
% Splits atoms by length.
% The last subatom is allowed to have a shorter length.
%
% If `Length` is zero this predicate does not terminate.
% This is the correct behavior, since there is an infinite number of
%  empty subatoms in each atom.
%
% @throws domain_error When `Length` is less than zero.

split_atom_length('', _, []):- !.
split_atom_length(A1, L, [H|T]):-
  sub_atom(A1, 0, L, _, H), !,
  atom_concat(H, A2, A1),
  split_atom_length(A2, L, T).
split_atom_length(A, _, [A]).



%! strip_atom(+In:atom, -Out:atom) is det.

strip_atom(A1, A2):-
  strip_atom([' ','\n','\t'], A1, A2).

%! strip_atom(+Strips:list(atom), +In:atom, -Out:atom) is det.
%! strip_atom_begin(+Strips:list(atom), +In:atom, -Out:atom) is det.
%! strip_atom_end(+Strips:list(atom), +In:atom, -Out:atom) is det.
% Strips the given atom's front and/or back for the given character.
%
% Notice that the order in which the strip atoms occur is significant.
%
% @arg Strips A list of atoms that will be stripped.
% @arg In The non-strippped atom
% @arg Out The stripped atom.

strip_atom(Strips, A1, A3):-
  strip_atom_begin(Strips, A1, A2),
  strip_atom_end(Strips, A2, A3).

strip_atom_begin(Strips, A1, A3):-
  member(Strip, Strips),
  atom_concat(Strip, A2, A1), !,
  strip_atom_begin(Strips, A2, A3).
strip_atom_begin(_, A, A).

strip_atom_end(Strips, A1, A3):-
  member(Strip, Strips),
  atom_concat(A2, Strip, A1), !,
  strip_atom_end(Strips, A2, A3).
strip_atom_end(_, A, A).
