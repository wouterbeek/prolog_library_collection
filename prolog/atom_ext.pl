:- module(
  atom_ext,
  [
    atom_after_char/3,    % +Atom,   +Char,    -Rest
    atom_ending_in/3,     % +Atom,   +Sub,     -NewAtom
    atom_postfix/2,       % +Atom,   ?Sub
    atom_postfix/3,       % +Atom,   ?Len,     ?Sub
    atom_to_term/2,       % +Atom,   -Term
    capitalize_atom/2,    % +Atom,   -Capitalized
    codes_atom/2,         % ?Cs,     ?Atom
    common_atom_prefix/3, % +Atom1,  +Atom2,   -Sub
    ensure_atom/2,        % +Term,   -Atom
    integer_padding/3,    % +I, +Len, -Atom
    integer_padding/4,    % +I, +Len, +PadChar, -Atom
    lower_upper/2,        % ?Lower, ?Upper
    lowercase_atom/2,     % +Atom,   -Lowercased
    new_atom/2,           % +Old,    -New
    repeating_atom/3,     % +Sub,    +Repeats, -Atom
    split_atom_length/3,  % +Atom,   +Len,     -Subs
  ]
).

/** <module> Atom extensions

Predicates for manipulating atoms.

We assume that atoms are encoded using ASCII (or an ASCII-compatible)
encoding scheme.

# Replace

In-atom replacements can best be made using DCGs.  This requires the
atom to be translated to/from a list of numeric codes.  For example,
escaping spaces and grave accent (e.g. in URIs):

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
@version 2015/07-2017/03
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(char_ext)).
:- use_module(library(typecheck)).





%! atom_after_char(+Atom, +Char, -Rest) is det.

atom_after_char(Atom, Char, Rest) :-
  State = last(-),
  (   sub_atom(Atom, _, _, L, Char),
      nb_setarg(1, State, L),
      fail
  ;   arg(1, State, L),
      L \== (-)
  ),
  sub_atom(Atom, _, L, 0, Rest).



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



%! atom_to_term(+Atom, -Term) is det.
% Return the Term described by the Atom.
%
% @see Wrapper around atom_to_term/3 that omits the variable bindings.

atom_to_term(Atom, Term) :-
  atom_to_term(Atom, Term, _).



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



%! lower_upper(+Lower, -Upper) is det.
%! lower_upper(-Lower, +Upper) is det.
%
% Multi-moded combination of upcase_atom/2 and downcase_atom/2.
%
% Copied from library(dialect/ifprolog), which cannot be reused
% because it redefines time/1.

lower_upper(Lower, Upper) :-
  nonvar(Lower), !,
  upcase_atom(Lower, Upper).
lower_upper(Lower, Upper) :-
  downcase_atom(Upper, Lower).


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
