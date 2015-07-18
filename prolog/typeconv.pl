:- module(
  typeconv,
  [
    typeconv/3 % +FromValue
               % +ToType:atom
               % -ToValue
  ]
).

/** <module> Type conversion

@author Wouter Beek
@version 2015/07
*/



%! typeconv(+FromValue, +ToType:atom, -ToValue) is det.

typeconv(FromValue, ToType, ToValue):-
  format(atom(Atom), '~w', [FromValue]),
  atom_to_value(Atom, ToType, ToValue).



% HELPERS %

%! atom_to_value(+Atom:atom, +Type:compound, -Value) is det.
% Interpret `Atom` according to `Type`.

% Atom.
atom_to_value(Atom, atom, Atom):- !.
% Between two integers (inclusive).
atom_to_value(Atom, between(L,H), I):- !,
  atom_to_value(Atom, integer, I),
  L =< I,
  H >= I.
% Boolean.
atom_to_value(Atom, boolean, Boolean):- !,
  to_boolean(Atom, Boolean).
% Character.
atom_to_value(Char, char, Char):- !,
  atom_length(Char, 1).
% List of characters.
atom_to_value(Atom, chars, Chars):- !,
  atom_chars(Atom, Chars).
% Code.
atom_to_value(Atom, code, Code):- !,
  atom_to_value(Atom, char, Char),
  char_code(Char, Code).
% Codes.
atom_to_value(Atom, codes, Codes):- !,
  atom_codes(Atom, Codes).
% Float.
atom_to_value(Atom, float, Float):- !,
  atom_number(Atom, Number),
  Float = float(Number).
% Integer.
atom_to_value(Atom, integer, I):- !,
  atom_number(Atom, I),
  integer(I).
% Negative integer.
atom_to_value(Atom, negative_integer, I):- !,
  atom_to_value(Atom, integer, I),
  I < 0.
% Non-negative integer.
atom_to_value(Atom, nonneg, I):- !,
  atom_to_value(Atom, integer, I),
  I >= 0.
% Number.
atom_to_value(Atom, number, Number):- !,
  atom_number(Atom, Number).
% Positive integer.
atom_to_value(Atom, positive_integer, I):- !,
  atom_to_value(Atom, integer, I),
  I > 0.
% One from a given list of atoms.
atom_to_value(Atom, oneof(L), Atom):- !,
  memberchk(Atom, L).
% String.
atom_to_value(Atom, string, String):- !,
  atom_string(Atom, String).



%! to_boolean(
%!   +Value:oneof([0,1,@(false),@(true),false,'False',off,on,true,'True']),
%!   -Boolean:boolean
%! ) is det.
% Maps values that are often associated with Boolean values to Boolean values.
%
% The following conversions are supported:
% | *Value*    | *|Boolean value|*   |
% | _false_    | _false_             |
% | _@(false)_ | _false_             |
% | _0_        | _false_             |
% | _off_      | _false_             |
% | _true_     | _true_              |
% | _@(true)_  | _true_              |
% | _1_        | _true_              |
% | _on_       | _true_              |

% Prolog native.
to_boolean(true,     true ).
to_boolean(false,    false).
% Prolog DSL for JSON.
to_boolean(@(true),  true ).
to_boolean(@(false), false).
% Integer boolean.
to_boolean(1,        true ).
to_boolean(0,        false).
% CKAN boolean.
to_boolean('True',   true ).
to_boolean('False',  false).
% Electric switch.
to_boolean(on,       true ).
to_boolean(off,      false).
