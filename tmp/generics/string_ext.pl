:- module(
  string_ext,
  [
    codes_string/2, % ?Codes:list(nonneg)
                    % ?String:string
    string_list_concat/3 % ?Strings:list(string)
                         % ?Separator:string
                         % ?String:string
  ]
).

/** <module> String: Extensions

Additional support for native strings in SWI-Prolog.

Non-native string representations in Prolog:
  - List of character codes
    Strings cannot be distinguished from lists of (non-negative) integers.
  - List of characters

---

@author Wouter Beek
@version 2014/08, 2014/11
*/

:- use_module(library(apply)).



%! codes_string(+Codes:list(nonneg), +String:string) is semidet.
%! codes_string(+Codes:list(nonneg), -String:string) is det.
%! codes_string(-Codes:list(nonneg), +String:string) is det.

codes_string(Codes, Atom):-
  atom_codes(Atom, Codes).



%! string_list_concat(
%!   +Strings:list(string),
%!   +Separator:string,
%!   +String:string
%! ) is semidet.
%! string_list_concat(
%!   +Strings:list(string),
%!   +Separator:string,
%!   -String:string
%! ) is det.
%! string_list_concat(
%!   -Strings:list(string),
%!   +Separator:string,
%!   +String:string
%! ) is det.

string_list_concat(Strings, Separator, String):-
  var(String), !,
  maplist(atom_string, [Separator0|Atoms], [Separator|Strings]),
  atomic_list_concat(Atoms, Separator0, Atom),
  atom_string(Atom, String).
string_list_concat(Strings, Separator, String):-
  maplist(atom_string, [Separator0,Atom], [Separator,String]),
  atomic_list_concat(Atoms, Separator0, Atom),
  maplist(atom_string, Atoms, Strings).
