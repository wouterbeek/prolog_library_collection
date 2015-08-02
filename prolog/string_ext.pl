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

/** <module> String extensions

Additional support for native strings in SWI-Prolog.

Non-native string representations in Prolog:
  - List of character codes
    Strings cannot be distinguished from lists of (non-negative) integers.
  - List of characters

---

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).



%! codes_string(+Codes:list(nonneg), +String:string) is semidet.
%! codes_string(+Codes:list(nonneg), -String:string) is det.
%! codes_string(-Codes:list(nonneg), +String:string) is det.
% Variant of the built-in string_codes/2.

codes_string(Cs, S):-
  string_codes(S, Cs).


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

string_list_concat(Ss, Sep, S):-
  var(S), !,
  maplist(atom_string, [Sep0|As], [Sep|Ss]),
  atomic_list_concat(As, Sep0, A),
  atom_string(A, S).
string_list_concat(Ss, Sep, S):-
  maplist(atom_string, [Sep0,A], [Sep,S]),
  atomic_list_concat(As, Sep0, A),
  maplist(atom_string, As, Ss).
