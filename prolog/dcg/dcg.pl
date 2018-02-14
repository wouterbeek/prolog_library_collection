:- module(
  dcg,
  [
    '...'//0,
    '...'//1,             % -Codes
    alpha//1,             % ?Code
    'alpha*'//2,          % -Codes:list(code), -Tail:list(code)
    'alpha+'//2,          % -Codes:list(code), -Tail:list(code)
    alphanum//1,          % ?Code
    'alphanum*'//2,       % -Codes:list(code), -Tail:list(code)
    'alphanum+'//2,       % -Codes:list(code), -Tail:list(code)
    atom_phrase/2,        % :Dcg_0, ?Atom
    atom_phrase/3,        % :Dcg_0, +Atomic, ?Atom
    dcg_between//2,       % +Low, +High
    dcg_between//3,       % +Low, +High, ?Code
    dcg_with_output_to/1, % :Dcg_0
    dcg_with_output_to/2, % +Sink, :Dcg_0
    'digit*'//1,          % -Codes:list(code)
    'digit*'//2,          % -Codes:list(code), -Tail:list(code)
    'digit+'//1,          % -Codes:list(code)
    'digit+'//2,          % -Codes:list(code), -Tail:list(code)
    ellipsis//2,          % +Atom, +MaxLength
    indent//1,            % +Indent
    must_see//1,          % :Dcg_0
    must_see_code//2,     % +Code, :Skip_0
    nl//0,
    remainder_as_atom//1, % -Atom
    string_phrase/2,      % :Dcg_0, ?String
    string_phrase/3       % :Dcg_0, +String1, -String2
  ]
).
:- reexport(library(dcg/basics)).

/** <module> DCG

@author Wouter Beek
@version 2018
*/

:- use_module(library(error)).

:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).

:- meta_predicate
    atom_phrase(//, ?),
    atom_phrase(//, ?, ?),
    dcg_with_output_to(//),
    dcg_with_output_to(+, //),
    must_see(//, ?, ?),
    must_see_code(+, //, ?, ?),
    string_phrase(//, ?),
    string_phrase(//, ?, ?).





%! ...// .
%! ...(-Codes:list(code))// .
%
% Wrapper around string//1.

... -->
  ...(_).


...(Codes) -->
  string(Codes).



%! alpha// .
%! alpha(?Code:code)// .

alpha -->
  alpha(_).


alpha(Code) --> dcg_between(0'a, 0'z, Code).
alpha(Code) --> dcg_between(0'A, 0'Z, Code).



%! 'alpha*'(-Codes:list(code), -Tail:list(code))// .

'alpha*'([H|T0], T) -->
  alpha(H), !,
  'alpha*'(T0, T).
'alpha*'(T, T) --> "".



%! 'alpha+'(-Codes:list(code), -Tail:list(code))// .

'alpha+'([H|T0], T) -->
  alpha(H),
  'alpha*'(T0, T).



%! alphanum(?Code:code)// .

alphanum(Code) --> alpha(Code).
alphanum(Code) --> digit(Code).



%! 'alphanum*'(-Codes:list(code), -Tail:list(code))// .

'alphanum*'([H|T0], T) -->
  alphanum(H), !,
  'alphanum*'(T0, T).
'alphanum*'(T, T) --> "".



%! 'alphanum+'(-Codes:list(code), -Tail:list(code))// .

'alphanum+'([H|T0], T) -->
  alphanum(H),
  'alphanum*'(T0, T).



%! atom_phrase(:Dcg_0, ?Atom:atom)// is nondet.
%! atom_phrase(:Dcg_0, +Atomic:atomic, ?Atom:atom)// is nondet.
%
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, Atom) :-
  var(Atom), !,
  phrase(Dcg_0, Codes),
  atom_codes(Atom, Codes).
atom_phrase(Dcg_0, Atom) :-
  atom_codes(Atom, Codes),
  phrase(Dcg_0, Codes).


atom_phrase(Dcg_0, Atomic, Atom) :-
  (   atom(Atomic)
  ->  atom_codes(Atomic, Codes1)
  ;   number(Atomic)
  ->  number_codes(Atomic, Codes1)
  ),
  phrase(Dcg_0, Codes1, Codes2),
  atom_codes(Atom, Codes2).



%! dcg_between(+Low:nonneg, +High:nonneg)// .
%! dcg_between(+Low:nonneg, +High:nonneg, ?Code:nonneg)// .

dcg_between(Low, High) -->
  dcg_between(Low, High, _).


dcg_between(Low, High, Code) -->
  [Code],
  {between(Low, High, Code)}.



%! dcg_with_output_to(:Dcg_0) is nondet.
%! dcg_with_output_to(+Sink, :Dcg_0) is nondet.

dcg_with_output_to(Dcg_0) :-
  dcg_with_output_to(current_output, Dcg_0).


dcg_with_output_to(Sink, Dcg_0) :-
  phrase(Dcg_0, Codes),
  with_output_to(Sink, put_codes(Codes)).



%! 'digit*'(-Codes:list(code))// .
%! 'digit*'(-Codes:list(code), -Tail:list(code))// .

'digit*'(Codes) -->
  'digit*'(Codes, []).


'digit*'([H|T0], T) -->
  digit(H), !,
  'digit*'(T0, T).
'digit*'(T, T) --> "".



%! 'digit+'(-Codes:list(code))// .
%! 'digit+'(-Codes:list(code), -Tail:list(code))// .

'digit+'(Codes) -->
  'digit+'(Codes, []).


'digit+'([H|T0], T) -->
  digit(H),
  'digit*'(T0, T).



%! ellipsis(+Atom, +MaxLen:nonneg)// is det.
%
% MaxLen is the maximum length of the ellipsed atom A.

ellipsis(Atom, Len) -->
  {atom_ellipsis(Atom, Len, Ellipsed)},
  atom(Ellipsed).



%! indent(+Indent:nonneg)// is det.

indent(0) --> !, "".
indent(N1) -->
  " ", !,
  {N2 is N1 - 1},
  indent(N2).



%! must_see(:Dcg_0)// .

must_see(Dcg_0, X, Y) :-
  call(Dcg_0, X, Y), !.
must_see(_:Dcg_0, _, _) :-
  Dcg_0 =.. [Pred|_],
  format(string(Msg), "‘~a’ expected", [Pred]),
  syntax_error(Msg).



%! must_see_code(+C, :Skip_0)// .

must_see_code(C, Skip_0) -->
  [C], !,
  Skip_0.
must_see_code(C, _) -->
  {char_code(Char, C)},
  syntax_error(expected(Char)).



%! nl// is det.

nl -->
  "\n".



%! remainder_as_atom(-Atom:atom)// is det.

remainder_as_atom(Atom) -->
  remainder(Codes),
  {atom_codes(Atom, Codes)}.



%! string_phrase(:Dcg_0, ?String) is nondet.
%! string_phrase(:Dcg_0, +String1, ?String2) is nondet.

string_phrase(Dcg_0, String) :-
  var(String), !,
  phrase(Dcg_0, Codes),
  string_codes(String, Codes).
string_phrase(Dcg_0, String) :-
  string_codes(String, Codes),
  phrase(Dcg_0, Codes).


string_phrase(Dcg_0, String1, String2) :-
  string_codes(String1, Codes1),
  phrase(Dcg_0, Codes1, Codes2),
  string_codes(String2, Codes2).
