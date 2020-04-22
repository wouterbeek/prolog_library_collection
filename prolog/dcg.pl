:- encoding(utf8).
:- module(
  dcg,
  [
    '...'//0,
    '...'//1,                 % -Codes
    add_indent//1,            % +Indent
    alpha//1,                 % ?Code
    'alpha*'//2,              % -Codes:list(code), -Tail:list(code)
    'alpha+'//2,              % -Codes:list(code), -Tail:list(code)
    alphanum//1,              % ?Code
    'alphanum*'//2,           % -Codes:list(code), -Tail:list(code)
    'alphanum+'//2,           % -Codes:list(code), -Tail:list(code)
    atom_phrase/2,            % :Dcg_0, ?Atom
    atom_phrase/3,            % :Dcg_0, +Atom1, ?Atom2
    dcg_atom//2,              % :Dcg_1, ?Atom
    dcg_atom_from_codes//2,   % :Dcg_1, ?Atom
    dcg_between//2,           % +Low, +High
    dcg_between//3,           % +Low, +High, ?Code
    dcg_bool//1,              % +Bool
    dcg_call//1,              % :Dcg_0
    dcg_call//2,              % :Dcg_1, ?Arg1
    dcg_call//3,              % :Dcg_2, ?Arg1, ?Arg2
    dcg_call//4,              % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call//5,              % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call//6,              % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_string//2,            % :Dcg_1, ?String
    dcg_string_from_codes//2, % :Dcg_1, ?String
    dcg_with_output_to/1,     % :Dcg_0
    dcg_with_output_to/2,     % +Sink, :Dcg_0
    default//2,               % :Dcg_0, ?Default_0
    'digit*'//1,              % -Codes:list(code)
    'digit*'//2,              % -Codes:list(code), -Tail:list(code)
    'digit+'//1,              % -Codes:list(code)
    'digit+'//2,              % -Codes:list(code), -Tail:list(code)
    digit_weight//1,          % ?N
    'digit_weight*'//1,       % ?Ns
    'digit_weight+'//1,       % ?Ns
    ellipsis//2,              % +Atom, +MaxLength
    indent//1,                % +Indent
    must_see//1,              % :Dcg_0
    must_see_code//2,         % +Code, :Skip_0
    nl//0,
    nonblank//0,
    nonblanks//0,
    parsing//0,
    remainder_as_atom//1,     % -Remainder
    remainder_as_string//1,   % -Remainder
    string_phrase/2,          % :Dcg_0, ?String
    string_phrase/3,          % :Dcg_0, +String1, -String2
    tab//1,                   % +N
    term//1,                  % +Term
    thousands//1,             % +N
    ws//0
  ]
).
:- reexport(library(dcg/basics)).

/** <module> Extended support for DCGs

*/

:- use_module(library(error)).

:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).
:- use_module(library(math_ext)).
:- use_module(library(string_ext)).

:- meta_predicate
    atom_phrase(//, ?),
    atom_phrase(//, ?, ?),
    dcg_atom(3, ?, ?, ?),
    dcg_atom_from_codes(3, ?, ?, ?),
    dcg_call(//, ?, ?),
    dcg_call(3, ?, ?, ?),
    dcg_call(4, ?, ?, ?, ?),
    dcg_call(5, ?, ?, ?, ?, ?),
    dcg_call(6, ?, ?, ?, ?, ?, ?),
    dcg_call(7, ?, ?, ?, ?, ?, ?, ?),
    dcg_string(3, ?, ?, ?),
    dcg_string_from_codes(3, ?, ?, ?),
    dcg_with_output_to(//),
    dcg_with_output_to(+, //),
    default(//, //, ?, ?),
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



%! add_indent(+Indent:positive_integer)// .

add_indent(N), "\n", indent(N) -->
  "\n", !,
  add_indent(N).
add_indent(N), [Code] -->
  [Code], !,
  add_indent(N).
add_indent(_) --> "".



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
%! atom_phrase(:Dcg_0, +Atom1:atomic, ?Atom2:atom)// is nondet.

atom_phrase(Dcg_0, Atom) :-
  var(Atom), !,
  phrase(Dcg_0, Codes),
  atom_codes(Atom, Codes).
atom_phrase(Dcg_0, Atom) :-
  atom_codes(Atom, Codes),
  phrase(Dcg_0, Codes).


atom_phrase(Dcg_0, Atom1, Atom2) :-
  must_be(atom, Atom1),
  atom_codes(Atom1, Codes1),
  phrase(Dcg_0, Codes1, Codes2),
  atom_codes(Atom2, Codes2).



%! dcg_atom(:Dcg_1, ?Atom:atom)// .
%
% This meta-DCG rule handles the translation between the word and the
% character level of parsing/generating.
%
% Typically, grammar *A* specifies how words can be formed out of
% characters.  A character is a code, and a word is a list of codes.
% Grammar *B* specifies how sentences can be built out of words.  Now
% the word is an atom, and the sentences in a list of atoms.
%
% This means that at some point, words in grammar *A*, i.e. lists of
% codes, need to be translated to words in grammar *B*, i.e. atoms.
%
% This is where dcg_atom//2 comes in.  We illustrate this with a
% schematic example:
%
% ```prolog
% sentence([W1,...,Wn]) -->
%   word2(W1),
%   ...,
%   word2(Wn).
%
% word2(W) -->
%   dcg_atom(word1, W).
%
% word1([C1, ..., Cn]) -->
%   char(C1),
%   ...,
%   char(Cn).
% ```
%
% @throws instantiation_error
% @throws type_error

dcg_atom(Dcg_1, Atom) -->
  {var(Atom)}, !,
  dcg_call(Dcg_1, Codes),
  {atom_codes(Atom, Codes)}.
dcg_atom(Dcg_1, Atom) -->
  {atom_codes(Atom, Codes)},
  dcg_call(Dcg_1, Codes).



%! dcg_atom_from_codes(:Dcg_1, ?Atom:atom)// .

dcg_atom_from_codes(Dcg_1, Atom) -->
  {var(Atom)}, !,
  dcg_codes_(Dcg_1, Cs),
  {atom_codes(Atom, Cs)}.
dcg_atom_from_codes(Dcg_1, Atom) -->
  {atom_codes(Atom, Cs)},
  dcg_codes_(Dcg_1, Cs).

dcg_codes_(Dcg_1, [H|T]) -->
  dcg_call(Dcg_1, H), !,
  dcg_codes_(Dcg_1, T).
dcg_codes_(_, []) --> "".



%! dcg_between(+Low:nonneg, +High:nonneg)// .
%! dcg_between(+Low:nonneg, +High:nonneg, ?Code:nonneg)// .

dcg_between(Low, High) -->
  dcg_between(Low, High, _).


dcg_between(Low, High, Code) -->
  [Code],
  {between(Low, High, Code)}.



%! dcg_bool(+Boolean:bool)// is det.

dcg_bool(false) --> !, "❌".
dcg_bool(true) --> "✓".



%! dcg_call(:Dcg_0)// .
%! dcg_call(:Dcg_1, ?Arg1)// .
%! dcg_call(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
%
% @see call/[1-8]

dcg_call(Dcg_0, X, Y) :-
  call(Dcg_0, X, Y).


dcg_call(Dcg_1, Arg1, X, Y) :-
  call(Dcg_1, Arg1, X, Y).


dcg_call(Dcg_2, Arg1, Arg2, X, Y) :-
  call(Dcg_2, Arg1, Arg2, X, Y).


dcg_call(Dcg_3, Arg1, Arg2, Arg3, X, Y) :-
  call(Dcg_3, Arg1, Arg2, Arg3, X, Y).


dcg_call(Dcg_4, Arg1, Arg2, Arg3, Arg4, X, Y) :-
  call(Dcg_4, Arg1, Arg2, Arg3, Arg4, X, Y).


dcg_call(Dcg_5, Arg1, Arg2, Arg3, Arg4, Arg5, X, Y) :-
  call(Dcg_5, Arg1, Arg2, Arg3, Arg4, Arg5, X, Y).



%! dcg_string(:Dcg_1, ?String)// .

dcg_string(Dcg_1, String) -->
  {var(String)}, !,
  dcg_call(Dcg_1, Codes),
  {string_codes(String, Codes)}.
dcg_string(Dcg_1, String) -->
  {string_codes(String, Codes)},
  dcg_call(Dcg_1, Codes).



%! dcg_string_from_codes(:Dcg_1, ?String:string)// .

dcg_string_from_codes(Dcg_1, String) -->
  {var(String)}, !,
  dcg_codes_(Dcg_1, Cs),
  {string_codes(String, Cs)}.
dcg_string_from_codes(Dcg_1, String) -->
  {string_codes(String, Cs)},
  dcg_codes_(Dcg_1, Cs).



%! dcg_with_output_to(:Dcg_0) is nondet.
%! dcg_with_output_to(+Sink, :Dcg_0) is nondet.

dcg_with_output_to(Dcg_0) :-
  dcg_with_output_to(current_output, Dcg_0).


dcg_with_output_to(Sink, Dcg_0) :-
  phrase(Dcg_0, Codes),
  with_output_to(Sink, put_codes(Codes)).



%! default(:Dcg_0, ?Default_0)// .

default(Dcg_0, _) -->
  Dcg_0, !.
default(_, Default_0) -->
  Default_0.



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



%! digit_weight(?Weight:between(0,9))// .

digit_weight(Weight) -->
  parsing, !,
  [Code],
  {code_type(Code, digit(Weight))}.
digit_weight(Weight) -->
  {code_type(Code, digit(Weight))},
  [Code].



%! 'digit_weight*'(-Weights:list(between(0,9)))// is det.

'digit_weight*'([H|T]) -->
  digit_weight(H), !,
  'digit_weight*'(T).
'digit_weight*'([]) --> "".



%! 'digit_weight+'(-Weights:list(between(0,9)))// is det.

'digit_weight+'([H|T]) -->
  digit_weight(H), !,
  'digit_weight*'(T).



%! ellipsis(+Original:text, +MaxLength:nonneg)// is det.
%
% @arg MaxLength The maximum length of the generated ellipsed string.

ellipsis(Original, MaxLength) -->
  {string_ellipsis(Original, MaxLength, Ellipsed)},
  atom(Ellipsed).



%! indent(+Indent:nonneg)// is det.

indent(0) --> !, "".
indent(N1) -->
  "  ", !,
  {N2 is N1 - 1},
  indent(N2).



%! must_see(:Dcg_0)// .

must_see(Dcg_0, X, Y) :-
  call(Dcg_0, X, Y), !.
must_see(_:Dcg_0, _, _) :-
  Dcg_0 =.. [Pred|_],
  syntax_error(expected(Pred)).



%! must_see_code(+Code, :Skip_0)// .

must_see_code(Code, Skip_0) -->
  [Code], !,
  Skip_0.
must_see_code(Code, _) -->
  {char_code(Char, Code)},
  syntax_error(expected(Char)).



%! nl// is det.

nl -->
  "\n".



%! nonblank// .
%
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank -->
  nonblank(_).



%! nonblanks// .

nonblanks -->
  nonblanks(_).



%! parsing// is semidet.
%
% Succeeds if currently parsing a list of codes (rather than
% generating a list of codes).

parsing(H, H) :-
  nonvar(H).



%! remainder_as_atom(-Atom:atom)// is det.

remainder_as_atom(Atom) -->
  remainder(Codes),
  {atom_codes(Atom, Codes)}.



%! remainder_as_string(-Remainder:string)// is det.

remainder_as_string(String) -->
  remainder(Codes),
  {string_codes(String, Codes)}.



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



%! tab(+N:nonneg)// is det.

tab(0) --> !, "".
tab(N1) -->
  " ",
  {N2 is N1 - 1},
  tab(N2).



%! term(+Term)// is det.

term(Term) -->
  {format(atom(Atom), "~w", [Term])},
  atom(Atom).



%! thousands(+N:integer)// is det.

thousands(N) -->
  {format(atom(Atom), "~D", [N])},
  atom(Atom).



%! ws// is det.

ws --> white.
% NO-BREAK SPACE (0240, 0xA0)
ws --> [160].
