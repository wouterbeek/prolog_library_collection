:- module(
  dcg_ext,
  [
    '?'//1,                % :Dcg_0
    '?'//2,                % :Dcg_0, ?Arg
    '#'//2,                % ?Occurrences, :Dcg_0
    '#'//3,                % ?Occurrences, :Dcg_1, -Args
    '+'//1,                % :Dcg_0
    '+'//2,                % :Dcg_1, ?L
    '*'//1,                % :Dcg_0
    '*'//2,                % :Dcg_1, ?L
    '*n'//2,               % ?High, :Dcg_0
    '*n'//3,               % ?High, :Dcg_1, -Args
    'm*'//2,               % ?Low, :Dcg_0
    'm*'//3,               % ?Low, :Dcg_1, -Args
    'm*n'//3,              % ?Low, ?High, :Dcg_0
    'm*n'//4,              % ?Low, ?High, :Dcg_1, -Args
    '...'//0,
    '...'//1,              % -Codes
    alpha//1,              % ?Code
    alphanum//1,           % ?Code
    atom_ci//1,            % ?Atom
    atom_phrase/2,         % :Dcg_0, ?Atom
    atom_phrase/3,         % :Dcg_0, +Atomic, ?Atom
    dcg_atom//2,           % :Dcg_1, ?Atom
    dcg_call//1,           % :Dcg_0
    dcg_call//2,           % :Dcg_1, ?Arg1
    dcg_debug/2,           % +Flag, :Dcg_0
    dcg_default//3,        % :Dcg_0, -Arg1, +Default
    dcg_string//2,         % :Dcg_1, ?String
    dcg_with_output_to/1,  % :Dcg_0
    dcg_with_output_to/2,  % +Sink, :Dcg_0
    digit_weight//1,       % ?Digit:between(0,9)
    ellipsis//2,           % +Atom, +Max
    eol//0,
    generate_as_digits//2, % +N:nonneg, +NumDigits
    generate_as_digits//3, % +N:nonneg, +Base:positive_integer, +NumDigits
    indent//1,             % +Indent:nonneg
    must_see//1,           % :Dcg_0
    must_see_code//2,      % +Code, :Skip_0
    nl//0,
    nonblank//0,
    parsing//0,
    rest//0,
    rest//1,               % -Rest:list(code)
    rest_as_atom//1,       % -Rest:atom
    rest_as_string//1,     % -Rest:string
    seplist*//2,           % :Dcg_0, :Sep_0
    seplist*//3,           % :Dcg_1, :Sep_0, +Args
    seplist+//2,           % :Dcg_0, :Sep_0
    seplist+//3,           % :Dcg_1, :Sep_0, +Args
    string_phrase/2,       % :Dcg_0, ?String
    string_phrase/3,       % :Dcg_0, +String1, -String2
    thousands//1,          % +Integer:integer
    'WS'//0
  ]
).
:- reexport(library(dcg/basics)).

/** <module> DCG extensions

@author Wouter Beek
@version 2017/04-2017/08
*/

:- use_module(library(aggregate)).
:- use_module(library(atom_ext)).
:- use_module(library(code_ext)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).

:- meta_predicate
    ?(//, ?, ?),
    ?(3, ?, ?, ?),
    *(//, ?, ?),
    *(3, -, ?, ?),
    +(//, ?, ?),
    +(3, -, ?, ?),
    #(+, //, ?, ?),
    #(+, 3, -, ?, ?),
    '*n'(?, //, ?, ?),
    '*n'(?, 3, -, ?, ?),
    'm*'(?, //, ?, ?),
    'm*'(?, 3, -, ?, ?),
    'm*n'(?, ?, //, ?, ?),
    'm*n'(?, ?, 3, -, ?, ?),
    'm*n__g'(?, ?, +, //, ?, ?),
    'm*n__g'(?, ?, +, 3, -, ?, ?),
    'm*n__p'(?, ?, +, //, ?, ?),
    'm*n__p'(?, ?, +, 3, -, ?, ?),
    atom_phrase(//, ?),
    atom_phrase(//, ?, ?),
    dcg_atom(3, ?, ?, ?),
    dcg_call(//, ?, ?),
    dcg_call(3, ?, ?, ?),
    dcg_debug(+, //),
    dcg_default(3, -, +, ?, ?),
    dcg_string(3, ?, ?, ?),
    dcg_with_output_to(//),
    dcg_with_output_to(+, //),
    must_see(//, ?, ?),
    must_see_code(+, //, ?, ?),
    seplist*(//, //, ?, ?),
    seplist*(3, //, +, ?, ?),
    seplist+(//, //, ?, ?),
    seplist+(3, //, +, ?, ?),
    string_phrase(//, ?),
    string_phrase(//, ?, ?).





%! ?(:Dcg_0)// .
%! ?(:Dcg_1, ?Arg)// .

?(Dcg_0) -->
  Dcg_0, !.
?(_) --> "".


?(Dcg_1, Arg) -->
  dcg_call(Dcg_1, Arg), !.
?(_, _) --> "".



%! *(:Dcg_0)// .
%! *(:Dcg_1, ?Args)// .

*(Dcg_0) -->
  'm*n'(0, _, Dcg_0).


*(Dcg_1, Args) -->
  'm*n'(0, _, Dcg_1, Args).



%! +(:Dcg_0)// .
%! +(:Dcg_1, ?Args)// .

+(Dcg_0) -->
  'm*n'(1, _, Dcg_0).


+(Dcg_1, Args) -->
  'm*n'(1, _, Dcg_1, Args).



%! #(?Occurrences, :Dcg_0)// is det.
%! #(?Occurrences, :Dcg_1, -Args)// is det.

#(N, Dcg_0) -->
  'm*n'(N, N, Dcg_0).


#(N, Dcg_1, Args) -->
  'm*n'(N, N, Dcg_1, Args).



%! '*n'(?High, :Dcg_0)// is det.
%! '*n'(?High, :Dcg_1, -Args)// is det.

'*n'(High, Dcg_0) -->
  'm*n'(_, High, Dcg_0).

'*n'(High, Dcg_1, Args) -->
  'm*n'(_, High, Dcg_1, Args).



%! 'm*'(?Low, :Dcg_0)// is det.
%! 'm*'(?Low, :Dcg_1, -Args)// is det.

'm*'(Low, Dcg_0) -->
  'm*n'(Low, _, Dcg_0).


'm*'(Low, Dcg_1, Args) -->
  'm*n'(Low, _, Dcg_1, Args).



%! 'm*n'(?Low, ?High, :Dcg_0)// is det.
%! 'm*n'(?Low, ?High, :Dcg_1, -Args)// is det.

'm*n'(Low, High, Dcg_0) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_0).
'm*n'(Low, High, Dcg_0) -->
  'm*n__g'(Low, High, 0, Dcg_0).

'm*n__g'(Low, _, Count, _) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_0),
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_0).

'm*n__p'(Low, High, Count1, Dcg_0) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_0),
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_0).
'm*n__p'(Low, _, Count, _) -->
  {(var(Low) -> true ; Low =< Count)}.


'm*n'(Low, High, Dcg_1, Args) -->
  parsing, !,
  'm*n__p'(Low, High, 0, Dcg_1, Args).
'm*n'(Low, High, Dcg_1, Args) -->
  'm*n__g'(Low, High, 0, Dcg_1, Args).

'm*n__g'(Low, _, Count, _, []) -->
  {(var(Low) -> true ; Low =< Count)}.
'm*n__g'(Low, High, Count1, Dcg_1, [H|T]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*n__g'(Low, High, Count2, Dcg_1, T).

'm*n__p'(Low, High, Count1, Dcg_1, [H|T]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H),
  {Count2 is Count1 + 1},
  'm*n__p'(Low, High, Count2, Dcg_1, T).
'm*n__p'(Low, _, Count, _, []) -->
  {(var(Low) -> true ; Low =< Count)}.



%! ...// .
%! ...(-Codes:list(code))// .
%
% Wrapper around string//1.

... -->
  ...(_).


...(Codes) -->
  string(Codes).



%! alpha// .
%! alpha(?Code)// .

alpha -->
  alpha(_).


alpha(C) -->
  [C],
  { between(0'a, 0'z, C)
  ; between(0'A, 0'Z, C)
  }, !.



%! alphanum(?Code)// .

alphanum(C) -->
  alpha(C), !.
alphanum(C) -->
  digit(C).



%! atom_ci(?Atom)// .
%
% ```prolog
% ?- phrase(atom_ci(http), Codes).
% Codes = "HTTP" ;
% Codes = "HTTp" ;
% Codes = "HTtP" ;
% Codes = "HTtp" ;
% Codes = "HtTP" ;
% Codes = "HtTp" ;
% Codes = "HttP" ;
% Codes = "Http" ;
% Codes = "hTTP" ;
% Codes = "hTTp" ;
% Codes = "hTtP" ;
% Codes = "hTtp" ;
% Codes = "htTP" ;
% Codes = "htTp" ;
% Codes = "httP" ;
% Codes = "http" ;
% false.
% ```

atom_ci(A) -->
  {ground(A)}, !,
  {atom_codes(A, Codes)},
  *(code_ci, Codes).
atom_ci(A) -->
  *(code_ci, Codes),
  {atom_codes(A, Codes)}.



%! atom_phrase(:Dcg_0, ?Atom)// is nondet.
%
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, Atom) :-
  var(Atom), !,
  phrase(Dcg_0, Codes),
  atom_codes(Atom, Codes).
atom_phrase(Dcg_0, Atom) :-
  must_be(atom, Atom),
  atom_codes(Atom, Codes),
  phrase(Dcg_0, Codes).


%! atom_phrase(:Dcg_0, +Atomic, ?Atom)// is nondet.
%
% @throws instantiation_error
% @throws type_error

atom_phrase(Dcg_0, Atomic, Atom) :-
  (   atom(Atomic)
  ->  atom_codes(Atomic, Codes1)
  ;   number(Atomic)
  ->  number_codes(Atomic, Codes1)
  ),
  phrase(Dcg_0, Codes1, Codes2),
  atom_codes(Atom, Codes2).



%! code_ci(+Code:code, -CiCode:code) is nondet.
%! code_ci(+Code:code)// .
%
% Returns case-insensitive variants of the given code.
% This includes the code itself.

% Lowercase is a case-insensitive variant of uppercase.
code_ci(Upper, Lower) :-
  code_type(Upper, upper(Lower)).
% Uppercase is a case-insensitive variant of lowercase.
code_ci(Lower, Upper) :-
  code_type(Lower, lower(Upper)).
% Every code is a case-insensitive variant of itself.
code_ci(Code, Code).


code_ci(Code) -->
  {code_ci(Code, CiCode)},
  [CiCode].



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

dcg_atom(Dcg_1, A) -->
  {var(A)}, !,
  dcg_call(Dcg_1, Codes),
  {atom_codes(A, Codes)}.
dcg_atom(Dcg_1, A) -->
  {must_be(atom, A)}, !,
  {atom_codes(A, Codes)},
  dcg_call(Dcg_1, Codes).



%! dcg_call(:Dcg_0)// .
%! dcg_call(:Dcg_1, ?Arg1)// .

dcg_call(Dcg_0, X, Y) :-
  call(Dcg_0, X, Y).


dcg_call(Dcg_1, Arg1, X, Y) :-
  call(Dcg_1, Arg1, X, Y).



%! dcg_debug(+Flag, :Dcg_0) is det.
%
% Write the first generation of Dcg_0 as a debug message under the
% given Flag.

dcg_debug(Flag, Dcg_0) :-
  debugging(Flag), !,
  phrase(Dcg_0, Codes),
  debug(Flag, "~s", [Codes]).
dcg_debug(_, _).



%! dcg_default(:Dcg_1, -Arg, +Def)// .

dcg_default(Dcg_1, Arg, _) --> dcg_call(Dcg_1, Arg), !.
dcg_default(_, Default, Default) --> "".



%! dcg_string(:Dcg_1, ?String)// .

dcg_string(Dcg_1, String) -->
  {var(String)}, !,
  dcg_call(Dcg_1, Codes),
  {string_codes(String, Codes)}.
dcg_string(Dcg_1, String) -->
  {must_be(string, String)}, !,
  {string_codes(String, Codes)},
  dcg_call(Dcg_1, Codes).



%! dcg_with_output_to(:Dcg_0) is nondet.
%! dcg_with_output_to(+Sink, :Dcg_0) is nondet.

dcg_with_output_to(Dcg_0) :-
  dcg_with_output_to(current_output, Dcg_0).


dcg_with_output_to(Sink, Dcg_0) :-
  phrase(Dcg_0, Codes),
  with_output_to(Sink, put_codes(Codes)).



%! digit_weight(?Digit:between(0,9))// .

digit_weight(Weight) -->
  parsing, !,
  [C],
  {code_type(C, digit(Weight))}.
digit_weight(Weight) -->
  {code_type(C, digit(Weight))},
  [C].



%! ellipsis(+Atom, +MaxLen:or([nonneg,oneof([inf])]))// is det.
%
% MaxLen is the maximum length of the ellipsed atom A.

ellipsis(Atom, Len) -->
  {atom_ellipsis(Atom, Len, Ellipsed)},
  atom(Ellipsed).



%! eol// .

eol --> "\n".
eol --> "\r\n".



%! generate_as_digits(+N:nonneg, +NumDigits:nonneg)// is det.
%! generate_as_digits(+N:nonneg, +Base:positive_integer, +NumDigits:nonneg)// is det.
%
% Generate the non-negative integer N using exactly NumDigits digits,
% using `0' as padding if needed.

generate_as_digits(N, M) -->
  generate_as_digits(N, 10, M).


generate_as_digits(_, _, 0) --> !, "".
generate_as_digits(N1, Base, M1) -->
  {M2 is M1 - 1},
  {D is N1 // Base ^ M2},
  digit_weight(D),
  {N2 is N1 mod Base ^ M2},
  generate_as_digits(N2, Base, M2).



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



%! nonblank// .
%
% Wrapper around nonblank//1 from library(dcg/basics).

nonblank -->
  nonblank(_).



%! parsing// is semidet.
%
% Succeeds if currently parsing a list of codes (rather than
% generating a list of codes).

parsing(H, H) :-
  nonvar(H).



%! rest// is det.
%! rest(-Rest:list(code))// is det.
%
% Same as `rest --> "".'

rest(X, X).


rest(X, X, []).



%! rest_as_atom(-Atom:atom)// is det.

rest_as_atom(Atom) -->
  rest(Codes),
  {atom_codes(Atom, Codes)}.



%! rest_as_string(-String:string)// is det.

rest_as_string(String) -->
  rest(Codes),
  {string_codes(String, Codes)}.



%! seplist*(:Dcg_0, :Sep_0)// is det.
%! seplist*(:Dcg_1, :Sep_0, +Args)// is det.

seplist*(Dcg_0, Sep_0) -->
  Dcg_0,
  (Sep_0 -> seplist*(Dcg_0, Sep_0) ; "").
seplist*(_, _) --> !, "".


% The first clause cannot contain a cut after the separator, because
% the separator may also appear after the separated list.  For
% example, "a b " could no longer be parsed if the separator was a
% space.
seplist*(Dcg_1, Sep_0, [H1,H2|T]) -->
  dcg_call(Dcg_1, H1),
  Sep_0,
  seplist*(Dcg_1, Sep_0, [H2|T]).
seplist*(Dcg_1, _, [H]) -->
  dcg_call(Dcg_1, H), !.
seplist*(_, _, []) --> !, "".



%! seplist+(:Dcg_0, :Sep_0)// is det.
%! seplist+(:Dcg_1, :Sep_0, +Args)// is det.

seplist+(Dcg_0, Sep_0) -->
  Dcg_0,
  (Sep_0 -> seplist*(Dcg_0, Sep_0) ; "").


seplist+(Dcg_1, Sep_0, [H|T]) -->
  dcg_call(Dcg_1, H),
  (Sep_0 -> seplist*(Dcg_1, Sep_0, T) ; "").



%! string_phrase(:Dcg_0, ?String) is nondet.

string_phrase(Dcg_0, String) :-
  var(String), !,
  phrase(Dcg_0, Codes),
  string_codes(String, Codes).
string_phrase(Dcg_0, String) :-
  must_be(string, String),
  string_codes(String, Codes),
  phrase(Dcg_0, Codes).


%! string_phrase(:Dcg_0, +S1, ?S2) is nondet.

string_phrase(Dcg_0, S1, S2) :-
  string_codes(S1, Codes1),
  phrase(Dcg_0, Codes1, Codes2),
  string_codes(S2, Codes2).



%! thousands(+I)// is det.

thousands(inf) --> !,
  "∞".
thousands(I) -->
  {format(atom(A), "~D", [I])},
  atom(A).



%! 'WS'// .
%
% A common definition of white space characters.
%
% ```ebnf
% WS ::= #x20 | #x9 | #xD | #xA
% ```
%
% @compat SPARQL 1.1 grammar rule 162
% @compat Well-Known Text (WKT)

'WS' --> [0x20]. % space
'WS' --> [0x09]. % horizontal tag
'WS' --> [0x0D]. % carriage return
'WS' --> [0x0A]. % linefeed / new line
