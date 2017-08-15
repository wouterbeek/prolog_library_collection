:- module(
  rfc7159,
  [
    'JSON-text'//1 % -Term
  ]
).

/** <module> RFC 7159

@author Wouter Beek
@compat RFC 7159
@see https://tools.ietf.org/html/rfc7159
@version 2017/04, 2017/08
*/

:- use_module(library(clpfd)).
:- use_module(library(dcg/dcg_ext), except([number//1,string//1])).
:- use_module(library(dcg/rfc5234)).
:- use_module(library(math_ext)).





%! array(-L)// .
%
% ```abnf
% array = begin-array [ value *( value-separator value ) ] end-array
% ```

array(L) -->
  'begin-array',
  (value(H) -> must_see(array0(T)), {L = [H|T]} ; {L = []}),
  'end-array'.

array0([H|T]) -->
  'value-separator', !,
  must_see(value(H)),
  array0(T).
array0([]) --> "".



%! 'begin-array'// .
%
% ```abnf
% begin-array     = ws %x5B ws   ; [ left square bracket
% ```

'begin-array' -->
  ws, "[", ws.



%! 'begin-object'// .
%
% ```abnf
% begin-object    = ws %x7B ws   ; { left curly bracket
% ```

'begin-object' -->
  ws, "{", ws.



%! char(-C)// .
%
% ```abnf
% char = unescaped /
%        escape ( %x22 /           ; " quotation mark  U+0022
%                 %x5C /           ; \ reverse solidus U+005C
%                 %x2F /           ; / solidus         U+002F
%                 %x62 /           ; b backspace       U+0008
%                 %x66 /           ; f form feed       U+000C
%                 %x6E /           ; n line feed       U+000A
%                 %x72 /           ; r carriage return U+000D
%                 %x74 /           ; t tab             U+0009
%                 %x75 4HEXDIG )   ; uXXXX        U+XXXX

char(C) -->
  unescaped(C).
char(C) -->
  escape,
  char_(C).

char_(0'\") --> "\"".
char_(0'\\) --> "\\".
char_(0'/)  --> "/".
char_(0'\b) --> "b".
char_(0'\f) --> "f".
char_(0'\n) --> "n".
char_(0'\r) --> "r".
char_(0'\t) --> "t".
char_(Code) -->
  "u", !,
  must_see(dcg_integer(#(4, 'HEXDIG'), 16, Code)).



%! escape// .
%
% ```abnf
% escape = %x5C   ; \
% ```

escape -->
  "\\".



%! 'end-array'// .
%
% ```abnf
% end-array       = ws %x5D ws   ; ] right square bracket
% ```

'end-array' --> ws, "]", ws.



%! 'end-object'// .
%
% ```abnf
% end-object      = ws %x7D ws   ; } right curly bracket
% ```

'end-object' --> ws, "}", ws.



%! false// .
%
% ```abnf
% false = %x66.61.6c.73.65   ; false
% ```

false --> "false".



%! 'JSON-text'(-Term)// .
%
% ```abnf
% JSON-text = ws value ws
% ```

'JSON-text'(Term) --> ws, value(Term), ws.



%! 'decimal-point'// .
%
% ```abnf
% decimal-point = %x2E   ; .
% ```

'decimal-point' --> ".".



%! 'digit1-9'(-Weight)// .
%
% ```abnf
% digit1-9 = %x31-39   ; 1-9
% ```

'digit1-9'(Weight) -->
  digit_weight(Weight),
  {between(1, 9, Weight)}.



%! e// .
%
% ```abnf
% e = %x65 / %x45   ; e E
% ```

e --> "e".
e --> "E".



%! exp(-Exp)// .
%
% ```abnf
% exp = e [ minus / plus ] 1*DIGIT
% ```

exp(N2) -->
  e,
  (minus -> {Sg = -1} ; plus -> {Sg = 1} ; {Sg = 1}),
  dcg_integer('m*'(1, digit_weight), N1),
  {N2 #= Sg * N1}.



%! frac(-Frac)// .
%
% ```abnf
% frac = decimal-point 1*DIGIT
% ```

frac(Frac) -->
  'decimal-point',
  'm*'(1, digit_weight, Weights),
  {fractional_weights(Frac, Weights)}.



%! int(-N)// .
%
% ```abnf
% int = zero / ( digit1-9 *DIGIT )
% ```

int(0) -->
  zero.
int(N) -->
  'digit1-9'(H),
  *(digit_weight, T),
  {integer_weights(N, [H|T])}.



%! member(-Pair)// .
%
% ```abnf
% member = string name-separator value
% ```

member(Key-Val) -->
  string(Key0),
  {atom_string(Key, Key0)},
  'name-separator',
  value(Val).



%! 'name-separator'// .
%
% ```abnf
% name-separator = ws %x3A ws   ; : colon
% ```

'name-separator' --> ws, ":", ws.



%! minus// .
%
% ```abnf
% minus = %x2D   ; -
% ```

minus --> "-".



%! number(-N)// .
%
% ```abnf
% number = [ minus ] int [ frac ] [ exp ]
% ```

number(N3) -->
  (minus -> {Sg = -1} ; {Sg = 1}),
  int(N1),
  (frac(Frac) -> "" ; ""),
  (exp(Exp) -> "" ; {Exp = 0}),
  {
    % Integer or float?
    (var(Frac) -> N2 = N1 ; N2 = N1 + Frac),
    N3 is Sg * N2 * (10 ** Exp)
  }.



%! null// .
%
% ```abnf
% null  = %x6e.75.6c.6c      ; null
% ```

null --> "null".



%! object(-Dict)// .
%
% ```abnf
% object = begin-object [ member *( value-separator member ) ] end-object
% ```

object(Dict) -->
  'begin-object',
  (member(H) -> must_see(object0(T)), {Pairs = [H|T]} ; {Pairs = []}),
  'end-object',
  {dict_pairs(Dict, _, Pairs)}.

object0([H|T]) -->
  'value-separator', !,
  must_see(member(H)),
  object0(T).
object0([]) --> "".



%! plus// .
%
% ```abnf
% plus = %x2B   ; +
% ```

plus --> "+".



%! 'quotation-mark'// .
%
% ```abnf
% quotation-mark = %x22   ; "
% ```

'quotation-mark' --> "\"".



%! string(-String)// .
%
% ```abnf
% string = quotation-mark *char quotation-mark
% ```

string(String) -->
  'quotation-mark',
  *(char, Cs),
  'quotation-mark',
  {string_codes(String, Cs)}.



%! true// .
%
% ```abnf
% true  = %x74.72.75.65      ; true
% ```

true --> "true".



%! unescaped(-C)// .
%
% ```abnf
% unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
% ```

unescaped(C) -->
  [C],
  {unescaped_(C)}.

unescaped_(C) :- between(0x20, 0x21, C).
unescaped_(C) :- between(0x23, 0x5b, C).
unescaped_(C) :- between(0x5d, 0x10ffff, C).



%! value// .
%
% ```abnf
% value = false / null / true / object / array / number / string
% ```

value(false) --> false, !.
value(null) --> null, !.
value(true) --> true, !.
value(Dict) --> object(Dict), !.
value(L) --> array(L), !.
value(N) --> number(N), !.
value(String) --> string(String).



%! 'value-separator'// .
%
% ```abnf
% value-separator = ws %x2C ws   ; , comma
% ```

'value-separator' --> ws, ",", ws.



%! ws// .
%
% ```abnf
% ws = *( %x20 /   ; Space
%         %x09 /   ; Horizontal tab
%         %x0A /   ; Line feed or New line
%         %x0D )   ; Carriage return
% ```

ws --> *('WS').



%! zero// .
%
% ```abnf
% zero = %x30   ; 0
% ```

zero --> "0".
