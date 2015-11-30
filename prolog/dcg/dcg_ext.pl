:- module(
  dcg_ext,
  [
    '?'//1, % :Dcg_1
    '?'//2, % :Dcg_1
            % -Content:list
    '*'//1, % :Dcg_1
    '*'//2, % :Dcg_1
            % -Content:list
    '+'//1, % :Dcg_1
    '+'//2, % :Dcg_1
            % -Content:list
    '#'//2, % +Occurrences, :Dcg_1
    '#'//3, % +Occurrences:nonneg
            % :Dcg_1
            % -Content:list
    '*n'//2, % ?High:nonneg
             % :Dcg_1
    '*n'//3, % ?High:nonneg
             % :Dcg_1
             % -Content:list
    'm*n'//4, % ?Low:nonneg
              % ?High:nonneg
              % :Dcg_1
              % -Content:list
     alpha//1, % ?Code:code
     alphadigit//1, % ?Code:code
     bit//1, % ?Integer:between(0,1)
     def//3, % :Dcg_1
             % -Argument
             % +Default
     digit//1, % ?Integer:between(0,9)
     digit//2, % ?Integer:between(0,9)
               % ?Code:code
     digit_code//1, % ?Code:code
    '*digit'//1, % ?Integer:nonneg
    '+digit'//1, % ?Integer:nonneg
    '#digit'//2, % +Occurrences:nonneg
                 % ?Integer:nonneg
     frac_pos/2, % +Fractional:between(0.0,1.0)
                 % -Digits:list(between(0,9))
     hex//1, % ?Integer:between(0,15)
    '*hex'//1, % ?Integer:nonneg
    '+hex'//1, % ?Integer:nonneg
    'm*nhex'//3, % ?Low:nonneg
                 % ?High:nonneg
                 % -Integer:nonneg
    hialpha//1, % ?Code:code
    lowalpha//1, % ?Code:code
    opt//2, % :Dcg_1
            % ?Argument
    percent_enc//1, % ?Code:code
    pos/2, % +Integer, -Digits
    pos/3, % +Integer:nonneg
           % +Base:nonneg
           % -Digits:list(between(0,9))
    pos_frac/2, % +Digits:list(between(0,9))
                % -FractionalPart:rational
    pos_sum/2, % +Digits, -Number
    pos_sum/3, % +Digits:list(nonneg)
               % +Base:positive_integer
               % -Number:nonneg
    sum_pos/2, % +Number, -Digits
    sum_pos/3 % +Number:nonneg
              % +Base:nonneg
              % -Digits:list(between(0,9))
  ]
).
:- reexport(library(dcg/basics), except([digit//1,digits//1])).

/** <module> DCG Extensions

My favorite collection of DCG rules.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).

:- meta_predicate(?(//,?,?)).
:- meta_predicate(?(3,-,?,?)).
:- meta_predicate(*(//,?,?)).
:- meta_predicate(*(3,-,?,?)).
:- meta_predicate(+(//,?,?)).
:- meta_predicate(+(3,-,?,?)).
:- meta_predicate(#(+,//,?,?)).
:- meta_predicate(#(+,3,-,?,?)).
:- meta_predicate('*n'(?,//,?,?)).
:- meta_predicate('*n'(?,3,-,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,//,?,?)).
:- meta_predicate('m*n__'(?,?,+,3,-,?,?)).
:- meta_predicate(def(3,-,+,?,?)).
:- meta_predicate(opt(3,?,?,?)).





?(Dcg_1) --> 'm*n'(0, 1, Dcg_1).

?(Dcg_1, L) --> 'm*n'(0, 1, Dcg_1, L).



*(Dcg_1) --> 'm*n'(0, _, Dcg_1).

*(Dcg_1, L) --> 'm*n'(0, _, Dcg_1, L).



#(N, Dcg_1) --> 'm*n'(N, N, Dcg_1).

#(N, Dcg_1, L) --> 'm*n'(N, N, Dcg_1, L).



+(Dcg_1) --> 'm*n'(1, _, Dcg_1).

+(Dcg_1, L) --> 'm*n'(1, _, Dcg_1, L).



%! '*n'(?High:nonneg, :Dcg_1, -Content:list)// .

'*n'(High, Dcg_1) --> 'm*n'(_, High, Dcg_1).


%! '*n'(?High:nonneg, :Dcg_1, -Content:list)// .

'*n'(High, Dcg_1, L) --> 'm*n'(_, High, Dcg_1, L).



%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1)// .

'm*n'(Low, High, Dcg_1) --> 'm*n__'(Low, High, 0, Dcg_1).

'm*n__'(Low, High, Count1, Dcg_1) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_1, !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1).
'm*n__'(Low, _, Count, _) --> {(var(Low) -> true ; Low =< Count)}.


%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Content:list)// .

'm*n'(Low, High, Dcg_1, L) --> 'm*n__'(Low, High, 0, Dcg_1, L).

'm*n__'(Low, High, Count1, Dcg_1, [H|T]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H), !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1, T).
'm*n__'(Low, _, Count, _, []) --> {(var(Low) -> true ; Low =< Count)}.



% Digit
'*digit'(I) --> *(digit, Ds), {pos_sum(Ds, I)}.
'+digit'(I) --> +(digit, Ds), {pos_sum(Ds, I)}.
'#digit'(M, I) --> #(M, digit, Ds), {pos_sum(Ds, I)}.



% Hexadecimal
'*hex'(I) --> *(hex, Ds), {pos_sum(Ds, I)}.
'+hex'(I) --> +(hex, Ds), {pos_sum(Ds, I)}.
'm*nhex'(M, N, I) --> 'm*n'(M, N, hex, Ds), {pos_sum(Ds, I)}.



%! alpha(?Code:code)// .
% Alphabetic ASCII character.
%
% ```abnf
% alpha = lowalpha | upalpha
% ```

alpha(C) --> lowalpha(C).
alpha(C) --> hialpha(C).



%! alphadigit(?Code:code)// .
% Alphabetic ASCII character or decimal digit.
%
% ```abnf
% alphadigit = alpha | digit
% ```

alphadigit(C) --> alpha(C).
alphadigit(C) --> digit(C).



%! bit(?Integer:between(0,1))// .
% Wrapper around bit//2.

bit(I) --> bit(I, _).


%! bit(?Integer:between(0,1), ?Code:code)// .
% Binary digit.
%
% ```abnf
% BIT = "0" / "1"
% ```

bit(0, 0'0) --> "0".
bit(1, 0'1) --> "1".



%! def(:Dcg_1, -Argument, +Default)// .

def(Dcg_1, Arg, _) --> dcg_call(Dcg_1, Arg), !.
def(_, Def, Def) --> "".



%! digit(?Weight:between(0,9))// .
% Wrapper around digit//2.

digit(D) --> digit(D, _).


%! digit(?Weight:between(0,9), ?Code:code)// .
% ```abnf
% digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
% ```

digit(I, C)   --> bit(I, C).
digit(2, 0'2) --> "2".
digit(3, 0'3) --> "3".
digit(4, 0'4) --> "4".
digit(5, 0'5) --> "5".
digit(6, 0'6) --> "6".
digit(7, 0'7) --> "7".
digit(8, 0'8) --> "8".
digit(9, 0'9) --> "9".



%! digit_code(?Code:code)// .
% Wrapper around digit//2.

digit_code(C) --> digit(C, _).



%! frac_pos(+Fractional:between(0.0,1.0), -Digits:list(between(0,9))) is det.

frac_pos(Frac, Ds):-
  fractional_integer(Frac, I),
  sum_pos(I, Ds).



%! hex(?Weight:between(0,15))// .
% Wrapper around hex//2.

hex(I) --> hex(I, _).


%! hex(?Weight:between(0,15), ?Code:code)// .
% Hexadecimal digit, supporting both uppercase and lowercase letters.
%
% ```abnf
% hex = digit
%     | "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f"
% ```

hex(I, C)    --> digit(I, C).
hex(10, 0'A) --> "A".
hex(11, 0'B) --> "B".
hex(12, 0'C) --> "C".
hex(13, 0'D) --> "D".
hex(14, 0'E) --> "E".
hex(15, 0'F) --> "F".
hex(10, 0'a) --> "a".
hex(11, 0'b) --> "b".
hex(12, 0'c) --> "c".
hex(13, 0'd) --> "d".
hex(14, 0'e) --> "e".
hex(15, 0'f) --> "f".



%! hialpha(?Code:code)// .
% Upper-case alphabetic ASCII character code.
%
% ```abnf
% hialpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```

hialpha(0'A) --> "A".
hialpha(0'B) --> "B".
hialpha(0'C) --> "C".
hialpha(0'D) --> "D".
hialpha(0'E) --> "E".
hialpha(0'F) --> "F".
hialpha(0'G) --> "G".
hialpha(0'H) --> "H".
hialpha(0'I) --> "I".
hialpha(0'J) --> "J".
hialpha(0'K) --> "K".
hialpha(0'L) --> "L".
hialpha(0'M) --> "M".
hialpha(0'N) --> "N".
hialpha(0'O) --> "O".
hialpha(0'P) --> "P".
hialpha(0'Q) --> "Q".
hialpha(0'R) --> "R".
hialpha(0'S) --> "S".
hialpha(0'T) --> "T".
hialpha(0'U) --> "U".
hialpha(0'V) --> "V".
hialpha(0'W) --> "W".
hialpha(0'X) --> "X".
hialpha(0'Y) --> "Y".
hialpha(0'Z) --> "Z".



%! lowalpha(?Code:code)// .
% Lower-case alphabetic ASCII character code.
%
% ```abnf
% lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i"
%          | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
%          | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
% ```

lowalpha(0'a) --> "a".
lowalpha(0'b) --> "b".
lowalpha(0'c) --> "c".
lowalpha(0'd) --> "d".
lowalpha(0'e) --> "e".
lowalpha(0'f) --> "f".
lowalpha(0'g) --> "g".
lowalpha(0'h) --> "h".
lowalpha(0'i) --> "i".
lowalpha(0'j) --> "j".
lowalpha(0'k) --> "k".
lowalpha(0'l) --> "l".
lowalpha(0'm) --> "m".
lowalpha(0'n) --> "n".
lowalpha(0'o) --> "o".
lowalpha(0'p) --> "p".
lowalpha(0'q) --> "q".
lowalpha(0'r) --> "r".
lowalpha(0's) --> "s".
lowalpha(0't) --> "t".
lowalpha(0'u) --> "u".
lowalpha(0'v) --> "v".
lowalpha(0'w) --> "w".
lowalpha(0'x) --> "x".
lowalpha(0'y) --> "y".
lowalpha(0'z) --> "z".



%! opt(:Dcg_1, ?Argument)// .

opt(Dcg_1, Arg) --> dcg_call(Dcg_1, Arg), !.
opt(_, _) --> "".



%! percent_enc(-Code:code)// .
% Parse a percent-encoded character code between 0 and 255
%
% ```abnf
% percent_enc = "%" hex hex
% ```

percent_enc(C) --> "%", hex(H1), hex(H2), {C is H1 * 16 + H2}.



%! pos(+Integer:nonneg, -Digits:list(between(0,9))) is det.
% Wrapper around pois/2 with decimal base.

pos(I, Ds):- pos(I, 10, Ds).


%! pos(+Integer:nonneg, +Base:nonneg, -Digits:list(between(0,9))) is det.

pos(I, Base, Ds):-
  pos_rev(I, Base, Ds0),
  reverse(Ds0, Ds).

pos_rev(I, Base, [I]):-
  I < Base, !.
pos_rev(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  pos_rev(I2, Base, T).



%! pos_frac(+Digits:list(between(0,9)), -FractionalPart:rational) is det.
% Positional fractional.

pos_frac(Ds, Frac):-
  aggregate_all(sum(Rat), (nth1(I, Ds, D), Rat is D rdiv (10 ^ I)), Frac).



%! pos_sum(+Digits:list(between(0,9)), -Integer:nonneg) is det.
% Positional summation.

pos_sum(Ds, I):- pos_sum(Ds, 10, I).


%! pos_sum(
%!   +Digits:list(between(0,9)),
%!   +Base:positive_integer,
%!   -Integer:nonneg
%! ) is det.

pos_sum(Ds, Base, I):- pos_sum(Ds, Base, 0, I).
pos_sum([D|Ds], Base, I1, I):- !,
  I2 is I1 * Base + D,
  pos_sum(Ds, Base, I2, I).
pos_sum([], _, I, I).



%! sum_pos(+Number:nonneg, -Digits:list(between(0,9))) is det.
% Wrapper around sum_pos/3 with decimal base.

sum_pos(I, Ds):-
  sum_pos(I, 10, Ds).


%! sum_pos(+Number:nonneg, +Base:nonneg, -Digits:list(between(0,9))) is det.

sum_pos(I, Base, Ds):-
  sum_pos0(I, Base, Ds0),
  reverse(Ds0, Ds).

sum_pos0(0, _, []):- !.
sum_pos0(I1, Base, [H|T]):-
  H is I1 mod Base,
  I2 is I1 // Base,
  sum_pos0(I2, Base, T).
