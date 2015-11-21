:- module(
  rfc1738_code,
  [
    alpha//1, % ?Code:code
    alphadigit//1, % ?Code:code
    digit//1, % ?Weight:between(0,9)
    digit//2, % ?Weight:between(0,9)
              % ?Code:code
    digits//1, % ?Integer:nonneg
    escape//1, % ?Code:code
    extra//1, % ?Code:code
    hex//1, % ?Weight:between(0,15)
    hialpha//1, % ?Code:code
    lowalpha//1, % ?Code:code
    national//1, % ?Code:code
    punctuation//1, % ?Code:code
    safe//1, % ?Code:code
    reserved//1, % ?Code:code
    uchar//1, % ?Code:code
    unreserved//1, % ?Code:code
    xchar//1 % ?Code:code
  ]
).

/** <module> RFC 1738: Codes

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/rfc_common)).





%! alpha(?Code:code)// .
% Alphabetic character.
%
% ```abnf
% alpha = lowalpha | upalpha
% ```

alpha(C) --> lowalpha(C).
alpha(C) --> hialpha(C).



%! alphadigit(?Code:code)// .
% ```abnf
% alphadigit = alpha | digit
% ```

alphadigit(C) --> alpha(C).
alphadigit(C) --> digit(C).



%! digit(?Weight:between(0,9))// .
%! digit(?Weight:between(0,9), ?Code:code)// .
% ```abnf
% digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
% ```

digit(W) --> digit(W, _).
digit(0, 0'0) --> "0".
digit(1, 0'1) --> "1".
digit(2, 0'2) --> "2".
digit(3, 0'3) --> "3".
digit(4, 0'4) --> "4".
digit(5, 0'5) --> "5".
digit(6, 0'6) --> "6".
digit(7, 0'7) --> "7".
digit(8, 0'8) --> "8".
digit(9, 0'9) --> "9".



%! digits(?Number:nonneg)// .
%! digits(?Number:nonneg, ?Codes:list(code))// .
% ```abnf
% digits = 1*digit
% ```

digits(N) --> '+DIGIT'(N).



%! escape(?Code:code)// .
% ```abnf
% escape = "%" hex hex
% ```

escape(C) --> "%", hex(H1), hex(H2), {C is H1 * 16 + H2}.



%! extra(?Code:code)// .
% ```abnf
% extra = "!" | "*" | "'" | "(" | ")" | ","
% ```

extra(0'!) --> "!".
extra(0'*) --> "*".
extra(0'') --> "'".
extra(0'() --> "(".
extra(0')) --> ")".
extra(0',) --> ",".



%! hex(?Weight:between(0,15))// .
%! hex(?Weight:between(0,15), ?Code:code)// .
% ```abnf
% hex = digit
%     | "A" | "B" | "C" | "D" | "E" | "F"
%     | "a" | "b" | "c" | "d" | "e" | "f"
% ```

hex(W) --> hex(W, _).
hex(W, C) --> digit(W, C).
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
% RFC 2616 defines this in a different but compatible way
% under the name 'LOALPHA'//1.
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



%! national(?Code:code)// .
% ```abnf
% national = "{" | "}" | "|" | "\" | "^" | "~" | "[" | "]" | "`"
% ```

national(0'{) --> "{".
national(0'}) --> "}".
national(0'|) --> "|".
national(0'\\) --> "\\".
national(0'^) --> "^".
national(0'~) --> "~".
national(0'[) --> "[".
national(0']) --> "]".
national(0'`) --> "`".



%! punctuation(?Code:code)// .
% ```abnf
% punctuation = "<" | ">" | "#" | "%" | <">
% ```

punctuation(0'<) --> "<".
punctuation(0'>) --> ">".
punctuation(0'#) --> "#".
punctuation(0'%) --> "%".
punctuation(0'") --> "\"".   %"



%! safe(?Code:code)// .
% ```abnf
% safe = "$" | "-" | "_" | "." | "+"
% ```

safe(0'$) --> "$".
safe(0'-) --> "-".
safe(0'_) --> "_".
safe(0'.) --> ".".
safe(0'+) --> "+".



%! reserved(?Code:Code)// .
% ```abnf
% reserved = ";" | "/" | "?" | ":" | "@" | "&" | "="
% ```

reserved(0';) --> ";".
reserved(0'/) --> "/".
reserved(0'?) --> "?".
reserved(0':) --> ":".
reserved(0'@) --> "@".
reserved(0'&) --> "&".
reserved(0'=) --> "=".



%! uchar(?Code:code)// .
% ```abnf
% uchar = unreserved | escape
% ```

uchar(C) --> unreserved(C).
uchar(C) --> escape(C).



%! unreserved(?Code:code)// .
% ```abnf
% unreserved = alpha | digit | safe | extra
% ```

unreserved(C) --> alpha(C).
unreserved(C) --> digit(C).
unreserved(C) --> safe(C).
unreserved(C) --> extra(C).



%! xchar(?Code:code)// .
% ```abnf
% xchar = unreserved | reserved | escape
% ```

xchar(C) --> unreserved(C).
xchar(C) --> reserved(C).
xchar(C) --> escape(C).
