:- module(
  rfc1738_code,
  [
    extra//1, % ?Code:code
    national//1, % ?Code:code
    punctuation//1, % ?Code:code
    safe//1, % ?Code:code
    reserved//1, % ?Code:code
    uchar//1, % ?Code:code
    unreserved//1, % ?Code:code
    xchar//1 % ?Code:code
  ]
).
:- reexport(library(dcg/dcg_ext), [
     alpha//1, % ?Code:code
     alphadigit//1, % ?Code:code
     digit//1, % ?Weight:between(0,9)
     digit//2, % ?Weight:between(0,9)
               % ?Code:code
     '+digit'//1 as digits, % ?Integer:nonneg
     hex//1, % ?Weight:between(0,15)
     hialpha//1, % ?Code:code
     lowalpha//1, % ?Code:code
     percent_enc//1 as escape % ?Code:code
   ]).
     
/** <module> RFC 1738: Codes

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





%! alpha(?Code:code)// .
% Alphabetic character.
%
% ```abnf
% alpha = lowalpha | upalpha
% ```



%! alphadigit(?Code:code)// .
% ```abnf
% alphadigit = alpha | digit
% ```



%! digit(?Weight:between(0,9), ?Code:code)// .
% ```abnf
% digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
% ```



%! digits(?Number:nonneg)// .
% ```abnf
% digits = 1*digit
% ```



%! escape(?Code:code)// .
% ```abnf
% escape = "%" hex hex
% ```



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



%! hialpha(?Code:code)// .
% ```abnf
% hialpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```



%! lowalpha(?Code:code)// .
% RFC 2616 defines this in a different but compatible way
% under the name 'LOALPHA'//1.
%
% ```abnf
% lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i"
%          | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
%          | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
% ```



%! national(?Code:code)// .
% ```abnf
% national = "{" | "}" | "|" | "\" | "^" | "~" | "[" | "]" | "`"
% ```

national(0'{)  --> "{".
national(0'})  --> "}".
national(0'|)  --> "|".
national(0'\\) --> "\\".
national(0'^)  --> "^".
national(0'~)  --> "~".
national(0'[)  --> "[".
national(0'])  --> "]".
national(0'`)  --> "`".



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
