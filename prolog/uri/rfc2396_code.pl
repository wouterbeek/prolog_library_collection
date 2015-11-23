:- module(
  rfc2396_code,
  [
    delims//1, % ?Code:code
    mark//1, % ?Code:code
    pchar//1, % ?Code:code
    reserved//1, % ?Code:code
    space//1, % ?Code:code
    unreserved//1, % ?Code:code
    unwise//1, % ?Code:code
    uric//1, % ?Code:code
    uric_no_slash//1 % ?Code:code
  ]
).
:- reexport(
  library(dcg/dcg_ext),
  [
    alpha//1, % ?Code:code
    alphadigit//1 as alphanum, % ?Code:code
    digit//1, % ?Integer:between(0,9)
    percent_enc//1 as escaped, % ?Code:code
    hex//1, % ?Integer:between(0,15)
    lowalpha//1, % ?Code:code
    hialpha//1 as upalpha % ?Code:code
  ]
).

/** <module> RFC 2396: Codes

@author Wouter Beek
@compat RFC 2396
@deprecated
@version 2015/11
*/





%! alphanum(?Code:code)// .
% RFC 1738 (URL) defines this under the name alphadigit//1.
%
% ```abnf
% alphanum = alpha | digit
% ```




%! delims(?Code:code)// .
% ```abnf
% delims = "<" | ">" | "#" | "%" | <">
% ```

delims(0'<)  --> "<".
delims(0'>)  --> ">".
delims(0'#)  --> "#".
delims(0'%)  --> "%".
delims(0'\") --> "\"".



%! escaped(?Code:code)// .
% RFC 1738 (URL) defines this under the name escape//1.
%
% ```abnf
% escaped = "%" hex hex
% ```



%! mark(?Code:code)// .
% ```abnf
% mark = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
% ```

mark(0'-) --> "-".
mark(0'_) --> "_".
mark(0'.) --> ".".
mark(0'!) --> "!".
mark(0'~) --> "~".
mark(0'*) --> "*".
mark(0'') --> "'".
mark(0'() --> "(".
mark(0')) --> ")".



%! pchar(?Code:code)// .
% ```abnf
% pchar = unreserved | escaped | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ```

pchar(C)   --> unreserved(C).
pchar(C)   --> escaped(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".
pchar(0'&) --> "&".
pchar(0'=) --> "=".
pchar(0'+) --> "+".
pchar(0'$) --> "$".
pchar(0',) --> ",".



%! reserved(?Code:code)// .
% ```abfn
% reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ","
% ```

reserved(0';) --> ";".
reserved(0'/) --> "/".
reserved(0'?) --> "?".
reserved(0':) --> ":".
reserved(0'@) --> "@".
reserved(0'&) --> "&".
reserved(0'=) --> "=".
reserved(0'+) --> "+".
reserved(0'$) --> "$".
reserved(0',) --> ",".



%! space(?Code:code)// .
% ```abnf
% space = <US-ASCII coded character 20 hexadecimal>
% ```

space(0' ) --> " ".



%! unreserved(?Code:code)// .
% ```abnf
% unreserved = alphanum | mark
% ```

unreserved(C) --> alphanum(C).
unreserved(C) --> mark(C).



%! unwise(?Code:code)// .
% ```abnf
% unwise = "{" | "}" | "|" | "\" | "^" | "[" | "]" | "`"
% ```

unwise(0'{)  --> "{".
unwise(0'})  --> "}".
unwise(0'|)  --> "|".
unwise(0'\\) --> "\\".
unwise(0'^)  --> "^".
unwise(0'[)  --> "[".
unwise(0'])  --> "]".
unwise(0'`)  --> "`".



%! upalpha(?Code:code)// .
% RFC 1738 (URL) defines this under the name hialpha//1.
%
% ```abnf
% upalpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
%         | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R"
%         | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z"
% ```



%! uric(?Code:code)// .
% RFC 1738 (URL) defines a reordering of this grammar rule
% under the name xchar//1.
%
% ```abnf
% uric = reserved | unreserved | escaped
% ```

uric(C) --> reserved(C).
uric(C) --> unreserved(C).
uric(C) --> escaped(C).



%! uric_no_slash(?Code:code)// .
% ```abnf
% uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@"
%               | "&" | "=" | "+" | "$" | ","
% ```

uric_no_slash(C)   --> unreserved(C).
uric_no_slash(C)   --> escaped(C).
uric_no_slash(0';) --> ";".
uric_no_slash(0'?) --> "?".
uric_no_slash(0':) --> ":".
uric_no_slash(0'@) --> "@".
uric_no_slash(0'&) --> "&".
uric_no_slash(0'=) --> "=".
uric_no_slash(0'+) --> "+".
uric_no_slash(0'$) --> "$".
uric_no_slash(0',) --> ",".
