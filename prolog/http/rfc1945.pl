:- module(
  rfc1945,
  [
    tspecials//1 % ?Code
  ]
).

/** <module> Hypertext Transfer Protocol -- HTTP/1.0

@author Wouter Beek
@compat https://tools.ietf.org/html/rfc1945
@version 2016/12
*/





%! tspecials(?Code)// .
%
% ```abnf
% tspecials = "(" | ")" | "<" | ">" | "@"
%           | "," | ";" | ":" | "\" | <">
%           | "/" | "[" | "]" | "?" | "="
%           | "{" | "}" | SP | HT
% ```

tspecials(0'() --> "(".
tspecials(0')) --> ")".
tspecials(0'<) --> "<".
tspecials(0'>) --> ">".
tspecials(0'@) --> "@".
tspecials(0',) --> ",".
tspecials(0';) --> ";".
tspecials(0':) --> ":".
tspecials(0'\\) --> "\\".
tspecials(0'") --> "\"". %"
tspecials(0'/) --> "/".
tspecials(0'[) --> "[".
tspecials(0']) --> "]".
tspecials(0'?) --> "?".
tspecials(0'=) --> "=".
tspecials(0'{) --> "{".
tspecials(0'}) --> "}".
tspecials(C)   --> 'SP'(C).
tspecials(C)   --> 'HT'(C).
