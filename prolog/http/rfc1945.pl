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





%! 'HT'(?Code)// .
%
% ```
% HT = <US-ASCII HT, horizontal-tab (9)>
% ```

'HT'(9) --> [9].



%! 'SP'(?Code)// .
%
% ```
% SP = <US-ASCII SP, space (32)>
% ```

'SP'(32) --> [32].



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
tspecials(C) --> 'SP'(C).
tspecials(C) --> 'HT'(C).
