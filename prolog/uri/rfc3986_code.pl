:- module(
  rfc3986_code,
  [
    'gen-delims'//1, % ?Code:code
    pchar//1, % ?Code:code
    'pct-encoded'//1, % ?Code:between(0,255)
    reserved//1, % ?Code:code
    'sub-delims'//1, % ?Code:code
    unreserved//1 % ?Code:code
  ]
).

/** <module> RFC 3986: Codes

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).





%! 'gen-delims'(?Code:code)// .
% ```abnf
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ```

'gen-delims'(0':) --> ":".
'gen-delims'(0'/) --> "/".
'gen-delims'(0'?) --> "?".
'gen-delims'(0'#) --> "#".
'gen-delims'(0'[) --> "[".
'gen-delims'(0']) --> "]".
'gen-delims'(0'@) --> "@".



%! pchar(?Code:code)// .
% ```abnf
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% ```

pchar(C) --> unreserved(C).
pchar(C) --> 'pct-encoded'(C).
pchar(C) --> 'sub-delims'(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".



%! 'pct-encoded'(?Code:between(0,255))// .
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) -->
  "%",
  '#'(2, 'HEXDIG', C, [convert1(positional)]).



%! reserved(?Code:code)// .
% ```abnf
% reserved = gen-delims / sub-delims
% ```

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).



%! 'sub-delims'(?Code:code)// .
% ```abnf
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
% ```

'sub-delims'(0'!) --> "!".
'sub-delims'(0'$) --> "$".
'sub-delims'(0'&) --> "&".
'sub-delims'(0'') --> "'".
'sub-delims'(0'() --> "(".
'sub-delims'(0')) --> ")".
'sub-delims'(0'*) --> "*".
'sub-delims'(0'+) --> "+".
'sub-delims'(0',) --> ",".
'sub-delims'(0';) --> ";".
'sub-delims'(0'=) --> "=".



%! unreserved(?Code:code)// .
% ```abnf
% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% ```

unreserved(C) --> 'ALPHA'(C).
unreserved(C) --> 'DIGIT'(_, C).
unreserved(0'-) --> "-".
unreserved(0'.) --> ".".
unreserved(0'_) --> "_".
unreserved(0'~) --> "~".
