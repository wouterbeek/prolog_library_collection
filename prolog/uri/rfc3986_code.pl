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
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CR'//0,
     'DIGIT'//1, % ?Weight:between(0,9)
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9)
                  % ?Code:code
     'LF'//0,
     'SP'//0
   ]).

/** <module> RFC 3986: Codes

@author Wouter Beek
@compat RFC 3986
@see http://tools.ietf.org/html/rfc3986
@version 2015/08, 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_rfc)).





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

pchar(C)   --> unreserved(C).
pchar(C)   --> 'pct-encoded'(C).
pchar(C)   --> 'sub-delims'(C).
pchar(0':) --> ":".
pchar(0'@) --> "@".



%! 'pct-encoded'(?Code:between(0,255))// .
% Similar to escape//1 in RFC 1738 which also supports
% uppercase hexadecimal letters.
%
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```

'pct-encoded'(C) --> "%", 'HEXDIG'(H1), 'HEXDIG'(H2), {C is H1 * 16 + H2}.



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

unreserved(C)   --> alphadigit(C).
unreserved(0'-) --> "-".
unreserved(0'.) --> ".".
unreserved(0'_) --> "_".
unreserved(0'~) --> "~".
