:- module(
  rfc5987_code,
  [
     'attr-char'//1, % ?Code:code
     'mime-charsetc'//1 % ?Code:code
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'DIGIT'//1, % ?Weight:between(0,9)
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9)
                  % ?Code:code
     'LWSP'//0
]).
:- reexport(library(uri/rfc3986), [
     'pct-encoded'//1 % ?Code:between(0,255)
   ]).

/** <module> RFC 5987: Codes

@author Wouter Beek
@compat RFC 5987
@see http://tools.ietf.org/html/rfc5987
@version 2015/11
*/





%! 'attr-char'(?Code:code)// .
% ```abnf
% attr-char = ALPHA / DIGIT
%           / "!" / "#" / "$" / "&" / "+" / "-" / "."
%           / "^" / "_" / "`" / "|" / "~"
%           ; token except ( "*" / "'" / "%" )
% ```

'attr-char'(C)   --> 'ALPHA'(C).
'attr-char'(C)   --> 'DIGIT'(C).
'attr-char'(0'!) --> "!".
'attr-char'(0'#) --> "#".
'attr-char'(0'$) --> "$".
'attr-char'(0'&) --> "&".
'attr-char'(0'+) --> "+".
'attr-char'(0'-) --> "-".
'attr-char'(0'.) --> ".".
'attr-char'(0'^) --> "^".
'attr-char'(0'_) --> "_".
'attr-char'(0'`) --> "`".
'attr-char'(0'|) --> "|".
'attr-char'(0'~) --> "~".



%! 'mime-charsetc'(?Code:code)// .
% ```abnf
% mime-charsetc = ALPHA / DIGIT
%               / "!" / "#" / "$" / "%" / "&"
%               / "+" / "-" / "^" / "_" / "`"
%               / "{" / "}" / "~"
%               ; as <mime-charset> in Section 2.3 of [RFC2978]
%               ; except that the single quote is not included
%               ; SHOULD be registered in the IANA charset registry
% ```

'mime-charsetc'(C)   --> 'ALPHA'(C).
'mime-charsetc'(C)   --> 'DIGIT'(C).
'mime-charsetc'(0'!) --> "!".
'mime-charsetc'(0'#) --> "#".
'mime-charsetc'(0'$) --> "$".
'mime-charsetc'(0'%) --> "%".
'mime-charsetc'(0'&) --> "&".
'mime-charsetc'(0'+) --> "+".
'mime-charsetc'(0'-) --> "-".
'mime-charsetc'(0'^) --> "^".
'mime-charsetc'(0'_) --> "_".
'mime-charsetc'(0'`) --> "`".
'mime-charsetc'(0'{) --> "{".
'mime-charsetc'(0'}) --> "}".
'mime-charsetc'(0'~) --> "~".
