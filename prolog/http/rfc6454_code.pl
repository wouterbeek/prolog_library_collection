:- module(
  rfc6454_code,
  [
    'obs-fold'//1, % ?Codes:list(code)
    'OWS'//0,
    'OWS'//1 % ?Codes:list(code)
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CHAR'//1, % ?Code:code
     'CR'//1, % ?Code:code
     'CRLF'//1, % ?Codes:list(code)
     'CTL'//1, % ?Code:code
     'DIGIT'//1, % ?Weight:between(0,9)
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9)
                  % ?Code:code
     'HTAB'//1, % ?Code:code
     'LF'//1, % ?Code:code
     'OCTET'//1, % ?Code:code
     'SP'//0,
     'SP'//1, % ?Code:code
     'VCHAR'//1, % ?Code:code
     'WSP'//0
   ]).

/** <module> RFC 6454: Codes

@author Wouter Beek
@compat RFC 6454
@license MIT License
@see http://tools.ietf.org/html/rfc6454
@version 2015/11
*/





%! 'obs-fold'(?Codes:list(code))// .
% ```abnf
% obs-fold = CRLF ( SP / HTAB )   ; obsolete line folding
% ```

'obs-fold'([H1,H2,H3]) --> 'CRLF'([H1,H2]), ('SP'(H3) ; 'HTAB'(H3)).



%! 'OWS'// .
%! 'OWS'(?Codes:list(code))// .
% ```abnf
% OWS = *( SP / HTAB / obs-fold )   ; "optional" whitespace
% ```

'OWS' --> 'OWS'(_).
'OWS'([H|T]) --> ('SP'(H) ; 'HTAB'(H) ; 'obs-fold'(H)), !, 'OWS'(T).
'OWS'([])    --> "".
