:- module(
  rfc6454_code,
  [
    'obs-fold'//0,
    'OWS'//0
  ]
).
:- reexport(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'CHAR'//1, % ?Code:code
     'CR'//0,
     'CRLF'//0,
     'CTL'//1, % ?Code:code
     'DIGIT'//1, % ?Weight:between(0,9)
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'DQUOTE'//0,
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9)
                  % ?Code:code
     'HTAB'//0,
     'LF'//0,
     'OCTET'//1, % ?Code:code
     'SP'//0,
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

:- use_module(library(dcg/dcg_ext)).





%! 'obs-fold'// .
% ```abnf
% obs-fold = CRLF ( SP / HTAB )   ; obsolete line folding
% ```

'obs-fold' --> 'CRLF', ('SP' ; 'HTAB').



%! 'OWS'// .
% ```abnf
% OWS = *( SP / HTAB / obs-fold )   ; "optional" whitespace
% ```

'OWS' --> *(ows).
ows --> 'SP'.
ows --> 'HTAB'.
ows --> 'obs-fold'.
