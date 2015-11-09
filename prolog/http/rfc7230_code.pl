:- module(
  rfc7230_code,
  [
    'BWS'//0,
    'obs-text'//1, % ?Code:code
    'quoted-pair'//1, % ?Code:code
    'OWS'//0,
    qdtext//1, % ?Code:code
    'RWS'//0,
    tchar//1 % ?TokenCharacter:code
  ]
).
:- reexport(
  library(dcg/rfc2234),
  [
    'ALPHA'//1, % ?Code:code
    'CR'//0,
    'CRLF'//0,
    'CTL'//0,
    'DIGIT'//1, % ?Weight:between(0,9)
    'DIGIT'//2, % ?Weight:between(0,9)
                % ?Code:code
    'DQUOTE'//0,
    'HEXDIG'//1, % ?Weight:between(0,15)
    'HEXDIG'//2, % ?Weight:between(0,15)
                 % ?Code:code
    'HTAB'//0,
    'LF'//0,
    'OCTET'//1,
    'OWS'//0,
    'RWS'//0,
    'SP'//0,
    'VCHAR'//1
  ]
).

/** <module> RFC 7230: Codes

@author Wouter Beek
@version 2015/11
*/





%! 'BWS'// .
%! 'BWS'(?Code:code)// .
% ```abnf
% BWS = OWS
% ```

'BWS' --> 'BWS'(_).
'BWS'(C) --> 'OWS'(C).



%! 'obs-text'(?Code:code)// .
% ```abnf
% obs-text = %x80-FF
% ```

'obs-text'(C) --> between_code_radix(hex('80'), hex('FF'), C).



%! 'OWS'// .
%! 'OWS'(?Codes:list(code))// .
% ```abnf
% OWS = *( SP / HTAB )
% ```

'OWS' --> 'OWS'(_).
'OWS'([H|T]) --> 'SP'(H), !, 'OWS'(T).
'OWS'([H|T]) --> 'HTAB'(H), !, 'OWS'(T).
'OWS'([]) --> "".



%! qdtext(?Code:code)// .
% ```abnf
% qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
% ```

qdtext(C) --> 'HTAB'(C).
qdtext(C) --> 'SP'(C).
qdtext(C) --> code_radix(hex(21), C).
qdtext(C) --> between_code_radix(hex(23), hex('5B'), C).
qdtext(C) --> between_code_radix(hex('5D'), hex('7E'), C).
qdtext(C) --> 'obs-text'(C).



%! 'quoted-pair(?Code:code)// .
% ```abnf
% quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
% ```

'quoted-pair'(C) --> "\\", ('HTAB'(C) ; 'SP'(C) ; 'VCHAR'(C) ; 'obs-text'(C)).



%! 'RWS'// .
%! 'RWS'(?Codes:list(code))// .
% ```abnf
% RWS = 1*( SP / HTAB )
% ```

'RWS' --> 'RWS'(_).
'RWS'([H|T]) --> ('SP'(H) ; 'HTAB'(H)), 'OWS'(T).



%! tchar(?TokenCharacter:code)// .
% ```abnf
% tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
%       / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
%       / DIGIT / ALPHA
%       ; any VCHAR, except delimiters
% ```

tchar(0'!) --> "!".
tchar(0'#) --> "#".
tchar(0'$) --> "$".
tchar(0'%) --> "%".
tchar(0'&) --> "&".
tchar(0'') --> "'".
tchar(0'*) --> "*".
tchar(0'+) --> "+".
tchar(0'-) --> "-".
tchar(0'.) --> ".".
tchar(0'^) --> "^".
tchar(0'_) --> "_".
tchar(0'`) --> "`".
tchar(0'|) --> "|".
tchar(0'~) --> "~".
tchar(C) --> 'DIGIT'(_, C).
tchar(C) --> 'ALPHA'(C).
