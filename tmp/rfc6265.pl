:- module(
  rfc6265,
  [
    'set-cookie'//0
  ]
).

/** <module> RFC 6265: HTTP State Management Mechanism

@author Wouter Beek
@compat RFC 6265
@see https://tools.ietf.org/html/rfc6265
@version 2017/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'rfc1123-date'//1 as 'sane-cookie-date'
   ]).

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! 'cookie-av'// .
%
% ```abnf
% cookie-av = expires-av / max-age-av / domain-av / path-av / secure-av /
%             httponly-av / extension-av
% ```

'cookie-av' --> 'expires-av'.
'cookie-av' --> 'max-age-av'.
'cookie-av' --> 'domain-av'.
'cookie-av' --> 'path-av'.
'cookie-av' --> 'secure-av'.
'cookie-av' --> 'httponly-av'.
'cookie-av' --> 'extension-av'.



%! 'expires-av'// .
%
% ```abnf
% expires-av = "Expires=" sane-cookie-date
% ```

'expires-av' -->
  "Expires=",
  'sane-cookie-date'.



%! 'max-age-av'// .
%
% ```abnf
% max-age-av = "Max-Age=" non-zero-digit *DIGIT
%              ; In practice, both expires-av and max-age-av
%              ; are limited to dates representable by the
%              ; user agent.
% ```

'max-age-av' -->
  "Max-Age=",
  'non-zero-digit',
  *('DIGIT').



%! 'non-zero-digit'// .
%
% ```abnf
% non-zero-digit = %x31-39   ; digits 1 through 9
% ```

'non-zero-digit' --> todo.



%! 'set-cookie'// .

http:http_header('set-cookie').
http:http_separable('set-cookie').
'set-cookie' -->
  'set-cookie-string'.



%! 'set-cookie-header'// .
%
% ```abnf
% set-cookie-header = "Set-Cookie:" SP set-cookie-string
% ```

'set-cookie-header' -->
  "Set-Cookie:",
  'SP',
  'set-cookie-string'.



%! 'set-cookie-string'// .
%
% ```abnf
% set-cookie-string = cookie-pair *( ";" SP cookie-av )
% ```

'set-cookie-string' -->
  'cookie-pair',
  *(sep_cookie_av).



%! 'sane-cookie-date'// .
%
% ```abnf
% sane-cookie-date = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
% ```

domain-av         = "Domain=" domain-value
domain-value      = <subdomain>
                  ; defined in [RFC1034], Section 3.5, as
                  ; enhanced by [RFC1123], Section 2.1
path-av           = "Path=" path-value
path-value        = <any CHAR except CTLs or ";">
secure-av         = "Secure"
httponly-av       = "HttpOnly"
extension-av      = <any CHAR except CTLs or ";">

cookie-pair       = cookie-name "=" cookie-value
cookie-name       = token
cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                  ; US-ASCII characters excluding CTLs,
                  ; whitespace DQUOTE, comma, semicolon,
                  ; and backslash
