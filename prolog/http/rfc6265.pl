:- module(
  rfc6265,
  [
    'set-cookie'//1 % -Pairs:list(pair)
  ]
).

/** <module> RFC 6265: HTTP State Management Mechanism

@author Wouter Beek
@compat RFC 6265
@version 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(http/rfc1034)).
:- use_module(library(http/rfc2616_date)).
:- use_module(library(http/rfc2616_token)).





%! 'cookie-av'(-Pair:pair)// is det.
% ```abnf
% cookie-av = expires-av / max-age-av / domain-av /
%             path-av / secure-av / httponly-av /
%             extension-av
% ```

'cookie-av'(X) --> 'expires-av'(X), !.
'cookie-av'(X) --> 'max-age-av'(X), !.
'cookie-av'(X) --> 'domain-av'(X), !.
'cookie-av'(X) --> 'path-av'(X), !.
'cookie-av'(X) --> 'secure-av'(X), !.
'cookie-av'(X) --> 'httponly-av'(X), !.
'cookie-av'(X) --> 'extension-av'(X).



%! 'cookie-pair'(-Pair:pair(string))// .
% ```abnf
% cookie-pair = cookie-name "=" cookie-value
% ```

'cookie-pair'(N-V) --> 'cookie-name'(N), "=", 'cookie-value'(V).



%! 'cookie-name'(-Name:string)// is det.
% ```abnf
% cookie-name = token
% ```

'cookie-name'(S) --> token(S).



%! 'cookie-octet'(-Code:code)// .
% ```abnf
% cookie-octet = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
%                ; US-ASCII characters excluding CTLs,
%                ; whitespace DQUOTE, comma, semicolon,
%                ; and backslash
% ```

'cookie-octet'(0x21) --> [0x21], !.
'cookie-octet'(C) -->
  [C],
  (   {between(0x23, 0x2B, C)}, !
  ;   {between(0x2D, 0x3A, C)}, !
  ;   {between(0x3C, 0x5B, C)}, !
  ;   {between(0x5D, 0x7E, C)}
  ).



%! 'cookie-value'(-Value)// is det.
% ```abnf
% cookie-value = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
% ```

'cookie-value'(S) --> 'DQUOTE', !, dcg_string('cookie-octet', S), 'DQUOTE'.
'cookie-value'(S) --> dcg_string(cookie_value_codes, S).
cookie_value_codes(Cs) --> *('cookie-octet', Cs).



%! 'domain-av'(-Domain:pair(atom))// .
% ```abnf
% domain-av = "Domain=" domain-value
% ```

'domain-av'(domain-V) --> "Domain=", 'domain-value'(V).



%! 'domain-value'(-Subdomain:atom)// .
% ```abnf
% domain-value = <subdomain>
%                ; defined in [RFC1034], Section 3.5, as
%                ; enhanced by [RFC1123], Section 2.1
% ```

'domain-value'(V) --> subdomain(V).



%! 'expires-av'(-Expires:pair(atom,compound))// .
% ```abnf
% expires-av = "Expires=" sane-cookie-date
% ```

'expires-av'(expires-DT) --> "Expires=", 'sane-cookie-date'(DT).



%! 'extension-av'(-Extension:pair(oneof([extension]),string))// .
% ```abnf
% extension-av = <any CHAR except CTLs or ";">
% ```

'extension-av'(extension-S) --> word(S).



%! 'httponly-av'(-HttpOnly:oneof([http_only]))// .
% ```abnf
% httponly-av = "HttpOnly"
% ```

'httponly-av'(http_only) --> "HttpOnly".



%! 'max-age-av'(-MaximumAge:pair(atom,nonneg))// is det.
% ```abnf
% max-age-av = "Max-Age=" non-zero-digit *DIGIT
%              ; In practice, both expires-av and max-age-av
%              ; are limited to dates representable by the
%              ; user agent.
% ```

'max-age-av'(max_age-N) -->
  "Max-Age=",
  'non-zero-digit'(H),
  *(digit, T),
  {pos_sum([H|T], N)}.



%! 'non-zero-digit'(-Digit:between(1,9))// is det.
% ```abnf
% non-zero-digit = %x31-39   ; digits 1 through 9
% ```

'non-zero-digit'(1) --> "1".
'non-zero-digit'(2) --> "2".
'non-zero-digit'(3) --> "3".
'non-zero-digit'(4) --> "4".
'non-zero-digit'(5) --> "5".
'non-zero-digit'(6) --> "6".
'non-zero-digit'(7) --> "7".
'non-zero-digit'(8) --> "8".
'non-zero-digit'(9) --> "9".



%! 'path-av'(-Path:pair(atom,string))// .
% ```abnf
% path-av = "Path=" path-value
% ```

'path-av'(path-S) --> "Path=", 'path-value'(S).



%! 'path-value'(-Path:stiring)// .
% ```abnf
% path-value = <any CHAR except CTLs or ";">
% ```

'path-value'(S) --> word(S).



%! 'sane-cookie-date'(-DateTime:compound)// is det.
% ```abnf
% sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
% ```

'sane-cookie-date'(V) --> 'rfc1123-date'(V).



%! 'secure-av'(-Secure:oneof([secure]))// is det.
% ```abnf
% secure-av = "Secure"
% ```

'secure-av'(secure) --> "Secure".



%! 'set-cookie'(-Pairs:list(pair))// is det.
% ```abnf
% set-cookie-header = "Set-Cookie:" SP set-cookie-string
% ```

'set-cookie'(L) --> 'set-cookie-string'(L).



%! 'set-cookie-string'(-Pairs:list(pair))// is det.
% ```abnf
% set-cookie-string = cookie-pair *( ";" SP cookie-av )
% ```

'set-cookie-string'([H|T]) --> 'cookie-pair'(H), *(sep_cookie_av, T).
sep_cookie_av(X) --> ";", 'SP', 'cookie-av'(X).





% HELPERS %

word(S) --> dcg_string(word_codes, S).
word_codes(Cs) --> *(word_code, Cs).
word_code(C) --> 'CHAR'(C), {\+ 'CTL'(C, _, _), C \= 0';}.
