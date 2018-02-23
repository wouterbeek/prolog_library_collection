:- module(
  rfc7234,
  [
  ]
).

/** <module> RFC 7234 - HTTP/1.1: Caching

@author Wouter Beek
@compat RFC 7234
@see https://tools.ietf.org/html/rfc7234
@version 2017/05-2017/06
*/

:- use_module(library(dcg)).
:- use_module(library(http/rfc7230)).
:- use_module(library(http/rfc7231)).
:- use_module(library(math_ext)).
:- use_module(library(uri/rfc3986)).

:- discontiguous
    http:http_header/1,
    http:http_separable/1.

:- dynamic
    http:http_header/1,
    http:http_separable/1.

:- multifile
    http:http_header/1,
    http:http_separable/1.





%! age(-Age:nonneg)// is det.
%
% ```abnf
% Age = delta-seconds
% ```

http:http_header(age).
age(Age) -->
  'delta-seconds'(Age).



%! 'cache-control'(-Directives:list(dict))// is det.
%
% ```abnf
% Cache-Control = 1#cache-directive
% ```

http:http_header('cache-control').
http:http_separable('cache-control').
'cache-control'(Directives) -->
  +##('cache-directive', Directives).



%! 'cache-directive'(-Directive:or([atom,pair(atom)]))// is det.
%
% ```abnf
% cache-directive = token [ "=" ( token | quoted-string ) ]
% ```

'cache-directive'(Directive) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Directive = Key-Val}
  ;   {Directive = Key}
  ).



%! 'delta-seconds'(-Delta:nonneg)// is det.
%
% ```abnf
% delta-seconds = 1*DIGIT
% ```

'delta-seconds'(Delta) -->
  dcg_integer(+('DIGIT'), Delta).



%! expires(-Datetime:compound)// is det.
%
% ```abnf
% Expires = HTTP-date
% ```

http:http_header(expires).
expires(Datetime) -->
  'HTTP-date'(Datetime).



%! 'extension-pragma'(-Extension:or([atom,pair(atom)]))// is det.
%
% ```abnf
% extension-pragma = token [ "=" ( token | quoted-string ) ]
% ```

'extension-pragma'(Extension) -->
  token(Key),
  (   "="
  ->  (token(Val) -> "" ; 'quoted-string'(Val)),
      {Extension = Key-Val}
  ;   {Extension = Key}
  ).



%! pragma(?Pragmas:list(or([atom,pair(atom)])))// is det.
%
% ```abnf
% Pragma = 1#pragma-directive
% ```

http:http_header(pragma).
http:http_separable(pragma).
pragma(Pragmas) -->
  +##('pragma-directive', Pragmas).



%! 'pragma-directive'(-Pragma:or([atom,pair(atom)]))// is det.
%
% ```abnf
% pragma-directive = "no-cache" | extension-pragma
% ```

'pragma-directive'('no-cache') -->
  atom_ci('no-cache'), !.
'pragma-directive'(Pragma) -->
  'extension-pragma'(Pragma).



%! 'warn-agent'(-Authority:compound)// is det.
%
% ```abnf
% warn-agent = ( uri-host [ ":" port ] ) | pseudonym
%            ; the name or pseudonym of the server adding
%            ; the Warning header field, for use in debugging
%            ; a single "-" is recommended when agent unknown
% ```

'warn-agent'(auth(_User,_Password,Host,Port)) -->
  'uri-host'(Host),
  (":" -> port(Port) ; ""), !.
'warn-agent'(Pseudonym) -->
  pseudonym(Pseudonym).



%! 'warn-code'(-Code:between(0,999))// is det.
%
% ```abnf
% warn-code = 3DIGIT
% ```

'warn-code'(Code) -->
  dcg_integer(#(3, 'DIGIT'), Code).



%! 'warn-date'(-Datetime:compound)// is det.
%
% ```abnf
% warn-date = DQUOTE HTTP-date DQUOTE
% ```

'warn-date'(Datetime) -->
  'DQUOTE',
  'HTTP-date'(Datetime),
  'DQUOTE'.



%! 'warn-text'(-Text:atom)// is det.
%
% ```abnf
% warn-text = quoted-string
% ```

'warn-text'(Text) -->
  'quoted-string'(Text).



%! warning(-Warnings:list(compound))// is det.
%
% ```abnf
% warning = 1#warning-value
% ```

warning(Warnings) -->
  +##('warning-value', Warnings).



%! 'warning-value'(-Warning:compound)// is det.
%
% ```abnf
% warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
% ```

'warning-value'(warning(Code,Agent,Text,Datetime)) -->
  'warn-code'(Code),
  'SP',
  'warn-agent'(Agent),
  'SP',
  'warn-text'(Text),
  ('SP' -> 'warn-date'(Datetime) ; "").
