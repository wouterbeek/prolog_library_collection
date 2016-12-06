:- module(
  rfc6797,
  [
    'strict-transport-security'//1 % -Directives:list(pair(atom))
  ]
).

/** <module> RFC 6797: HTTP Strict Transport Security (HSTS)

@author Wouter Beek
@compat RFC 6797
@see https://tools.ietf.org/html/rfc6797
@version 2015/11-2016/01, 2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     'quoted-string'//1, % -String:atom
     token//1            % -Token:atom
   ]).

:- multifile
    http_known_known/1.





%! directive(-Directive:pair(atom))// is det.
%
% ```abnf
% directive = directive-name [ "=" directive-value ]
% ```

directive(Key-Val) -->
  'directive-name'(Key),
  ("=" -> 'directive-value'(Val) ; {Val = true}).



%! 'directive-name'(-Name:atom)// is det.
%
% ```abnf
% directive-name = token
% ```

'directive-name'(Name) -->
  token(Name).



%! 'directive-value'(-Val:atom)// is det.
%
% ```abnf
% directive-value = token | quoted-string
% ```

'directive-value'(Val) -->
  token(Val), !.
'directive-value'(Val) -->
  'quoted-string'(Val).



%! 'strict-transport-security'(-Directives:list(compound))// is det.
%
% ```abnf
% Strict-Transport-Security = "Strict-Transport-Security" ":"
%                             [ directive ]  *( ";" [ directive ] )
% ```

http_known_known('strict-transport-security').
'strict-transport-security'(L) -->
  (directive(H) -> {L = [H|T]} ; {L = T}),
  *(sep_directive, T), !.

sep_directive(X) -->
  ";",
  ?('LWS'),
  directive(X).
