:- module(
  rfc6797,
  [
    'strict-transport-security'//1 % ?Directives:list(or([string,pair(string)]))
  ]
).

/** <module> RFC 6797: HTTP Strict Transport Security (HSTS)

@author
@compat RFC 6797
@see https://tools.ietf.org/html/rfc6797
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     'quoted-string'//1,
     token//1
   ]).





%! directive(?Directive:or([string,pair(string)]))// .
% ```abnf
% directive = directive-name [ "=" directive-value ]
% ```

directive(T) -->
  'directive-name'(N),
  ("=" -> 'directive-value'(V), {T = N-V} ; {T = N}).



%! 'directive-name'(?Name:string)// .
% ```abnf
% directive-name = token
% ```

'directive-name'(S) --> token(S).



%! 'directive-value'(?Value:string)// .
% ```abnf
% directive-value = token | quoted-string
% ```

'directive-value'(S) --> token(S), !.
'directive-value'(S) --> 'quoted-string'(S).



%! 'strict-transport-security'(?Directives:list(or([string,pair(string)])))// .
% ```abnf
% Strict-Transport-Security = "Strict-Transport-Security" ":"
%                             [ directive ]  *( ";" [ directive ] )
% ```

'strict-transport-security'([H|T]) --> directive(H), !, *(sep_directive, T).
'strict-transport-security'(L)     --> *(sep_directive, L).
sep_directive(X) --> ";", ?('LWS'), directive(X).
