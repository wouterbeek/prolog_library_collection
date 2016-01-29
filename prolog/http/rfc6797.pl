:- module(
  rfc6797,
  [
    'strict-transport-security'//1 % -Directives:list(dict)
  ]
).

/** <module> RFC 6797: HTTP Strict Transport Security (HSTS)

@author Wouter Beek
@compat RFC 6797
@see https://tools.ietf.org/html/rfc6797
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     'quoted-string'//1, % -String:string
     token//1            % -Token:string
   ]).





%! directive(-Directive:dict)// is det.
% ```abnf
% directive = directive-name [ "=" directive-value ]
% ```

directive(D2) -->
  {D1 = _{'@type': 'llo:directive', 'llo:key': Key}},
  'directive-name'(Key),
  ("=" -> 'directive-value'(Value), {D2 = D1.put({'llo:value': Value})} ; {D2 = D1}).



%! 'directive-name'(-Name:string)// is det.
% ```abnf
% directive-name = token
% ```

'directive-name'(S) --> token(S).



%! 'directive-value'(-Value:string)// is det.
% ```abnf
% directive-value = token | quoted-string
% ```

'directive-value'(S) --> token(S), !.
'directive-value'(S) --> 'quoted-string'(S).



%! 'strict-transport-security'(-Directives:list(dict))// is det.
% ```abnf
% Strict-Transport-Security = "Strict-Transport-Security" ":"
%                             [ directive ]  *( ";" [ directive ] )
% ```

'strict-transport-security'([H|T]) --> directive(H), !, *(sep_directive, T).
'strict-transport-security'(L)     --> *(sep_directive, L).

sep_directive(X) --> ";", ?('LWS'), directive(X).
