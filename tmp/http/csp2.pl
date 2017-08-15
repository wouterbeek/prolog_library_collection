:- module(
  csp2,
  [
    'content-security-policy'//1,             % ?Policies
    'content-security-policy-report-only'//1, % ?Policies
    csp//1                                    % -Value
  ]
).

/** <module> Content Security Policy Level 2

@author Wouter Beek

@compat Content Security Policy Level 2 W3C Candicate Recommendation

@see http://www.w3.org/TR/CSP2/

@version 2015/11-2015/12, 2017/01, 2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code
     'DIGIT'//2, % ?Weight
                 % ?Code
     'VCHAR'//1, % ?Code
     'WSP'//0,
     'WSP'//1    % ?Code
   ]).
:- use_module(library(http/dcg_http)).





%! 'content-security-policy'(-Policies:list(atom))// is det.
%
% ```abnf
% "Content-Security-Policy:" 1#policy-token
% ```

'content-security-policy'(Policies) -->
  '+#'('policy-token', Policies), !.



%! 'content-security-policy-report-only'(-Policies:list(atom))// is det.
%
% ```abnf
% "Content-Security-Policy-Report-Only:" 1#policy-token
% ```

'content-security-policy-report-only'(Policies) -->
  '+#'('policy-token', Policies), !.



%! csp(-Value:oneof([active]))// is det.
%
% ```abnf
% "CSP:" csp-header-value
% ```

csp(S) -->
  'csp-header-value'(S).



%! 'csp-header-value'(?Value:oneof([active]))// is det.
%
% ```abnf
% csp-header-value = *WSP "active" *WSP
% ```

'csp-header-value'(active) -->
  *('WSP'), !,
  atom_ci(active),
  *('WSP'), !.



%! 'directive-name'(?Name:atom)// is det.
%
% ```abnf
% directive-name = 1*( ALPHA / DIGIT / "-" )
% ```

'directive-name'(Name) -->
  dcg_atom(+(directive_name_), Name).

'directive-name_'(Code) --> 'ALPHA'(Code).
'directive-name_'(Code) --> 'DIGIT'(_, Code).
'directive-name_'(0'-) --> "-".



%! 'directive-token'(-Directive:pair(atom))// is det.
%
% ```abnf
% directive-token = *WSP [ directive-name [ WSP directive-value ] ]
% ```
%
% @bug This grammar allows an infinity of empty directive tokens.

'directive-token'(Pair) -->
  'directive-name'(Key),
  ('WSP' -> 'directive-value'(Val), {Pair = Key-Val} ; {Pair = Key-true}).



%! 'directive-value'(?Value:atom)// is det.
%
% ```abnf
% directive-value = *( WSP / <VCHAR except ";" and ","> )
% ```

'directive-value'(Value) -->
  dcg_atom(*('directive-value_'), Value).

'directive-value_'(Code) --> 'WSP'(Code).
'directive-value_'(_) --> ";", {fail}.
'directive-value_'(_) --> ",", {fail}.
'directive-value_'(Code) --> 'VCHAR'(Code).



%! 'policy-token'(-Policy:list(pair(atom)))// is det.
%
% ```abnf
% policy-token = [ directive-token *( ";" [ directive-token ] ) ]
% ```
%
% @bug This grammar allows an infinity of empty policy tokens.

'policy-token'([H|T]) -->
  'directive-token'(H),
  directive_tokens(T).

% A directive token must be preceded by a separator.
directive_tokens([H|T]) -->
  sep,
  'directive-token'(H), !,
  directive_tokens(T).
% Allow consecutive separators.
directive_tokens(L) -->
  +(sep), !,
  directive_tokens(L).
directive_tokens([]) --> "".

sep -->
  ";",
  *('WSP'), !.
