:- module(
  csp2,
  [
    'content-security-policy'//1,             % ?Policies:list(dict)
    'content-security-policy-report-only'//1, % ?Policies:list(dict)
    csp//1                                    % -Value:string
  ]
).

/** <module> Content Security Policy Level 2

@author Wouter Beek
@compat Content Secirity Policy Level 2 W3C Candicate Recommendation
@see http://www.w3.org/TR/CSP2/
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'VCHAR'//1, % ?Code:code
     'WSP'//0,
     'WSP'//1 % ?Code:code
   ]).
:- use_module(library(http/dcg_http)).





%! 'content-security-policy'(-Policies:list(dict))// is det.
% ```abnf
% "Content-Security-Policy:" 1#policy-token
% ```

'content-security-policy'(L) --> '+#'('policy-token', L).



%! 'content-security-policy-report-only'(-Policies:list(dict))// is det.
% ```abnf
% "Content-Security-Policy-Report-Only:" 1#policy-token
% ```

'content-security-policy-report-only'(L) --> '+#'('policy-token', L).



%! csp(-Value:string)// is det.
% ```abnf
% "CSP:" csp-header-value
% ```

csp(S) --> 'csp-header-value'(S).



%! 'csp-header-value'(-Value:string)// is det.
% ```abnf
% csp-header-value = *WSP "active" *WSP
% ```

'csp-header-value'("active") --> *('WSP'), atom_ci(active), *('WSP').



%! 'directive-name'(-Name:atom)// is det.
% ```abnf
% directive-name = 1*( ALPHA / DIGIT / "-" )
% ```

'directive-name'(A) --> +(directive_name_code, Cs), {atom_codes(A, Cs)}.
directive_name_code(C)   --> 'ALPHA'(C).
directive_name_code(C)   --> 'DIGIT'(_, C).
directive_name_code(0'-) --> "-".



%! 'directive-token'(-Directive:pair)// is det.
% ```abnf
% directive-token = *WSP [ directive-name [ WSP directive-value ] ]
% ```
%
% @bug This grammar allows an infinity of empty directive tokens.

'directive-token'(X) -->
  'directive-name'(N), !,
  ('WSP' -> 'directive-value'(V), {X = N-V} ; {X = N-true}).



%! 'directive-value'(-Value:string)// is det.
% ```abnf
% directive-value = *( WSP / <VCHAR except ";" and ","> )
% ```

'directive-value'(S) --> *(directive_value_code, Cs), {string_codes(S, Cs)}.
directive_value_code(C) --> 'WSP'(C).
directive_value_code(_) --> ";", !, {fail}.
directive_value_code(_) --> ",", !, {fail}.
directive_value_code(C) --> 'VCHAR'(C).



%! 'policy-token'(-Policy:dict)// is det.
% ```abnf
% policy-token = [ directive-token *( ";" [ directive-token ] ) ]
% ```
%
% @bug This grammar allows an infinity of empty policy tokens.

'policy-token'(D) -->
  'directive-token'(H), !,
  directive_tokens(T),
  {dict_pairs(D, policy, [H|T])}.

% A directive token must be preceded by a separator.
directive_tokens([H|T]) --> sep, 'directive-token'(H), !, directive_tokens(T).
% Allow consecutive separators.
directive_tokens(L)   --> +(sep), !, directive_tokens(L).
directive_tokens([])  --> "".

sep --> ";", *('WSP').
