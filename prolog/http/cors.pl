:- module(
  cors,
  [
    'access-control-allow-credentials'//1, % -AllowCredentials:dict
    'access-control-allow-headers'//1,     % -HeaderNames:list(string)
    'access-control-allow-methods'//1,     % -Methods:list(string)
    'access-control-allow-origin'//1       % -Origins:list
  ]
).

/** <module> Cross-Origin Resource Sharing

@author Wouter Beek
@compat Cross-Origin Resource Sharing
@see http://www.w3.org/TR/cors
@version 2015/11-2016/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http11), [
     'field-name'//1, % -Name:string
     method//1        % -Method:string
   ]).
:- use_module(library(http/rfc6454)).





%! 'access-control-allow-credentials'(-AllowCredentials:dict)// is det.
% ```abnf
% Access-Control-Allow-Credentials: "Access-Control-Allow-Credentials" ":" true
% ```

'access-control-allow-credentials'(_{
  '@type': 'xsd:boolean',
  '@value': true
}) -->
  atom_ci(true).



%! 'access-control-allow-headers'(-HeaderNames:list(string))// is det.
% ```abnf
% Access-Control-Allow-Headers: "Access-Control-Allow-Headers" ":" #field-name
% ```

'access-control-allow-headers'(_{'@list': L}) --> *#('field-name', L).



%! 'access-control-allow-methods'(-Methods:list(string))// is det.
% ```abnf
% Access-Control-Allow-Methods: "Access-Control-Allow-Methods" ":" #Method
% ```

'access-control-allow-methods'(_{'@list': L}) --> *#(method, L).



%! 'access-control-allow-origin'(-Origins)// is det.
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'access-control-allow-origin'(_{'@list': L})   --> 'origin-list-or-null'(L).
'access-control-allow-origin'("*") --> "*".



%! 'access-control-request-method'(-Method)// is det.
% ```abnf
% Access-Control-Request-Method: "Access-Control-Request-Method" ":" Method
% ```

'access-control-request-method'(M) --> method(M).
