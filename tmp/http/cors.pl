:- module(
  cors,
  [
    'access-control-allow-credentials'//1, % -AllowCredentials:oneof([true])
    'access-control-allow-headers'//1,     % -HeaderNames:list(string)
    'access-control-allow-methods'//1,     % -Methods:list(string)
    'access-control-allow-origin'//1,      % -Origins:list
    'access-control-max-age'//1            % -Delta:nonneg
  ]
).

/** <module> Cross-Origin Resource Sharing

@author Wouter Beek
@compat Cross-Origin Resource Sharing
@see http://www.w3.org/TR/cors
@version 2015/11-2016/02, 2016/12-2017/01
*/

:- use_module(library(dcg)).
:- use_module(library(http/dcg_http)).
:- use_module(library(http/http11), [
     'delta-seconds'//1, % -Delta:nonneg
     'field-name'//1,    % -Name:atom
     method//1           % -Method:atom
   ]).
:- use_module(library(http/rfc6454)).





%! 'access-control-allow-credentials'(-AllowCredentials:oneof([true]))// is det.
%
% ```abnf
% Access-Control-Allow-Credentials: "Access-Control-Allow-Credentials" ":" true
% ```

'access-control-allow-credentials'(true) -->
  atom_ci(true).



%! 'access-control-allow-headers'(-HeaderNames:list(atom))// is det.
%
% ```abnf
% Access-Control-Allow-Headers: "Access-Control-Allow-Headers" ":" #field-name
% ```

'access-control-allow-headers'(Names) -->
  *#('field-name', Names), !.



%! 'access-control-allow-methods'(-Methods:list(atom))// is det.
%
% ```abnf
% Access-Control-Allow-Methods: "Access-Control-Allow-Methods" ":" #Method
% ```

'access-control-allow-methods'(Methods) -->
  *#(method, Methods), !.



%! 'access-control-allow-origin'(-Origins:list(atom))// is det.
%
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'access-control-allow-origin'(Origins) -->
  'origin-list-or-null'(Origins), !.
'access-control-allow-origin'([_]) -->
  "*".



%! 'access-control-expose-headers'(-L)// is det.
%
% ```abnf
% Access-Control-Expose-Headers = "Access-Control-Expose-Headers" ":" #field-name
% ```

'access-control-expose-headers'(L) -->
  *#('field-name', L).



%! 'access-control-max-age'(-Delta:nonneg)// is det.
%
% ```abnf
% Access-Control-Max-Age = "Access-Control-Max-Age" ":" delta-seconds
% ```

'access-control-max-age'(Delta) -->
  'delta-seconds'(Delta).



%! 'access-control-request-method'(-Method:atom)// is det.
%
% ```abnf
% Access-Control-Request-Method: "Access-Control-Request-Method" ":" Method
% ```

'access-control-request-method'(Method) -->
  method(Method).
