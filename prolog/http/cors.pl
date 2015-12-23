:- module(
  cors,
  [
    'access-control-allow-credentials'//1, % -AllowCredentials:boolean
    'access-control-allow-headers'//1, % -HeaderNames:list(string)
    'access-control-allow-methods'//1, % -Methods:list
    'access-control-allow-origin'//1 % -Origins:list(dict)
  ]
).

/** <module> Cross-Origin Resource Sharing

@author Wouter Beek
@compat Cross-Origin Resource Sharing
@see http://www.w3.org/TR/cors
@version 2015/11-2015/12
*/

:- use_module(library(http/dcg_http)).
:- use_module(library(http/rfc2616), ['Method'//1]).
:- use_module(library(http/rfc2616_header), ['field-name'//1]).
:- use_module(library(http/rfc6454)).





%! 'access-control-allow-credentials'(-AllowCredentials:boolean)// is det.
% ```abnf
% Access-Control-Allow-Credentials: "Access-Control-Allow-Credentials" ":" true
% ```

'access-control-allow-credentials'(true) --> "true".



%! 'access-control-allow-headers'(-HeaderNames:list(string))// is det.
% ```abnf
% Access-Control-Allow-Headers: "Access-Control-Allow-Headers" ":" #field-name
% ```

'access-control-allow-headers'(L) --> *#('field-name', L).



%! 'access-control-allow-methods'(-Methods:list)// is det.
% ```abnf
% Access-Control-Allow-Methods: "Access-Control-Allow-Methods" ":" #Method
% ```

'access-control-allow-methods'(L) --> *#('Method', L).



%! 'access-control-allow-origin'(-Origins:list(dict))// is det.
% ```abnf
% Access-Control-Allow-Origin = "Access-Control-Allow-Origin"
%                               ":" origin-list-or-null
%                             | "*"
% ```

'access-control-allow-origin'(L) --> 'origin-list-or-null'(L).
'access-control-allow-origin'(*) --> "*".
