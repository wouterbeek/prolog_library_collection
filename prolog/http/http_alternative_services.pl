:- module(
  http_alternative_services,
  [
    'alt-svc'//1 % -AltVals:list
  ]
).

/** <module> HTTP Alernative Services

@author Wouter Beek
@see http://httpwg.org/http-extensions/alt-svc.html
@version 2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/http11)).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:atom
     token//1            % -Token:atom
   ]).





%! 'alt-authority'(-Auth:atom)// .
%
% ```bnf
% alt-authority = quoted-string ; containing [ uri-host ] ":" port
% ```

'alt-authority'(Auth) -->
  'quoted-string'(Auth).



%! 'alt-svc'(-AltVals:list(pair(compound,list(compound))))// .
%
% ```bnf
% Alt-Svc = clear / 1#alt-value
% ```

'alt-svc'([]) -->
  clear.
'alt-svc'(AltVals) -->
  +('alt-value', AltVals).



%! 'alt-value'(-AltVal:pair(compound,list(compound)))// .
%
% ```bnf
% alt-value = alternative *( OWS ";" OWS parameter )
% ```

'alt-value'(Alt-Params) -->
  alternative(Alt),
  *(alt_value, Params).

alt_value(Param) -->
  'OWS',
  ";",
  'OWS',
  parameter(Param).



%! alternative(-Alt:compound)// .
%
% ```bnf
% alternative = protocol-id "=" alt-authority
% ```

alternative(Alt) -->
  'protocol-id'(Key),
  "=",
  'alt-authority'(Val),
  {Alt =.. [Key,Val]}.



%! clear// .
%
% ```bnf
% clear = %s"clear"; "clear", case-sensitive
% ```

clear -->
  atom_ci(clear).



%! 'protocol-id'(-Protocol:atom)// .
%
% ```bnf
% protocol-id = token ; percent-encoded ALPN protocol name
% ```

'protocol-id'(Protocol) -->
  token(Protocol).



%! parameter(-Param:compound)// .
%
% ```bnf
% parameter = token "=" ( token / quoted-string )
% ```

parameter(Param) -->
  token(Key),
  "=",
  (token(Val) -> "" ; 'quoted-string'(Val)),
  {Param =.. [Key,Val]}.
