:- module(
  rfc6266,
  [
    'Content-Disposition'//1, % ?ContentDisposition:compound
    'content-disposition'//2 % ?Type:string
                             % ?Parameters:list(pair)
  ]
).

/** <module> RFC 6266: Use of the Content-Disposition Header Field in the HTTP

@author Wouter Beek
@compat RFC 6266
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(http/rfc2616_code)).
:- use_module(library(http/rfc2616_token)).
:- use_module(library(http/rfc5987)).





%! 'content-disposition'(?Type:string, ?Parameters:list(pair(string)))// .
% ```abnf
% content-disposition = "Content-Disposition" ":"
%                       disposition-type *( ";" disposition-parm )
% ```

'content-disposition'(Type, Params) -->
  "Content-Disposition", ":", 'LWS',
  'Content-Disposition'('Content-Disposition'(Type, Params)).
'Content-Disposition'('Content-Disposition'(Type,Params)) -->
  'disposition-type'(Type),
  *(disposition_param, Params, []).
disposition_param(Param) --> ";", 'disposition-parm'(Param).



%! 'disp-ext-parm(?Parameter:pair(string))// .
% ```abnf
% disp-ext-parm = token "=" value
%               | ext-token "=" ext-value
% ```

'disp-ext-parm'(Key-Val) --> token(Key), "=", value(Val).
'disp-ext-parm'(Key-x(X,Y,Z)) --> 'ext-token'(Key), "=", 'ext-value'(X, Y, Z).



%! 'disp-ext-type'(?Type:string)// .
% ```abfn
% disp-ext-type = token
% ```

'disp-ext-type'(Type) --> token(Type).



%! 'disposition-parm'(?Parameters:list(pair(string)))// .
% ```abnf
% disposition-parm = filename-parm | disp-ext-parm
% ```

'disposition-parm'(Param) --> 'filename-parm'(Param).
'disposition-parm'(Param) --> 'disp-ext-parm'(Param).



%! 'disposition-type'(?Type:or([oneof([attachement,inline]),string]))// .
% ```abnf
% disposition-type = "inline" | "attachment" | disp-ext-type
%                  ; case-insensitive
% ```

'disposition-type'(inline) --> "inline".
'disposition-type'(attachement) --> "attachment".
'disposition-type'(Type) --> 'disp-ext-type'(Type).



%! 'ext-token'(?Value:string)// .
% ```abnf
% ext-token = <the characters in token, followed by "*">
% ```

'ext-token'(Val) --> token(Val0), "*", {string_concat(Val0, "*", Val)}.



%! 'filename-parm'(?Parameter:pair(string))// .
% ```abnf
% filename-parm = "filename" "=" value
%               | "filename*" "=" ext-value
% ```

'filename-parm'("filename"-Val) --> "filename=", value(Val).
'filename-parm'("filename*"-x(X,Y,Z)) --> "filename*=", 'ext-value'(X, Y, Z).
