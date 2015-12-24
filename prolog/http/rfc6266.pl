:- module(
  rfc6266,
  [
    'content-disposition'//1 % -Disposition:dict
  ]
).

/** <module> RFC 6266: Use of the Content-Disposition Header Field in the HTTP

Takes over the definition and registration of the `Content-Disposition`
header from RFC 2616.

@author Wouter Beek
@compat RFC 6266
@see http://tools.ietf.org/html/rfc6266
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     token//1, % ?Token:string
     'quoted-string'//1, % QuotedString:string
     value//1 % ?Value:string
   ]).
:- use_module(library(http/rfc5987_token), [
     'ext-value'//3 % ?Charset:string
                    % ?Language:list(string)
                    % ?Value:string
   ]).





%! 'content-disposition'(-Disposition:dict)// is det.
% ```abnf
% content-disposition = "Content-Disposition" ":"
%                       disposition-type *( ";" disposition-parm )
% ```

'content-disposition'(disposition{type: Type, parameters: Params}) -->
  'disposition-type'(Type),
  *(disposition_param, Params).
disposition_param(X) --> ?('LWS'), ";", ?('LWS'), 'disposition-parm'(X).



%! 'disp-ext-parm(?Parameter:pair(string))// .
% ```abnf
% disp-ext-parm = token "=" value | ext-token "=" ext-value
% ```

'disp-ext-parm'(Key-Val)        --> token(Key), !,    "=", value(Val).
'disp-ext-parm'(Key-ext(X,Y,Z)) --> 'ext-token'(Key), "=", 'ext-value'(X, Y, Z).



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

'disposition-type'(inline)      --> "inline".
'disposition-type'(attachement) --> "attachment".
'disposition-type'(Type)        --> 'disp-ext-type'(Type).



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

'filename-parm'("filename"-Val)         --> "filename=", !, value(Val).
'filename-parm'("filename*"-ext(X,Y,Z)) --> "filename*=", 'ext-value'(X, Y, Z).
