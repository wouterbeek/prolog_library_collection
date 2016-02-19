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
@version 2015/11-2016/01
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     token//1,           % -Token:string
     'quoted-string'//1, % -String:string
     value//1            % -Value:string
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1 % -Value:dict
   ]).





%! 'content-disposition'(-Disposition:dict)// is det.
% ```abnf
% content-disposition = "Content-Disposition" ":"
%                       disposition-type *( ";" disposition-parm )
% ```

'content-disposition'(D2) -->
  'disposition-type'(Type),
  {D1 = _{'@type': 'llo:disposition', 'llo:disposition-type': Type}},
  (+(disposition_param, L) -> {D2 = D1.put(_{'llo:parameters': L})} ; {D2 = D1}).

disposition_param(D) --> ?('LWS'), ";", ?('LWS'), 'disposition-parm'(D).



%! 'disp-ext-parm'(-Parameter:dict)// is det.
% ```abnf
% disp-ext-parm = token "=" value | ext-token "=" ext-value
% ```

'disp-ext-parm'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  token(Key), !, "=", value(Value).
'disp-ext-parm'(_{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}) -->
  'ext-token'(Key), "=", 'ext-value'(Value).



%! 'disp-ext-type'(-Type:string)// is det.
% ```abfn
% disp-ext-type = token
% ```

'disp-ext-type'(S) --> token(S).



%! 'disposition-parm'(-Parameter:dict)// is det.
% ```abnf
% disposition-parm = filename-parm | disp-ext-parm
% ```

'disposition-parm'(D) --> 'filename-parm'(D).
'disposition-parm'(D) --> 'disp-ext-parm'(D).



%! 'disposition-type'(-Type:string)// is det.
% ```abnf
% disposition-type = "inline" | "attachment" | disp-ext-type
%                  ; case-insensitive
% ```

'disposition-type'("inline")      --> atom_ci(inline), !.
'disposition-type'("attachement") --> atom_ci(attachment), !.
'disposition-type'(S2)            --> 'disp-ext-type'(S1), {string_lower(S1, S2)}.



%! 'ext-token'(-Value:string)// is det.
% ```abnf
% ext-token = <the characters in token, followed by "*">
% ```

'ext-token'(S2) --> token(S1), "*", {string_concat(S1, "*", S2)}.



%! 'filename-parm'(-Parameter:dict)// is det.
% ```abnf
% filename-parm = "filename" "=" value
%               | "filename*" "=" ext-value
% ```

'filename-parm'(D) -->
  {D = _{'@type': 'llo:parameter', 'llo:key': Key, 'llo:value': Value}},
  atom_ci(filename), ("*" -> {Key = "filename*"} ; {Key = "filename"}), "=",
  (value(Value), ! ; 'ext-value'(Value)).
