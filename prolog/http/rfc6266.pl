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

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     token//1, % -Token:string
     'quoted-string'//1, % -String:string
     value//1 % -Value:string
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1 % -Value:dict
   ]).





%! 'content-disposition'(-Disposition:dict)// is det.
% ```abnf
% content-disposition = "Content-Disposition" ":"
%                       disposition-type *( ";" disposition-parm )
% ```

'content-disposition'(D) -->
  'disposition-type'(Type),
  *(disposition_param, T),
  {dict_pairs(D, disposition, [type-Type|T])}.
disposition_param(X) --> ?('LWS'), ";", ?('LWS'), 'disposition-parm'(X).



%! 'disp-ext-parm(-Parameter:pair)// is det.
% ```abnf
% disp-ext-parm = token "=" value | ext-token "=" ext-value
% ```

'disp-ext-parm'(N-V) -->
  token(N0), !,
  {atom_string(N, N0)},
  "=",
  value(V).
'disp-ext-parm'(N-D) -->
  'ext-token'(N0),
  {atom_string(N, N0)},
  "=",
  'ext-value'(D).



%! 'disp-ext-type'(-Type:string)// is det.
% ```abfn
% disp-ext-type = token
% ```

'disp-ext-type'(Type) --> token(Type).



%! 'disposition-parm'(?Parameters:list(pair))// is det.
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

'disposition-type'(inline) --> atom_ci(inline), !.
'disposition-type'(attachement) --> atom_ci(attachment), !.
'disposition-type'(S) --> 'disp-ext-type'(S0), {string_lower(S0, S)}.



%! 'ext-token'(-Value:string)// is det.
% ```abnf
% ext-token = <the characters in token, followed by "*">
% ```

'ext-token'(S) --> token(S0), "*", {string_concat(S0, "*", S)}.



%! 'filename-parm'(?Parameter:pair)// is det.
% ```abnf
% filename-parm = "filename" "=" value
%               | "filename*" "=" ext-value
% ```

'filename-parm'(filename-S) --> atom_ci(filename), "=", !, value(S).
'filename-parm'('filename*'-D) --> atom_ci(filename), "*=", 'ext-value'(D).
