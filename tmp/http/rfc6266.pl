:- module(
  rfc6266,
  [
    'content-disposition'//1 % -Disposition:compound
  ]
).

/** <module> RFC 6266: Use of the Content-Disposition Header Field in the HTTP

Takes over the definition and registration of the
`Content-Disposition` header from RFC 2616.

@author Wouter Beek
@compat RFC 6266
@see http://tools.ietf.org/html/rfc6266
@version 2015/11-2016/01, 2016/12
*/

:- use_module(library(dcg)).
:- use_module(library(http/rfc2616), [
     'LWS'//0,
     token//1,           % -Token:atom
     'quoted-string'//1, % -String:atom
     value//1            % -Val:atom
   ]).
:- use_module(library(http/rfc5987), [
     'ext-value'//1 % -Val:compound
   ]).





%! 'content-disposition'(-Disposition:compound)// is det.
%
% ```abnf
% content-disposition = "Content-Disposition" ":"
%                       disposition-type
%                       *( ";" disposition-parm )
% ```

'content-disposition'(content_disposition(Type,Params)) -->
  'disposition-type'(Type),
  (+(disposition_param, Params) -> "" ; {Params = []}).

disposition_param(D) -->
  ?('LWS'),
  ";",
  ?('LWS'),
  'disposition-parm'(D).



%! 'disp-ext-parm'(-Parameter:pair(atom))// is det.
%
% ```abnf
% disp-ext-parm = token "=" value
%               | ext-token "=" ext-value
% ```

'disp-ext-parm'(Key-Val) -->
  token(Key), !,
  "=",
  value(Val).
'disp-ext-parm'(Key-Val) -->
  'ext-token'(Key),
  "=",
  'ext-value'(Val).



%! 'disp-ext-type'(-Type:atom)// is det.
%
% ```abfn
% disp-ext-type = token
% ```

'disp-ext-type'(Token) -->
  token(Token).



%! 'disposition-parm'(-Parameter:dict)// is det.
%
% ```abnf
% disposition-parm = filename-parm | disp-ext-parm
% ```

'disposition-parm'(Param) -->
  'filename-parm'(Param), !.
'disposition-parm'(Param) -->
  'disp-ext-parm'(Param).



%! 'disposition-type'(-Type:atom)// is det.
%
% ```abnf
% disposition-type = "inline" | "attachment" | disp-ext-type
%                  ; case-insensitive
% ```

'disposition-type'(inline) -->
  atom_ci(inline), !.
'disposition-type'(attachement) -->
  atom_ci(attachment), !.
'disposition-type'(Type) -->
  'disp-ext-type'(Type0),
  {downcase_atom(Type0, Type)}.



%! 'ext-token'(-Token:atom)// is det.
%
% ```abnf
% ext-token = <the characters in token, followed by "*">
% ```

'ext-token'(Token) -->
  token(Token0),
  "*",
  {atom_concat(Token0, '*', Token)}.



%! 'filename-parm'(-Param:pair(atom))// is det.
%
% ```abnf
% filename-parm = "filename" "=" value
%               | "filename*" "=" ext-value
% ```

'filename-parm'(Key-Val) -->
  atom_ci(filename),
  ("*" -> {Key = 'filename*'} ; {Key = filename}),
  "=",
  (value(Val) -> "" ; 'ext-value'(Val)).
