:- module(
  rfc5987_token,
  [
    charset//1, % ?Characterset:string
    'ext-parameter'//1, % ?Parameter:pair(string)
    'ext-value'//3, % ?Charset:string
                    % ?Language:list(string)
                    % ?Value:string
    language//1, % ?LanguageTag:list(string)
    'mime-charset'//1, % ?Characterset:string
    parameter//1, % ?Parameter:pair(string)
    parmname//1, % Name:string
    'reg-parameter'//1, % ?Parameter:pair(string)
    'value-chars'//1 % Value:string
  ]
).

/** <module> RFC 5987: Character Set and Language Encoding for
             Hypertext Transfer Protocol (HTTP) Header Field Parameters

This modifies the following RFC 2616 rules:
  - attribute//1
  - parameter//1
  - 'quoted-string'//1
  - token//1
  - value//1

@author Wouter Beek
@compat RFC 5987
@see http://tools.ietf.org/html/rfc5987
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(http/rfc5987_code)).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % ?LanguageTag:list(string)
   ]).
:- use_module(library(string_ext)).





%! charset(?Characterset:string)// .
% ```abnf
% charset = "UTF-8" / "ISO-8859-1" / mime-charset
% ```

charset("UTF-8")      --> "UTF-8", !.
charset("ISO-8859-1") --> "ISO-8859-1", !.
charset(Charset)      --> 'mime-charset'(Charset).



%! 'ext-parameter'(?Parameter:pair(string))// .
% Extended paramter.
%
% ```abnf
% ext-parameter = parmname "*" LWSP "=" LWSP ext-value
% ```

'ext-parameter'(N-ext(Charset,Lang,Val)) -->
  parmname(N), "*",
  'LWSP', "=", 'LWSP',
  'ext-value'(Charset, Lang, Val).



%! 'ext-value'(?Charset:string, ?Language:list(string), ?Value:string)// .
% Extended parameter value.
%
% ```abnf
% ext-value = charset  "'" [ language ] "'" value-chars
%           ; like RFC 2231's <extended-initial-value>
%           ; (see [RFC2231], Section 7)
% ```

'ext-value'(Charset, Lang, Value) -->
  charset(Charset), "'", opt(language, Lang), "'", 'value-chars'(Value).



%! language(?LanguageTag:list(string))// .

language(L) --> 'Language-Tag'(L).



%! 'mime-charset'(Characterset:string)// .
% ```abnf
% mime-charset  = 1*mime-charsetc
% ```

'mime-charset'(S) --> +('mime-charsetc', Cs), {string_codes(S, Cs)}.



%! parameter(?Parameter:pair(string))// is det.
% ```abnf
% parameter = reg-parameter / ext-parameter
% ```

parameter(Param) --> 'reg-parameter'(Param), !.
parameter(Param) --> 'ext-parameter'(Param).



%! parmname(?Name:string)// .
% ```abnf
% parmname = 1*attr-char
% ```

parmname(S) --> +('attr-char', Cs), {string_codes(S, Cs)}.

  

%! 'reg-parameter'(?Parameter:pair(string))// is det.
% A regular parameter, as defined in RFC 2616.
%
% ```abnf
% reg-parameter = parmname LWSP "=" LWSP value
% ```

'reg-parameter'(N-V) --> parmname(N), 'LWSP', "=", 'LWSP', value(V).



%! 'value-chars'(?Value:string)// .
% ```abnf
% value-chars = *( pct-encoded / attr-char )
% ```

'value-chars'(S) --> *(value_char, Cs), {string_codes(S, Cs)}.
value_char(C) --> 'pct-encoded'(C).
value_char(C) --> 'attr-char'(C).
