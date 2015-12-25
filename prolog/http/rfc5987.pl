:- module(
  rfc5987,
  [
    'attr-char'//1, % ?Code:code
    charset//1, % ?Characterset:string
    'ext-parameter'//1, % ?Parameter:pair(string)
    'ext-value'//1, % -Value:dict
    language//1, % ?LanguageTag:list(string)
    'mime-charset'//1, % ?CharacterSet:string
    'mime-charsetc'//1, % ?Code:code
    parameter//1, % ?Parameter:pair(string)
    parmname//1, % Name:string
    'reg-parameter'//1, % ?Parameter:pair(string)
    'value-chars'//1 % Value:string
  ]
).

/** <module> RFC 5987: Character Set and Language Encoding for
             Hypertext Transfer Protocol (HTTP) Header Field Parameters

@author Wouter Beek
@compat RFC 5987
@see http://tools.ietf.org/html/rfc5987
@version 2015/11-2015/12
*/

:- use_module(library(dcg/dcg_atom)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_word)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1, % ?Code:code
     'DIGIT'//1, % ?Weight:between(0,9)
     'DIGIT'//2, % ?Weight:between(0,9)
                 % ?Code:code
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9)
                  % ?Code:code
     'LWSP'//0
   ]).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:string
     token//1 % -Token:string
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % ?LanguageTag:list(string)
   ]).
:- use_module(library(uri/rfc3986), [
     'pct-encoded'//1 % ?Code:between(0,255)
   ]).





%! 'attr-char'(-Code:code)// .
% ```abnf
% attr-char = ALPHA / DIGIT
%           / "!" / "#" / "$" / "&" / "+" / "-" / "."
%           / "^" / "_" / "`" / "|" / "~"
%           ; token except ( "*" / "'" / "%" )
% ```

'attr-char'(C)   --> 'ALPHA'(C).
'attr-char'(C)   --> 'DIGIT'(C).
'attr-char'(0'!) --> "!".
'attr-char'(0'#) --> "#".
'attr-char'(0'$) --> "$".
'attr-char'(0'&) --> "&".
'attr-char'(0'+) --> "+".
'attr-char'(0'-) --> "-".
'attr-char'(0'.) --> ".".
'attr-char'(0'^) --> "^".
'attr-char'(0'_) --> "_".
'attr-char'(0'`) --> "`".
'attr-char'(0'|) --> "|".
'attr-char'(0'~) --> "~".



%! charset(-Characterset:string)// is det.
% ```abnf
% charset = "UTF-8" / "ISO-8859-1" / mime-charset
% ```

charset("UTF-8")      --> atom_ci('UTF-8'), !.
charset("ISO-8859-1") --> atom_ci('ISO-8859-1'), !.
charset(Charset)      --> 'mime-charset'(Charset).



%! 'ext-parameter'(-Parameter:pair(string,dict))// is det.
% Extended paramter.
%
% ```abnf
% ext-parameter = parmname "*" LWSP "=" LWSP ext-value
% ```

'ext-parameter'(N-D) -->
  parmname(N), "*",
  'LWSP', "=", 'LWSP',
  'ext-value'(D).



%! 'ext-value'(-Value:dict)// is det.
% Extended parameter value.
%
% ```abnf
% ext-value = charset  "'" [ language ] "'" value-chars
%           ; like RFC 2231's <extended-initial-value>
%           ; (see [RFC2231], Section 7)
% ```

'ext-value'(ext_value{charset: Charset, language: Lang, value: Value}) -->
  charset(Charset),
  "'",
  opt(language, Lang),
  "'",
  'value-chars'(Value).



%! language(-LanguageTag:list(string))// is det.

language(L) --> 'Language-Tag'(L).



%! 'mime-charset'(-Characterset:string)// is det.
% ```abnf
% mime-charset  = 1*mime-charsetc
% ```

'mime-charset'(S) --> +('mime-charsetc', Cs), {string_codes(S, Cs)}.



%! 'mime-charsetc'(-Code:code)// .
% ```abnf
% mime-charsetc = ALPHA / DIGIT
%               / "!" / "#" / "$" / "%" / "&"
%               / "+" / "-" / "^" / "_" / "`"
%               / "{" / "}" / "~"
%               ; as <mime-charset> in Section 2.3 of [RFC2978]
%               ; except that the single quote is not included
%               ; SHOULD be registered in the IANA charset registry
% ```

'mime-charsetc'(C)   --> 'ALPHA'(C).
'mime-charsetc'(C)   --> 'DIGIT'(C).
'mime-charsetc'(0'!) --> "!".
'mime-charsetc'(0'#) --> "#".
'mime-charsetc'(0'$) --> "$".
'mime-charsetc'(0'%) --> "%".
'mime-charsetc'(0'&) --> "&".
'mime-charsetc'(0'+) --> "+".
'mime-charsetc'(0'-) --> "-".
'mime-charsetc'(0'^) --> "^".
'mime-charsetc'(0'_) --> "_".
'mime-charsetc'(0'`) --> "`".
'mime-charsetc'(0'{) --> "{".
'mime-charsetc'(0'}) --> "}".
'mime-charsetc'(0'~) --> "~".



%! parameter(-Parameter:pair(string))// is det.
% ```abnf
% parameter = reg-parameter / ext-parameter
% ```

parameter(Param) --> 'reg-parameter'(Param), !.
parameter(Param) --> 'ext-parameter'(Param).



%! parmname(-Name:string)// is det.
% ```abnf
% parmname = 1*attr-char
% ```

parmname(S) --> +('attr-char', Cs), {string_codes(S, Cs)}.

  

%! 'reg-parameter'(-Parameter:pair(string))// is det.
% A regular parameter, as defined in RFC 2616.
%
% ```abnf
% reg-parameter = parmname LWSP "=" LWSP value
% ```

'reg-parameter'(N-V) --> parmname(N), 'LWSP', "=", 'LWSP', value(V).



%! value(-Value:string)// is det.
% ```abnf
% value = token / quoted-string
% ```

value(S) --> token(S).
value(S) --> 'quoted-string'(S).



%! 'value-chars'(-Value:string)// is det.
% ```abnf
% value-chars = *( pct-encoded / attr-char )
% ```

'value-chars'(S) --> *(value_char, Cs), {string_codes(S, Cs)}.
value_char(C) --> 'pct-encoded'(C).
value_char(C) --> 'attr-char'(C).
