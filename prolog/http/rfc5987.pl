:- module(
  rfc5987,
  [
    'attr-char'//1,     % -Code:code
    charset//1,         % -Charset:atom
    'ext-parameter'//1, % -Param:dict
    'ext-value'//1,     % -Val:dict
    language//1,        % -LTag:list(atom)
    'mime-charset'//1,  % -Charset:atom
    'mime-charsetc'//1, % -Code:code
    parameter//1,       % -Param:dict
    parmname//1,        % -Name:atom
    'reg-parameter'//1, % -Param:dict
    'value-chars'//1    % -Val:atom
  ]
).

/** <module> RFC 5987: Character Set and Language Encoding for
             Hypertext Transfer Protocol (HTTP) Header Field Parameters

The following abbreviations are used for variables:

| ExtVal  | ExtendedValue |

The following terms are used:

| ExtVal | ext_val(Charset,LTag,Val) |
| Param  | pair(Key,Val)             |
| Val    | ExtVal or atom            |

@author Wouter Beek
@compat RFC 5987
@see http://tools.ietf.org/html/rfc5987
@version 2015/11-2016/01, 2016/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1,  % ?Code
     'DIGIT'//1,  % ?Weight:between(0,9)
     'DIGIT'//2,  % ?Weight:between(0,9), ?Code
     'HEXDIG'//1, % ?Weight:between(0,9)
     'HEXDIG'//2, % ?Weight:between(0,9), ?Code
     'LWSP'//0
   ]).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String:atom
     token//1            % -Token:atom
   ]).
:- use_module(library(ltag/rfc5646), [
     'Language-Tag'//1 % -LTag:list(atom)
   ]).
:- use_module(library(uri/rfc3986), [
     'pct-encoded'//1 % -Code:between(0,255)
   ]).





%! 'attr-char'(-Code:code)// .
%
% ```abnf
% attr-char = ALPHA / DIGIT / "!" / "#" / "$" / "&" / "+" / "-" / "."
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



%! charset(-Charset:atom)// is det.
%
% ```abnf
% charset = "UTF-8" / "ISO-8859-1" / mime-charset
% ```

charset('UTF-8')      --> atom_ci('UTF-8'), !.
charset('ISO-8859-1') --> atom_ci('ISO-8859-1'), !.
charset(Charset)      --> 'mime-charset'(Charset).



%! 'ext-parameter'(-Parameter:pair)// is det.
%
% Extended parameter.
%
% ```abnf
% ext-parameter = parmname "*" LWSP "=" LWSP ext-value
% ```

'ext-parameter'(Key-Val) -->
  parmname(Key), "*",
  'LWSP', "=", 'LWSP',
  'ext-value'(Val).



%! 'ext-value'(-ExtVal:compound)// is det.
%
% Extended parameter value.
%
% ```abnf
% ext-value = charset  "'" [ language ] "'" value-chars
%           ; like RFC 2231's <extended-initial-value>
%           ; (see [RFC2231], Section 7)
% ```

'ext-value'(ext_val(Charset,LTag,Val)) -->
  charset(Charset),
  "'",
  opt(language, LTag),
  "'",
  'value-chars'(Val).



%! language(-LTag:list(atom))// is det.
%
% ```abnf
% language = <Language-Tag, defined in [RFC5646], Section 2.1>
% ```
                                                               
language(LTag) -->
  'Language-Tag'(LTag).



%! 'mime-charset'(-Charset:atom)// is det.
%
% ```abnf
% mime-charset  = 1*mime-charsetc
% ```

'mime-charset'(Charset) -->
  +('mime-charsetc', Cs), !,
  {atom_codes(Charset, Cs)}.



%! 'mime-charsetc'(-Code:code)// .
%
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



%! parameter(-Param:pair)// is det.
%
% ```abnf
% parameter = reg-parameter / ext-parameter
% ```

parameter(Param) --> 'reg-parameter'(Param), !.
parameter(Param) --> 'ext-parameter'(Param).



%! parmname(-Name:atom)// is det.
%
% ```abnf
% parmname = 1*attr-char
% ```

parmname(Name) -->
  +('attr-char', Cs), !,
  {atom_codes(Name, Cs)}.

  

%! 'reg-parameter'(-Param:pair(atom))// is det.
%
% A regular parameter, as defined in RFC 2616.
%
% ```abnf
% reg-parameter = parmname LWSP "=" LWSP value
% ```

'reg-parameter'(Key-Val) -->
  parmname(Key),
  'LWSP',
  "=",
  'LWSP',
  value(Val).



%! value(-Val:atom)// is det.
%
% ```abnf
% value = token / quoted-string
% ```

value(Val) --> token(Val), !.
value(Val) --> 'quoted-string'(Val).



%! 'value-chars'(-Val:atom)// is det.
%
% ```abnf
% value-chars = *( pct-encoded / attr-char )
% ```

'value-chars'(Val) -->
  *(value_char, Cs), !,
  {atom_codes(Val, Cs)}.

value_char(C) --> 'pct-encoded'(C).
value_char(C) --> 'attr-char'(C).
