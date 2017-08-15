:- module(
  rfc5987,
  [
    'attr-char'//1,     % -Code
    charset//1,         % -Charset
    'ext-parameter'//1, % -Parameter
    'ext-value'//1,     % -Value
    language//1,        % -LanguageTag
    'mime-charset'//1,  % -Charset
    'mime-charsetc'//1, % -Code
    parameter//1,       % -Parameter
    parmname//1,        % -Name
    'reg-parameter'//1, % -Parameter
    'value-chars'//1    % -Value
  ]
).

/** <module> RFC 5987: Character Set and Language Encoding for
             Hypertext Transfer Protocol (HTTP) Header Field Parameters

@author Wouter Beek
@compat RFC 5987
@see http://tools.ietf.org/html/rfc5987
@version 2017/05-2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/rfc2234), [
     'ALPHA'//1,  % ?Code
     'DIGIT'//1,  % ?Weight
     'DIGIT'//2,  % ?Weight, ?Code
     'HEXDIG'//1, % ?Weight
     'HEXDIG'//2, % ?Weight, ?Code
     'LWSP'//0
   ]).
:- use_module(library(dcg/rfc5646), [
     'Language-Tag'//1 % -LanguageTag
   ]).
:- use_module(library(http/rfc2616), [
     'quoted-string'//1, % -String
     token//1            % -Token
   ]).
:- use_module(library(uri/rfc3986), [
     'pct-encoded'//1 % -Code
   ]).





%! 'attr-char'(?Code:code)// .
%
% ```abnf
% attr-char = ALPHA / DIGIT / "!" / "#" / "$" / "&" / "+" / "-" / "."
%           / "^" / "_" / "`" / "|" / "~"
%           ; token except ( "*" / "'" / "%" )
% ```

'attr-char'(Code) --> 'ALPHA'(Code).
'attr-char'(Code) --> 'DIGIT'(Code).
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



%! charset(?Charset:atom)// is det.
%
% ```abnf
% charset = "UTF-8" / "ISO-8859-1" / mime-charset
% ```

charset('UTF-8') --> atom_ci('UTF-8').
charset('ISO-8859-1') --> atom_ci('ISO-8859-1').
charset(Charset) --> 'mime-charset'(Charset).



%! 'ext-parameter'(?Parameter:pair)// is det.
%
% Extended parameter.
%
% ```abnf
% ext-parameter = parmname "*" LWSP "=" LWSP ext-value
% ```

'ext-parameter'(Name-Value) -->
  parmname(Name), "*",
  'LWSP',
  "=",
  'LWSP',
  'ext-value'(Value).



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
  ?(language, LTag),
  "'",
  'value-chars'(Val).



%! language(-LanguageTag:list(atom))// is det.
%
% ```abnf
% language = <Language-Tag, defined in [RFC5646], Section 2.1>
% ```
                                                               
language(LanguageTag) -->
  'Language-Tag'(LanguageTag).



%! 'mime-charset'(?Charset:atom)// is det.
%
% ```abnf
% mime-charset  = 1*mime-charsetc
% ```

'mime-charset'(Charset) -->
  dcg_atom(+('mime-charsetc'), Charset).



%! 'mime-charsetc'(?Code:code)// .
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

'mime-charsetc'(Code) --> 'ALPHA'(Code).
'mime-charsetc'(Code) --> 'DIGIT'(Code).
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



%! parameter(?Parameter:pair(atom))// is det.
%
% ```abnf
% parameter = reg-parameter / ext-parameter
% ```

parameter(Parameter) --> 'reg-parameter'(Parameter).
parameter(Parameter) --> 'ext-parameter'(Parameter).



%! parmname(?Name:atom)// is det.
%
% ```abnf
% parmname = 1*attr-char
% ```

parmname(Name) -->
  dcg_atom(+('attr-char'), Name).

  

%! 'reg-parameter'(?Parameter:pair(atom))// is det.
%
% A regular parameter, as defined in RFC 2616.
%
% ```abnf
% reg-parameter = parmname LWSP "=" LWSP value
% ```

'reg-parameter'(Name-Value) -->
  parmname(Name),
  'LWSP',
  "=",
  'LWSP',
  value(Value).



%! value(?Value:atom)// is det.
%
% ```abnf
% value = token / quoted-string
% ```

value(Value) --> token(Value).
value(Value) --> 'quoted-string'(Value).



%! 'value-chars'(?Value:atom)// is det.
%
% ```abnf
% value-chars = *( pct-encoded / attr-char )
% ```

'value-chars'(Value) -->
  dcg_atom(*('value-chars_'), Value).

'value-chars_'(Code) --> 'pct-encoded'(Code).
'value-chars_'(Code) --> 'attr-char'(Code).
