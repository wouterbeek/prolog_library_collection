:- module(
  rfc5987,
  [
    'ext-value'//3
  ]
).

/** <module> RFC 5987

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(ltag/rfc5646), ['Language-Tag'//1]).
:- use_module(library(uri/rfc3986)).





%! 'attr-char'(?Code:code)// .
% ```abnf
% attr-char = ALPHA / DIGIT
%           / "!" / "#" / "$" / "&" / "+" / "-" / "."
%           / "^" / "_" / "`" / "|" / "~"
%           ; token except ( "*" / "'" / "%" )
% ```

'attr-char'(C) --> 'ALPHA'(C).
'attr-char'(C) --> 'DIGIT'(C).
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



%! charset(?Characterset:string)// .
% ```abnf
% charset = "UTF-8" / "ISO-8859-1" / mime-charset
% ```

charset("UTF-8") --> "UTF-8".
charset("ISO-8859-1") --> "ISO-8859-1".
charset(Charset) --> 'mime-charset'(Charset).



%! 'ext-value'(?Charset:string, ?Language:list(string), ?List:list)// .
% ```abnf
% ext-value = charset  "'" [ language ] "'" value-chars
%           ; like RFC 2231's <extended-initial-value>
%           ; (see [RFC2231], Section 7)
% ```

'ext-value'(Charset, Language, L) -->
  charset(Charset),
  "'",
  (language(Language) ; ""),
  "'",
  'value-chars'(L).


%! language(?LanguageTag:list(string))// .

language(L) --> 'Language-Tag'(L).



%! 'mime-charset'(Characterset:string)// .
% ```abnf
% mime-charset  = 1*mime-charsetc
% ```

'mime-charset'(S) --> dcg_string(mime_charset, S).
mime_charset([H1,H2|T]) --> 'mime-charsetc'(H1), mime_charset([H2|T]).



%! 'mime-charsetc'(?Code:code)// .
% ```abnf
% mime-charsetc = ALPHA / DIGIT
%               / "!" / "#" / "$" / "%" / "&"
%               / "+" / "-" / "^" / "_" / "`"
%               / "{" / "}" / "~"
%               ; as <mime-charset> in Section 2.3 of [RFC2978]
%               ; except that the single quote is not included
%               ; SHOULD be registered in the IANA charset registry
% ```

'mime-charsetc'(C) --> 'ALPHA'(C).
'mime-charsetc'(C) --> 'DIGIT'(C).
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



%! 'value-chars'// .
% ```abnf
% value-chars   = *( pct-encoded / attr-char )
% ```

'value-chars'(S) --> dcg_string(value_chars, S).
value_chars([H|T]) --> 'pct-encoded'(H), !, value_chars(T).
value_chars([H|T]) --> 'attr-char'(H), !, value_chars(T).
value_chars([]) --> "".
