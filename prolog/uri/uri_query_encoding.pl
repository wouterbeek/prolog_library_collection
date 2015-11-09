:- module(
  uri_query_encoding,
  [
    iri_query_encoding//0,
    optional_uri_query_encoding//0,
    uri_query_encoding//0
  ]
).

/** <module> URI Query Encoding

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(uri/uri_code)).





iri_query_encoding -->
  query_encoding(true).



optional_uri_query_encoding, "%2F" -->
  "/", !,
  optional_uri_query_encoding.
optional_uri_query_encoding, "%3A" -->
  ":", !,
  optional_uri_query_encoding.
optional_uri_query_encoding, [C] -->
  [C], !,
  optional_uri_query_encoding.
optional_uri_query_encoding --> "".



uri_query_encoding -->
  query_encoding(false).



%! query_encoding(+Iri:boolean)// .
% # Syntax
%
% ```abnf
%  pchar =  unreserved / pct-encoded / sub-delims / ":" / "@"
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
%  pct-encoded = "%" HEXDIG HEXDIG
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
%  sub-delims =   "!" / "$" / "&" / "'" / "(" / ")"
%               / "*" / "+" / "," / ";" / "="
%  unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
%  ucschar =   %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%            / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%            / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%            / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%            / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%            / %xD0000-DFFFD / %xE1000-EFFFD
% ```
%
% @compat RFC 3986
% @compat RFC 3987

query_encoding(I), [Code] -->
  (   forward_slash(Code)
  ;   question_mark(Code)
  ;   colon(Code)
  ;   ampersat(Code)
  ;   unreserved(I, Code)
  ;   'sub-delims'(Code)
  ;   {I == true},
      private(Code)
  ), !,
  query_encoding(I).
query_encoding(I),
  percent_sign,
  hexadecimal_digit(Weight1),
  hexadecimal_digit(Weight2)
-->
  between_code(0, 255, Code), !,
  {
    Weight1 is Code // 16,
    Weight2 is Code mod 16
  },
  query_encoding(I).
query_encoding(_) --> [].
