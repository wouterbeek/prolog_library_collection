:- module(
  uri_ext,
  [
    uri_optional_query_enc//0,
    uri_query_enc//0
  ]
).
:- reexport(library(uri)).

/** <module> URI extensions

@author Wouter Beek
@version 2015/08, 2015/10-2016/04
*/

:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_ext)).

%! uri_optional_query_enc// .

uri_optional_query_enc, "%2C" --> ",", !, uri_optional_query_enc.
uri_optional_query_enc, "%2F" --> "/", !, uri_optional_query_enc.
uri_optional_query_enc, "%3A" --> ":", !, uri_optional_query_enc.
uri_optional_query_enc, "%40" --> "@", !, uri_optional_query_enc.
uri_optional_query_enc, [C]   --> [C], !, uri_optional_query_enc.
uri_optional_query_enc        --> [].



%! uri_query_enc// .
% ```abnf
% query = *( pchar / "/" / "?" )
% pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
% pct-encoded = "%" HEXDIG HEXDIG
% sub-delims = "!" / "$" / "&" / "'" / "(" / ")"
%            / "*" / "+" / "," / ";" / "="
% unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% ```

uri_query_enc, "/" --> "/",             !, uri_query_enc.
uri_query_enc, "?" --> "?",             !, uri_query_enc.
uri_query_enc, ":" --> ":",             !, uri_query_enc.
uri_query_enc, "@" --> "@",             !, uri_query_enc.
uri_query_enc, [C] --> unreserved(C),   !, uri_query_enc.
uri_query_enc, [C] --> 'sub-delims'(C), !, uri_query_enc.
uri_query_enc, "%", hex(W1), hex(W2) -->
  between_code(0, 255, C), !,
  {W1 is C // 16, W2 is C mod 16},
  uri_query_enc.
uri_query_enc --> [].
