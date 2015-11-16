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
