:- module(
  rfc2978,
  [
    iana_charset/1 % -Encoding:atom
  ]
).

/** <module> RFC 2978

@author Wouter Beek
@see https://www.iana.org/assignments/character-sets/character-sets.xhtml
@version 2017/06-2017/07
*/

:- use_module(library(csv_ext)).
:- use_module(library(uri/uri_ext)).





%! iana_charset(-Encoding:atom) is nondet.

iana_charset(Encoding) :-
  call_on_uri(
    'http://www.iana.org/assignments/character-sets/character-sets-1.csv',
    iana_charset0,
    [encoding(octet)]
  ).

iana_charset0(In, Metadata, Metadata) :-
  csv_read_stream_row(
    In,
    row(PreferredName,Name,_MIBenum,_Source,_Reference,_Aliases,_Note),
    [case(down),skip_header(true)]
  ),
  (PreferredName == '' -> Encoding = Name ; Encoding = PreferredName).
