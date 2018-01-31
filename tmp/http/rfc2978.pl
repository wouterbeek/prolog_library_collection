:- module(
  rfc2978,
  [
    iana_charset/1 % -Encoding
  ]
).

/** <module> RFC 2978

@author Wouter Beek
@see https://www.iana.org/assignments/character-sets/character-sets.xhtml
@version 2017/06-2018/01
*/

:- use_module(library(http/http_client2)).
:- use_module(library(csv_ext)).





%! iana_charset(-Encoding:atom) is nondet.

iana_charset(Enc) :-
  Uri = 'http://www.iana.org/assignments/character-sets/character-sets-1.csv',
  http_open2(Uri, In),
  call_cleanup(
    (
      csv_read_stream_row(
        In,
        row(PreferredName,Name,_,_,_,_,_),
        [case(down),skip_header(true)]
      ),
      (PreferredName == '' -> Enc = Name ; Enc = PreferredName)
    ),
    close(In)
  ).
