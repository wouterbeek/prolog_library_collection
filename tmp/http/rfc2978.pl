:- module(
  rfc2978,
  [
    iana_charset/1 % -Enc:atom
  ]
).

/** <module> RFC 2978

@author Wouter Beek
@see https://www.iana.org/assignments/character-sets/character-sets.xhtml
@version 2017/06-2017/07, 2017/10
*/

:- use_module(library(http/http_open)).
:- use_module(library(csv_ext)).





%! iana_charset(-Enc:atom) is nondet.

iana_charset(Enc) :-
  setup_call_cleanup(
    http_open(
      'http://www.iana.org/assignments/character-sets/character-sets-1.csv',
      In,
      [status_code(Status)]
    ),
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
