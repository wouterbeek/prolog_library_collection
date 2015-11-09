:- module(
  rfc1738_ftp,
  [
    ftpurl//6, % ?User:string
               % ?Password:string
               % ?Host:or([list(nonneg),list(string)])
               % ?Port:nonneg
               % ?Path:list(string)
               % ?Type:string
    fpath//1, % ?Path:list(string)
    fsegment//1, % ?Segment:string
    ftptype//1 % ?Type:oneof(["A","D","I","a","d","i"])
  ]
).

/** <module> RFC 1738: FTP protocol

@author Wouter Beek
@compat RFC 1738
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_word)).
:- use_module(library(url/rfc1738_code)).
:- use_module(library(url/rfc1738_component)).





%! ftpurl(
%!   ?User:string,
%!   ?Password:string,
%!   ?Host:or([list(nonneg),list(string)]),
%!   ?Port:nonneg,
%!   ?Path:list(string),
%!   ?Type:string
%! )// .
% ```abnf
% ftpurl = "ftp://" login [ "/" fpath [ ";type=" ftptype ]]
% ```

ftpurl(User, Password, Host, Port, Path, Type) -->
  "ftp://",
  login(User, Password, Host, Port),
  ("/", fpath(Path), (";type=", ftptype(Type) ; "") ; "").



%! fpath(?Path:list(string))// .
% ```abnf
% fpath = fsegment *[ "/" fsegment ]
% ```

fpath([H|T]) --> fsegment(H), fsegments(T).
fsegments([H|T]) --> "/", !, fsegment(H), fsegments(T).
fsegments([]) --> "".



%! fsegment(?Segment:string)// .
% ```abnf
% fsegment = *[ uchar | "?" | ":" | "@" | "&" | "=" ]
% ```

fsegment(S) --> dcg_string(fsegment_codes, S).
fsegment_codes([H|T]) --> uchar(H), !, fsegment_codes(T).
fsegment_codes([0'?|T]) --> "?", !, fsegment_codes(T).
fsegment_codes([0':|T]) --> ":", !, fsegment_codes(T).
fsegment_codes([0'@|T]) --> "@", !, fsegment_codes(T).
fsegment_codes([0'&|T]) --> "&", !, fsegment_codes(T).
fsegment_codes([0'=|T]) --> "=", !, fsegment_codes(T).



%! ftptype(?Type:oneof(["A","D","I","a","d","i"]))// .
% ```abnf
% ftptype = "A" | "I" | "D" | "a" | "i" | "d"
% ```

ftptype("A") --> "A".
ftptype("D") --> "D".
ftptype("I") --> "I".
ftptype("a") --> "a".
ftptype("d") --> "d".
ftptype("i") --> "i".
