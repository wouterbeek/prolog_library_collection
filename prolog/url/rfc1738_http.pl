:- module(
  rfc1738_http,
  [
    hpath//1, % ?Path:list(string)
    hsegment//1, % ?Segment:string
    httpurl//4, % ?Host
                % ?Port
                % ?Path:list(string)
                % ?Search:string
    search//1 % ?Search:string
  ]
).

/** <module> RFC 1738: HTTP protocol

@author Wouter Beek
@compat RFC 1738
@deprecated
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(url/rfc1738_component)).





%! hpath(?Path:list(string))// .
% ```abnf
% hpath = hsegment *[ "/" hsegment ]
% ```

hpath([H|T]) --> hsegment(H), *(sep_hsegment, T).
sep_hsegment(C) --> "/", hsegment(C).



%! hsegment(?Segment:string)// .
% ```abnf
% hsegment = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

hsegment(S) --> *(code0, Cs), {string_codes(S, Cs)}.



%! httpurl(?Host, ?Port, ?Path, ?Search)// .
% ```abnf
% httpurl = "http://" hostport [ "/" hpath [ "?" search ]]
% ```

httpurl(Host, Port, Path, Search) -->
  "http://",
  hostport(Host, Port),
  ("/" -> hpath(Path), ("?" -> search(Search) ; "") ; "").



%! search(?Search:string)// .
% ```abnf
% search = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

search(S) --> *(code0, Cs), {string_codes(S, Cs)}.





% HELPERS %

code0(C)   --> uchar(C).
code0(0';) --> ";".
code0(0':) --> ":".
code0(0'@) --> "@".
code0(0'&) --> "&".
code0(0'=) --> "=".
