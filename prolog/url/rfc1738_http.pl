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

:- use_module(library(url/rfc1738_component)).
:- use_module(library(url/rfc1738_helpers)).





%! hpath(?Path:list(string))// .
% ```abnf
% hpath = hsegment *[ "/" hsegment ]
% ```

hpath([H|T]) --> hsegment(H), hsegments(T).
hsegments([H|T]) --> "/", !, hsegment(H), hsegments(T).
hsegments([]) --> "".



%! hsegment(?Segment:string)// .
% ```abnf
% hsegment = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

hsegment(S) --> content2(S).



%! httpurl(?Host, ?Port, ?Path, ?Search)// .
% ```abnf
% httpurl = "http://" hostport [ "/" hpath [ "?" search ]]
% ```

httpurl(Host, Port, Path, Search) -->
  "http://",
  hostport(Host, Port),
  ("/", hpath(Path), ("?", search(Search) ; "") ; "").



%! search(?Search:string)// .
% ```abnf
% search = *[ uchar | ";" | ":" | "@" | "&" | "=" ]
% ```

search(S) --> content2(S).
