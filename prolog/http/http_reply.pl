:- module(
  http_reply,
  [
    http_reply_nothing/1 % +Request:list(compound)
  ]
).

/** <module> HTTP reply

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(http/http_header)).





%! http_reply_nothing(+Request:list(compound)) is det.

http_reply_nothing(Req):-
  memberchk(pool(client(_, _ , _In, Out)), Req),
  http_reply_header(Out, status(no_content), []).
