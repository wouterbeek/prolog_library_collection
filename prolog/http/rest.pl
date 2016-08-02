:- module(
  rest,
  [
    rest_exception/2, % +MTs, +E
    rest_handler/2,   % +Req, :Plural_3
    rest_handler/5    % +Req, +HandleId, :Exists_1, :Singular_4, :Plural_3
  ]
).

/** <module> REST

@author Wouter Beek
@version 2016/02, 2016/04-2016/06, 2016/08
*/

:- use_module(library(http/html_write)). % HTML meta.
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).

:- html_meta
   rest_call_or_exception(3, +, +),
   rest_call_or_exception(4, +, +, +),
   rest_handler(+, 3),
   rest_handler(+, +, 1, 4, 3),
   rest_handler_mediatype(3, +, +),
   rest_handler_mediatype(4, +, +, +).





%! rest_call_or_exception(:Plural_3, +Req, +Method, +MediaType) is det.
%! rest_call_or_exception(:Singular_4, +Req, +Method, +MediaType, +Res) is det.

rest_call_or_exception(Plural_3, Req, Method, MT) :-
  catch(call(Plural_3, Req, Method, MT), E, rest_exception(MT, E)).


rest_call_or_exception(Singular_4, Req, Method, MT, Res) :-
  catch(call(Singular_4, Req, Method, MT, Res), E, rest_exception(MT, E)).



%! rest_exception(+MTs, +E) is det.

rest_exception(MTs, E) :-
  once((
    member(MT, MTs),
    rest_exception0(MT, E)
  )).


rest_exception0(_, E) :-
  var(E).
rest_exception0(text/html, E) :-
  throw(E).
rest_exception0(MT, error(E,_)) :-
  rest_exception0(MT, E).
% The REST resource already exists.
rest_exception0(application/json, existence_error(_,_)) :-
  reply_json_dict(_{}, [status(409)]).
% The REST request could not be processed.
rest_exception0(_, E) :-
  http_status_reply(bad_request(E)).



%! rest_handler(+Req, :Plural_3) is det.
%! rest_handler(+Req, +HandleId, :Exists_1, :Singular_4, :Plural_3) is det.

rest_handler(Req, Plural_3) :-
  http_accept(Req, MTs),
  memberchk(method(Method), Req),
  rest_handler_mediatype(Plural_3, Req, Method, MTs), !.
rest_handler(Req, _) :-
  rest_not_found(Req).


rest_handler(Req, HandleId, Exists_1, Singular_4, Plural_3) :-
  memberchk(request_uri(Local), Req),
  http_accept(Req, MTs),
  memberchk(method(Method), Req),
  http_link_to_id(HandleId, Endpoint),
  (   Local == Endpoint
  ->  rest_handler_mediatype(Plural_3, Req, Method, MTs)
  ;   iri_to_resource(Local, Res),
      call(Exists_1, Res)
  ->  rest_handler_mediatype(Singular_4, Req, Method, MTs, Res)
  ), !.
rest_handler(Req, _, _, _, _) :-
  rest_not_found(Req).



%! rest_handler_mediatype(:Plural_3, +Req, +Method, +MTs) is det.
%! rest_handler_mediatype(:Singular_4, +Req, +Method, +MTs, +Res) is det.

rest_handler_mediatype(Plural_3, Req, Method, MTs) :-
  member(MT, MTs),
  rest_call_or_exception(Plural_3, Req, Method, MT), !.
rest_handler_mediatype(_, _, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).


rest_handler_mediatype(Singular_4, Req, Method, MTs, Res) :-
  member(MT, MTs),
  rest_call_or_exception(Singular_4, Req, Method, MT, Res), !.
rest_handler_mediatype(_, _, _, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).


rest_not_found(Req) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).


why_not_acceptable(p("No acceptable media type could be served.")).
