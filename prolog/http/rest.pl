:- module(
  rest,
  [
    rest_handler/2, % +Req, :Plural_2
    rest_handler/5  % +Req, +HandleId, :Exists_1, :Singular_3, :Plural_2
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
   rest_call_or_exception(2, +, +),
   rest_call_or_exception(3, +, +, +),
   rest_handler(+, 2),
   rest_handler(+, +, 1, 3, 2),
   rest_handler_mediatype(2, +, +),
   rest_handler_mediatype(3, +, +, +).





%! rest_call_or_exception(:Plural_2,   +Method, +MediaType      ) is det.
%! rest_call_or_exception(:Singular_3, +Method, +MediaType, +Res) is det.

rest_call_or_exception(Plural_2, Method, MT) :-
  catch(call(Plural_2, Method, MT), E, rest_exception(MT, E)).


rest_call_or_exception(Singular_3, Method, MT, Res) :-
  catch(call(Singular_3, Method, MT, Res), E, rest_exception(MT, E)).



%! rest_exception(+MediaType, +E) is det.

rest_exception(_, E) :-
  var(E), !.
rest_exception(text/html, E) :- !,
  throw(E).
rest_exception(MT, error(E,_)) :- !,
  rest_exception(MT, E).
% The REST resource already exists.
rest_exception(application/json, existence_error(_,_)) :- !,
  reply_json_dict(_{}, [status(409)]).
% The REST request could not be processed.
rest_exception(_, E) :-
  http_status_reply(bad_request(E)).



%! rest_handler(+Req, :Plural_2) is det.
%! rest_handler(+Req, +HandleId, :Exists_1, :Singular_3, :Plural_2) is det.

rest_handler(Req, Plural_2) :-
  http_accept(Req, MTs),
  http_method(Req, Method),
  rest_handler_mediatype(Plural_2, Method, MTs), !.
rest_handler(Req, _) :-
  rest_not_found(Req).


rest_handler(Req, HandleId, Exists_1, Singular_3, Plural_2) :-
  memberchk(request_uri(Local), Req),
  http_accept(Req, MTs),
  http_method(Req, Method),
  http_link_to_id(HandleId, Endpoint),
  (   Local == Endpoint
  ->  rest_handler_mediatype(Plural_2, Method, MTs)
  ;   iri_to_resource(Local, Res),
      call(Exists_1, Res)
  ->  rest_handler_mediatype(Singular_3, Method, MTs, Res)
  ), !.
rest_handler(Req, _, _, _, _) :-
  rest_not_found(Req).



%! rest_handler_mediatype(:Plural_2,   +Method, +MTs      ) is det.
%! rest_handler_mediatype(:Singular_3, +Method, +MTs, +Res) is det.

rest_handler_mediatype(Plural_2, Method, MTs) :-
  member(MT, MTs),
  rest_call_or_exception(Plural_2, Method, MT), !.
rest_handler_mediatype(_, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).


rest_handler_mediatype(Singular_3, Method, MTs, Res) :-
  member(MT, MTs),
  rest_call_or_exception(Singular_3, Method, MT, Res), !.
rest_handler_mediatype(_, _, _, _) :-
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).


rest_not_found(Req) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).


why_not_acceptable(p("No acceptable media type could be served.")).
