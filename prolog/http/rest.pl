:- module(
  rest,
  [
    rest_exception/2,  % +MTs, +E
    rest_media_type/2, % +MTs, :Goal_1
    rest_method/2,     % +Req, :Plural_2
    rest_method/3,     % +Req, +HandleId, :Singular_3
    rest_method/4      % +Req, :Plural_2, +HandleId, :Singular_3
  ]
).

/** <module> REST

There are two phases in handling REST requests:

  1. rest_method/[2-4] where we answer OPTIONS requests or
     determine the method and throw an error if that method is not
     supported.

     The following two calls are made:
     
       - call(Singular_3, Res, Req, Method, MTs)

       - call(Plural_2, Req, Method, MTs)

     This allows additional errors (e.g., authentication) that are
     method-specific to be thrown while still holding on to all media
     types to take media type preferences into account.

  2. rest_method_type/3 where we have covered all method-specific
     errors (for which we need the full list of media types) and where
     we can now make calls for specific media types.

     The following call is made:
     
       - call(Goal_2, Method, MT)

@author Wouter Beek
@version 2016/02-2016/12
*/

:- use_module(library(http/html_write)). % HTML meta.
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_write)).
:- use_module(library(http/json)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).

:- html_meta
   rest_media_type(+, 1),
   rest_method(+, 2),
   rest_method(+, +, 3),
   rest_method(+, 2, +, 3),
   rest_method0(+, +, +, 2, +, 3).





%! rest_exception(+MTs, +E) is det.

rest_exception(MTs, error(E,_)) :- !,
  rest_exception(MTs, E).
% The exception reply can be returned in an acceptable media type.
rest_exception(MTs, E) :-
  member(MT, MTs),
  rest_exception_media_type(MT, E), !.
% The exception reply cannot be returned in an acceptable media type,
% so just pick one.
rest_exception(_, E) :-
  once(rest_exception_media_type(_, E)).


% 400 Bad Request
rest_exception_media_type(text/html, bad_request(E)) :-
  http_status_reply(bad_request(E)).
rest_exception_media_type(MT, existence_error(http_parameter,Key)) :- !,
  (   MT == application/json
  ->  Headers = ['Content-Type'-media_type(application/json,[])],
      Dict = _{message: "Missing parameter", value: Key},
      with_output_to(codes(Cs), json_write_dict(current_output, Dict))
  ;   Headers = [],
      Cs = []
  ),
  reply_http_message(400, Headers, Cs).
% 401 Unauthorized (RFC 7235)
rest_exception_media_type(text/html, 401) :-
  http_status_reply(authorise(basic,'')).
% 402 Payment Required
% 403 Forbidden
% 404 Not Found
rest_exception_media_type(text/html, 404) :-
  http_current_request(Req),
  http_404([], Req).
rest_exception_media_type(text/html, E) :-
  throw(E).
% 407 Proxy Authentication Required (RFC 7235)
% 408 Request Time-out
% 409 Conflict
% 410 Gone
% 411 Length Required
% 412 Precondition Failed (RFC 7232)
% 413 Payload Too Large (RFC 7231)
% 414 URI Too Long (RFC 7231)
% 415 Unsupported Media Type
% 416 Range Not Satisfiable (RFC 7233)
% 417 Expectation Failed
% 418 I'm a teapot (RFC 2324)
% 421 Misdirected Request (RFC 7540)
% 422 Unprocessable Entity (WebDAV; RFC 4918)
% 423 Locked (WebDAV; RFC 4918)
% 424 Failed Dependency (WebDAV; RFC 4918)
% 426 Upgrade Required
% 428 Precondition Required (RFC 6585)
% 429 Too Many Requests (RFC 6585)
% 431 Request Header Fields Too Large (RFC 6585)
% 451 Unavailable For Legal Reasons



%! rest_media_type(+MTs, :Goal_1) is det.
%
% @tbd Add body for 405 code in multiple media types.

% Media type accepted, on to application-specific reply.
rest_media_type(MTs, Goal_1) :-
  member(media(MT,_,_,_), MTs),
  call(Goal_1, MT), !.
% 406 Not Acceptable
rest_media_type(_, _) :-
  format("Status: 406~n"),
  format("Content-Type: text/text~n~n").



%! rest_method(+Req, :Plural_2) is det.
%! rest_method(+Req, +HandleId, :Singular_3) is det.
%! rest_method(+Req, :Plural_2, +HandleId, :Singular_3) is det.
%
% @tbd Return info for 405 status code.

rest_method(Req, Plural_2) :-
  rest_method(Req, Plural_2, _, _).


rest_method(Req, HandleId, Singular_3) :-
  rest_method(Req, _, HandleId, Singular_3).


rest_method(Req, Plural_2, HandleId, Singular_3) :-
  memberchk(path(Path), Req),
  strip_module(Plural_2, Mod, _),
  Mod:http_current_handler(Path, _, Opts),
  memberchk(methods(Methods0), Opts),
  sort([head,options|Methods0], Methods),
  memberchk(method(Method), Req),
  rest_method0(Req, Method, Methods, Mod:Plural_2, HandleId, Mod:Singular_3).


% OPTIONS request
rest_method0(_, options, Methods, _, _, _) :- !,
  reply_http_message(200, ['Allow'-Methods]).
% Method accepted, on to matching the Media Types.
rest_method0(Req, Method, Methods, Plural_2, HandleId, Singular_3) :-
  memberchk(Method, Methods), !,
  memberchk(request_uri(Iri1), Req),
  memberchk(accept(MTs0), Req),
  (var(MTs0) -> MTs = [] ; MTs = MTs0),
  iri_to_resource(Iri1, Res),
  (   ground(HandleId),
      http_link_to_id(HandleId, Iri2),
      \+ iri_to_resource(Iri2, Res)
  ->  call(Singular_3, Res, Method, MTs)
  ;   call(Plural_2, Method, MTs)
  ).
% 405 Method Not Allowed
rest_method0(_, _, _, _, _) :-
  reply_http_message(405).
