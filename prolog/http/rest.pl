:- module(
  rest,
  [
    http_is_get/1,     % +Method
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
@version 2016/02-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(http/html_write)). % HTML meta.
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_io)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/json)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(settings)).

:- html_meta
   rest_media_type(+, 1),
   rest_method(+, 2),
   rest_method(+, +, 3),
   rest_method(+, 2, +, 3),
   rest_method0(+, +, +, 2, +, 3).

:- setting(
     http:server,
     list,
     ['Prolog-Library-Collection'],
     "The name of the server that issues HTTP replies."
).





%! http_is_get(+Method) is semidet.
%
% Succeeds for GET and HEAD requests.  HEAD requests are handled just
% like GET requests.  The SWI HTTP library deals with leaving out the
% body for HEAD requests.

http_is_get(get).
http_is_get(head).



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
  http_status_reply(bad_request(E), current_output, [], _).
rest_exception_media_type(MT, existence_error(http_parameter,Key)) :- !,
  (   MT = media(application/json,_)
  ->  Headers = ['Content-Type'-[media_type(application/json,[])]],
      Dict = _{message: "Missing parameter", value: Key},
      with_output_to(codes(Cs), json_write_dict(current_output, Dict))
  ;   Headers = [],
      Cs = []
  ),
  phrase('HTTP-message'(1-1, 400, Headers, Cs), Msg),
  format(current_output, '~s', [Msg]).
% 401 Unauthorized (RFC 7235)
rest_exception_media_type(text/html, 401) :-
  http_status_reply(authorise(basic,''), current_output, [], _).
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
  member(MT, MTs),
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
  setting(http:server, Products),
  phrase(
    'HTTP-message'(1-1, 200, ['Allow'-[Methods],'Server'-[Products]], []),
    Msg
  ),
  debug(rest, "~s", [Msg]),
  format(current_output, '~s', [Msg]).
% Method accepted, on to matching the Media Types.
rest_method0(Req, Method, Methods, Plural_2, HandleId, Singular_3) :-
  memberchk(Method, Methods), !,
  memberchk(request_uri(Uri1), Req),
  ignore(memberchk(accept(MTs0), Req)),
  (var(MTs0) -> MTs = [_] ; clean_media_types(MTs0, MTs)),
  iri_to_resource(Uri1, Res),
  (   ground(HandleId),
      http_link_to_id(HandleId, Uri2),
      \+ iri_to_resource(Uri2, Res)
  ->  call(Singular_3, Res, Method, MTs)
  ;   call(Plural_2, Method, MTs)
  ).
% 405 Method Not Allowed
rest_method0(_, _, _, _, _) :-
  phrase(
    'HTTP-message'(1-1, 405, [], []),
    Msg
  ),
  format(current_output, '~s', [Msg]).

clean_media_types(L1, L2) :-
  maplist(clean_media_type, L1, Pairs),
  desc_pairs_values(Pairs, L2).

clean_media_type(
  media(Parent/Child,Parms,QVal,_),
  QVal-media(Parent/Child,Parms)
).

'Allow'(Val) -->
  seplist(method, ", ", Val).

'Content-Type'(MT) -->
  'media-type'(MT).

'field-name'(Key) -->
  atom(Key).

'header-field'(Key-Args) -->
  'field-name'(Key),
  ":",
  'OWS',
  {Dcg_0 =.. [Key|Args]},
  Dcg_0,
  'CRLF'.

'HTTP-message'(Version, Status, Headers, Body) -->
  'status-line'(Version, Status),
  *('header-field', Headers), !,
  'CRLF',
  Body.

'HTTP-name' -->
  "HTTP".

'HTTP-version'(X-Y) -->
  'HTTP-name',
  "/",
  digit(X),
  ".",
  digit(Y).

'media-type'(media(Type/Subtype,Params)) -->
  type(Type),
  "/",
  subtype(Subtype),
  *(sep_parameter, Params), !.

method(Method1) -->
  {upcase_atom(Method1, Method2)},
  atom(Method2).

parameter(Key-Val) -->
  atom(Key),
  "=",
  atom(Val).

product(Name-Version) --> !,
  atom(Name),
  "/",
  'product-version'(Version).
product(Name) --> !,
  atom(Name).

'product-version'(Version) -->
  atom(Version).

'reason-phrase'(Status) -->
  {http_status_label(Status, Lbl)},
  atom(Lbl).

sep_parameter(Param) -->
  ";",
  'OWS',
  parameter(Param).

'Server'(Products) -->
  seplist(product, 'RWS', Products).

'status-code'(Status) -->
  number(Status).

% @bug Status code must be given as header?
%'status-line'(Version, Status) -->
%  'HTTP-version'(Version), 'SP',
%  'status-code'(Status), 'SP',
%  'reason-phrase'(Status), 'CRLF'.
'status-line'(_, Status) -->
  "Status: ", integer(Status), 'CRLF'.

subtype(A) -->
  atom(A).

type(A) -->
  atom(A).

'CRLF' --> "\r\n".

'OWS' --> " ".

'RWS' --> " ".

'SP' --> " ".
