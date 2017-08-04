:- module(
  http_server,
  [
    data_uri/2,               % +Segments, -Uri
    http_absolute_location/2, % +Spec, -Path
    http_current_location/1,  % -Uri
    http_server_init/1,       % +Dict
    http_is_get/1,            % +Method
    http_link_to_id/2,        % +HandleId, -Local
    rest_exception/2,         % +MediaTypes, +Exception
    rest_media_type/2,        % +MediaTypes, :Goal_1
    rest_method/2,            % +Request, :Goal_2
    rest_method/4,            % +Request, +HandleId, :Plural_2, :Singular_3
    uri_remove_host/2         % +Uri1, -Uri2
  ]
).
:- reexport(library(http/http_cors)).
:- reexport(library(http/http_dispatch)).
:- reexport(library(http/http_header)).
:- reexport(library(http/http_host)).
:- reexport(library(http/http_json)).
:- reexport(library(http/http_parameters)).
:- reexport(library(http/http_path)).
:- reexport(library(http/http_resource)).
:- reexport(library(http/http_session)).
:- reexport(library(http/http_stream)).
:- reexport(library(http/http_wrapper)).
:- reexport(library(http/js_write)).
:- reexport(library(http/json)).

/** <module> HTTP Server

@author Wouter Beek
@version 2017/04-2017/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/rfc7230)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    rest_media_type(+, 1),
    rest_method(+, 2),
    rest_method(+, +, 2, 3).

:- setting(
     http:client_name,
     list(string),
     ["SWI-Prolog"],
     "The name of the client that creates HTTP replies."
   ).
:- setting(
     http:data_host,
     atom,
     'example.org',
     "The authority component of data IRIs."
   ).
:- setting(
     http:data_scheme,
     oneof([http,https]),
     https,
     "The scheme components of data IRIs."
   ).
:- setting(
     http:server_name,
     list(string),
     ["SWI-Prolog"],
     "The name of the server that creates HTTP replies."
   ).





%! data_uri(+Segments, -Uri) is det.

data_uri(Segments, Uri) :-
  setting(http:public_scheme, Scheme),
  setting(http:public_host, Host),
  setting(http:public_port, Port),
  uri_comps(Uri, uri(Scheme,auth(_,Host,Port),Segments,_,_)).



%! http_absolute_location(+Spec, -Path) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_current_location(-Uri) is det.

http_current_location(Uri) :-
  http_current_request(Request),
  memberchk(path(Uri), Request).



%! http_server_init(+Dict) is det.

http_server_init(Dict1) :-
  (   dict_get(endpoint, Dict1, Dict2)
  ->  (   dict_get(host, Dict2, Host)
      ->  set_setting(http:public_host, Host)
      ;   true
      ),
      (   dict_get(port, Dict2, Port)
      ->  set_setting(http:public_port, Port)
      ;   true
      ),
      (   dict_get(scheme, Dict2, Scheme)
      ->  set_setting(http:public_scheme, Scheme)
      ;   true
      )
  ;   true
  ).



%! http_is_get(+Method) is semidet.
%
% Succeeds for GET and HEAD requests.  HEAD requests are handled just
% like GET requests.  The SWI HTTP library deals with leaving out the
% body for HEAD requests.

http_is_get(get).
http_is_get(head).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! rest_exception(+MediaTypes, +E) is det.

rest_exception(MediaTypes, error(E,_)) :- !,
  rest_exception(MediaTypes, E).
% The exception reply can be returned in an acceptable Media Type.
rest_exception(MediaTypes, E) :-
  member(MediaType, MediaTypes),
  rest_exception_media_type(MediaType, E), !.
% The exception reply cannot be returned in an acceptable Media Type,
% so just pick one.
rest_exception(_, E) :-
  once(rest_exception_media_type(_, E)).

% 400 “Bad Request”
rest_exception_media_type(media(text/html,_), bad_request(E)) :-
  http_status_reply(bad_request(E), current_output, [], _).
rest_exception_media_type(
  media(application/json,_),
  existence_error(http_parameter,Key)
) :-
  atom_json_dict(A, _{message: "Missing parameter", value: Key}, []),
  atom_codes(A, Cs),
  phrase(
    'HTTP-message'(
      http(
        status_line(1-1,400,_),
        ['content-type'-media(application/json,[])],
        Cs
      )
    ),
    Msg
  ),
  format(current_output, '~s', [Msg]).
% 401 “Unauthorized” (RFC 7235)
rest_exception_media_type(media(text/html,_), 401) :-
  http_status_reply(authorise(basic,''), current_output, [], _).
% 402 “Payment Required”
% 403 “Forbidden”
% 404 “Not Found”
rest_exception_media_type(media(text/html,_), 404) :-
  http_current_request(Request),
  http_404([], Request).
rest_exception_media_type(media(text/html,_), E) :-
  throw(E).
% 405 “Method Not Allowed”
% 406 “Unacceptable”
% 407 “Proxy Authentication Required” (RFC 7235)
% 408 “Request Time-out”
% 409 “Conflict”
% 410 “Gone”
% 411 “Length Required”
% 412 “Precondition Failed” (RFC 7232)
% 413 “Payload Too Large” (RFC 7231)
% 414 “URI Too Long” (RFC 7231)
% 415 “Unsupported Media Type”
% 416 “Range Not Satisfiable” (RFC 7233)
% 417 “Expectation Failed”
% 418 “I'm a teapot” (RFC 2324)
% 421 “Misdirected Request” (RFC 7540)
% 422 “Unprocessable Entity” (WebDAV; RFC 4918)
% 423 “Locked” (WebDAV; RFC 4918)
% 424 “Failed Dependency” (WebDAV; RFC 4918)
% 426 “Upgrade Required”
% 428 “Precondition Required” (RFC 6585)
% 429 “Too Many Requests” (RFC 6585)
% 431 “Request Header Fields Too Large” (RFC 6585)
% 451 “Unavailable For Legal Reasons”
% CATCHALL
rest_exception_media_type(_, Status) :-
  phrase('HTTP-message'(http(start_line(1-1,Status),[],[])), Msg),
  format(current_output, '~s', [Msg]).



%! rest_media_type(+MediaTypes, :Goal_1) is det.

rest_media_type(MediaTypes, Goal_1) :-
  member(MediaType, MediaTypes), !,
  call(Goal_1, MediaType), !.
rest_media_type(MediaTypes, _) :-
  rest_exception(MediaTypes, 406).



%! rest_method(+Request, :Goal_2) is det.
%! rest_method(+Request, +HandleId, :Plural_2, :Singular_3) is det.

rest_method(Request, Plural_2) :-
  rest_method(Request, _, Plural_2, _:_).


rest_method(Request, HandleId, Module:Plural_2, Module:Singular_3) :-
  memberchk(method(Method), Request),
  memberchk(path(Path), Request),
  Module:http_current_handler(Path, _, Options),
  memberchk(methods(Methods), Options),
  (   Method == options
  ->  setting(http:server_name, Products),
      phrase(
        'HTTP-message'(
          http(status_line(1-1,204,_),[allow-Methods,server-Products],[])
        ),
        Msg
      ),
      debug(rest, "~s", [Msg]),
      format(current_output, '~s', [Msg])
  ;   % 405 “Method Not Allowed”
      \+ memberchk(Method, Methods)
  ->  request_media_types(Request, MediaTypes),
      rest_exception(MediaTypes, 405)
  ;   % `Method' is one of the accepted `Methods'.
      memberchk(request_uri(Uri), Request),
      % Remove the query and fragment components from the URI in order
      % to compare it to the current `HandleId'.
      uri_comps(Uri, uri(Scheme,Authority,Segments,_,_)),
      uri_comps(HandleUri, uri(Scheme,Authority,Segments,_,_)),
      format("Strict-Transport-Security: max-age=86400; includeSubDomains~n"),
      request_media_types(Request, MediaTypes),
      (   (var(HandleId) -> true ; http_link_to_id(HandleId, HandleUri))
      ->  call(Module:Plural_2, Method, MediaTypes)
      ;   data_uri(Segments, Resource),
          call(Module:Singular_3, Resource, Method, MediaTypes)
      )
  ).

request_media_types(Request, MediaTypes) :-
  memberchk(accept(MediaTypes0), Request), !,
  clean_media_types(MediaTypes0, MediaTypes).
request_media_types(_, [_]).

clean_media_types(L1, L2) :-
  maplist(clean_media_type, L1, Pairs),
  sort(1, @>=, Pairs, Sorted),
  pairs_values(Sorted, L2).

clean_media_type(
  media(MediaType0,Parms,QValue,_),
  QValue-media(MediaType0,Parms)
).



%! uri_remove_host(+Uri1, -Uri2) is det.

uri_remove_host(Uri1, Uri2) :-
  uri_comps(Uri1, uri(Scheme,auth(_,Host,Port),Segments,Query,Fragment)),
  ground(host(Scheme, Host)),
  setting(http:public_scheme, Scheme),
  setting(http:public_host, Host),
  setting(http:public_port, Port), !,
  uri_comps(Uri2, uri(_,_,Segments,Query,Fragment)).
uri_remove_host(Uri, Uri).
