:- module(
  http_server,
  [
    data_uri/2,               % +Segments, -Uri
    http_absolute_location/2, % +Spec, -Path
    http_current_location/1,  % -Uri
    http_server_init/1,       % +Dict
    http_is_get/1,            % +Method
    http_link_to_id/2,        % +HandleId, -Local
    http_reply_json/1,        % +Json
    rest_exception/2,         % +MediaTypes, +Exception
    rest_media_type/2,        % +MediaTypes, :Goal_1
    rest_method/2,            % +Request, :Goal_2
    rest_method/4             % +Request, +HandleId, :Plural_2, :Singular_3
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
@version 2017/04-2017/08
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(http/http_error)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    rest_media_type(+, 1),
    rest_method(+, 2),
    rest_method(+, +, 2, 3).

:- multifile
    error:has_type/2,
    http:convert_parameter/3.

error:has_type(or(Types), Term) :-
  member(Type, Types),
  error:has_type(Type, Term), !.

http:convert_parameter(positive_integer, Atom, Integer) :-
  atom_number(Atom, Integer),
  must_be(positive_integer, Integer).

:- initialization
   current_prolog_flag(version_data, swi(Major,Minor,Patch,Extra)),
   format(string(Version), "v~d.~d.~d (~w)", [Major,Minor,Patch,Extra]),
   set_setting(http:products, ["SWI-Prolog"-Version]).

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
     http:products,
     list(pair(string)),
     [],
     "The products that implement the server that creates HTTP replies."
   ).





%! data_uri(+Segments:list(atom), -Uri:atom) is det.

data_uri(Segments, Uri) :-
  setting(http:public_scheme, Scheme),
  setting(http:public_host, Host),
  setting(http:public_port, Port),
  uri_comps(Uri, uri(Scheme,auth(_User,_Password,Host,Port),Segments,_,_)).



%! http_absolute_location(+Spec, -Path:atom) is det.

http_absolute_location(Spec, Path) :-
  http_absolute_location(Spec, Path, []).



%! http_current_location(-Uri:atom) is det.

http_current_location(Uri) :-
  http_current_request(Request),
  memberchk(path(Uri), Request).



%! http_reply_json(+Json) is det.

http_reply_json(Json) :-
  format("Content-Type: application/json\n"),
  nl,
  json_write_dict(current_output, Json).



%! http_server_init(+Conf:dict) is det.

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



%! http_is_get(@Method:atom) is semidet.
%
% Succeeds for GET and HEAD requests.  HEAD requests are handled just
% like GET requests.  The SWI HTTP library deals with leaving out the
% body for HEAD requests.

http_is_get(get).
http_is_get(head).



%! http_link_to_id(+HandleId, -Local) is det.

http_link_to_id(HandleId, Local) :-
  http_link_to_id(HandleId, [], Local).



%! rest_exception(+MediaTypes:list(compound), +Exception:compound) is det.

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
rest_exception_media_type(media(application/json,_),
                          existence_error(http_parameter,Key)) :-
  reply_json_dict(_{message: "Missing parameter", value: Key}, [status(400)]).
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
  format("Status: ~d\n", [Status]),
  nl.



%! rest_media_type(+MediaTypes:list(compound), :Goal_1) is det.

rest_media_type(MediaTypes, Goal_1) :-
  member(MediaType, MediaTypes),
  call(Goal_1, MediaType), !.
rest_media_type(MediaTypes, _) :-
  rest_exception(MediaTypes, 406).



%! rest_method(+Request:list(compound), :Goal_2) is det.
%! rest_method(+Request:list(compound), +HandleId, :Plural_2,
%!             :Singular_3) is det.

rest_method(Request, Plural_2) :-
  rest_method(Request, _, Plural_2, _:_).


rest_method(Request, HandleId, Module:Plural_2, Module:Singular_3) :-
  memberchk(method(Method), Request),
  memberchk(path(Path), Request),
  Module:http_current_handler(Path, _, Options),
  memberchk(methods(Methods), Options),
  (   Method == options
  ->  setting(http:products, Products),
      format("Status: 204\n"),
      write_allow_header(Methods),
      write_server_header(Products),
      nl
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
      format("Strict-Transport-Security: max-age=31536000; includeSubDomains~n"),
      request_media_types(Request, MediaTypes),
      (   (var(HandleId) -> true ; http_link_to_id(HandleId, HandleUri))
      ->  call(Module:Plural_2, Method, MediaTypes)
      ;   data_uri(Segments, Resource),
          call(Module:Singular_3, Resource, Method, MediaTypes)
      )
  ).



% ```
% Allow = #method
% method = token
% ```

write_allow_header([H|T]) :-
  format("Allow: ~a", [H]),
  maplist(write_allow_header_, T),
  nl.

write_allow_header_(X) :-
  format(", ~a", [X]).



% ```
% Server = product *( RWS ( product | comment ) )
% product = token ["/" product-version]
% product-version = token
% ```

write_server_header([H|T]) :-
  format("Server: "),
  product_(H),
  maplist(write_server_header_, T),
  nl.

write_server_header_(X) :-
  format(" "),
  product_(X).

product_(X-Y) :- !,
  format("~a/~a", [X,Y]).
product_(X) :- !,
  format("~a", [X]).


  
% A sequence of Media Types (from most to least acceptable).
request_media_types(Request, MediaTypes) :-
  memberchk(accept(MediaTypes0), Request),
  clean_media_types(MediaTypes0, MediaTypes), !.
% Any Media Type is accepted (`*`).
request_media_types(_, [_]).

clean_media_types(L1, L2) :-
  maplist(clean_media_type, L1, Pairs),
  sort(1, @>=, Pairs, Sorted),
  pairs_values(Sorted, L2).

clean_media_type(
  media(MediaType0,Parms,QValue,_),
  QValue-media(MediaType0,Parms)
).
