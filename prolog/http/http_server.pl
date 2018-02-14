:- module(
  http_server,
  [
    data_uri/2,               % +Segments, -Uri
    http_absolute_location/2, % +Spec, -Path
    http_current_location/1,  % -Uri
    http_server_init/1,       % +Dict
    http_is_get/1,            % +Method
    http_link_to_id/2,        % +HandleId, -Local
    http_media_types/2,       % +Request, -MediaTypes
    http_reply_json/1,        % +Json
    rest_media_type/2,        % +MediaTypes, :Goal_1
    rest_method/2,            % +Request, :Goal_2
    rest_method/4,            % +Request, +HandleId, :Plural_2, :Singular_3
    rest_options/1,           % +Methods
    rest_parameters/2         % +Request, +Parameters
  ]
).

/** <module> HTTP Server

401 Unauthorized (RFC 7235)
402 Payment Required
403 Forbidden
404 Not Found
405 Method Not Allowed
406 Unacceptable
407 Proxy Authentication Required (RFC 7235)
408 Request Time-out
409 Conflict
410 Gone
411 Length Required
412 Precondition Failed (RFC 7232)
413 Payload Too Large (RFC 7231)
414 URI Too Long (RFC 7231)
415 Unsupported Media Type
416 Range Not Satisfiable (RFC 7233)
417 Expectation Failed
418 I'm a teapot (RFC 2324)
421 Misdirected Request (RFC 7540)
422 Unprocessable Entity (WebDAV; RFC 4918)
423 Locked (WebDAV; RFC 4918)
424 Failed Dependency (WebDAV; RFC 4918)
426 Upgrade Required
428 Precondition Required (RFC 6585)
429 Too Many Requests (RFC 6585)
431 Request Header Fields Too Large (RFC 6585)
451 Unavailable For Legal Reasons

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
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
    html:rest_exception/1,
    http:convert_parameter/3.

error:has_type(or(Types), Term) :-
  member(Type, Types),
  error:has_type(Type, Term), !.

http:convert_parameter(positive_integer, Atom, Integer) :-
  (   atom_number(Atom, Integer)
  ->  must_be(positive_integer, Integer)
  ;   instantiation_error(positive_integer)
  ).

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



%! http_media_types(+Request:compound, +MediaTypes:list(compound)) is det.

% A sequence of Media Types (from most to least acceptable).
http_media_types(Request, MediaTypes) :-
  memberchk(accept(MediaTypes0), Request),
  clean_media_types(MediaTypes0, MediaTypes), !.
% Any Media Type is accepted (`*`).
http_media_types(_, [_]).

clean_media_types(L1, L2) :-
  maplist(clean_media_type, L1, Pairs),
  sort(1, @>=, Pairs, Sorted),
  pairs_values(Sorted, L2).

clean_media_type(
  media(Super/Sub,Parms,QValue,_),
  QValue-media(Super/Sub,Parms)
).



%! http_reply_json(+Json) is det.

http_reply_json(Json) :-
  format("Content-Type: application/json; charset=UTF-8\n\n"),
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



%! rest_exception(+MediaTypes:list(compound), +Error:between(400,499)) is det.

% Map compound terms to HTTP server error dict.
rest_exception(MediaTypes, error(existence_error(http_parameter,Key))) :- !,
  format(string(Message), "", [Key]),
  rest_exception(MediaTypes, _{message: Message, status: 400}).
rest_exception(MediaTypes, error(instantiation_error,_)) :- !,
  rest_exception(MediaTypes, error(http_server(_{message: "Instantiation error", status: 400}))).
rest_exception(MediaTypes, error(representation_error(_),_)) :- !,
  rest_exception(MediaTypes, error(http_server(_{message: "Representation error", status: 400}))).
% Try to return the exception reply in an acceptable Media Type first.
rest_exception(MediaTypes, error(http_server(Dict))) :- !,
  member(MediaType, MediaTypes),
  rest_exception_media_type(MediaType, Dict), !.
% The exception reply cannot be returned in an acceptable Media Type.
rest_exception(_, error(http_server(Dict))) :-
  format("Status: ~d\n\n", [Dict.status]).

% HTML hookable by a specific website style.
rest_exception_media_type(media(text/html,_), Dict) :-
  html:rest_exception(Dict).
rest_exception_media_type(media(application/json,_), Dict) :-
  reply_json_dict(Dict, [status(Dict.status)]).



%! rest_media_type(+MediaTypes:list(compound), :Goal_1) is det.

rest_media_type(MediaTypes, Goal_1) :-
  member(MediaType, MediaTypes),
  call(Goal_1, MediaType), !.
rest_media_type(MediaTypes, _) :-
  rest_exception(MediaTypes, error(http_server(_{status: 406}))).



%! rest_method(+Request:list(compound), :Goal_2) is det.
%! rest_method(+Request:list(compound), +HandleId, :Plural_2, :Singular_3) is det.

rest_method(Request, Plural_2) :-
  rest_method(Request, _, Plural_2, _:_).


rest_method(Request, HandleId, Mod:Plural_2, Mod:Singular_3) :-
  memberchk(method(Method), Request),
  memberchk(path(Path), Request),
  Mod:http_current_handler(Path, _, Options),
  memberchk(methods(Methods), Options),
  (   Method == options
  ->  rest_options(Methods)
  ;   % 405 Method Not Allowed
      \+ memberchk(Method, Methods)
  ->  http_media_types(Request, MediaTypes),
      rest_exception(MediaTypes, error(http_server(_{status: 405})))
  ;   % `Method' is one of the accepted `Methods'.
      memberchk(request_uri(Uri), Request),
      % Remove the query and fragment components from the URI in order
      % to compare it to the current `HandleId'.
      uri_comps(Uri, uri(Scheme,Authority,Segments,_,_)),
      uri_comps(HandleUri, uri(Scheme,Authority,Segments,_,_)),
      format("Strict-Transport-Security: max-age=31536000; includeSubDomains\n"),
      http_media_types(Request, MediaTypes),
      catch(
        (   (var(HandleId) -> true ; http_link_to_id(HandleId, HandleUri))
        ->  call(Mod:Plural_2, Method, MediaTypes)
        ;   data_uri(Segments, Resource),
            call(Mod:Singular_3, Resource, Method, MediaTypes)
        ),
        Error,
        rest_exception(MediaTypes, Error)
      )
  ).



%! rest_options(+Methods:list(atom)) is det.

rest_options(Methods) :-
  setting(http:products, Products),
  format("Status: 204\n"),
  write_allow_header(Methods),
  write_server_header(Products),
  nl.



%! rest_parameters(+Request:compound, +Parameters:list(compound)) is det.

rest_parameters(Request, Params) :-
  http_parameters(Request, Params, [attribute_declarations(http:param)]), !.
rest_parameters(_, _) :-
  throw(error(http_status(400),rest_parameters)).



%! write_allow_header(+Methods:list(atom)) is det.
%
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



%! write_server_header(+Products:list(pair(atom))) is det.
%
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
