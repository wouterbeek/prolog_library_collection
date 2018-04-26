:- module(
  http_server,
  [
    data_uri/2,                % +Segments, -Uri
    http_absolute_location/2,  % +Spec, -Path
    http_current_location/1,   % -Uri
    http_parameter_conflict/2, % +Parameter1, +Parameter2
    http_server_init/1,        % +Dict
    http_is_get/1,             % +Method
    http_link_to_id/2,         % +HandleId, -Local
    http_media_types/2,        % +Request, -MediaTypes
    http_reply_json/1,         % +Json
    rest_media_type/2,         % +MediaTypes, :Goal_1
    rest_method/2,             % +Request, :Goal_2
    rest_method/4,             % +Request, +HandleId, :Plural_2, :Singular_3
    rest_options/1,            % +Methods
    rest_parameters/2          % +Request, +Parameters
  ]
).

/** <module> HTTP Server

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(error)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- use_module(library(dict)).
:- use_module(library(pair_ext)).
:- use_module(library(uri_ext)).

:- http_handler(/, http_not_found_handler,
                [methods([get,head,options]),prefix,priority(-1)]).

:- meta_predicate
    rest_media_type(+, 1),
    rest_method(+, 2),
    rest_method(+, +, 2, 3).

:- multifile
    error:has_type/2,
    html:page_exception/2,
    http:convert_parameter/3,
    http:error_status_message_hook/3,
    http:not_found_media_type/2,
    http:param/2.

error:has_type(or(Types), Term) :-
  member(Type, Types),
  error:has_type(Type, Term), !.

http:convert_parameter(positive_integer, Atom, Integer) :-
  (   atom_number(Atom, Integer)
  ->  must_be(positive_integer, Integer)
  ;   instantiation_error(positive_integer)
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



%! http_not_found_handler(+Request:compound) is det.
%
% Default HTTP handler for replies with a 404 status code.

http_not_found_handler(Request) :-
  rest_method(Request, http_not_found_method(Request)).

% GET,HEAD
http_not_found_method(Request, Method, MediaTypes) :-
  http_is_get(Method),
  memberchk(request_uri(Uri), Request),
  rest_media_type(MediaTypes, http:not_found_media_type(Uri)).

% GET,HEAD: application/json
http:not_found_media_type(Uri, media(application/json,_)) :-
  format(string(Msg), "😿 Path ‘~a’ does not exist on this server.", [Uri]),
  http_reply_json(_{message: Msg, status: 404}).



%! http_parameter_conflict(+Parameter1:compound, +Parameter2:compound) is det.

http_parameter_conflict(Param1, Param2) :-
  ground([Param1,Param2]), !,
  Param1 =.. [Key1,_],
  Param2 =.. [Key2,_],
  throw(error(conflicting_http_parameters([Key1,Key2]))).
http_parameter_conflict(_, _).



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

rest_exception(_, http_error(media_types_not_supported(MediaTypes))) :- !,
  format(
    string(Msg),
    "😿 None of the specified Media Types is supported: “~w”.",
    MediaTypes
  ),
  rest_exception_media_type(media(application/json,_), 406, Msg).
rest_exception(MediaTypes, E) :-
  error_status_message(E, Status, Msg),
  member(MediaType, MediaTypes),
  rest_exception_media_type(MediaType, Status, Msg), !.

% application/json
rest_exception_media_type(media(application/json,_), Status, Msg) :-
  reply_json_dict(_{message: Msg, status: Status}, [status(Status)]).
% text/html
rest_exception_media_type(media(text/html,_), Status, Msg) :-
  html:page_exception(Status, Msg).

error_status_message(E, Status, Msg) :-
  http:error_status_message_hook(E, Status, Msg), !.
error_status_message(error(conflicting_http_parameters(Keys)), 400, Msg) :-
  atomics_to_string(Keys, ", ", KeysLabel),
  format(
    string(Msg),
    "😿 Your request is incorrect!  You have specified the following conflicting HTTP parameters: ‘[~s]’.",
    [KeysLabel]
  ).
error_status_message(
  error(type_error(Type,Value),context(_,http_parameter(Key))),
  400,
  Msg
) :-
  format(
    string(Msg),
    "😿 Your request is incorrect!  You have specified the value ‘~w’ for HTTP parameter ‘~a’.  However, values for this parameter must be of type ‘~w’.",
    [Value,Key,Type]
  ).
error_status_message(error(existence_error(Type,Term),_), 404, Msg) :- !,
  format(
    string(Msg),
    "😿 Your request is incorrect!  There is no resource denoted by term ‘~w’ of type ‘~w’.",
    [Term,Type]
  ).
error_status_message(error(syntax_error(grammar(Language,Atom)),_), 400, Msg) :- !,
  format(
    string(Msg),
    "😿 Could not parse according to the ~a grammar: “~a”",
    [Language,Atom]
  ).
error_status_message(http_error(method_not_allowed(Method)), 405, Msg) :- !,
  format(
    string(Msg),
    "😿 HTTP method ‘~a’ is not allowed for this path.",
    [Method]
  ).
error_status_message(E, 500, Msg) :-
  format(string(Msg), "😿 The following error occurred on the server: ‘~w’.", [E]).



%! rest_media_type(+MediaTypes:list(compound), :Goal_1) is det.

rest_media_type(MediaTypes, Goal_1) :-
  member(MediaType, MediaTypes),
  call(Goal_1, MediaType), !.
rest_media_type(MediaTypes, _) :-
  rest_exception(MediaTypes, http_error(media_types_not_supported(MediaTypes))).



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
      rest_exception(MediaTypes, http_error(method_not_allowed(Method)))
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
  format("Status: 204\n"),
  write_allow_header(Methods),
  write_server_header,
  nl.



%! rest_parameters(+Request:compound, +Parameters:list(compound)) is det.

rest_parameters(Request, Params) :-
  http_parameters(Request, Params, [attribute_declarations(http:param)]).



%! write_allow_header(+Methods:list(atom)) is det.
%
% ```
% Allow = #method
% method = token
% ```

write_allow_header([H|T]) :-
  format("Allow: ~a", [H]),
  maplist(write_sep_allow, T),
  nl.

write_sep_allow(X) :-
  format(", ~a", [X]).



%! write_server_header(+Products:list(pair(atom))) is det.
%
% ```
% Server = product *( RWS ( product | comment ) )
% product = token ["/" product-version]
% product-version = token
% ```

write_server_header :-
  setting(http:products, Products),
  write_products(Products).

write_products([H|T]) :-
  format("Server: "),
  write_product(H),
  maplist(write_sep_product, T),
  nl.

write_product(X-Y) :- !,
  format("~a/~a", [X,Y]).
write_product(X) :- !,
  format("~a", [X]).

write_sep_product(X) :-
  format(" "),
  write_product(X).
