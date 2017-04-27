:- module(
  rest,
  [
    rest_exception/2,  % +MTs, +E
    rest_media_type/2, % +MTs, :Goal_1
    rest_method/2,     % +Req, :Plural_2
    rest_method/3,     % +Req, +HandleId, :Singular_3
    rest_method/4,     % +Req, :Plural_2, +HandleId, :Singular_3
    rest_reply/6,      % -Result, +Succeeds, +Fails, +In, +InPath1, -InPath2
  % Generators
    'media-type'//1    % +MT
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
@version 2016/02-2017/02
*/

rest_method0(Req, Method, Methods, Plural_2, HandleId, Singular_3) :-
  memberchk(Method, Methods), !,
  memberchk(request_uri(Uri1), Req),
  request_media_types(Req, MTs),
  % For our REST support we make the resource URIs agnostic to the
  % scheme and host of the server.  This allows a website with data to
  % be hosted/tested on a different machine.
  uri_resource(Uri1, Res),
  (   ground(HandleId),
      http_link_to_id(HandleId, Uri2),
      \+ uri_resource(Uri2, Res)
  ->  call(Singular_3, Res, Method, MTs)
  ;   call(Plural_2, Method, MTs)
  ).



%! rest_reply(-Result, +Succeeds, +Fails, +In, +InPath1, -InPath2) is nondet.

rest_reply(Result, Succeeds, Fails, In, InPath, InPath) :-
  %%%%peek_string(In, 1000, Str), writeln(Str), %DEB
  once((
    member(InEntry, InPath),
    _{'@type': uri, headers: Headers, status: Status} :< InEntry
  )),
  (   _{'content-type': ContentType} :< Headers
  ->  http_parse_header_value(content_type, ContentType, media(MT0,_)),
      rest_reply_stream(MT0, Status, In, Result)
  ;   % No Content-Type header, so there should be no content.
      at_end_of_stream(In)
  ),
  http_status_must_be(Status, Succeeds, Fails).

rest_reply_stream(application/json, Status, In, Result) :- !,
  json_read_dict(In, Result0),
  (   http_status_is_success(Status)
  ->  (is_list(Result0) -> member(Result, Result0) ; Result = Result0)
  ;   print_term(Result0)
  ).
rest_reply_stream(text/html, _, In, _) :- !,
  html_download(In, Dom),
  print_term(Dom).
rest_reply_stream(text/plain, _, In, Result) :- !,
  read_stream_to_string(In, Result).
