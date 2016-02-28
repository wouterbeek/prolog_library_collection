:- module(
  rest,
  [
    rest_handler/5,   % +Request, +HandleId, :Exists_1, :Singular_3, :Plural_2
    rest_mediatype/3, % +Method, +MediaTypes, :Plural_2
    rest_mediatype/4  % +Method, +MediaTypes, +Resource, :Singular_3
  ]
).

/** <module> REST

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(http/html_write)). % HTML meta.
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)).
:- use_module(library(uri/uri_ext)).

:- html_meta
   rest_call_or_exception(2, +, +),
   rest_call_or_exception(3, +, +, +),
   rest_handler(+, +, 1, 3, 2),
   rest_mediatype(+, +, 2),
   rest_mediatype(+, +, +, 3).





%! rest_call_or_exception(:Plural_2, +Method, +MediaType) is det.

rest_call_or_exception(Plural_2, Method, MT) :-
  catch(call(Plural_2, Method, MT), E, rest_exception(MT, E)).


%! rest_call_or_exception(:Singular_3, +Method, +MediaType, +Resource) is det.

rest_call_or_exception(Singular_3, Method, MT, Res) :-
  catch(call(Singular_3, Method, MT, Res), E, rest_exception(MT, E)).



%! rest_exception(+MediaType, +Exception) is det.

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


%! rest_handler(+Request, +HandleId, :Exists_1, :Singular_3, Plural_2) is det.

rest_handler(Req, HandleId, Exists_1, Singular_3, Plural_2) :-
  memberchk(request_uri(Local), Req),
  http_accept(Req, MTs),
  http_method(Req, Method),
  http_link_to_id(HandleId, Endpoint),
  (   Local == Endpoint
  ->  call(Plural_2, Method, MTs)
  ;   uri_path(Res, Local),
      call(Exists_1, Res)
  ->  call(Singular_3, Method, MTs, Res)
  ), !.
rest_handler(Req, _, _, _, _) :-
  memberchk(request_uri(Res), Req),
  http_status_reply(Req, not_found(Res)).


% No media type is specified.  Anything goes.
rest_mediatype(Method, [], Plural_2) :-
  rest_call_or_exception(Plural_2, Method, _), !.
% Some media type(s) is/are specified.  Try them in descending order of
% preference.
rest_mediatype(Method, MTs, Plural_2) :-
  rest_mediatype0(Method, MTs, Plural_2).

% None of the specified media types could be matched.  Give an error.
rest_mediatype0(_, [], _) :- !,
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).
rest_mediatype0(Method, [MT|_], Plural_2) :-
  rest_call_or_exception(Plural_2, Method, MT), !.
rest_mediatype0(Method, [_|MTs], Plural_2) :-
  rest_mediatype0(Method, MTs, Plural_2).


% No media type is specified.  Anything goes.
rest_mediatype(Method, [], Res, Singular_3) :-
  rest_call_or_exception(Singular_3, Method, _, Res), !.
% Some media type(s) is/are specified.  Try them in descending order of
% preference.
rest_mediatype(Method, MTs, Res, Singular_3) :-
  rest_mediatype0(Method, MTs, Res, Singular_3).

% None of the specified media types could be matched.  Give an error.
rest_mediatype0(_, [], _, _) :- !,
  why_not_acceptable(Why),
  http_status_reply(not_acceptable(Why)).
rest_mediatype0(Method, [MT|_], Res, Singular_3) :-
  rest_call_or_exception(Singular_3, Method, MT, Res), !.
rest_mediatype0(Method, [_|MTs], Res, Singular_3) :-
  rest_mediatype0(Method, MTs, Res, Singular_3).


why_not_acceptable(p("No acceptable media type could be served.")).
