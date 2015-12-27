:- module(
  rest,
  [
    rest/2 % +Request:list(compound)
           % :Goal_6
  ]
).

/** <module> REST

@author Wouter Beek
@version 2015/09, 2015/12
*/

:- use_module(library(http/google_login)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_receive)).

:- meta_predicate(rest(+,6)).





%! rest(+Request:list(compound), :Goal_6) is det.

rest(Req, Goal_6):-
  memberchk(method(Method), Req),
  rest_accept(Accept, Req),
  memberchk(path(Path), Req),
  http_absolute_uri(Path, Iri),
  (has_current_user -> current_user(User) ; User = 0),
  memberchk(pool(client(_,_,_,Out)), Req),
  call(Goal_6, Req, Method, Accept, Iri, User, Out).


rest_accept(X/Y, Req):-
  memberchk(accept(Accept), Req),
  once((
    % For some entries in `Request`
    % the media type may be uninstantiated.
    member(media(X0/Y0,_,_,_), Accept),
    ground(X0/Y0),
    X/Y = X0/Y0
  )).
