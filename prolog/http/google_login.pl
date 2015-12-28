:- module(
  google_login,
  [
    current_user/1, % ?User:atom
    has_current_user/0,
    the_show_must_go_on/1 % +User:atom
  ]
).

/** <module> Google login

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(google_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_session)).

:- http_handler(root(login), login, [id(login)]).


%! current_user(?SessionId, ?User:atom) is semidet.

:- dynamic(current_user/2).


%! google_clieant:create_user(+Profile:dict) is det.
% Profile contains the following properties:
%   * client_data:term
%   * email:string,
%   * email_verified:boolean
%   * family_name:string
%   * given_name:string
%   * locale:string
%   * name:string
%   * picture:string
%   * sub:string

:- multifile(google_client:create_user/1).


%! google_client:login_existing_user(+Claim:dict) is det.

:- multifile(google_client:login_existing_user/1).





%! current_user(+User:atom) is semidet.
%! current_user(-User:atom) is semidet.

current_user(User):-
  http_session_id(SessionId),
  once(current_user(SessionId, User)).



%! has_current_user is semmidet.

has_current_user:-
  current_user(_).



%! login(+Request:list(compound)) is det.

login(Req):-
  oauth_authenticate(Req, 'google.com', []).



%! the_show_must_go_on(+User:atom) is det.

the_show_must_go_on(User):-
  http_session_id(SessionId),
  with_mutex(google_login, (
    retractall(current_user(SessionId,_)),
    assert(current_user(SessionId,User))
  )),
  http_current_handler(/, _:Handler_1),
  user:call(Handler_1, _).
