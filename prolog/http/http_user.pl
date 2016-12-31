:- module(
  http_user,
  [
    current_user/1,     % ?User
    has_current_user/0,
    login_link/1,       % -Link
    logout_link/1       % -Link
  ]
).

/** <module> HTTP user

Support for letting users login/logout of a Web Service.

This module is designed to work with an arbitrary database for storing
the users of a Web Service.  In order to use this module for a
concrete Web Service the following two things must be defined:

  * google_client:create_user_hook(+Profile, -User)

    Should create a new user entry in the database based on the given
    Google Profile and return the resulting record identifier User.

  * google_client:current_user_hook(+Profile, -User)

    Should retrieve the user from the database that matches the given
    Google Profile, if any.

@author Wouter Beek
@version 2016/04, 2016/06, 2016/10, 2016/12
*/

:- use_module(library(debug)).
:- use_module(library(google_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(persistency)).

:- initialization
   db_attach('user_login.db', []).

:- persistent
   current_site_user(user:atom,openid:atom),
   stay_signed_in(openid:atom,cookie:atom,peer:atom,time:integer,expires:integer).

:- http_handler(root(login), login, []).
:- http_handler(root(logout), logout, []).

:- debug(http(login)).

:- multifile
    google_client:create_user/1,
    google_client:create_user_hook/2,
    google_client:current_user_hook/2,
    google_client:login_existing_user/1.

%! google_client:create_user(+Profile) is det.
%
% Profile contains the following properties:
%
%   * client_data:term
%
%   * email:string,
%
%   * email_verified:boolean
%
%   * family_name:string
%
%   * given_name:string
%
%   * locale:string
%
%   * name:string
%
%   * picture:string
%
%   * sub:string

google_client:create_user(Profile) :-
  google_client:create_user_hook(Profile, User),
  google_login(User, Profile).


%! google_client:login_existing_user(+Profile) is det.

google_client:login_existing_user(Profile) :-
  google_client:current_user_hook(Profile, User),
  google_login(User, Profile).





%! google_login(+User, +Profile) is det.

google_login(User, Profile) :-
  http_open_session(_, []),
  google_fake_open_id(Profile, OpenId),
  (   current_site_user(User, OpenId)
  ->  true
  ;   retractall_current_site_user(_, OpenId),
      assert_current_site_user(User, OpenId),
      debug(http(login), "User ~w logged in using ~w", [User,OpenId])
  ),

  http_session_retractall(openid(_)),
  http_session_assert(openid(OpenId)),

  (   true(Profile.client_data.stay)
  ->  debug(http(login), "User remained signed in using ~p", [OpenId]),
      http_openid:openid_hook(stay_signed_in(OpenId))
  ;   true
  ),
  http_redirect(moved_temporary, Profile.client_data.return_to).

google_fake_open_id(Profile, OpenId) :-
  atomic_list_concat(['http://google.com/fake_open_id/',Profile.sub], OpenId).

true(true).
true(yes).



%! current_user(+User) is semidet.
%! current_user(-User) is semidet.

current_user(User) :-
  openid_logged_in(OpenId),
  current_site_user(User, OpenId),
  debug(http(login), "Current user ~w is using ~w", [User,OpenId]).



%! has_current_user is semidet.
% Succeeds iff the user is currently logged in.

has_current_user :-
  current_user(_).



%! login(+Req) is det.

login(Req) :-
  http_parameters(
    Req,
    [
      'openid.return_to'(ReturnTo, [default(/)]),
      stay(Stay, [default(false)])
    ]
  ),
  oauth_authenticate(
    Req,
    'google.com',
    [client_data(_{return_to:ReturnTo,stay:Stay})]
  ).



%! login_link(-Link) is det.

login_link(Link) :-
  http_base_location_iri(Iri),
  http_link_to_id(login, ['openid.return_to'(Iri)], Link).



%! logout(+Req) is det.

logout(Req) :-
  openid_logged_in(OpenId), !,
  openid_logout(OpenId),
  http_parameters(Req, ['openid.return_to'(ReturnTo, [default(/)])]),
  http_redirect(moved_temporary, ReturnTo).



%! logout_link(-Link) is det.

logout_link(Link) :-
  http_base_location_iri(Iri),
  http_link_to_id(logout, ['openid.return_to'(Iri)], Link).



stay_login_cookie(login).

http_openid:openid_hook(stay_signed_in(OpenId)) :-
  assertion(in_header_state),
  http_session_cookie(Cookie),
  get_time(Now0),
  Now is round(Now0),
  http_peer(Peer),
  Expires is Now + 31 * 24 * 60 * 60,   % 31 days from now.
  assert_stay_signed_in(OpenId, Cookie, Peer, Now, Expires),
  http_session_option(path(Path)),
  debug(http(login), "Created stay-signed-in for ~q", [OpenId]),
  http_timestamp(Expires, RFC1123),
  stay_login_cookie(CookieName),
  format(
    "Set-Cookie: ~w=~w; Expires=~w; path=~w\r\n",
    [CookieName,Cookie,RFC1123,Path]
  ).
http_openid:openid_hook(logout(OpenId)) :-
  nonvar(OpenId),
  assertion(in_header_state),
  retractall_stay_signed_in(OpenId, _, _, _, _),
  http_session_option(path(Path)),
  stay_login_cookie(CookieName),
  format(
    "Set-Cookie: ~w=; Expires=Tue, 01-Jan-1970 00:00:00 GMT; Path=~w\r\n",
    [CookieName,Path]
  ),
  fail.
http_openid:openid_hook(logged_in(OpenId)) :-
  http_in_session(_),
  http_session_data(openid(OpenId)), !.
http_openid:openid_hook(logged_in(OpenId)) :-
  http_cookie(login, Cookie),
  stay_signed_in(OpenId, Cookie, _, _, _), !,
  http_open_session(_, []),
  http_session_assert(openid(OpenId)),
  debug(http(login), "Granted stay-signed-in for ~q", [OpenId]).
% See
% https://developers.google.com/accounts/docs/OpenID#shutdown-timetable
http_openid:openid_hook(
  x_parameter(
    'https://www.google.com/accounts/o8/ud',
    openid_shutdown_ack,
    '2015-04-20'
  )
).

in_header_state:-
  current_output(Cgi),
  cgi_property(Cgi, state(header)), !.
