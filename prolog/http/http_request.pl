:- module(
  http_request,
  [
    http_get/2, % +Uri:atom
                % :Success_3
    http_get/3, % +Uri:atom
                % :Success_3
                % +Options:list(compound)
    http_get/4, % +Uri:atom
                % :Success_3
                % :Error_3
                % +Options:list(compound)
    http_post/3, % +Uri:atom
                 % +Data:compound
                 % :Success_3
    http_post/4, % +Uri:atom
                 % +Data:compound
                 % :Success_3
                 % +Options:list(compound)
    http_post/5 % +Uri:atom
                % +Data:compound
                % :Success_3
                % :Error_3
                % +Options:list(compound)
  ]
).

/** <module> HTTP requests

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(apply)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(option)).

:- meta_predicate(http_get(+,3)).
:- meta_predicate(http_get(+,3,+)).
:- meta_predicate(http_get(+,3,3,+)).
:- meta_predicate(http_post(+,+,3)).
:- meta_predicate(http_post(+,+,3,+)).
:- meta_predicate(http_post(+,+,3,3,+)).
:- meta_predicate(http_stream(+,+,3,3,+)).

:- predicate_options(http_get/3, 3, [
     pass_to(http_get/4, 4)
   ]).
:- predicate_options(http_get/4, 4, [
     gzip(+boolean),
     pass_to(http_open/3, 3),
     pass_to(zopen/3, 3)
   ]).
:- predicate_options(http_post/4, 4, [
     pass_to(http_post/5, 5)
   ]).
:- predicate_options(http_post/5, 5, [
     gzip(+boolean),
     pass_to(http_open/3, 3),
     pass_to(zopen/3, 3)
   ]).

cert_verify(_, _, _, _, _).





%! http_get(+Uri:atom, :Success_3) is det.
% Wrapper around http_get/3

http_get(Uri, Success_3):-
  http_get(Uri, Success_3, []).


%! http_get(+Uri:atom, :Success_3, +Options:list(compound)) is det.
% Wrapper around http_get/4.

http_get(Uri, Success_3, Opts):-
  http_get(Uri, Success_3, http_error_default, Opts).


%! http_get(+Uri:atom, :Success_3, :Error_3, +Options:list(compound)) is det.

http_get(Uri, Success_3, Error_3, Opts0):-
  merge_options(
    [
      cert_verify_hook(cert_verify),
      headers(Headers),
      method(get),
      status_code(Status)
    ],
    Opts0,
    Opts
  ),
  setup_call_cleanup(
    http_open(Uri, Read, Opts),
    (   option(gzip(true), Opts)
    ->  merge_options([close_parent(false)], Opts, Opts0),
        setup_call_cleanup(
          zopen(Read, Read0, Opts0),
          http_stream(Status, Headers, Success_3, Error_3, Read0),
          close(Read0)
        )
    ;   http_stream(Status, Headers, Success_3, Error_3, Read)
    ),
    close(Read)
  ).



%! http_post(+Uri:atom, +Data:compound, :Success_3) is det.

http_post(Uri, Data, Success_3):-
  http_post(Uri, Data, Success_3, []).


%! http_post(
%!   +Uri:atom,
%!   +Data:compound,
%!   :Success_3,
%!   +Options:list(compound)
%! ) is det.

http_post(Uri, Data, Success_3, Opts):-
  http_post(Uri, Data, Success_3, http_error_default, Opts).


%! http_post(
%!   +Uri:atom,
%!   +Data:compound,
%!   :Success_3,
%!   :Error_3,
%!   +Options:list(compound)
%! ) is det.

http_post(Uri, Data, Success_3, Error_3, Opts0):-
  merge_options(
    [
      cert_verify_hook(cert_verify),
      headers(Headers),
      method(post),
      post(Data),
      status_code(Status)
    ],
    Opts0,
    Opts
  ),
  setup_call_cleanup(
    http_open(Uri, Read, Opts),
    http_stream(Status, Headers, Success_3, Error_3, Read),
    close(Read)
  ).





% HELPERS %

%! http_stream(
%!   +Status:between(100,599),
%!   +Headers:list(compound),
%!   :Success_3,
%!   :Error_3,
%!   +Read:stream
%! ) is det.

http_stream(Status, Headers, Success_3, _, Read):-
  between(200, 299, Status), !,
  call(Success_3, Status, Headers, Read).
http_stream(Status, Headers, _, Error_3, Read):-
  call(Error_3, Status, Headers, Read).



%! http_error_default(
%!   +Status:between(100,599),
%!   +Headers:list(compound),
%!   +Read:stream
%! ) is det.

http_error_default(Status, Headers, Read):-gtrace,
  format(user_error, '[STATUS ~D] ', [Status]),
  maplist(print_header(user_error), Headers),
  copy_stream_data(Read, user_error),
  format(user_error, '\n', []).



%! print_header(+Write:stream, +Header:compound) is det.

print_header(Write, header(N,V)):-
  format(Write, '~w=~w\n', [N,V]).
