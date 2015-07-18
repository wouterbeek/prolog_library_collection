:- module(
  http_request,
  [
    http_get/2, % +Uri:atom
                % :Success_2
    http_get/3, % +Uri:atom
                % :Success_2
                % +Options:list(compound)
    http_get/4, % +Uri:atom
                % :Success_2
                % :Error_2
                % +Options:list(compound)
    http_post/3, % +Uri:atom
                 % +Data:compound
                 % :Success_2
    http_post/4, % +Uri:atom
                 % +Data:compound
                 % :Success_2
                 % +Options:list(compound)
    http_post/5 % +Uri:atom
                % +Data:compound
                % :Success_2
                % :Error_2
                % +Options:list(compound)
  ]
).

/** <module> HTTP request

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(option)).

:- meta_predicate(http_get(+,2)).
:- meta_predicate(http_get(+,2,+)).
:- meta_predicate(http_get(+,2,2,+)).
:- meta_predicate(http_post(+,+,2)).
:- meta_predicate(http_post(+,+,2,+)).
:- meta_predicate(http_post(+,+,2,2,+)).
:- meta_predicate(http_stream(+,2,2,+)).

:- predicate_options(http_get/3, 3, [
     pass_to(http_get/4, 4)
   ]).
:- predicate_options(http_get/4, 4, [
     pass_to(http_open/3, 3)
   ]).
:- predicate_options(http_post/4, 4, [
     pass_to(http_post/5, 5)
   ]).
:- predicate_options(http_post/5, 5, [
     pass_to(http_open/3, 3)
   ]).





%! http_get(+Uri:atom, :Success_2) is det.
% Wrapper around http_get/3

http_get(Uri, Success_2):-
  http_get(Uri, Success_2, []).


%! http_get(+Uri:atom, :Success_2, +Options:list(compound)) is det.
% Wrapper around http_get/4.

http_get(Uri, Success_2, Opts):-
  http_get(Uri, Success_2, http_error_default, Opts).


%! http_get(+Uri:atom, :Success_2, :Error_2, +Options:list(compound)) is det.

http_get(Uri, Success_2, Error_2, Opts0):-
  merge_options([method(get),status_code(Status)], Opts0, Opts),
  setup_call_cleanup(
    http_open(Uri, Read, Opts),
    http_stream(Status, Success_2, Error_2, Read),
    close(Read)
  ).



%! http_post(+Uri:atom, +Data:compound, :Success_2) is det.

http_post(Uri, Data, Success_2):-
  http_post(Uri, Data, Success_2, []).


%! http_post(
%!   +Uri:atom,
%!   +Data:compound,
%!   :Success_2,
%!   +Options:list(compound)
%! ) is det.

http_post(Uri, Data, Success_2, Opts):-
  http_post(Uri, Data, Success_2, http_error_default, Opts).


%! http_post(
%!   +Uri:atom,
%!   +Data:compound,
%!   :Success_2,
%!   :Error_2,
%!   +Options:list(compound)
%! ) is det.

http_post(Uri, Data, Success_2, Error_2, Opts0):-
  merge_options([method(post),post(Data),status_code(Status)], Opts0, Opts),
  setup_call_cleanup(
    http_open(Uri, Read, Opts),
    http_stream(Status, Success_2, Error_2, Read),
    close(Read)
  ).





% HELPERS %

%! http_stream(
%!   +Status:between(100,599),
%!   :Success_2,
%!   :Error_2,
%!   +Read:stream
%! ) is det.

http_stream(Status, Success_2, _, Read):-
  between(200, 299, Status), !,
  call(Success_2, Status, Read).
http_stream(Status, _, Error_2, Read):-
  call(Error_2, Status, Read).



%! http_error_default(+Status:between(100,599), +Read:stream) is det.

http_error_default(Status, Read):-
  format(user_error, '[STATUS ~D] ', [Status]),
  copy_stream_data(Read, user_error),
  format(user_error, '\n', []).
