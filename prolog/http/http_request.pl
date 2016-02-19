:- module(
  http_request,
  [
    http_default_success/2, % +M,   +Read
    http_get/1,             % +Iri
    http_get/2,             % +Iri, :Success_2
    http_get/3,             % +Iri, :Success_2, +Opts
    http_post/2,            % +Iri, +Data
    http_post/3,            % +Iri, +Data,      :Success_2
    http_post/4             % +Iri, +Data,      :Success_2, +Opts
  ]
).

/** <module> HTTP requests

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2015/07-2015/08, 2015/11, 2016/01-2016/02
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_info)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(option)).
:- use_module(library(os/open_any2)).

:- meta_predicate
    http_get(+,2),
    http_get(+,2,+),
    http_post(+,+,2),
    http_post(+,+,2,+),
    http_request(+,2,+).

:- predicate_options(http_get/3, 3, [
     pass_to(http_request/3, 3)
   ]).
:- predicate_options(http_post/4, 4, [
     pass_to(http_request/3, 3)
   ]).
:- predicate_options(http_request/3, 3, [
     pass_to(open_any2/5, 5)
   ]).





%! http_default_success(+M, +Read) is det.

http_default_success(M, Read) :-
  print_dict(M),
  copy_stream_data(Read, user_output).





%! http_get(+Iri) is det.
%! http_get(+Iri, :Success_2) is det.
%! http_get(+Iri, :Success_2, +Opts) is det.
% Wrapper around http_request/3.

http_get(Iri) :-
  http_get(Iri, http_default_success).

http_get(Iri, Success_2) :-
  http_get(Iri, Success_2, []).

http_get(Iri, Success_2, Opts1) :-
  merge_options([method(get)], Opts1, Opts2),
  http_request(Iri, Success_2, Opts2).



%! http_post(+Iri, +Data:compound) is det.
%! http_post(+Iri, +Data:compound, :Success_2) is det.
%! http_post(+Iri, +Data:compound, :Success_2, +Opts) is det.
% Wrapper around http_post/5 with default HTTP error goal.

http_post(Iri, Data) :-
  http_post(Iri, Data, http_default_success).

http_post(Iri, Data, Success_2) :-
  http_post(Iri, Data, Success_2, []).

http_post(Iri, Data, Success_2, Opts1) :-
  merge_options([method(post),post(Data)], Opts1, Opts2),
  http_request(Iri, Success_2, Opts2).





% HELPERS %

%! http_request(+Iri, :Success_2, +Opts) is det.

http_request(Iri, Success_2, Opts1) :-
  merge_options([metadata(M)], Opts1, Opts2),
  setup_call_cleanup(
    open_any2(Iri, read, Read, Close_0, Opts2),
    call(Success_2, M, Read),
    close_any2(Close_0)
  ).
