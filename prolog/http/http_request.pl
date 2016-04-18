:- module(
  http_request,
  [
    http_default_success/2, % +MIn, +In
    http_get/1,             % +Iri
    http_get/2,             % +Iri,             :Success_2
    http_get/3,             % +Iri,             :Success_2, +Opts
    http_post/2,            % +Iri, +Data
    http_post/3,            % +Iri, +Data,      :Success_2
    http_post/4             % +Iri, +Data,      :Success_2, +Opts
  ]
).

/** <module> HTTP requests

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2015/07-2015/08, 2015/11, 2016/01-2016/02, 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(os/open_any2)).

:- meta_predicate
    http_get(+, 2),
    http_get(+, 2, +),
    http_post(+, +, 2),
    http_post(+, +, 2, +).





%! http_default_success(+MIn, +In) is det.

http_default_success(MIn, In) :-
  print_dict(MIn),
  copy_stream_data(In, user_output).





%! http_get(+Iri) is det.
%! http_get(+Iri, :Success_2) is det.
%! http_get(+Iri, :Success_2, +Opts) is det.

http_get(Iri) :-
  http_get(Iri, http_default_success).

http_get(Iri, Success_2) :-
  http_get(Iri, Success_2, []).

http_get(Iri, Success_2, Opts0) :-
  merge_options([method(get)], Opts0, Opts),
  call_on_stream(Iri, Success_2, Opts).



%! http_post(+Iri, +Data:compound) is det.
%! http_post(+Iri, +Data:compound, :Success_2) is det.
%! http_post(+Iri, +Data:compound, :Success_2, +Opts) is det.

http_post(Iri, Data) :-
  http_post(Iri, Data, http_default_success).


http_post(Iri, Data, Success_2) :-
  http_post(Iri, Data, Success_2, []).


http_post(Iri, Data, Success_2, Opts0) :-
  merge_options([method(post),post(Data)], Opts0, Opts),
  call_on_stream(Iri, Success_2, Opts).
