:- module(
  http_request,
  [
    http_get/1,  % +Iri
    http_get/2,  % +Iri,        :Goal_3
    http_get/3,  % +Iri,        :Goal_3, +Opts
    http_post/2, % +Iri, +Data
    http_post/3, % +Iri, +Data, :Goal_3
    http_post/4  % +Iri, +Data, :Goal_3, +Opts
  ]
).

/** <module> HTTP requests

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(apply)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(option)).
:- use_module(library(print_ext)).
:- use_module(library(os/open_any2)).

:- meta_predicate
    http_get(+, 3),
    http_get(+, 3, +),
    http_post(+, +, 3),
    http_post(+, +, 3, +).





%! http_get(+Iri) is det.
%! http_get(+Iri, :Goal_3) is det.
%! http_get(+Iri, :Goal_3, +Opts) is det.

http_get(Iri) :-
  http_get(Iri, http_default_success).

http_get(Iri, Goal_3) :-
  http_get(Iri, Goal_3, []).

http_get(Iri, Goal_3, Opts0) :-
  merge_options([method(get)], Opts0, Opts),
  call_on_stream(Iri, Goal_3, Opts).



%! http_post(+Iri, +Data:compound) is det.
%! http_post(+Iri, +Data:compound, :Goal_3) is det.
%! http_post(+Iri, +Data:compound, :Goal_3, +Opts) is det.

http_post(Iri, Data) :-
  http_post(Iri, Data, http_default_success).


http_post(Iri, Data, Goal_3) :-
  http_post(Iri, Data, Goal_3, []).


http_post(Iri, Data, Goal_3, Opts0) :-
  merge_options([method(post),post(Data)], Opts0, Opts),
  call_on_stream(Iri, Goal_3, Opts).





% HELPERS %

%! http_default_success(+In, +M1, -M2) is det.

http_default_success(In, M, _) :-
  print_dict(M),
  copy_stream_data(In, user_output).
