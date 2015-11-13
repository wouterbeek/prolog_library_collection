:- module(
  http_request,
  [
    http_error_default/2, % +Metadata:dict
                          % +Read:stream
    http_get/2, % +Iri, :Success_2
    http_get/3, % +Iri, :Success_2, +Options
    http_get/4, % +Iri:iri
                % :Success_2
                % :Error_2
                % +Options:list(compound)
    http_post/3, % +Iri, +Data, :Success_2
    http_post/4, % +Iri, +Data, :Success_2, +Options
    http_post/5 % +Iri:iri
                % +Data:compound
                % :Success_2
                % :Error_2
                % +Options:list(compound)
  ]
).

/** <module> HTTP requests

Higher-level HTTP requests build on top of library(http/http_open),
posing an alternative to library(http/http_client).

@author Wouter Beek
@version 2015/07-2015/08, 2015/11
*/

:- use_module(library(apply)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(option)).
:- use_module(library(os/open_any2)).

:- meta_predicate(http_get(+,2)).
:- meta_predicate(http_get(+,2,+)).
:- meta_predicate(http_get(+,2,2,+)).
:- meta_predicate(http_post(+,+,2)).
:- meta_predicate(http_post(+,+,2,+)).
:- meta_predicate(http_post(+,+,2,2,+)).
:- meta_predicate(http_request(+,2,2,+)).
:- meta_predicate(http_stream(+,2,2,+)).

:- predicate_options(http_get/3, 3, [
     pass_to(http_get/4, 4)
   ]).
:- predicate_options(http_get/4, 4, [
     pass_to(http_request/4, 4)
   ]).
:- predicate_options(http_post/4, 4, [
     pass_to(http_post/5, 5)
   ]).
:- predicate_options(http_post/5, 5, [
     pass_to(http_request/4, 4)
   ]).
:- predicate_options(http_request/4, 4, [
     gzip(+boolean),
     pass_to(http_open/3, 3),
     pass_to(zopen/3, 3)
   ]).





%! http_get(+Iri:iri, :Success_2) is det.
% Wrapper around http_get/3 with default options.

http_get(Iri, Success_2):-
  http_get(Iri, Success_2, []).


%! http_get(+Iri:iri, :Success_2, +Options:list(compound)) is det.
% Wrapper around http_get/4.

http_get(Iri, Success_2, Opts):-
  http_get(Iri, Success_2, http_error_default, Opts).


%! http_get(+Iri:iri, :Success_2, :Error_2, +Options:list(compound)) is det.

http_get(Iri, Success_2, Error_2, Opts1):-
  merge_options([method(get)], Opts1, Opts2),
  http_request(Iri, Success_2, Error_2, Opts2).



%! http_post(+Iri:iri, +Data:compound, :Success_2) is det.
% Wrapper around http_post/4 with default options.

http_post(Iri, Data, Success_2):-
  http_post(Iri, Data, Success_2, []).


%! http_post(
%!   +Iri:iri,
%!   +Data:compound,
%!   :Success_2,
%!   +Options:list(compound)
%! ) is det.
% Wrapper around http_post/5 with default HTTP error goal.

http_post(Iri, Data, Success_2, Opts):-
  http_post(Iri, Data, Success_2, http_error_default, Opts).


%! http_post(
%!   +Iri:iri,
%!   +Data:compound,
%!   :Success_2,
%!   :Error_2,
%!   +Options:list(compound)
%! ) is det.

http_post(Iri, Data, Success_2, Error_2, Opts1):-
  merge_options([method(post),post(Data)], Opts1, Opts2),
  http_request(Iri, Success_2, Error_2, Opts2).





% HELPERS %

%! http_request(
%!   +Iri:iri,
%!   :Success_2,
%!   :Error_2,
%!   +Options:list(compound)
%! ) is det.

http_request(Iri, Success_2, Error_2, Opts1):-
  merge_options([metadata(M)], Opts1, Opts2),
  setup_call_cleanup(
    open_any2(Iri, read, Read, Close_0, Opts2),
    http_stream(M, Success_2, Error_2, Read),
    close_any2(Close_0)
  ).



%! http_stream(+Metadata:dict, :Success_2, :Error_2, +Read:stream) is det.

http_stream(M, Success_2, _, Read):-
  between(200, 299, M.http.status_code), !,
  call(Success_2, M, Read).
http_stream(M, _, Error_2, Read):-
  call(Error_2, M, Read).



%! http_error_default(+Metadata:dict, +Read:stream) is det.

http_error_default(M, Read):-
  %format(user_error, "REQUEST: ~a~n", [M.iri]),
  %dict_pairs(M.request, headers, ReqHs),
  %maplist(print_header(user_error), ReqHs),
  %(get_dict(data, M, Data) -> format(user_error, "~a~n", [Data]) ; true),
  %nl(user_error),
  %
  format(user_error, "RESPONSE: ~D~n", [M.http.status_code]),
  dict_pairs(M.http.headers, http_headers, ResHs),
  maplist(print_header(user_error), ResHs),
  copy_stream_data(Read, user_error),
  nl(user_error).



%! print_header(+Write:stream, +Header:pair) is det.

print_header(Write, N-V):-
  format(Write, "~w=~w~n", [N,V]).
