:- module(
  http_request,
  [
    http_get/2, % +Iri:iri
                % :Success_2
    http_get/3, % +Iri:iri
                % :Success_2
                % +Options:list(compound)
    http_get/4, % +Iri:iri
                % :Success_2
                % :Error_2
                % +Options:list(compound)
    http_post/3, % +Iri:iri
                 % +Data:compound
                 % :Success_2
    http_post/4, % +Iri:iri
                 % +Data:compound
                 % :Success_2
                 % +Options:list(compound)
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
@version 2015/07-2015/08
*/

:- use_module(library(apply)).
:- use_module(library(http/http_json)). % JSON support.
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(option)).
:- use_module(library(zlib)).

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





%! http_get(+Iri:iri, :Success_2) is det.
% Wrapper around http_get/3

http_get(Iri, Success_2):-
  http_get(Iri, Success_2, []).


%! http_get(+Iri:iri, :Success_2, +Options:list(compound)) is det.
% Wrapper around http_get/4.

http_get(Iri, Success_2, Opts):-
  http_get(Iri, Success_2, http_error_default, Opts).


%! http_get(+Iri:iri, :Success_2, :Error_2, +Options:list(compound)) is det.

http_get(Iri, Success_2, Error_2, Opts1):-
  merge_options(
    [
      cert_verify_hook(cert_verify),
      headers(ResHs),
      method(get),
      status_code(Status)
    ],
    Opts1,
    Opts2
  ),
  extract_request_headers(Opts2, ReqHs),
  setup_call_cleanup(
    http_open(Iri, Read, Opts2),
    (
      dict_pairs(Req, headers, ReqHs),
      dict_pairs(Res, headers, ResHs),
      M = http{iri:Iri,request:Req,response:Res,status:Status},
      (   option(gzip(true), Opts2)
      ->  merge_options([close_parent(false)], Opts2, Opts3),
          setup_call_cleanup(
            zopen(Read, Read0, Opts3),
            http_stream(M, Success_2, Error_2, Read0),
            close(Read0)
          )
      ;   http_stream(M, Success_2, Error_2, Read)
      )
    ),
    close(Read)
  ).



%! http_post(+Iri:iri, +Data:compound, :Success_2) is det.

http_post(Iri, Data, Success_2):-
  http_post(Iri, Data, Success_2, []).


%! http_post(
%!   +Iri:iri,
%!   +Data:compound,
%!   :Success_2,
%!   +Options:list(compound)
%! ) is det.

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
  merge_options(
    [
      cert_verify_hook(cert_verify),
      headers(ResHs),
      method(post),
      post(Data),
      status_code(Status)
    ],
    Opts1,
    Opts2
  ),
  extract_request_headers(Opts2, ReqHs),
  setup_call_cleanup(
    http_open(Iri, Read, Opts2),
    (
      (   (Data = codes(Cs) ; Data = codes(_,Cs))
      ->  (   length(Cs, L),
              L =< 1000
          ->  Prefix0 = Cs
          ;   length(Prefix0, 1000),
              append(Prefix0, _, Cs)
          )
      ;   Prefix0 = []
      ),
      atom_codes(Prefix, Prefix0),
      dict_pairs(Req, headers, ReqHs),
      dict_pairs(Res, headers, ResHs),
      M = http{data:Prefix,iri:Iri,request:Req,response:Res,status:Status},
      (   option(gzip(true), Opts2)
      ->  merge_options([close_parent(false)], Opts2, Opts3),
          setup_call_cleanup(
            zopen(Read, Read0, Opts3),
            http_stream(M, Success_2, Error_2, Read0),
            close(Read0)
          )
      ;   http_stream(M, Success_2, Error_2, Read)
      )
    ),
    close(Read)
  ).





% HELPERS %

%! extract_request_headers(
%!   +Options:list(compound),
%!   -RequestHeaders:list(pair)
%! ) is det.

extract_request_headers([], []):- !.
extract_request_headers([request_header(K=V)|T1], [K-V|T2]):- !,
  extract_request_headers(T1, T2).
extract_request_headers([_|T1], T2):-
  extract_request_headers(T1, T2).



%! http_stream(+Metadata:dict, :Success_2, :Error_2, +Read:stream) is det.

http_stream(M, Success_2, _, Read):-
  between(200, 299, M.status), !,
  call(Success_2, M, Read).
http_stream(M, _, Error_2, Read):-
  call(Error_2, M, Read).



%! http_error_default(+Metadata:dict, +Read:stream) is det.

http_error_default(M, Read):-
  format(user_error, 'REQUEST: ~a~n', [M.iri]),
  dict_pairs(M.request, headers, ReqHs),
  maplist(print_header(user_error), ReqHs),
  (dict_key(data, M, Data) -> format(user_error, '~a~n', [Data]) ; true),
  nl(user_error),

  format(user_error, 'RESPONSE: ~D~n', [M.status]),
  dict_pairs(M.response, headers, ResHs),
  maplist(print_header(user_error), ResHs),
  copy_stream_data(Read, user_error),
  nl(user_error).



%! print_header(+Write:stream, +Header:pair) is det.

print_header(Write, N-V):-
  format(Write, '~w=~w~n', [N,V]).
