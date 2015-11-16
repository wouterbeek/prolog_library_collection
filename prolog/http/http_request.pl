:- module(
  http_request,
  [
    http_get/2, % +Iri, :Success_2
    http_get/3, % +Iri, :Success_2, +Options
    http_get/4, % +Iri:atom
                % :Success_2
                % :Error_2
                % +Options:list(compound)
    http_post/3, % +Iri, +Data, :Success_2
    http_post/4, % +Iri, +Data, :Success_2, +Options
    http_post/5 % +Iri:atom
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
     pass_to(open_any2/5, 5)
   ]).





%! http_get(+Iri:atom, :Success_2) is det.
% Wrapper around http_get/3 with default options.

http_get(Iri, Success_2):-
  http_get(Iri, Success_2, []).


%! http_get(+Iri:atom, :Success_2, +Options:list(compound)) is det.
% Wrapper around http_get/4.

http_get(Iri, Success_2, Opts):-
  http_get(Iri, Success_2, _, Opts).


%! http_get(+Iri:atom, :Success_2, :Error_2, +Options:list(compound)) is det.

http_get(Iri, Success_2, Error_2, Opts1):-
  merge_options([method(get)], Opts1, Opts2),
  http_request(Iri, Success_2, Error_2, Opts2).



%! http_post(+Iri:atom, +Data:compound, :Success_2) is det.
% Wrapper around http_post/4 with default options.

http_post(Iri, Data, Success_2):-
  http_post(Iri, Data, Success_2, []).


%! http_post(
%!   +Iri:atom,
%!   +Data:compound,
%!   :Success_2,
%!   +Options:list(compound)
%! ) is det.
% Wrapper around http_post/5 with default HTTP error goal.

http_post(Iri, Data, Success_2, Opts):-
  http_post(Iri, Data, Success_2, _, Opts).


%! http_post(
%!   +Iri:atom,
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
%!   +Iri:atom,
%!   :Success_2,
%!   :Error_2,
%!   +Options:list(compound)
%! ) is det.

http_request(Iri, Success_2, Error_2, Opts1):-
  merge_options([metadata(M)], Opts1, Opts2),
  (   var(Error_2)
  ->  Opts3 = Opts2
  ;   merge_options([http_error(Error_2)], Opts2, Opts3)
  ),
  setup_call_cleanup(
    open_any2(Iri, read, Read, Close_0, Opts3),
    call(Success_2, M, Read),
    close_any2(Close_0)
  ).
