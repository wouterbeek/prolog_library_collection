:- module(
  rest,
  [
    rest/2 % +Request:list(compound)
           % :Goal_5
  ]
).

/** <module> REST

@author Wouter Beek
@version 2015/09, 2015/12
*/

:- use_module(library(http/google_login)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_receive)).
:- use_module(library(semweb/rdf_db), [rdf_global_id/2]).

:- multifile(rest:prefix/1).

:- meta_predicate(rest(+,5)).





%! rest(+Request:list(compound), :Goal_5) is det.

rest(Req, Goal_5):-
  % Acceptable media types.
  rest_accept(Accept, Req),
  % Resource and location IRIs.
  rest:prefix(Prefix0),
  rdf_global_id(Prefix0:'', Prefix),
  memberchk(path(Path), Req),
  directory_file_path(Prefix, Path, Res),
  http_absolute_uri(Path, Loc),
  call(Goal_5, Req, Method, Accept, Res-Loc, Out).


rest_accept(X/Y, Req):-
  memberchk(accept(Accept), Req),
  once((
    % For some entries in `Request`
    % the media type may be uninstantiated.
    member(media(X0/Y0,_,_,_), Accept),
    ground(X0/Y0),
    X/Y = X0/Y0
  )).
