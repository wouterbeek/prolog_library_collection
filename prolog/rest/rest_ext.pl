:- module(
  rest_ext,
  [
    rest_filter/4 % +Request:list(compound)
                  % +Method:oneof([delete,get,head,option,post,put])
                  % +Accept:list(compound)
                  % -Location:iri
  ]
).

/** <module> REST extensions

Rudimentary REST support.

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(http/http_path)).
:- use_module(library(http/http_receive)).





%! rest_filter(
%!   +Request:list(compound),
%!   +Method:oneof([delete,get,head,option,post,put]),
%!   +Accept:list(compound),
%!   -Location:iri
%! ) is semidet.

rest_filter(Req, Method, Accept, Location):-
  memberchk(method(Method), Req),
  rest_filter_accept(Accept, Req),
  memberchk(path(Path), Req),
  http_absolute_uri(Path, Location).

rest_filter_accept(X/Y, _):-
  var(X),
  var(Y), !.
rest_filter_accept(X/Y, Req):-
  memberchk(accept(Accept), Req),
  once((
    % For some entries in `Request`
    % the media type may be uninstantiated.
    member(media(X0/Y0,_,_,_), Accept),
    ground(X0/Y0),
    X/Y = X0/Y0
  )).
