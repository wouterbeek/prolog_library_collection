:- module(
  http_ext,
  [
    http_parameters_fail/2 % +Request
                           % ?Parameters:list
  ]
).

/** <module> HTTP extensions

@author Wouter Beek
@version 2012/10
*/

:- use_module(library(http/http_parameters)).



%% http_parameters_fail(Request, Parameters) is semidet.
% Like http_parameters/2, but fails when a given parameter is not found
% in the request.
%
% @see http_parameters/2

http_parameters_fail(Request, Parameters):-
  catch(
    http_parameters(Request, Parameters),
    error(existence_error(_Type, _Term), _Context),
    fail
  ).

