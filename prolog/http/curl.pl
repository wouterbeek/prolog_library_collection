:- module(
  curl,
  [
    curl_get/1,    % +Uri
    curl_get/2,    % +Uri, +MT
    curl_head/1,   % +Uri
    curl_head/2,   % +Uri, +MT
    curl_options/1 % +Uri
  ]
).

/** <module> cURL

@author Wouter Beek
@version 2016/12-2017/01
*/

:- reexport(library(http/http_io)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).





%! curl_get(+Uri) is det.

curl_get(Uri) :-
  curl_get(Uri, _).


curl_get(Uri, MT) :-
  http_get(Uri, print_body0, [request_header(accept=MT),verbose(all)]).

print_body0(In, Path, Path) :-
  copy_stream_data(In, user_output).



%! curl_head(+Uri) is det.
%! curl_head(+Uri, +MT) is det.

curl_head(Uri) :-
  curl_head(Uri, []).


curl_head(Uri, MT) :-
  http_head(Uri, [request_header(accept=MT),verbose(all)]).



%! curl_options(+Uri) is det.
%
% The OPTIONS method requests information about the communication
% options available for the target resource, at either the origin
% server or an intervening intermediary.  This method allows a client
% to determine the options and/or requirements associated with a
% resource, or the capabilities of a server, without implying a
% resource action.

curl_options(Uri) :-
  http_options(Uri, [verbose(all)]).
