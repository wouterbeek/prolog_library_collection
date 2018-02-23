:- module(
  curl,
  [
    curl_get/1,     % +Uri
    curl_get/2,     % +Uri, +MT
    curl_get/3,     % +Uri, :Goal_3, +MT
    curl_head/1,    % +Uri
    curl_head/2,    % +Uri, +MT
    curl_options/1, % +Uri
    curl_post/2,    % +Uri, +Data
    curl_post/3,    % +Uri, +Data, +MT
    curl_post/4     % +Uri, +Data, :Goal_3, +MT
  ]
).
:- reexport(library(http/http_io)).

/** <module> cURL

@author Wouter Beek
@version 2016/12-2017/01
*/

:- use_module(library(dcg)).
:- use_module(library(http/rest)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).
:- use_module(library(uri_ext)).

:- meta_predicate
    curl_get(+, 3, +),
    curl_post(+, +, 3, +).





%! curl_get(+Uri) is det.
%! curl_get(+Uri, +MT) is det.
%! curl_get(+Uri, :Goal_3, +MT) is det.

curl_get(Uri) :-
  curl_get(Uri, _).


curl_get(Uri, MT) :-
  curl_get(Uri, curl_reply0, MT).


curl_get(Uri, Goal_3, MT) :-
  curl_options0(MT, Opts),
  http_get(Uri, Goal_3, Opts).



%! curl_head(+Uri) is det.
%! curl_head(+Uri, +MT) is det.

curl_head(Uri) :-
  curl_head(Uri, _).


curl_head(Uri, MT) :-
  curl_options0(MT, Opts),
  http_head(Uri, Opts).



%! curl_options(+Uri) is det.
%
% The OPTIONS method requests information about the communication
% options available for the target resource, at either the origin
% server or an intervening intermediary.  This method allows a client
% to determine the options and/or requirements associated with a
% resource, or the capabilities of a server, without implying a
% resource action.

curl_options(Uri) :-
  curl_options0(_, Opts),
  http_options(Uri, Opts).



%! curl_post(+Uri, +Data) is det.
%! curl_post(+Uri, +Data, +MT) is det.
%! curl_post(+Uri, +Data, :Goal_3, +MT) is det.

curl_post(Uri, Data) :-
  curl_post(Uri, Data, _).


curl_post(Uri, Data, MT) :-
  curl_post(Uri, Data, curl_reply0, MT).


curl_post(Uri, Data, Goal_3, MT) :-
  curl_options0(MT, Opts),
  http_post(Uri, Data, Goal_3, Opts).





% HELPERS %

curl_options0(MT, Opts) :-
  curl_options0([], MT, Opts).


curl_options0(Opts1, MT, Opts3) :-
  (   var(MT)
  ->  Opts2 = Opts1
  ;   Opts2 = [accept(MT)|Opts1]
  ),
  merge_options([verbose(all)], Opts2, Opts3).



curl_reply0(In, Path, Path) :-
  copy_stream_data(In, user_output).
