:- module(
  json_ext,
  [
    json_to_dict/2, % +UriSpec, -Dict
    json_to_dict/3  % +UriSpec, -Dict, +Options
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2017/07
*/

:- use_module(library(debug)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(yall)).





%! json_to_dict(+UriSpec:term, -Dict:dict) is det.
%! json_to_dict(+UriSpec:term, -Dict:dict, +Options:list(compound)) is det.
%
% @arg Options The following options are supported:
%
%   * metadata(--list(dict))
%
%     Returns the metadata describing the process of opening the
%     stream.
%
%   * Other options are passed to uri_open/5 and stream_close/4.

json_to_dict(UriSpec, Dict) :-
  json_to_dict(UriSpec, Dict, []).


json_to_dict(UriSpec, Dict, Options1) :-
  Options2 = [request_header('Accept'='application/json')|Options1],
  call_on_uri(UriSpec, json_read_dict1(Dict, Options1), Options2).

json_read_dict1(Dict, Options, In, Metadata, Metadata) :-
  (   debugging(json_ext)
  ->  peek_string(In, 1000, String),
      debug(json_ext, "~s", [String])
  ;   true
  ),
  json_read_dict(In, Dict, Options).
