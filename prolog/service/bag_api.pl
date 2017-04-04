:- module(
  bag_api,
  [
    bag_building/1, % -Building
    bag_building/2  % -Building, +Query
  ]
).
:- reexport(library(print_ext)).

/** <module> BAG API

@author Wouter Beek
@version 2017/02
*/

:- use_module(library(http/rest)).
:- use_module(library(lists)).
:- use_module(library(pagination/client_pagination)).
:- use_module(library(settings)).
:- use_module(library(uri/uri_ext)).

:- meta_predicate
    bag_request(+, 3),
    bag_request(+, 3, +).

:- setting(
     host,
     atom,
     'bag.basisregistraties.overheid.nl',
     "The host of the BAG API."
   ).
:- setting(
     key,
     atom,
     '',
     "The key for the BAG API."
   ).
:- setting(
     scheme,
     oneof([http,https]),
     https,
     "The URI scheme of the BAG API."
   ).




%! bag_building(-Building) is nondet.
%! bag_building(-Building, +Opts) is nondet.
%! bag_building(+Query, -Building, +Opts) is nondet.
%
% An example of a ‘Query’:
%
% ```prolog
% _{
%   geometrie: _{
%     contains: _{
%       coordinates: [5.9623762, 52.2118093],
%       type: 'Point'
%     }
%   }
% }
% ```
%
% The following options are supported:
%
%   * geldigOp(+string)
%
%     Filtert op objecten die geldig zijn op de opgegeven datum
%     ‘JJJJ-MM-DD’.
%
%   * page(+integer)
%
%     Paginanummer voor paginering.  Default is ‘1’.

bag_building(Building) :-
  bag_building(Building, []).


bag_building(Building, Opts) :-
  bag_building0(_, Building, Opts).


bag_building0(Query, Building, UriOpts) :-
  bag_uri([panden], UriOpts, Uri),
  (var(Query) -> ReqOpts = [] ; ReqOpts = [method(put),post(json(Query))]),
  bag_request(Uri, rest_reply(Building, [200], [403,429]), ReqOpts).





% HELPERS %

%! bag_request(+Uri, :Goal_3) is det.
%! bag_request(+Uri, :Goal_3, +Opts) is det.

bag_request(Uri, Goal_3) :-
  bag_request(Uri, Goal_3, []).


bag_request(Uri, Goal_3, Opts1) :-
  setting(key, Key),
  merge_options(
    Opts1,
    [
      method(get),
      request_header('Accept'='application/hal+json'),
      request_header('X-Api-Key'=Key),
      verbose(all)
    ],
    Opts2
  ),
  client_pagination(Uri, Goal_3, Opts2).



%! bag_uri(-Uri) is det.

bag_uri(Uri) :-
  bag_uri([], Uri).


bag_uri(Segments1, Uri) :-
  bag_uri(Segments1, _, Uri).


bag_uri(Segments1, Query, Uri) :-
  setting(scheme, Scheme),
  setting(host, Host),
  append([api,v1], Segments1, Segments2),
  uri_comps(Uri, uri(Scheme,Host,Segments2,Query,_)).
