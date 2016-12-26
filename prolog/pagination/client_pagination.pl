:- module(
  client_pagination,
  [
    client_pagination/2, % +Uri, :Goal_3
    client_pagination/3  % +Uri, :Goal_3, +Opts
  ]
).

/** <module> Client pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(os/io)).

:- meta_predicate
    client_pagination(+, 3),
    client_pagination(+, 3, +).





%! client_pagination(+Uri, :Goal_3) is nondet.
%! client_pagination(+Uri, :Goal_3, +Opts) is nondet.
%
% The following options are supported:
%
%   - base_uri/1 is used to resolve relative URIs in the HTTP Link
%     header.  By default this is the given request Uri.
%
%   - Other options are given to call_on_stream/3.

client_pagination(Uri, Goal_3) :-
  client_pagination(Uri, Goal_3, []).


client_pagination(Uri1, Goal_3, Opts1) :-
  InPath = metadata([]),
  (   merge_options([metadata(InPath0)], Opts1, Opts2),
      call_on_stream(Uri1, Goal_3, Opts2),
      nb_setarg(1, InPath, InPath0)
  ;   dicts_getchk(headers, InPath, Headers),
      dict_get(link, Headers, LinkA),
      option(base_uri(BaseUri), Opts, Uri1),
      atom_phrase(link(BaseUri,Links), LinkA),
      once((
        member(link(Uri2,Params), Links),
        memberchk(rel-next, Params)
      )),
      client_pagination(Uri2, Goal_3, Opts)
  ).
