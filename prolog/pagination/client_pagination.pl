:- module(
  client_pagination,
  [
    client_pagination/2, % +Uri, :Goal_3
    client_pagination/3  % +Uri, :Goal_3, +Opts
  ]
).

/** <module> Client pagination

@author Wouter Beek
@version 2016/12-2017/01
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/rfc5988)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate
    client_pagination(+, 3),
    client_pagination(+, 3, +).





%! client_pagination(+Uri, :Goal_3) is nondet.
%! client_pagination(+Uri, :Goal_3, +Opts) is nondet.
%
% The following options are supported:
%
%   * base_uri(+atom)
%
%     Used to resolve relative URIs in the HTTP Link header.  By
%     default this is the given request Uri.
%
%   * Other options are passed to call_on_stream/3.
%
% Notice that we pull off some tricks to:
%
%   1. retain the HTTP metadata after the last non-deterministic call
%      is made in ‘call_on_stream/3’,
%
%   2. ensure that this predicate is deterministic when there is no
%      ‘next’ link, and non-deterministic otherwise.

client_pagination(Uri, Goal_3) :-
  client_pagination(Uri, Goal_3, []).


client_pagination(Uri1, Goal_3, Opts1) :-
  merge_options([metadata(InPath)], Opts1, Opts2),
  State = state(_),
  when(nonvar(InPath), nb_setarg(1, State, InPath)),
  (   call_on_stream(uri(Uri1), Goal_3, Opts2)
  ;   State = state(InPath),
      nonvar(InPath),
      once((
        member(InEntry, InPath),
        _{'@type': uri, headers: Headers} :< InEntry
      )),
      _{link: Link} :< Headers,
      option(base_uri(BaseUri), Opts1, Uri1),
      atom_phrase(link(BaseUri,Links), Link),
      once((
        member(link(Uri2,Params), Links),
        memberchk(rel-next, Params)
      )),
      % Detect cyclic ‘Link’ headers.
      (Uri1 == Uri2 -> print_message(warning, pagination_loop(Uri1)) ; true),
      client_pagination(Uri2, Goal_3, Opts1)
  ).
