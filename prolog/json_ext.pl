:- module(
  json_ext,
  [
    atomize_json/2, % +Json:dict
                    % -AtomizedJson:dict
    json_read_any/2, % +File, -Dict
    json_read_any/3 % +File:atom
                    % -Dict:dict
                    % +Options:list(compound)
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09-2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/json)).
:- use_module(library(os/open_any2)).

:- predicate_options(json_read_dict/3, 3, [
     pass_to(json_read_dict/3, 3),
     pass_to(open_any2/5, 5)
   ]).





%! atomize_json(+Json:dict, -AtomizedJson:dict) is det.

atomize_json(L1, L2):-
  is_list(L1), !,
  maplist(atomize_json, L1, L2).
atomize_json(D1, D2):-
  atomize_dict(D1, D2).



%! json_read_any(+Source, -Dict:dict, +Options:list(compound)) is det.
% Wrapper around json_read_any/3 with default options.

json_read_any(Source, Dict):-
  json_read_any(Source, Dict, []).


%! json_read_any(+Source, -Dict:dict, +Options:list(compound)) is det.
% Read JSON from any source.
%
% Options are passed to open_any2/5 and json_read_dict/3.

json_read_any(Source, Dict, Opts):-
  setup_call_cleanup(
    open_any2(Source, read, Read, Close_1, Opts),
    json_read_dict(Read, Dict, Opts),
    close_any2(Close_1)
  ).
