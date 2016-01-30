:- module(
  json_ext,
  [
    atom_json_dict/2, % ?Atom, ?Json
    atomize_json/2, % +Json:dict
                    % -AtomizedJson:dict
    json_read_any/2, % +Source, -Json
    json_read_any/3, % +Source
                     % -Json:dict
                     % +Options:list(compound)
    json_write_any/2, % +Sink, +Json
    json_write_any/3 % +Sink
                     % +Json:dict
                     % +Options:list(compound)
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/json)).
:- use_module(library(os/open_any2)).

:- predicate_options(json_read_dict/3, 3, [
     pass_to(json_read_dict/3, 3),
     pass_to(open_any2/5, 5)
   ]).





%! atom_json_dict(+Atom, -Json) is det.
%! atom_json_dict(-Atom, +Json) is det.

atom_json_dict(A, D) :-
  atom_json_dict(A, D, []).



%! atomize_json(+Json:dict, -AtomizedJson:dict) is det.

atomize_json(L1, L2):-
  is_list(L1), !,
  maplist(atomize_json, L1, L2).
atomize_json(D1, D2):-
  atomize_dict(D1, D2).



%! json_read_any(+Source, -Dict:dict) is det.
% Wrapper around json_read_any/3 with default options.

json_read_any(Source, Dict):-
  json_read_any(Source, Dict, []).


%! json_read_any(+Source, -Dict:dict, +Options:list(compound)) is det.
% Read JSON from any source.
%
% Options are passed to open_any2/5 and json_read_dict/3.

json_read_any(Source, Dict, Opts):-
  setup_call_cleanup(
    open_any2(Source, read, Read, Close_0, Opts),
    json_read_dict(Read, Dict, Opts),
    close_any2(Close_0)
  ).



%! json_write_any(+Sink, -Dict:dict) is det.
% Wrapper around json_write_any/3 with default options.

json_write_any(Sink, Dict):-
  json_write_any(Sink, Dict, []).


%! json_write_any(+Sink, -Dict:dict, +Options:list(compound)) is det.
% Write JSON to any sink.
%
% Options are passed to open_any2/5 and json_write_dict/3.

json_write_any(Sink, Dict, Opts):-
  setup_call_cleanup(
    open_any2(Sink, write, Write, Close_0, Opts),
    json_write_dict(Write, Dict, Opts),
    close_any2(Close_0)
  ).
