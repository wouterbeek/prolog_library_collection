:- module(
  json_ext,
  [
    atom_json_dict/2,   % ?A,      ?D
    atomize_json/2,     % +D,      -AtomizedD
    json_read_any/2,    % +Source, -D
    json_read_any/3,    % +Source, -D, +Opts
    json_var_to_null/2, % +Term,   -NullifiedTerm
    json_write_any/2,   % +Sink,   +D
    json_write_any/3    % +Sink,   +D, +Opts
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01, 2016/03-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(http/json)).
:- use_module(library(os/open_any2)).
:- use_module(library(yall)).





%! atom_json_dict(+A, -D) is det.
%! atom_json_dict(-A, +D) is det.

atom_json_dict(A, D) :-
  atom_json_dict(A, D, []).



%! atomize_json(+D, -AtomizedD) is det.

atomize_json(L1, L2):-
  is_list(L1), !,
  maplist(atomize_json, L1, L2).
atomize_json(D1, D2):-
  atomize_dict(D1, D2).



%! json_read_any(+Source, -D) is det.
%! json_read_any(+Source, -D, +Opts) is det.
% Read JSON from any source.

json_read_any(Source, D):-
  json_read_any(Source, D, []).


json_read_any(Source, D, Opts1) :-
  merge_options([request_header('Accept'='application/json')], Opts1, Opts2),
  call_on_stream(
    Source,
    {D,Opts2}/[In,M,M]>>json_read_dict(In, D, Opts2),
    Opts2
  ).



%! json_var_to_null(+Term, -NullifiedTerm) is det.
% Maps Prolog terms to themselves unless they are variables,
% in which case they are mapped to the atom `null`.
%
% The is used for exporting seedpoints, where Prolog variables
% have no equivalent in JSON.

json_var_to_null(X, null) :- var(X), !.
json_var_to_null(X, X).



%! json_write_any(+Sink, -D) is det.
%! json_write_any(+Sink, -D, +Opts) is det.
% Write JSON to any sink.

json_write_any(Sink, D):-
  json_write_any(Sink, D, []).


json_write_any(Sink, D, Opts):-
  call_to_stream(
    Sink,
    {D,Opts}/[Out,M,M]>>json_write_dict(Out, D, Opts),
    Opts
  ).
