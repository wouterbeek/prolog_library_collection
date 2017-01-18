:- module(
  json_ext,
  [
    atom_json_dict/2,   % ?A, ?Dict
    atomize_json/2,     % +Dict, -AtomizedDict
    json_escape/2,      % +Str1, -Str2
    json_read_any/2,    % +Source, -Dict
    json_read_any/3,    % +Source, -Dict, +Opts
    json_var_to_null/2, % +Term, -NullifiedTerm
    json_write_any/2,   % +Sink, +Dict
    json_write_any/3,   % +Sink, +Dict, +Opts
    json_write_dict/1,  % +Dict
    string_json_dict/3, % +Str, -Dict
    string_json_dict/2  % +Str, -Dict, +Opts
  ]
).
:- reexport(library(http/json)).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(os/io)).
:- use_module(library(yall)).





%! atom_json_dict(+A, -Dict) is det.
%! atom_json_dict(-A, +Dict) is det.

atom_json_dict(A, Dict) :-
  atom_json_dict(A, Dict, []).



%! atomize_json(+Dict, -AtomizedDict) is det.

atomize_json(L1, L2):-
  is_list(L1), !,
  maplist(atomize_json, L1, L2).
atomize_json(Dict1, Dict2):-
  atomize_dict(Dict1, Dict2).



%! json_escape(+Str1, -Str2) is det.
%
% Use backslash escapes for:
%
%   - beep
%   - double quote
%   - forward slash
%   - horizontal tab
%   - newline
%   - form feed
%   - return

json_escape(Str1, Str2) :-
  string_phrase(json_escape_codes, Str1, Str2).


json_escape_codes, [0'\\,C]   --> [0'\\,C], !, json_escape_codes.
json_escape_codes, "\\\""     --> "\"",     !, json_escape_codes.
json_escape_codes, [0'\\,0'/] --> [0'/],    !, json_escape_codes.
json_escape_codes, "\\b"      --> [7],      !, json_escape_codes.
json_escape_codes, "\\t"      --> [9],      !, json_escape_codes.
json_escape_codes, "\\n"      --> [10],     !, json_escape_codes.
json_escape_codes, "\\f"      --> [12],     !, json_escape_codes.
json_escape_codes, "\\r"      --> [13],     !, json_escape_codes.



%! json_read_any(+Source, -Dict) is det.
%! json_read_any(+Source, -Dict, +Opts) is det.
%
% Read JSON from any source.

json_read_any(Source, Dict):-
  json_read_any(Source, Dict, []).


json_read_any(Source, Dict, Opts1) :-
  merge_options([request_header('Accept'='application/json')], Opts1, Opts2),
  call_on_stream(
    Source,
    {Dict,Opts2}/[In,Meta,Meta]>>json_read_dict(In, Dict, Opts2),
    Opts2
  ).



%! json_var_to_null(+Term, -NullifiedTerm) is det.
%
% Maps Prolog terms to themselves unless they are variables, in which
% case they are mapped to the atom `null`.
%
% The is used for exporting seedpoints, where Prolog variables have no
% equivalent in JSON.

json_var_to_null(X, null) :-
  var(X), !.
json_var_to_null(X, X).



%! json_write_any(+Sink, -Dict) is det.
%! json_write_any(+Sink, -Dict, +Opts) is det.
%
% Write JSON to any sink.

json_write_any(Sink, Dict):-
  json_write_any(Sink, Dict, []).


json_write_any(Sink, Dict, Opts):-
  call_to_stream(
    Sink,
    {Dict,Opts}/[Out]>>json_write_dict(Out, Dict, Opts),
    Opts
  ).



%! json_write_dict(+Dict) is det.

json_write_dict(Dict) :-
  json_write_dict(current_output, Dict).



%! string_json_dict(+Str, -Dict) is det.
%! string_json_dict(-Str, +Dict) is det.
%! string_json_dict(+Str, -Dict, +Opts) is det.
%! string_json_dict(-Str, +Dict, +Opts) is det.

string_json_dict(Str, Dict) :-
  string_json_dict(Str, Dict, []).


string_json_dict(Str, Dict, Opts) :-
  atom_string(A, Str),
  atom_json_dict(A, Dict, Opts).
