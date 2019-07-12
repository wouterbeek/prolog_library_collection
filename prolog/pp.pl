:- module(
  pp,
  [
    ansi_format/2,       % +Attributes, +String
    call_bool_pp/1,      % :Goal_0
    call_pp/1,           % :Goal_1
    dcg_ansi_format/2,   % +Attributes, :Dcg_0
    pp_bool/1,           % +Bool
    pp_json/1,           % +Dict
    pp_json/2,           % +Indent, +Dict
    print_file_peek/2,   % +File, +Length
    print_stream_peek/2, % +In, +Length
    print_term/1,        % +Term
    print_term_nl/1,     % +Term
    print_term_nl/2      % +Term, +Options
  ]
).
:- reexport(library(ansi_term)).
:- reexport(library(pprint)).

/** <module> Pretty-print

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(call_ext)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(file_ext)).
:- use_module(library(string_ext)).

:- meta_predicate
    call_bool_pp(0),
    call_pp(1),
    dcg_ansi_format(+, //).





%! ansi_format(+Attributes:list(compound), +String:string) is det.

ansi_format(Attrs, String) :-
  ansi_format(Attrs, String, []).



%! call_bool_pp(:Goal_0) is det.

call_bool_pp(Goal_0) :-
  call_bool(Goal_0, Bool),
  pp_bool(Bool).



%! call_pp(:Goal_1) is det.

call_pp(Goal_1) :-
  catch(call(Goal_1, Term), Error, true),
  (var(Error) -> print_term(Term) ; print_message(warning, Error)).



%! dcg_ansi_format(+Attributes:list(compound), :Dcg_0) is det.

dcg_ansi_format(Attrs, Dcg_0) :-
  string_phrase(Dcg_0, String),
  ansi_format(Attrs, String, []).



%! pp_bool(+Bool:boolean) is det.

pp_bool(false) :-
  format("❌").
pp_bool(true) :-
  format("✓").



%! pp_json(+Dict:dict) is det.
%! pp_json(+Indent:nonneg, +Dict:dict) is det.

pp_json(Dict) :-
  pp_json(0, Dict).


pp_json(N1, Dict) :-
  is_dict(Dict), !,
  dict_pairs(Dict, Pairs),
  format("{\n"),
  N2 is N1 + 1,
  pp_dict_pairs(N2, Pairs),
  pp_tab(N1),
  format("}").
pp_json(_, Str) :-
  string(Str), !,
  format('"~s"', [Str]).
pp_json(_, Term) :-
  format("~w", [Term]).

pp_dict_pair(N, Key-Value) :-
  pp_tab(N),
  format("~a: ", [Key]),
  pp_json(N, Value).

pp_dict_pairs(N, [H]) :- !,
  pp_dict_pair(N, H),
  format("\n").
pp_dict_pairs(N, [H|T]) :-
  pp_dict_pair(N, H),
  format(",\n"),
  pp_dict_pairs(N, T).

pp_tab(0) :- !.
pp_tab(N1) :-
  format("  "),
  N2 is N1 - 1,
  pp_tab(N2).



%! print_file_peek(+File:atom, +Length:nonneg) is det.

print_file_peek(File, Length1) :-
  Length2 is Length1 + 5,
  peek_file(File, Length2, String),
  print_string_(String, Length1).



%! print_stream_peek(+In:stream, Length:nonneg) is det.

print_stream_peek(In, Length1) :-
  Length2 is Length1 + 5,
  peek_string(In, Length2, String),
  print_string_(String, Length1).

print_string_(String1, Length) :-
  string_ellipsis(String1, Length, String2),
  format(user_ouput, "~s", [String2]).



%! print_term(+Term:term) is det.

print_term(Term) :-
  print_term(Term, []).



%! print_term_nl(+Term:term) is det.
%! print_term_nl(+Term:term, +Options:list(compound)) is det.

print_term_nl(Term) :-
  print_term_nl(Term, []).


print_term_nl(Term, Options) :-
  print_term(Term, Options),
  nl.
