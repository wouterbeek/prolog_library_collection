:- module(
  debug_ext,
  [
    call_collect_messages/1,    % :Goal_0
    call_collect_messages/3,    % :Goal_0, -Status, -Messages
    debug_all_files/0,
    debug_collect_messages/1,   % :Goal_0
    debug_concurrent_maplist/3, % +Flag, :Goal_1, +Args1
    debug_concurrent_maplist/4, % +Flag, :Goal_2, +Args1, +Args2
    debug_concurrent_maplist/5, % +Flag, :Goal_3, +Args1, +Args2, +Args3
    debug_maplist/3,            % +Flag, :Goal_1, +Args1
    debug_verbose/2,            % +Flag, :Goal_0
    debug_verbose/3,            % +Flag, :Goal_0, +Format
    debug_verbose/4,            % +Flag, :Goal_0, +Format, +Args
    debug_with_output_to/2,     % ?Flag, :Goal_0
    dmon/0,
    if_debug/2,                 % ?Flag, :Goal_0
    number_of_open_files/1,     % -N
    pof/1,                      % :Goal_0
    pofa/0,
    pofz/0,
    print_error/1,              % +Error:compound
    tmon/0
  ]
).
:- reexport(library(debug)).

/** <module> Debug extensions

Tools that ease debugging SWI-Prolog programs.

@author Wouter Beek
@version 2015/07-2015/11, 2016/01-2016/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(check_installation)). % Private predicates.
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(default)).
:- use_module(library(lists)).
:- use_module(library(os/dir_ext)).
:- use_module(library(portray_text)).
:- use_module(library(print_ext)).
:- use_module(library(swi_ide)).
:- use_module(library(thread)).

:- meta_predicate
    call_collect_messages(0),
    call_collect_messages(0,-,-),
    debug_collect_messages(0),
    debug_concurrent_maplist(+,1,+),
    debug_concurrent_maplist(+,2,+,+),
    debug_concurrent_maplist(+,3,+,+,+),
    debug_maplist(+, 1, +),
    debug_maplist0(+, 1, +, +, +),
    debug_verbose(?,0),
    debug_verbose(?,0,+),
    debug_verbose(?,0,+,+),
    debug_with_output_to(?,0),
    if_debug(?,0),
    pof(0).





%! call_collect_messages(:Goal_0) is det.

call_collect_messages(Goal_0):-
  call_collect_messages(Goal_0, Status, Es),
  process_warnings(Es),
  process_status(Status).

process_status(true):- !.
process_status(fail):- !, fail.
process_status(E):- print_error(E).

process_warnings([]):- !.
process_warnings(Es):- maplist(print_error, Es).
%process_warnings(Es):-
%  length(Es, N),
%  (N =:= 0 -> true ; print_message(warnings, number_of_warnings(N))).


%! call_collect_messages(
%!   :Goal_0,
%!   -Status:compound,
%!   -Messages:list(compound)
%! ) is det.

call_collect_messages(Goal_0, Status, Messages):-
  check_installation:run_collect_messages(Goal_0, Status, Messages).



%! debug_all_files is det.
% Loads all subdirectories and Prolog files contained in those directories.

debug_all_files:-
  absolute_file_name(project(.), Dir, [access(read),file_type(directory)]),
  directory_file_path(Dir, '*.pl', Wildcard),
  expand_file_name(Wildcard, Files1),
  exclude(do_not_load, Files1, Files2),
  maplist(use_module0, Files2).
use_module0(File):-
  print_message(informational, loading_module(File)),
  use_module(File).

do_not_load(File1):-
  file_base_name(File1, File2),
  file_name_extension(File3, pl, File2),
  do_not_load0(File3).

do_not_load0(dcg_ascii).
do_not_load0(dcg_unicode).



%! debug_collect_messages(:Goal_0) is det.

debug_collect_messages(Goal_0):-
  call_collect_messages(Goal_0, Status, Warns),
  process_warnings(Warns),
  process_status(Status),
  (   Status == true
  ->  true
  ;   Status == false
  ->  false
  ;   print_error(Status)
  ).



%! debug_concurrent_maplist(+Flag, :Goal_1, +Args1) is det.

debug_concurrent_maplist(Flag, Goal_1, Args1):-
  debugging(Flag), !,
  maplist(Goal_1, Args1).
debug_concurrent_maplist(_, Goal_1, Args1):-
  concurrent_maplist(Goal_1, Args1).


%! debug_concurrent_maplist(+Flag, :Goal_2, +Args1, +Args2) is det.

debug_concurrent_maplist(Flag, Goal_2, Args1, Args2):-
  debugging(Flag), !,
  maplist(Goal_2, Args1, Args2).
debug_concurrent_maplist(_, Goal_2, Args1, Args2):-
  concurrent_maplist(Goal_2, Args1, Args2).


%! debug_concurrent_maplist(+Flag, :Goal_3, +Args1, +Args2, +Args3) is det.

debug_concurrent_maplist(Flag, Goal_3, Args1, Args2, Args3):-
  debugging(Flag), !,
  maplist(Goal_3, Args1, Args2, Args3).
debug_concurrent_maplist(_, Goal_3, Args1, Args2, Args3):-
  concurrent_maplist(Goal_3, Args1, Args2, Args3).



%! debug_maplist(+Flag, :Goal_1, +Args1) .

debug_maplist(Flag, Goal_1, L1) :-
  debugging(Flag), !,
  length(L1, Len1),
  debug_maplist0(Flag, Goal_1, L1, 1, Len1).
debug_maplist(_, Goal_1, L1) :-
  maplist(Goal_1, L1).

debug_maplist0(_, _, [], _, _).
debug_maplist0(Flag, Goal_1, [H|T], Len1, Len) :-
  call(Goal_1, H),
  (Len1 mod 100 =:= 0 -> debug(Flag, "~D out of ~D calls.", [Len1,Len]) ; true),
  Len2 is Len1 + 1,
  debug_maplist0(Flag, Goal_1, T, Len2, Len).



%! debug_verbose(+Flag, :Goal_0) is det.

debug_verbose(Flag, Goal_0):-
  debug_with_output_to(Flag, verbose(Goal_0)).


%! debug_verbose(+Flag, :Goal_0, +Fragment) is det.

debug_verbose(Flag, Goal_0, Fragment):-
  debug_with_output_to(Flag, verbose(Goal_0, Fragment)).


%! debug_verbose(+Flag, :Goal_0, +Fragment, +Args) is det.

debug_verbose(Flag, Goal_0, Fragment, Args):-
  debug_with_output_to(Flag, verbose(Goal_0, Fragment, Args)).



%! debug_with_output_to(?Flag, :Goal_0) is det.

debug_with_output_to(Flag, Goal_0):-
  with_output_to(atom(A), Goal_0),
  debug(Flag, '~a', [A]).



dmon:- prolog_ide(debug_monitor).



%! if_debug(?Flag, :Goal_0) is det.
% Calls the given goal only if the given flag is an active debugging topic.
% Succeeds if Flag is uninstantiated.
%
% @see library(debug)

if_debug(Flag, _):-   var(Flag), !.
if_debug(Flag, _):-   \+ debugging(Flag), !.
if_debug(_, Goal_0):- call(Goal_0).



%! number_of_open_files(-N:nonneg) is det.

number_of_open_files(N):-
  aggregate_all(count, (member(X, [input,output]), stream_property(_, X)), N).



%! pof(:Goal_0) is det.

pof(Goal_0):- pofa, Goal_0, pofz.
pofa:- number_of_open_files(N), format(user_output, "<~D|", [N]).
pofz:- number_of_open_files(N), format(user_output, "|~D>~n", [N]).



%! print_error(+Error:compound) is det.

print_error(exception(E)):- !, print_error(E).
print_error(E):- print_message(error, E).



tmon:- prolog_ide(thread_monitor).





% MESSAGES %

:- multifile(prolog:message//1).
prolog:message(loading_module(File)) --> ['[M] ',File].
prolog:message(number_of_warnings(N)) --> ['~D warnings'-[N]].
