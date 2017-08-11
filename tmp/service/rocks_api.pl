:- module(
  rocks_api,
  [
  % Predicates with alias
    rocks_alias/1,     % ?Alias
    rocks_ls/0,
    rocks_ls/1,        % +PageOpts
    rocks_rm/1,        % +Alias
  % Predicates with RocksDB handle
    rocks_merge_set/5, % +Mode, +Key, +Left, +Right, -Result
    rocks_merge_sum/5, % +Mode, +Key, +Left, +Right, -Result
    rocks_nullify/1,   % +RocksDB
    rocks_pull/3       % +RocksDB, -Key, -Val
  ]
).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(file_ext)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pagination_cli)).
:- use_module(library(settings)).

%! rocks_alias(+Alias) is semidet.
%! rocks_alias(-Alias) is nondet.
%
% Enumerates the exisiting RocksDB indices.

rocks_alias(Alias) :-
  setting(index_dir, Dir),
  (   ground(Alias)
  ->  once(directory_subdirectory(Dir, Alias, Subdir)),
      exists_directory(Subdir)
  ;   directory_subdirectory(Dir, Alias, _)
  ).
  


%! rocks_ls is det.
%! rocks_ls(+PageOpts) is det.
%
% Prints the existing RocksDB indices to stdout.

rocks_ls :-
  rocks_ls(_{}).


rocks_ls(PageOpts) :-
  pagination(Alias, rocks_alias(Alias), PageOpts, Result),
  cli_pagination_result(Result, pp_aliases).

pp_aliases(Aliases) :-
  maplist(writeln, Aliases).



%! rocks_merge_set(+Mode, +Key, +Left, +Right, -Result) is det.

rocks_merge_set(partial, _, X, Y, Z) :-
  ord_union(X, Y, Z),
  debug(rocks_api, "Partial set merge: ~p ~p → ~p", [X,Y,Z]).
rocks_merge_set(full, _, X, Y, Z) :-
  append([X|Y], XY),
  sort(XY, Z),
  debug(rocks_api, "Full set merge: ~p ~p → ~p", [X,Y,Z]).



%! rocks_merge_sum(+Mode, +Key, +Left, +Right, -Result) is det.

rocks_merge_sum(partial, _, X, Y, Z) :-
  Z is X + Y.
rocks_merge_sum(full, _, Initial, Additions, Sum) :-
  sum_list([Initial|Additions], Sum).



%! rocks_nullify(+RocksDB) is det.

rocks_nullify(RocksDB) :-
  forall(
    rocks_key(RocksDB, Key),
    rocks_merge(RocksDB, Key, 0)
  ).



%! rocks_pull(+RocksDB, -Key, -Val) is nondet.

rocks_pull(RocksDB, Key, Val) :-
  rocks_enum(RocksDB, Key, Val),
  rocks_delete(RocksDB, Key).



%! rocks_rm(+Alias) is det.

rocks_rm(Alias) :-
  % Make sure the RocksDB index is closed before its files are
  % removed.
  catch(rocks_close(Alias), _, true),
  rocks_dir0(Alias, Dir),
  delete_directory_and_contents_silent(Dir).
