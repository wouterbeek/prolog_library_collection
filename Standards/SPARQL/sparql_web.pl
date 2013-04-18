:- module(
  sparql_web,
  [
    set_default_remote/1, % +Remote:atom
    sparql_input_web/1, % -Markup:list
    sparql_input_web/2, % +Remote:atom
                        % -Markup:list
    sparql_output_web/2, % +Query:atom
                         % -Markup:list
    sparql_output_web/3 % +Remote:atom
                        % +Query:atom
                        % -Markup:list
  ]
).

/** <module> SPARQL Web

Web front-end for SPARQL queries.

@author Wouter Beek
@version 2012/12, 2013/03-2013/04
*/

:- use_module(server(web_console)).
:- use_module(sparql(sparql_ext)).
:- use_module(html(html)).

:- dynamic(default_remote(_Remote)).
:- assert(default_remote(dbpedia)).

:- debug(sparql_web).



% The default remote musy be registered at the SPARQL backend.
set_default_remote(Remote):-
  \+ sparql_remote(Remote, _Server, _Path),
  !,
  debug(
    sparql_web,
    'Cannot set SPARQL default remote ~w, since it has not been configured.',
    [Remote]
  ),
  existence_error(remote, Remote).
% The default remote stays the same.
set_default_remote(Remote):-
  default_remote(Remote),
  !.
% The default remote is changed.
set_default_remote(NewRemote):-
  retract(default_remote(OldRemote)),
  debug(
    sparql_web,
    'Reset SPARQL remote default from ~w to ~w.',
    [OldRemote, NewRemote]
  ),
  assert(default_remote(NewRemote)).

sparql_input_web(Markup):-
  input_ui(Markup).

sparql_input_web(Remote, Markup):-
  set_default_remote(Remote),
  input_ui(Markup).

sparql_output_web(Query, Markup):-
  default_remote(Remote),
  sparql_output_web(Remote, Query, Markup).

%% sparql_output_web(+Remote:atom, +Query:atom, -Markup:list) is det.
% Returns markup for the given SPARQL query.

sparql_output_web(Remote, Query, Markup):-
  statistics(cputime, CPU_Before),
  query_sparql(Remote, Query, VarNames, Results),
  statistics(cputime, CPU_After),
  CPU is CPU_After - CPU_Before,
  write_table(Results, [cputime(CPU), variables(VarNames)], Markup).

%% write_statistics(+Options:list(nvpairs), -Markup:dom) is det.
% Returns the markup element representing the statistics of a SPARQL query.

write_statistics(Options, element(p, [], [Message])):-
  option(count(Count), Options),
  option(cputime(CPU), Options),
  format(
    atom(Message),
    'Query completed in ~3f seconds ~D rows.',
    [CPU, Count]
  ).

%% write_table(+Rows:list, +Options:list(nvpair), -Markup:dom) is det.
% Returns the markup table for the given rows returned as the reply for a
% SPARQL query.

write_table(Rows, Options, [Statistics, Table]):-
  is_list(Rows),
  !,
  length(Rows, Count),
  write_statistics([count(Count) | Options], Statistics),
  findall(
    Row0,
    (
      member(Row, Rows),
      Row =.. [row | Row0]
    ),
    Rows0
  ),
  option(variables(VarNames), Options),
  list_to_table([header(true)], [VarNames | Rows0], Table).
write_table(Row, Options, Markup):-
  write_table([Row], Options, Markup).

