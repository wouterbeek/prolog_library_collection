:- module(
  aleph_settings,
  [
    set/2 % +Name:atom
          % +Value
  ]
).

/** <module> ALEPH_SETTINGS

Settings for the Aleph framework.

@author Wouter Beek
@version 2013/06
*/

:- use_module(library(settings)).



%! check_setting(+Name:atom, +Value) is semidet.
% Succeeds if either the value is of the defined type,
% or there is no setting with the given name.
%
% @tbd Make this predicate fail on non-existing settings as well.

% special case for threads as only SWI supports it.
check_setting(threads, Value):-
  set_def(threads, _, _, Domain, _DefaultValue, _),
  check_legal(Domain, Value),
  !.
check_setting(Name, Value):-
  set_def(Name, _, _, Domain, _DefaultValue, _),
  !,
  (
    check_legal(Domain, Value)
  ->
    true
  ;
    err_message(set(Name, Value))
  ).
% Succeeds for unrecognized setting names.
check_setting(_Name, _Value).

%! default_setting(?Name:atom, ?Value) is nondet.

default_setting(Name, DefaultValue):-
  set_def(Name, _, _, _Domain, DefaultValue, _),
  DefaultValue \= ''.

%! set(+Name:atom, +Value) is semidet.
% Sets the new value for the setting with the given name.

set(Name,Value):-
  check_setting(Name, Value),
  (
    Value = 1e10
  ->
    V is 1e10
  ;
    Value = +1e10
  ->
    V is 1e10
  ;
    Value = -1e10
  ->
    V is -1e10
  ;
    V = Value
  ),
  % Retract the old value.
  retractall('$aleph_global'(Name, set(Name, _OldValue))),
  % Assert the new value.
  assertz('$aleph_global'(Name, set(Name, V))),
  % @tbd This broadcast is never listened to.
  broadcast(set(Name, V)),
  special_consideration(Name, Value).

% set_def(Parameter,Class,TextDescr,Type,Default,Flag)
set_def(abduce, search-search_strategy,
  'Abduce Atoms and Generalise',
  [true, false], false,
  show).
set_def(best, search-search_space,
  'Label to beat',
  prolog_term,'',
  show).
set_def(cache_clauselength, miscellaneous,
  'Maximum Length of Cached Clauses',
  int(1)-int(+1e10), 3,
  show).
set_def(caching, miscellaneous,
  'Cache Clauses in Search',
  [true, false], false,
  show).
set_def(check_redundant, miscellaneous,
  'Check for Redundant Literals',
  [true, false], false,
  show).
set_def(check_good, miscellaneous,
  'Check good clauses for duplicates',
  [true, false], false,
  show).
set_def(check_useless, saturation,
  'Remove I/O unconnected Literals',
  [true, false], false,
  show).
set_def(classes, tree,
  'Class labels',
  prolog_term,'',
  show).
set_def(clauselength_distribution, search-search_strategy,
  'Probablity Distribution over Clauses',
  prolog_term,'',
  show).
set_def(clauselength, search-search_space,
  'Maximum Clause Length',
  int(1)-int(+1e10), 4,
  show).
set_def(clauses, search-search_space,
  'Maximum Clauses per Theory',
  int(1)-int(+1e10),'',
  show).
set_def(condition, evaluation,
  'Condition SLP',
  [true, false], false,
  show).
set_def(confidence, tree,
  'Confidence for Rule Pruning',
  float(0.0)-float(1.0), 0.95,
  show).
set_def(construct_bottom, saturation,
  'Build a bottom clause',
  [saturation, reduction, false], saturation,
  show).
set_def(depth, miscellaneous,
  'Theorem Proving Depth',
  int(1)-int(+1e10), 10,
  show).
set_def(evalfn, evaluation,
  'Evaluation Function',
  [coverage, compression, posonly, pbayes, accuracy, laplace,
  auto_m, mestimate, mse, entropy, gini, sd, wracc, user], coverage,
  show).
set_def(explore, search-search_space,
  'Exhaustive Search of all alternatives',
  [true, false], false,
  show).
set_def(good, miscellaneous,
  'Store good clauses',
  [true, false], false,
  show).
set_def(goodfile, miscellaneous,
  'File of good clauses',
  write(filename),'',
  show).
set_def(gsamplesize, evaluation,
  'Size of random sample',
  int(1)-int(+1e10), 100,
  show).
set_def(i, saturation,
  'bound layers of new variables',
  int(1)-int(+1e10), 2,
  show).
set_def(interactive, search-search_strategy,
  'Interactive theory construction',
  [true, false], false,
  show).
set_def(language, search-search_space,
  'Maximum occurrence of any predicate symbol in a clause',
  int(1)-int(+1e10), +1e10,
  show).
set_def(lazy_negs, evaluation,
  'Lazy theorem proving on negative examples',
  [true, false], false,
  show).
set_def(lazy_on_contradiction, evaluation,
  'Lazy theorem proving on contradictions',
  [true, false], false,
  show).
set_def(lazy_on_cost, evaluation,
  'Lazy theorem proving on cost',
  [true, false], false,
  show).
set_def(lookahead, search-search_space,
  'Lookahead for automatic refinement operator',
  int(1)-int(+1e10), 1,
  show).
set_def(m, evaluation,
  'M-estimate',
  float(0.0)-float(+1e10),'',
  show).
set_def(max_abducibles, search-search_space,
  'Maximum number of atoms in an abductive explanation',
  int(1)-int(+1e10), 2,
  show).
set_def(max_features, miscellaneous,
  'Maximum number of features to be constructed',
  int(1)-int(+1e10), +1e10,
  show).
set_def(minacc, evaluation,
  'Minimum clause accuracy',
  float(0.0)-float(1.0), 0.0,
  show).
set_def(mingain, tree,
  'Minimum expected gain',
  float(0.000001)-float(+1e10), 0.05,
  show).
set_def(minpos, evaluation,
  'Minimum pos covered by a clause',
  int(0)-int(+1e10), 1,
  show).
set_def(minposfrac, evaluation,
  'Minimum proportion of positives covered by a clause',
  float(0.0)-float(1.0), 0,
  show).
set_def(minscore, evaluation,
  'Minimum utility of an acceptable clause',
  float(-1e10)-float(+1e10), -1e10,
  show).
set_def(moves, search-search_strategy,
  'Number of moves in a randomised local search',
  int(0)-int(+1e10), 5,
  show).
set_def(newvars, search-search_space,
  'Existential variables in a clause',
  int(0)-int(+1e10), +1e10,
  show).
set_def(nodes, search-search_space,
  'Nodes to be explored in the search',
  int(1)-int(+1e10), 5000,
  show).
set_def(noise, evaluation,
  'Maximum negatives covered',
  int(0)-int(+1e10), 0,
  show).
set_def(nreduce_bottom, saturation,
  'Negative examples based reduction of bottom clause',
  [true, false], false,
  show).
set_def(openlist, search-search_space,
  'Beam width in a greedy search',
  int(1)-int(+1e10), +1e10,
  show).
set_def(optimise_clauses, miscellaneous,
  'Perform query Optimisation',
  [true, false], false,
  show).
set_def(permute_bottom, saturation,
  'Randomly permute order of negative literals in the bottom clause',
  [true, false], false,
  show).
set_def(portray_examples, miscellaneous,
  'Pretty print examples',
  [true, false], false,
  show).
set_def(portray_hypothesis, miscellaneous,
  'Pretty print hypotheses',
  [true, false], false,
  show).
set_def(portray_literals, miscellaneous,
  'Pretty print literals',
  [true, false], false,
  show).
set_def(portray_search, miscellaneous,
  'Pretty print search',
  [true, false], false,
  show).
set_def(print, miscellaneous,
  'Literals printed per line',
  int(1)-int(+1e10), 4,
  show).
set_def(prior, miscellaneous,
  'Prior class distribution',
  prolog_term,'',
  show-ro).
set_def(proof_strategy, miscellaneous,
  'Current proof strategy',
  [restricted_sld, sld, user], restricted_sld,
  show).
set_def(prooftime, miscellaneous,
  'Theorem proving time',
  float(0.0)-float(+1e10), +1e10,
  show).
set_def(prune_tree, tree,
  'Tree pruning',
  [true, false], false,
  show).
set_def(recordfile, miscellaneous,
  'Log filename',
  write(filename),'',
  show).
set_def(record, miscellaneous,
  'Log to file',
  [true, false], false,
  show).
set_def(refineop, search-search_strategy,
  'Current refinement operator',
  [user, auto, scs, false],'',
  show-ro).
set_def(refine, search-search_strategy,
  'Nature of customised refinement operator',
  [user, auto, scs, false], false,
  show).
set_def(resample, search-search_strategy,
  'Number of times to resample an example',
  int(1)-int(+1e10), 1,
  show).
set_def(rls_type, search-search_strategy,
  'Type of randomised local search',
  [gsat, wsat, rrr, anneal], gsat,
  show).
set_def(rulefile, miscellaneous,
  'Rule file',
  write(filename),'',
  show).
set_def(samplesize, search-search_strategy,
  'Size of sample',
  int(0)-int(+1e10), 0,
  show).
set_def(scs_percentile, search-search_strategy,
  'Percentile of good clauses for SCS search',
  float(0.0)-float(100.0),'',
  show).
set_def(scs_prob, search-search_strategy,
  'Probability of getting a good clause in SCS search',
  float(0.0)-float(1.0),'',
  show).
set_def(scs_sample, search-search_strategy,
  'Sample size in SCS search',
  int(1)-int(+1e10), '',
  show).
set_def(search, search-search_strategy,
  'Search Strategy',
  [bf, df, heuristic, ibs, ils, rls, scs, id, ic, ar, false], bf,
  show).
set_def(searchstrat, search-search_strategy,
  'Current Search Strategy',
  [bf, df, heuristic, ibs, ils, rls, scs, id, ic, ar], bf,
  show-ro).
set_def(searchtime, search-search_strategy,
  'Search time in seconds',
  float(0.0)-float(+1e10), +1e10,
  show).
set_def(skolemvars, miscellaneous,
  'Counter for non-ground examples',
  int(1)-int(+1e10), 10000,
  show).
set_def(splitvars, saturation,
  'Split variable co-refencing',
  [true, false], false,
  show).
set_def(stage, miscellaneous,
  'Aleph processing mode',
  [saturation, reduction, command], command,
  show-ro).
set_def(store_bottom, saturation,
  'Store bottom',
  [true, false], false,
  show).
set_def(subsample, search-search_strategy,
  'Subsample for evaluating a clause',
  [true,false], false,
  show).
set_def(subsamplesize, search-search_strategy,
  'Size of subsample for evaluating a clause',
  int(1)-int(+1e10), +1e10,
  show).
set_def(temperature, search-search_strategy,
  'Temperature for randomised search annealing',
  float(0.0)-float(+1e10), '',
  show).
set_def(test_neg, miscellaneous,
  'Negative examples for testing theory',
  read(filename),'',
  show).
set_def(test_pos, miscellaneous,
  'Positive examples for testing theory',
  read(filename),'',
  show).
set_def(threads, miscellaneous,
  'Number of threads',
  int(1)-int(+1e10), 1,
        show).
set_def(train_neg, miscellaneous,
  'Negative examples for training',
  read(filename),'',
  show).
set_def(train_pos, miscellaneous,
  'Positive examples for training',
  read(filename),'',
  show).
set_def(tree_type, tree,
  'Type of tree to construct',
  [classification, class_probability, regression, model], '',
  show).
set_def(tries, search-search_strategy,
  'Number of restarts for a randomised search',
  int(1)-int(+1e10), 10,
  show).
set_def(typeoverlap, miscellaneous,
  'Type overlap for induce_modes',
  float(0.0)-float(1.0), 0.95,
  show).
set_def(uniform_sample, search-search_strategy,
  'Distribution to draw clauses from randomly',
  [true, false], false,
  show).
set_def(updateback, miscellaneous,
  'Update background knowledge with clauses found on search',
  [true, false], true,
  noshow).
set_def(verbosity, miscellaneous,
  'Level of verbosity',
  int(0)-int(+1e10), 1,
  show).
% The current version of Aleph.
% This is set internally.
set_def(version, miscellaneous,
  'Aleph version',
  int(0)-int(+1e10), 5,
  show-ro).
set_def(walk, search-search_strategy,
  'Random walk probability for Walksat',
  float(0.0)-float(1.0), '',
  show).

setting_definition(A,B,C,D,E,F1):-
  set_def(A,B,C,D,E,F),
  (F = noshow -> F1 = dontshow; F = F1).

