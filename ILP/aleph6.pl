:- module(
  aleph6,
  [
    ilp/1, % +Base:atom
    ilp/2, % +Base:atom
           % +Stream:stream
    ilp/3, % +Base:atom
           % +Stream:stream
           % +Mode:atom
    ilp_file/1, % +File:atom
    ilp_mode/1, % ?Mode:atom
    induce/0,
    induce_max/0,
    read_all/1, % +Base:atom
    read_all/2, % +BackgroundBase:atom
                % +ExamplesBase:atom
    read_all/3, % +BackgroundBase:atom
                % +PositiveExamplesBase:atom
                % +NegativeExamplesBase:atom
    sat/1, % +Example:number
    set_current_stream/1 % +Stream
  ]
).

/** <module> ALEPH6
A Learning Engine for Proposing Hypotheses.

This is the source for Aleph written and maintained

It was originally written to run with the Yap Prolog Compiler
Yap can be found at: http://sourceforge.net/projects/yap/
Yap must be compiled with -DDEPTH_LIMIT=1

It should also run with SWI Prolog, although performance may be
sub-optimal.

If you obtain this version of Aleph and have not already done so
please subscribe to the Aleph mailing list. You can do this by
mailing majordomo@comlab.ox.ac.uk with the following command in the
body of the mail message: subscribe aleph

Aleph is freely available for academic purposes.
If you intend to use it for commercial purposes then
please contact Ashwin Srinivasan first.

---+ Changes from Aleph5

  * Use =|library(settings)|= to cover the many settings that Aleph uses.
    This makes the settings module-aware and module-specific.
    This is more modular than using global variables.
    This introduces type checking of setting values (using the types
    recognized by must_be/2). This replaces the less advanced
    type checking operation.
  * Fixed several settings that had default values that were
    inconsitent with their domain restriction.
  * Many list operations that were performed by custom predicates now
    use predicates from =|library(lists)|=.
    These are better documented and may provide a performance improvement.

@author Ashwin Srinivasan (ashwin@comlab.ox.ac.uk),
@author Wouter Beek (me@wouterbeek.com)
@see www.comlab.ox.ac.uk/oucl/research/areas/machlearn/Aleph/index.html
@version 2012/06-2012/08, 2013/03, 2013/06
*/

:- use_module(generics(db_ext)).
:- use_module(generics(logging)).
:- use_module(generics(meta_ext)).
:- use_module(generics(os_ext)).
:- use_module(library(broadcast)).
:- use_module(library(dialect/hprolog), [memberchk_eq/2]).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(time)).

% This must come before this predicate is defined dynamic.
:- redefine_system_predicate(false).

:- dynamic(current_stream/1).
:- dynamic(example/3).
:- dynamic(false/0).

:- db_add_novel(user:prolog_file_type(b, background_knowledge)).
:- db_add_novel(user:prolog_file_type(f, pos)).
:- db_add_novel(user:prolog_file_type(n, neg)).

:- op(500,fy,#).
:- op(500,fy,*).
:- op(900,xfy,because).

:- dynamic '$aleph_feature'/2.
:- dynamic '$aleph_global'/2.
:- dynamic '$aleph_good'/3.

:- dynamic '$aleph_local'/2.

:- dynamic '$aleph_sat'/2.
:- dynamic '$aleph_sat_atom'/2.
:- dynamic '$aleph_sat_ovars'/2.
:- dynamic '$aleph_sat_ivars'/2.
:- dynamic '$aleph_sat_varsequiv'/2.
:- dynamic '$aleph_sat_varscopy'/3.
:- dynamic '$aleph_sat_terms'/4.
:- dynamic '$aleph_sat_vars'/4.
:- dynamic '$aleph_sat_litinfo'/6.

:- dynamic '$aleph_search_cache'/1.
:- dynamic '$aleph_search_prunecache'/1.
:- dynamic '$aleph_search'/2.
:- dynamic '$aleph_search_seen'/2.
:- dynamic '$aleph_search_expansion'/4.
:- dynamic '$aleph_search_gain'/4.
:- dynamic '$aleph_search_node'/8.

:- dynamic '$aleph_link_vars'/2.
:- dynamic '$aleph_has_vars'/3.
:- dynamic '$aleph_has_ovar'/4.
:- dynamic '$aleph_has_ivar'/4.
:- dynamic '$aleph_determination'/2.

:- thread_local('$aleph_search_cache'/1).
:- thread_local('$aleph_search_prunecache'/1).
:- thread_local('$aleph_search'/2).
:- thread_local('$aleph_search_seen'/2).
:- thread_local('$aleph_search_expansion'/4).
:- thread_local('$aleph_search_gain'/4).
:- thread_local('$aleph_search_node'/8).

:- multifile false/0.
:- multifile prune/1.
:- multifile refine/2.
:- multifile cost/3.
:- multifile prove/2.



% SETTINGS: EVALUATION %

:- setting(condition, boolean, false, 'Condition SLP').

:- setting(evalfn, oneof([accuracy,auto_m,compression,coverage,entropy,gini,laplace,mestimate,mse,pbayes,posonly,sd,user,wracc]), coverage, 'Evaluation Function').

:- setting(gsamplesize, positive_integer, 100, 'Size of random sample').

:- setting(lazy_negs, boolean, false, 'Lazy theorem proving on negative examples').

:- setting(lazy_on_contradiction, boolean, false, 'Lazy theorem proving on contradictions').

:- setting(lazy_on_cost, boolean, false, 'Lazy theorem proving on cost').

% @tbd The default value was chosen arbitrarily.
:- setting(m, between(0.0,10000000000.0), 0.0, 'M-estimate').

:- setting(minacc, between(0.0,1.0), 0.0, 'Minimum clause accuracy').

:- setting(minpos, nonneg, 1, 'Minimum pos covered by a clause').

:- setting(minposfrac, between(0.0,1.0), 0.0, 'Minimum proportion of positives covered by a clause').

:- setting(minscore, float, -10000000000.0, 'Minimum utility of an acceptable clause').

:- setting(noise, nonneg, 0, 'Maximum negatives covered').

% SETTINGS: MISCELLANEOUS %

:- setting(cache_clauselength, positive_integer, 3, 'Maximum Length of Cached Clauses').

:- setting(caching, boolean, false, 'Cache Clauses in Search').

:- setting(check_good, boolean, false, 'Check good clauses for duplicates').

:- setting(check_redundant, boolean, false, 'Check for Redundant Literals').

:- setting(depth, positive_integer, 10, 'Theorem Proving Depth').

:- setting(good, boolean, false, 'Store good clauses').

:- setting(goodfile, atom, '', 'File of good clauses').

:- setting(max_features, positive_integer, 10000000000, 'Maximum number of features to be constructed').

:- setting(optimise_clauses, boolean, false, 'Perform query Optimisation').

:- setting(portray_examples, boolean, false, 'Pretty print examples').

:- setting(portray_hypothesis, boolean, false, 'Pretty print hypotheses').

:- setting(portray_literals, boolean, false, 'Pretty print literals').

:- setting(portray_search, boolean, false, 'Pretty print search').

:- setting(print, positive_integer, 4, 'Literals printed per line').

:- setting(prior, any, '', 'Prior class distribution').

:- setting(proof_strategy, oneof([restricted_sld,sld,user]), restricted_sld, 'Current proof strategy').

:- setting(prooftime, between(0.0,10000000000.0), 10000000000.0, 'Theorem proving time').

% @tbd File name.
:- setting(test_neg, atom, '', 'Negative examples for testing theory').

% @tbd File name.
:- setting(test_pos, atom, '', 'Positive examples for testing theory').

:- setting(threads, positive_integer, 1, 'Number of threads').

% @tbd File name.
:- setting(train_neg, atom, '', 'Negative examples for training').

% @tbd File name.
:- setting(train_pos, atom, '', 'Positive examples for training').

% @tbd File name.
:- setting(recordfile, atom, '', 'Log filename').

:- setting(record, boolean, false, 'Log to file').

% @tbd File name.
:- setting(rulefile, atom, '', 'Rule file').

:- setting(skolemvars, positive_integer, 10000, 'Counter for non-ground examples').

:- setting(stage, oneof([command,reduction,saturation]), command, 'Aleph processing mode').

:- setting(updateback, boolean, true, 'Update background knowledge with clauses found on search').

:- setting(verbosity, nonneg, 1, 'Level of verbosity').

:- setting(version, nonneg, 5, 'Aleph version').

:- setting(typeoverlap, between(0.0,1.0), 0.95, 'Type overlap for induce_modes').

% SETTING: SATURATION %

:- setting(check_useless, boolean, false, 'Remove I/O unconnected Literals').

:- setting(construct_bottom, oneof([false,reduction,saturation]), saturation, 'Build a bottom clause').

:- setting(i, positive_integer, 2, 'Bound layers of new variables').

:- setting(nreduce_bottom, boolean, false, 'Negative examples based reduction of bottom clause').

:- setting(permute_bottom, boolean, false, 'Randomly permute order of negative literals in the bottom clause').

:- setting(splitvars, boolean, false, 'Split variable co-refencing').

:- setting(store_bottom, boolean, false, 'Store bottom').

% SETTINGS: SEARCH: SEARCH SPACE %

:- setting(best, any, '', 'Label to beat').

:- setting(clauselength, positive_integer, 4, 'Maximum Clause Length').

% @tbd The default value was chosen arbitrarily.
:- setting(clauses, positive_integer, 10000000000, 'Maximum Clauses per Theory').

:- setting(explore, boolean, false, 'Exhaustive Search of all alternatives').

:- setting(language, positive_integer, 10000000000, 'Maximum occurrence of any predicate symbol in a clause').

:- setting(lookahead, positive_integer, 1, 'Lookahead for automatic refinement operator').

:- setting(max_abducibles, positive_integer, 2, 'Maximum number of atoms in an abductive explanation').

:- setting(newvars, nonneg, 10000000000, 'Existential variables in a clause').

:- setting(nodes, positive_integer, 5000, 'Nodes to be explored in the search').

:- setting(openlist, positive_integer, 10000000000, 'Beam width in a greedy search').

% SETTINGS: SEARCH: SEARCH STRATEGY %

:- setting(abduce, boolean, false, 'Abduce Atoms and Generalise').

:- setting(clauselength_distribution, any, '', 'Probablity Distribution over Clauses').

:- setting(interactive, boolean, false, 'Interactive theory construction').

:- setting(moves, nonneg, 5, 'Number of moves in a randomised local search').

% @tbd Are =refine= and =refineop= the same setting?
:- setting(refine, oneof([auto,false,scs,user]), false, 'Nature of customised refinement operator').

% @tbd The default value was arbitrarily chosen.
:- setting(refineop, oneof([auto,false,rls,scs,user]), false, 'Current refinement operator').

:- setting(resample, positive_integer, 1, 'Number of times to resample an example').

:- setting(rls_type, oneof([anneal,gsat,rrr,wsat]), gsat, 'Type of randomised local search').

:- setting(samplesize, nonneg, 0, 'Size of sample').

% @tbd The default value was arbitrarily chosen.
:- setting(scs_percentile, between(0.0,100.0), 0.0, 'Percentile of good clauses for SCS search').

% @tbd The default value was arbitrarily chosen.
:- setting(scs_prob, between(0.0,1.0), 0.0, 'Probability of getting a good clause in SCS search').

% @tbd The default value was arbitrarily chosen.
:- setting(scs_sample, positive_integer, 100, 'Sample size in SCS search').

:- setting(search, oneof([ar,bf,df,false,heuristic,ibs,ic,id,ils,rls,scs]), bf, 'Search Strategy').

:- setting(searchstrat, oneof([ar,bf,df,heuristic,ibs,ic,id,ils,rls,scs]), bf, 'Current Search Strategy').

:- setting(searchtime, between(0.0,10000000000.0), 10000000000.0, 'Search time in seconds').

:- setting(subsample, boolean, false, 'Subsample for evaluating a clause').

:- setting(subsamplesize, positive_integer, 10000000000, 'Size of subsample for evaluating a clause').

% @tbd The default value was chosen arbitrarily.
:- setting(temperature, between(0.0,10000000000.0), 0.0, 'Temperature for randomised search annealing').

:- setting(tries, positive_integer, 10, 'Number of restarts for a randomised search').

:- setting(uniform_sample, boolean, false, 'Distribution to draw clauses from randomly').

:- setting(walk, between(0.0,1.0), 0.0, 'Random walk probability for Walksat').

% SETTINGS: TREE %

:- setting(classes, any, '', 'Class labels').

:- setting(confidence, between(0.0,1.0), 0.95, 'Confidence for Rule Pruning').

:- setting(mingain, between(0.000001,10000000000.0), 0.05, 'Minimum expected gain').

:- setting(prune_tree, boolean, false, 'Tree pruning').

% @tbd The default value has been chosen arbitrarily.
:- setting(tree_type, oneof([class_probability,classification,model,regression]), classification, 'Type of tree to construct').

% UNKNOWN %

:- setting(autorefine, boolean, false, 'Unknown').

:- setting(construct_features, boolean, false, 'Unknown').

:- setting(gcws, boolean, false, 'Unknown').

:- setting(greedy, boolean, false, 'Unknown').

:- setting(maxcover, boolean, false, 'Unknown').

:- setting(prune_defs, boolean, false, 'Unknown').

% @tbd Add must_be/2 support for streams?
:- setting(recordfile_stream, any, '', 'Unknown').

:- setting(recursion, boolean, false, 'Unknown').



% ALEPH

aleph_random(X):-
  I = 1000000,
  X is float(random(I-1)) / float(I).

depth_bound_call(Goal, Limit):-
  call_with_depth_limit(Goal, Limit, Result),
  Result \= depth_limit_exceeded.



% ILP %

%! ilp(+Base:atom) is det.
% Runs ILP on the files with the given name but in =det= and in =nondet= mode.
% The default stream for logging is used.
%
% @see ilp/3.

ilp(Base):-
  current_log_stream(Stream),
  ilp(Base, Stream),
  close_log_stream(Stream).

%! ilp(+Base:atom, +Stream:stream) is det.
% @see ilp/3.

ilp(Base, Stream):-
  ilp(Base, Stream, det).

%! ilp(+Base:atom, +Stream:stream, +Mode:atom) is det.
% Runs the ILP algorithm.
%
% @arg Base The base name of the input files.
% @arg Stream The stream to which the results are written.
% @arg Mode Either 'det', 'nondet' and 'both'. Whether the algorithm
%        gives all results (slow) or only the top result (fast).

ilp(Base, Stream, Mode):-
  set_current_stream(Stream),
  % The default stream is the swipl terminal.
  unless(
    current_stream(_SomeStream),
    asserta(current_stream(user_output))
  ),
  reset,
  read_all(Base),
  (
    Mode == det
  ->
    induce
  ;
    Mode == nondet
  ->
    induce_max
  ;
    Mode == both
  ->
    induce,
    induce_max
  ),
  reset(Base).

%! ilp_file(+File:atom) is det.
% Perform ILP for the given input file.
%
% @arg File The atomic name of a file.

ilp_file(File):-
  % Make sure the file has a correct extension for being
  % an ILP input file.
  file_base_name(File, File_),
  file_name_extension(Base, Extension, File_),
  (
    user:prolog_file_type(Extension, background_knowledge)
  ;
    user:prolog_file_type(Extension, neg)
  ;
    user:prolog_file_type(Extension, pos)
  ),
  ilp(Base).

%! ilp_mode(+Mode:atom) is semidet.
%! ilp_mode(-Mode:atom) is nondet.
% The modes in which ILP can run:
%     1. =both= first runs in =det= mode and then runs in =nondet= mode.
%        Intended for debug purposes.
%     2. =det= for deterministic operation, returning only the top result.
%     3. =nondet= for nondeterministic operation, exhausting all results.

ilp_mode(both).
ilp_mode(det).
ilp_mode(nondet).



% CONSTRUCT BOTTOM

%! get_atoms(+Predicates, +Depth, +MaxDepth, +Last, -LastLit)
% Layered generation of ground atoms to add to the bottom clause.
%
% @arg Predicates A list of Predicate/Arity entries obtained from the
%        determinations.
% @arg Depth The current variable-chain depth.
% @arg MaxDepth The maximum allowed variable chain depth (i setting).
% @arg Last The last atom number so far.
% @arg Lastlit The atom number after all atoms to MaxDepths have been
%        generated.

get_atoms([], _, _, Last, Last):-
  !.
get_atoms(Predicates, Depth, MaxDepth, Last, LastLit):-
  Depth =< MaxDepth,
  Depth0 is Depth - 1,
  % New terms generated?
  '$aleph_sat_terms'(_,Depth0,_,_),
  !,
  get_atoms1(Predicates, Depth, MaxDepth, Last, Last1),
  Depth1 is Depth + 1,
  get_atoms(Predicates, Depth1, MaxDepth, Last1, LastLit).
get_atoms(_Predicates, _Depth, _MaxDepth, Last, Last).

get_atoms1([], _Depth, _MaxDepth, Last, Last).
get_atoms1([Predicate | Predicates], Depth, MaxDepth, Last, LastLit):-
  gen_layer(Predicate, Depth),
  flatten(Depth,MaxDepth, Last, Last1),
  get_atoms1(Predicates, Depth, MaxDepth, Last1, LastLit).

% flatten(+Depth,+MaxDepth,+Last,-LastLit)
% flatten a set of ground atoms by replacing all in/out terms with variables
%  constants are wrapped in a special term called aleph_const(...)
%  eg suppose p/3 had modes p(+char,+char,#int)
%  then p(a,a,3) becomes p(X,X,aleph_const(3))
% ground atoms to be flattened are assumed to be in the i.d.b atoms
% vars and terms are actually integers which are stored in vars/terms databases
%  so eg above actually becomes p(1,1,aleph_const(3)).
%  where variable 1 stands for term 2 (say) which in turn stands for a
%  Depth is current variable-chain depth
%  MaxDepth is maximum allowed variable chain depth (i setting)
%  Last is last atom number so far
%  Lastlit is atom number after ground atoms here have been flattened
% If permute_bottom is set to true, then the order of ground atoms is
% shuffled. The empirical utility of doing this has been investigated by
% P. Schorn in "Random Local Bottom Clause Permutations for Better Search Space
% Exploration in Progol-like ILP Systems.", 16th International Conference on
% ILP (ILP 2006).
flatten(Depth,MaxDepth,Last,Last1):-
  retractall('$aleph_local'(flatten_num,_)),
  asserta('$aleph_local'(flatten_num,Last)),
  '$aleph_sat_atom'(_,_), !,
  (aleph5_setting(permute_bottom,Permute) -> true; Permute = false),
  flatten_atoms(Permute,Depth,MaxDepth,Last1).
flatten(_,_,_,Last):-
  retract('$aleph_local'(flatten_num,Last)), !.

flatten_atoms(true,Depth,MaxDepth,Last1):-
  findall(L-M,retract('$aleph_sat_atom'(L,M)),LitModes),
  aleph_rpermute(LitModes,PLitModes),
  member(Lit1-Mode,PLitModes),
  retract('$aleph_local'(flatten_num,LastSoFar)),
  (Lit1 = not(Lit) -> Negated = true; Lit = Lit1, Negated = false),
  flatten_atom(Depth,MaxDepth,Lit,Negated,Mode,LastSoFar,Last1),
  asserta('$aleph_local'(flatten_num,Last1)),
  fail.
flatten_atoms(false,Depth,MaxDepth,Last1):-
  repeat,
  retract('$aleph_sat_atom'(Lit1,Mode)),
  retract('$aleph_local'(flatten_num,LastSoFar)),
  (Lit1 = not(Lit) -> Negated = true; Lit = Lit1, Negated = false),
  flatten_atom(Depth,MaxDepth,Lit,Negated,Mode,LastSoFar,Last1),
  asserta('$aleph_local'(flatten_num,Last1)),
  ('$aleph_sat_atom'(_,_) ->
      fail;
      retract('$aleph_local'(flatten_num,Last1))), !.
flatten_atoms(_,_,_,Last):-
  retract('$aleph_local'(flatten_num,Last)), !.


% flatten_atom(+Depth,+Depth1,+Lit,+Negated,+Mode,+Last,-Last1)
%  update lits database by adding ``flattened atoms''. This involves:
%  replacing ground terms at +/- positions in Lit with variables
%  and wrapping # positions in Lit within a special term stucture
%  Mode contains actual mode and term-place numbers and types for +/-/#
%  Last is the last literal number in the lits database at present
%  Last1 is the last literal number after the update
flatten_atom(Depth,Depth1,Lit,Negated,Mode,Last,Last1):-
  arg(3,Mode,O), arg(4,Mode,C),
  integrate_args(Depth,Lit,O),
  integrate_args(Depth,Lit,C),
  (Depth = Depth1 -> CheckOArgs = true; CheckOArgs = false),
  flatten_lits(Lit,CheckOArgs,Depth,Negated,Mode,Last,Last1).

% variabilise literals by replacing terms with variables
% if var splitting is on then new equalities are introduced into bottom clause
% if at final i-layer, then literals with o/p args that do not contain at least
%   one output var from head are discarded
flatten_lits(Lit,CheckOArgs,Depth,Negated,Mode,Last,_):-
  functor(Lit,Name,Arity),
  asserta('$aleph_local'(flatten_lits,Last)),
  Depth1 is Depth - 1,
  functor(OldFAtom,Name,Arity),
  flatten_lit(Lit,Mode,OldFAtom,_,_),
  functor(FAtom,Name,Arity),
  apply_equivs(Depth1,Arity,OldFAtom,FAtom),
  retract('$aleph_local'(flatten_lits,OldLast)),
  (CheckOArgs = true ->
    arg(3,Mode,Out),
    get_vars(FAtom,Out,OVars),
    (in_path(OVars) ->
      add_new_lit(Depth,FAtom,Mode,OldLast,Negated,NewLast);
      NewLast = OldLast) ;
    add_new_lit(Depth,FAtom,Mode,OldLast,Negated,NewLast)),
  asserta('$aleph_local'(flatten_lits,NewLast)),
  fail.
flatten_lits(_,_,_,_,_,_,Last1):-
  retract('$aleph_local'(flatten_lits,Last1)).


% flatten_lit(+Lit,+Mode,+FAtom,-IVars,-OVars)
% variabilise Lit as FAtom
%  Mode contains actual mode and
%  In, Out, Const positions as term-place numbers with types
%   replace ground terms with integers denoting variables
%  or special terms denoting constants
%   variable numbers arising from variable splits are disallowed
%  returns Input and Output variable numbers
flatten_lit(Lit,mode(Mode,In,Out,Const),FAtom,IVars,OVars):-
  functor(Mode,_,Arity),
  once(copy_modeterms(Mode,FAtom,Arity)),
  flatten_vars(In,Lit,FAtom,IVars),
  flatten_vars(Out,Lit,FAtom,OVars),
  flatten_consts(Const,Lit,FAtom).

% flatten_vars(+TPList,+Lit,+FAtom,-Vars):-
% FAtom is Lit with terms-places in TPList replaced by variables
flatten_vars([],_,_,[]).
flatten_vars([Pos/Type|Rest],Lit,FAtom,[Var|Vars]):-
  tparg(Pos,Lit,Term),
  '$aleph_sat_terms'(TNo,_,Term,Type),
  '$aleph_sat_vars'(Var,TNo,_,_),
  \+('$aleph_sat_varscopy'(Var,_,_)),
  tparg(Pos,FAtom,Var),
  flatten_vars(Rest,Lit,FAtom,Vars).

% replace a list of terms at places marked by # in the modes
% with a special term structure denoting a constant
flatten_consts([],_,_).
flatten_consts([Pos/_|Rest],Lit,FAtom):-
  tparg(Pos,Lit,Term),
  tparg(Pos,FAtom,aleph_const(Term)),
  flatten_consts(Rest,Lit,FAtom).

% in_path(+ListOfOutputVars)
% check to avoid generating useless literals in the last i layer
in_path(OVars):-
  '$aleph_sat'(hovars,Vars), !,
  (Vars=[];OVars=[];intersects(Vars,OVars)).
in_path(_).

% update_equivs(+VariableEquivalences,+IDepth)
% update variable equivalences created at a particular i-depth
% is non-empty only if variable splitting is allowed
update_equivs([],_):- !.
update_equivs(Equivs,Depth):-
  retract('$aleph_sat_varsequiv'(Depth,Eq1)), !,
  update_equiv_lists(Equivs,Eq1,Eq2),
  asserta('$aleph_sat_varsequiv'(Depth,Eq2)).
update_equivs(Equivs,Depth):-
  Depth1 is Depth - 1,
  get_equivs(Depth1,Eq1),
  update_equiv_lists(Equivs,Eq1,Eq2),
  asserta('$aleph_sat_varsequiv'(Depth,Eq2)).

update_equiv_lists([],E,E):- !.
update_equiv_lists([Var/E1|Equivs],ESoFar,E):-
  aleph_delete(Var/E2,ESoFar,ELeft), !,
  update_list(E1,E2,E3),
  update_equiv_lists(Equivs,[Var/E3|ELeft],E).
update_equiv_lists([Equiv|Equivs],ESoFar,E):-
  update_equiv_lists(Equivs,[Equiv|ESoFar],E).

% get variable equivalences at a particular depth
% recursively descend to greatest depth below this for which equivs exist
% also returns the database reference of entry
get_equivs(Depth,[]):-
  Depth < 0, !.
get_equivs(Depth,Equivs):-
  '$aleph_sat_varsequiv'(Depth,Equivs), !.
get_equivs(Depth,E):-
  Depth1 is Depth - 1,
  get_equivs(Depth1,E).

% apply equivalences inherited from Depth to a flattened literal
% if no variable splitting, then succeeds only once
apply_equivs(Depth,Arity,Old,New):-
  get_equivs(Depth,Equivs),
  rename(Arity,Equivs,[],Old,New).

% rename args using list of Var/Equivalences
rename(_,[],_,L,L):- !.
rename(0,_,_,_,_):- !.
rename(Pos,Equivs,Subst0,Old,New):-
        arg(Pos,Old,OldVar),
        member(OldVar/Equiv,Equivs), !,
        member(NewVar,Equiv),
        arg(Pos,New,NewVar),
        Pos1 is Pos - 1,
        rename(Pos1,Equivs,[OldVar/NewVar|Subst0],Old,New).
rename(Pos,Equivs,Subst0,Old,New):-
        arg(Pos,Old,OldVar),
        (member(OldVar/NewVar,Subst0) ->
                arg(Pos,New,NewVar);
                arg(Pos,New,OldVar)),
        Pos1 is Pos - 1,
        rename(Pos1,Equivs,Subst0,Old,New).


% add a new literal to lits database
% performs variable splitting if splitvars is set to true
add_new_lit(Depth,FAtom,Mode,OldLast,Negated,NewLast):-
  arg(1,Mode,M),
  functor(FAtom,Name,Arity),
  functor(SplitAtom,Name,Arity),
  once(copy_modeterms(M,SplitAtom,Arity)),
  arg(2,Mode,In), arg(3,Mode,Out), arg(4,Mode,Const),
        split_vars(Depth,FAtom,In,Out,Const,SplitAtom,IVars,OVars,Equivs),
        update_equivs(Equivs,Depth),
        add_lit(OldLast,Negated,SplitAtom,In,Out,IVars,OVars,LitNum),
        insert_eqs(Equivs,Depth,LitNum,NewLast), !.

% modify the literal database: check if performing lazy evaluation
% of bottom clause, and update input and output terms in literal
add_lit(Last,Negated,FAtom,I,O,_,_,Last):-
  aleph5_setting(construct_bottom,CBot),
  (CBot = false ; CBot = reduction),
  (Negated = true -> Lit = not(FAtom); Lit = FAtom),
  '$aleph_sat_litinfo'(_,0,Lit,I,O,_), !.
add_lit(Last,Negated,FAtom,In,Out,IVars,OVars,LitNum):-
  LitNum is Last + 1,
  update_iterms(LitNum,IVars),
  update_oterms(LitNum,OVars,[],Dependents),
  add_litinfo(LitNum,Negated,FAtom,In,Out,Dependents),
  assertz('$aleph_sat_ivars'(LitNum,IVars)),
  assertz('$aleph_sat_ovars'(LitNum,OVars)), !.


% update lits database after checking that the atom does not exist
% used during updates of lit database by lazy evaluation
update_lit(LitNum,true,FAtom,I,O,D):-
  '$aleph_sat_litinfo'(LitNum,0,not(FAtom),I,O,D), !.
update_lit(LitNum,false,FAtom,I,O,D):-
  '$aleph_sat_litinfo'(LitNum,0,FAtom,I,O,D), !.
update_lit(LitNum,Negated,FAtom,I,O,D):-
  gen_nlitnum(LitNum),
  add_litinfo(LitNum,Negated,FAtom,I,O,D),
  get_vars(FAtom,I,IVars),
  get_vars(FAtom,O,OVars),
  assertz('$aleph_sat_ivars'(LitNum,K,IVars)),
  assertz('$aleph_sat_ovars'(LitNum,K,OVars)), !.

% add a literal to lits database without checking
add_litinfo(LitNum,true,FAtom,I,O,D):-
  !,
  assertz('$aleph_sat_litinfo'(LitNum,0,not(FAtom),I,O,D)).
add_litinfo(LitNum,_,FAtom,I,O,D):-
  assertz('$aleph_sat_litinfo'(LitNum,0,FAtom,I,O,D)).

% update database with input terms of literal
update_iterms(_,[]).
update_iterms(LitNum,[VarNum|Vars]):-
  retract('$aleph_sat_vars'(VarNum,TNo,I,O)),
  update(I,LitNum,NewI),
  asserta('$aleph_sat_vars'(VarNum,TNo,NewI,O)),
  update_dependents(LitNum,O),
  update_iterms(LitNum,Vars).

% update database with output terms of literal
% return list of dependent literals
update_oterms(_,[],Dependents,Dependents).
update_oterms(LitNum,[VarNum|Vars],DSoFar,Dependents):-
  retract('$aleph_sat_vars'(VarNum,TNo,I,O)),
  update(O,LitNum,NewO),
  asserta('$aleph_sat_vars'(VarNum,TNo,I,NewO)),
  update_list(I,DSoFar,D1),
  update_oterms(LitNum,Vars,D1,Dependents).

% update Dependent list of literals with LitNum
update_dependents(_,[]).
update_dependents(LitNum,[Lit|Lits]):-
  retract('$aleph_sat_litinfo'(Lit,Depth,Atom,ITerms,OTerms,Dependents)),
  update(Dependents,LitNum,NewD),
  asserta('$aleph_sat_litinfo'(Lit,Depth,Atom,ITerms,OTerms,NewD)),
  update_dependents(LitNum,Lits).

% update dependents of head with literals that are simply generators
%   that is, literals that require no input args
update_generators:-
  findall(L,('$aleph_sat_litinfo'(L,_,_,[],_,_),L>1),GList),
  GList \= [], !,
  retract('$aleph_sat_litinfo'(1,Depth,Lit,I,O,D)),
  append(GList, D, D1),
  asserta('$aleph_sat_litinfo'(1,Depth,Lit,I,O,D1)).
update_generators.

% mark literals
mark_lits(Lits):-
  member(Lit,Lits),
  asserta('$aleph_local'(marked,Lit/0)),
  fail.
mark_lits(_).

% recursively mark literals with minimum depth to bind output vars in head
mark_lits([],_,_).
mark_lits(Lits,OldVars,Depth):-
  mark_lits(Lits,Depth,true,[],Predecessors,OldVars,NewVars),
  aleph_delete_list(Lits,Predecessors,P1),
  Depth1 is Depth + 1,
  mark_lits(P1,NewVars,Depth1).

mark_lits([],_,_,P,P,V,V).
mark_lits([Lit|Lits],Depth,GetPreds,PSoFar,P,VSoFar,V):-
  retract('$aleph_local'(marked,Lit/Depth0)), !,
  (Depth < Depth0 ->
    mark_lit(Lit,Depth,GetPreds,VSoFar,P1,V2),
    update_list(P1,PSoFar,P2),
    mark_lits(Lits,Depth,GetPreds,P2,P,V2,V);
    asserta('$aleph_local'(marked,Lit/Depth0)),
    mark_lits(Lits,Depth,GetPreds,PSoFar,P,VSoFar,V)).
mark_lits([Lit|Lits],Depth,GetPreds,PSoFar,P,VSoFar,V):-
  mark_lit(Lit,Depth,GetPreds,VSoFar,P1,V2), !,
  update_list(P1,PSoFar,P2),
  mark_lits(Lits,Depth,GetPreds,P2,P,V2,V).
mark_lits([_|Lits],Depth,GetPreds,PSoFar,P,VSoFar,V):-
  mark_lits(Lits,Depth,GetPreds,PSoFar,P,VSoFar,V).

mark_lit(Lit,Depth,GetPreds,VSoFar,P1,V1):-
  retract('$aleph_sat_litinfo'(Lit,_,Atom,I,O,D)),
  asserta('$aleph_local'(marked,Lit/Depth)),
  asserta('$aleph_sat_litinfo'(Lit,Depth,Atom,I,O,D)),
  (GetPreds = false ->
    P1 = [],
    V1 = VSoFar;
    get_vars(Atom,O,OVars),
    update_list(OVars,VSoFar,V1),
    get_predicates(D,V1,D1),
    mark_lits(D1,Depth,false,[],_,VSoFar,_),
    get_vars(Atom,I,IVars),
    get_predecessors(IVars,[],P1)).

% mark lits that produce outputs that are not used by any other literal
mark_floating_lits(Lit,Last):-
  Lit > Last, !.
mark_floating_lits(Lit,Last):-
  '$aleph_sat_litinfo'(Lit,_,_,_,O,D),
  O \= [],
  (D = []; D = [Lit]), !,
  asserta('$aleph_local'(marked,Lit/0)),
  Lit1 is Lit + 1,
  mark_floating_lits(Lit1,Last).
mark_floating_lits(Lit,Last):-
  Lit1 is Lit + 1,
  mark_floating_lits(Lit1,Last).

% mark lits in bottom clause that are specified redundant by user
% requires definition of redundant/2 that have distinguished first arg ``bottom''
mark_redundant_lits(Lit,Last):-
  Lit > Last, !.
mark_redundant_lits(Lit,Last):-
  get_pclause([Lit],[],Atom,_,_,_),
  redundant(bottom,Atom), !,
  asserta('$aleph_local'(marked,Lit/0)),
  Lit1 is Lit + 1,
  mark_redundant_lits(Lit1,Last).
mark_redundant_lits(Lit,Last):-
  Lit1 is Lit + 1,
  mark_redundant_lits(Lit1,Last).

% get literals that are linked and do not link to any others (ie predicates)
get_predicates([],_,[]).
get_predicates([Lit|Lits],Vars,[Lit|T]):-
  '$aleph_sat_litinfo'(Lit,_,Atom,I,_,[]),
  get_vars(Atom,I,IVars),
  subset(IVars,Vars), !,
  get_predicates(Lits,Vars,T).
get_predicates([_|Lits],Vars,T):-
  get_predicates(Lits,Vars,T).

% get all predecessors in the bottom clause of a set of literals
get_predecessors([],[]).
get_predecessors([Lit|Lits],P):-
  (Lit = 1 -> Pred = [];
    get_ivars1(false,Lit,IVars),
    get_predecessors(IVars,[],Pred)),
  get_predecessors(Pred,PPred),
  update_list(Pred,PPred,P1),
  get_predecessors(Lits,P2),
  update_list(P2,P1,P).

% get list of literals in the bottom clause that produce a set of vars
get_predecessors([],P,P).
get_predecessors([Var|Vars],PSoFar,P):-
  '$aleph_sat_vars'(Var,_,_,O),
  update_list(O,PSoFar,P1),
  get_predecessors(Vars,P1,P).

% removal of literals in bottom clause by negative-based reduction.
% A greedy strategy is employed, as implemented within the ILP system
% Golem (see Muggleton and Feng, "Efficient induction
% of logic programs", Inductive Logic Programming, S. Muggleton (ed.),
% AFP Press). In this, given a clause H:- B1, B2,...Bn, let Bi be the
% first literal s.t. H:-B1,...,Bi covers no more than the allowable number
% of negatives. The clause H:- Bi,B1,...,Bi-1 is then reduced. The
% process continues until there is no change in the length of a clause
% within an iteration. The algorithm is O(n^2).
rm_nreduce(Last,N):-
  aleph5_setting(nreduce_bottom,true), !,
  get_litnums(1,Last,BottomLits),
        '$aleph_global'(atoms,atoms(neg,Neg)),
  aleph5_setting(depth,Depth),
  aleph5_setting(prooftime,Time),
  aleph5_setting(proof_strategy,Proof),
  aleph5_setting(noise,Noise),
  neg_reduce(BottomLits,Neg,Last,Depth/Time/Proof,Noise),
  get_marked(1,Last,Lits),
  length(Lits,N),
  p1_message('negative-based removal'), p_message(N/Last).
rm_nreduce(_,0).

neg_reduce([Head|Body],Neg,Last,DepthTime,Noise):-
  get_pclause([Head],[],Clause,TV,_,_),
  neg_reduce(Body,Clause,TV,2,Neg,DepthTime,Noise,NewLast),
  NewLast \= Last, !,
  NewLast1 is NewLast - 1,

  length(Prefix, NewLast1),
  append(Prefix, [LastLit|Rest], [Head|Body]),

  mark_lits(Rest),
  insert_lastlit(LastLit,Prefix,Lits1),
  neg_reduce(Lits1,Neg,NewLast,DepthTime,Noise).
neg_reduce(_,_,_,_,_).

neg_reduce([],_,_,N,_,_,_,N).
neg_reduce([L1|Lits],C,TV,N,Neg,ProofFlags,Noise,LastLit):-
  get_pclause([L1],TV,Lit1,TV1,_,_),
  extend_clause(C,Lit1,Clause),
        prove(ProofFlags,neg,Clause,Neg,NegCover,Count),
  Count > Noise, !,
  N1 is N + 1,
  neg_reduce(Lits,Clause,TV1,N1,NegCover,ProofFlags,Noise,LastLit).
neg_reduce(_,_,_,N,_,_,_,N).

insert_lastlit(LastLit,Lits,Lits1):-
  get_predecessors([LastLit],Prefix),
  aleph_delete_list(Prefix,Lits,Suffix),
  append(Prefix, [LastLit | Suffix], Lits1).

find_last_ancestor([],_,Last,_,Last):- !.
find_last_ancestor([Lit|Lits],L,_,LitNum,Last):-
  '$aleph_sat_litinfo'(Lit,_,_,_,_,D),
  memberchk(L,D), !,
  NextLit is LitNum + 1,
  find_last_ancestor(Lits,L,LitNum,NextLit,Last).
find_last_ancestor([_|Lits],L,Last0,LitNum,Last):-
  NextLit is LitNum + 1,
  find_last_ancestor(Lits,L,Last0,NextLit,Last).

% removal of literals that are repeated because of mode differences
rm_moderepeats(_,_):-
  '$aleph_sat_litinfo'(Lit1,_,Pred1,_,_,_),
  '$aleph_sat_litinfo'(Lit2,_,Pred1,_,_,_),
  Lit1 >= 1, Lit2 > Lit1,
  retract('$aleph_sat_litinfo'(Lit2,_,Pred1,_,_,_)),
  asserta('$aleph_local'(marked,Lit2/0)),
  fail.
rm_moderepeats(Last,N):-
  '$aleph_local'(marked,_), !,
  get_marked(1,Last,Lits),
  length(Lits,N),
  p1_message('repeated literals'), p_message(N/Last),
  remove_lits(Lits).
rm_moderepeats(_,0).

% removal of symmetric literals
rm_symmetric(_,_):-
  '$aleph_global'(symmetric,_),
  '$aleph_sat_litinfo'(Lit1,_,Pred1,[I1|T1],_,_),
  is_symmetric(Pred1,Name,Arity),
  get_vars(Pred1,[I1|T1],S1),
  '$aleph_sat_litinfo'(Lit2,_,Pred2,[I2|T2],_,_),
  Lit1 \= Lit2,
  is_symmetric(Pred2,Name,Arity),
  Pred1 =.. [_|Args1],
  Pred2 =.. [_|Args2],
  symmetric_match(Args1,Args2),
  get_vars(Pred2,[I2|T2],S2),
  equal_set(S1,S2),
  asserta('$aleph_local'(marked,Lit2/0)),
  retract('$aleph_sat_litinfo'(Lit2,_,Pred2,[I2|T2],_,_)),
  fail.
rm_symmetric(Last,N):-
  '$aleph_local'(marked,_), !,
  get_marked(1,Last,Lits),
  length(Lits,N),
  p1_message('symmetric literals'), p_message(N/Last),
  remove_lits(Lits).
rm_symmetric(_,0).

is_symmetric(not(Pred),not(Name),Arity):-
  !,
  functor(Pred,Name,Arity),
  '$aleph_global'(symmetric,symmetric(Name/Arity)).
is_symmetric(Pred,Name,Arity):-
  functor(Pred,Name,Arity),
  '$aleph_global'(symmetric,symmetric(Name/Arity)).

symmetric_match([],[]).
symmetric_match([aleph_const(Term)|Terms1],[aleph_const(Term)|Terms2]):-
  !,
  symmetric_match(Terms1,Terms2).
symmetric_match([Term1|Terms1],[Term2|Terms2]):-
  integer(Term1), integer(Term2),
  symmetric_match(Terms1,Terms2).

% removal of literals that are repeated because of commutativity
rm_commutative(_,_):-
  '$aleph_global'(commutative,commutative(Name/Arity)),
  p1_message('checking commutative literals'),
  p_message(Name/Arity),
  functor(Pred,Name,Arity), functor(Pred1,Name,Arity),
  '$aleph_sat_litinfo'(Lit1,_,Pred,[I1|T1],O1,_),
        % check for marked literals
  % (SWI-Prolog specific: suggested by Vasili Vrubleuski)
        \+('$aleph_local'(marked,Lit1/0)),
  get_vars(Pred,[I1|T1],S1),
  '$aleph_sat_litinfo'(Lit2,_,Pred1,[I2|T2],O2,_),
  Lit1 \= Lit2 ,
  O1 = O2,
  get_vars(Pred1,[I2|T2],S2),
  equal_set(S1,S2),
  asserta('$aleph_local'(marked,Lit2/0)),
  retract('$aleph_sat_litinfo'(Lit2,_,Pred1,[I2|T2],_,_)),
  fail.
rm_commutative(Last,N):-
  '$aleph_local'(marked,_), !,
  get_marked(1,Last,Lits),
  length(Lits,N),
  p1_message('commutative literals'), p_message(N/Last),
  remove_lits(Lits).
rm_commutative(_,0).

% recursive marking of literals that do not contribute to establishing
% variable chains to output vars in the head
% or produce outputs that are not used by any literal
% controlled by setting flag check_useless
rm_uselesslits(_,0):-
  aleph5_setting(check_useless,false), !.
rm_uselesslits(Last,N):-
  '$aleph_sat'(hovars,OVars),
  OVars \= [], !,
  get_predecessors(OVars,[],P),
  '$aleph_sat'(hivars,IVars),
  mark_lits(P,IVars,0),
  get_unmarked(1,Last,Lits),
  length(Lits,N),
  p1_message('useless literals'), p_message(N/Last),
  remove_lits(Lits).
rm_uselesslits(_,0).

% call user-defined predicate redundant/2 to remove redundant
% literals from bottom clause. Redundancy checking only done on request
rm_redundant(_,0):-
  aleph5_setting(check_redundant,false), !.
rm_redundant(Last,N):-
  mark_redundant_lits(1,Last),
  get_marked(1,Last,Lits),
  length(Lits,N),
  p1_message('redundant literals'), p_message(N/Last),
  remove_lits(Lits).

% get a list of unmarked literals
get_unmarked(Lit,Last,[]):-
  Lit > Last, !.
get_unmarked(Lit,Last,Lits):-
  retract('$aleph_local'(marked,Lit/_)), !,
  Next is Lit + 1,
  get_unmarked(Next,Last,Lits).
get_unmarked(Lit,Last,[Lit|Lits]):-
  retract('$aleph_sat_litinfo'(Lit,_,_,_,_,_)), !,
  Next is Lit + 1,
  get_unmarked(Next,Last,Lits).
get_unmarked(Lit,Last,Lits):-
  Next is Lit + 1,
  get_unmarked(Next,Last,Lits).

% get a list of marked literals
get_marked(Lit,Last,[]):-
  Lit > Last, !.
get_marked(Lit,Last,[Lit|Lits]):-
  retract('$aleph_local'(marked,Lit/_)), !,
  (retract('$aleph_sat_litinfo'(Lit,_,_,_,_,_)) ->
    true;
    true),
  Next is Lit + 1,
  get_marked(Next,Last,Lits).
get_marked(Lit,Last,Lits):-
  Next is Lit + 1,
  get_marked(Next,Last,Lits).

% update descendent lists of literals by removing useless literals
remove_lits(L):-
  retract('$aleph_sat_litinfo'(Lit,Depth,A,I,O,D)),
  aleph_delete_list(L,D,D1),
  asserta('$aleph_sat_litinfo'(Lit,Depth,A,I,O,D1)),
  fail.
remove_lits(_).

% gen_layer(+Name/+Arity, +Depth)
% Generate a new literal at depth Depth.
% Forced backtracking will give all lits.
%
% @arg Name The name of a predicate.
% @arg Arity A numeric arity indicator.
% @arg Depth A numeric depth indicator.

gen_layer(Name/Arity, Depth):-
  (
    Name/Arity = (not)/1
  ->
    '$aleph_global'(modeb, modeb(NSucc, not(Mode))),
    functor(Mode, Name1, Arity1),
    functor(Lit1, Name1, Arity1),
    once(copy_modeterms(Mode, Lit1, Arity1)),
    Lit = not(Lit1)
  ;
    functor(Mode, Name, Arity),
    functor(Lit, Name, Arity),
    '$aleph_global'(modeb,modeb(NSucc,Mode)),
    once(copy_modeterms(Mode, Lit, Arity))
  ),
  split_args(Mode,Mode,Input,Output,Constants),
  (
    Input = []
  ->
    Call1 = true,
    Call2 = true
  ;
    aleph_delete(Arg/Type,Input,OtherInputs),
    Depth1 is Depth - 1,
    construct_incall(Lit,Depth1,[Arg/Type],Call1),
    construct_call(Lit,Depth,OtherInputs,Call2)
  ),
  Call1,
  Call2,
  aleph_background_predicate(Lit),
  get_successes(Lit, NSucc, mode(Mode, Input, Output, Constants)),
  fail.
gen_layer(_PredicateArity, _Depth).

aleph_background_predicate(Lit):-
  predicate_property(Lit, P),
  (
    P = interpreted
  ;
    P = built_in
  ),
  !.

get_successes(Literal,1,M):-
  depth_bound_call(Literal),
  update_atoms(Literal,M), !.
get_successes(Literal,*,M):-
  depth_bound_call(Literal),
  update_atoms(Literal,M).
get_successes(Literal,N,M):-
  integer(N),
  N > 1,
  reset_succ,
  get_nsuccesses(Literal,N,M).

% get at most N matches for a literal
get_nsuccesses(Literal,N,M):-
  depth_bound_call(Literal),
  retract('$aleph_local'(last_success,Succ0)),
  Succ0 < N,
  Succ1 is Succ0 + 1,
  update_atoms(Literal,M),
  asserta('$aleph_local'(last_success,Succ1)),
  (Succ1 >= N -> !; true).

update_atoms(Atom,M):-
  '$aleph_sat_atom'(Atom,M), !.
update_atoms(Atom,M):-
  assertz('$aleph_sat_atom'(Atom,M)).

% call with input term that is an ouput of a previous literal
construct_incall(_,_,[],true):- !.
construct_incall(not(Lit),Depth,Args,Call):-
  !,
  construct_incall(Lit,Depth,Args,Call).
construct_incall(Lit,Depth,[Pos/Type],Call):-
  !,
  Call = legal_term(exact,Depth,Type,Term),
  tparg(Pos,Lit,Term).
construct_incall(Lit,Depth,[Pos/Type|Args],(Call,Calls)):-
  tparg(Pos,Lit,Term),
  Call = legal_term(exact,Depth,Type,Term),
  (var(Depth)-> construct_incall(Lit,_,Args,Calls);
    construct_incall(Lit,Depth,Args,Calls)).

construct_call(_,_,[],true):- !.
construct_call(not(Lit),Depth,Args,Call):-
  !,
  construct_call(Lit,Depth,Args,Call).
construct_call(Lit,Depth,[Pos/Type],Call):-
  !,
  Call = legal_term(upper,Depth,Type,Term),
  tparg(Pos,Lit,Term).
construct_call(Lit,Depth,[Pos/Type|Args],(Call,Calls)):-
  tparg(Pos,Lit,Term),
  Call = legal_term(upper,Depth,Type,Term),
  construct_call(Lit,Depth,Args,Calls).

% generator of legal terms seen so far
legal_term(exact,Depth,Type,Term):-
  '$aleph_sat_terms'(TNo,Depth,Term,Type),
  once('$aleph_sat_vars'(_,TNo,_,[_|_])).
legal_term(upper,Depth,Type,Term):-
  '$aleph_sat_terms'(TNo,Depth1,Term,Type),
  Depth1 \= unknown,
  Depth1 < Depth,
  once('$aleph_sat_vars'(_,TNo,_,[_|_])).



% VARIABLE SPLITTING

split_vars(Depth,FAtom,I,O,C,SplitAtom,IVars,OVars,Equivs):-
  aleph5_setting(splitvars,true), !,
  get_args(FAtom,I,[],IVarList),
  get_args(FAtom,O,[],OVarList),
  get_var_equivs(Depth,IVarList,OVarList,IVars,OVars0,Equivs0),
  (Equivs0 = [] ->
    OVars = OVars0, SplitAtom = FAtom, Equivs = Equivs0;
    functor(FAtom,Name,Arity),
    functor(SplitAtom,Name,Arity),
    copy_args(FAtom,SplitAtom,I),
    copy_args(FAtom,SplitAtom,C),
    rename_ovars(O,Depth,FAtom,SplitAtom,Equivs0,Equivs),
    get_argterms(SplitAtom,O,[],OVars)).
split_vars(_,FAtom,I,O,_,FAtom,IVars,OVars,[]):-
  get_vars(FAtom,I,IVars),
  get_vars(FAtom,O,OVars).

% get equivalent classes of variables from co-references
get_var_equivs(Depth,IVarList,OVarList,IVars,OVars,Equivs):-
  sort(IVarList,IVars),
  sort(OVarList,OVars),
  (Depth = 0 ->
    intersect1(IVars,OVarList,IOCoRefs,_),
    get_repeats(IVarList,IOCoRefs,ICoRefs);
    intersect1(IVars,OVarList,ICoRefs,_)),
  get_repeats(OVarList,ICoRefs,CoRefs),
  add_equivalences(CoRefs,Depth,Equivs).

add_equivalences([],_,[]).
add_equivalences([Var|Vars],Depth,[Var/E|Rest]):-
  % (Depth = 0 -> E = []; E = [Var]),
  E = [Var],
  add_equivalences(Vars,Depth,Rest).

get_repeats([],L,L).
get_repeats([Var|Vars],Ref1,L):-
  memberchk(Var,Vars), !,
  update(Ref1,Var,Ref2),
  get_repeats(Vars,Ref2,L).
get_repeats([_|Vars],Ref,L):-
  get_repeats(Vars,Ref,L).

% rename all output vars that are co-references
% updates vars database and return equivalent class of variables
rename_ovars([],_,_,_,L,L).
rename_ovars([ArgNo|Args],Depth,Old,New,CoRefs,Equivalences):-
  (ArgNo = Pos/_ -> true; Pos = ArgNo),
  tparg(Pos,Old,OldVar),
  aleph_delete(OldVar/Equiv,CoRefs,Rest), !,
  copy_var(OldVar,NewVar,Depth),
  tparg(Pos,New,NewVar),
  rename_ovars(Args,Depth,Old,New,[OldVar/[NewVar|Equiv]|Rest],Equivalences).
rename_ovars([ArgNo|Args],Depth,Old,New,CoRefs,Equivalences):-
  (ArgNo = Pos/_ -> true; Pos = ArgNo),
  tparg(Pos,Old,OldVar),
  tparg(Pos,New,OldVar),
  rename_ovars(Args,Depth,Old,New,CoRefs,Equivalences).

% create new  equalities to  allow co-references to re-appear in search
insert_eqs([],_,L,L).
insert_eqs([OldVar/Equivs|Rest],Depth,Last,NewLast):-
  '$aleph_sat_vars'(OldVar,TNo,_,_),
  '$aleph_sat_terms'(TNo,_,_,Type),
  add_eqs(Equivs,Depth,Type,Last,Last1),
  insert_eqs(Rest,Depth,Last1,NewLast).

add_eqs([],_,_,L,L).
add_eqs([V1|Rest],Depth,Type,Last,NewLast):-
  add_eqs(Rest,Depth,V1,Type,Last,Last1),
  add_eqs(Rest,Depth,Type,Last1,NewLast).

add_eqs([],_,_,_,L,L).
add_eqs([Var2|Rest],Depth,Var1,Type,Last,NewLast):-
  (Depth = 0 ->
    add_lit(Last,false,(Var1=Var2),[1/Type],[2/Type],[Var1],[Var2],Last1);
    add_lit(Last,false,(Var1=Var2),[1/Type,2/Type],[],[Var1,Var2],[],Last1)),
  add_eqs(Rest,Depth,Var1,Type,Last1,NewLast).



% utilities for updating mappings between terms and variables

% integrate terms specified by a list of arguments
% integrating a term means:
%  updating 2 databases: terms and vars
%  terms contains the term along with a term-id
%  vars contains a var-id <-> term-id mapping
% var and term-ids are integers
integrate_args(_,_,[]).
integrate_args(Depth,Literal,[Pos/Type|T]):-
        tparg(Pos,Literal,Term),
        integrate_term(Depth,Term/Type),
  (retract('$aleph_sat_terms'(TNo,Depth,Term,unknown)) ->
    asserta('$aleph_sat_terms'(TNo,Depth,Term,Type));
    true),
        integrate_args(Depth,Literal,T).

% integrate a term
integrate_term(Depth,Term/Type):-
        '$aleph_sat_terms'(TNo,Depth,Term,Type),
        '$aleph_sat_vars'(_,TNo,_,[_|_]), !.
integrate_term(Depth,Term/Type):-
        '$aleph_sat_terms'(TNo,Depth1,Term,Type),
        (Type = unknown ; '$aleph_sat_vars'(_,TNo,_,[])), !,
  (Depth1 = unknown ->
          retract('$aleph_sat_terms'(TNo,Depth1,Term,Type)),
    asserta('$aleph_sat_terms'(TNo,Depth,Term,Type));
    true).
integrate_term(_,Term/Type):-
        '$aleph_sat_terms'(_,_,Term,Type),
        Type \= unknown,
        !.
integrate_term(Depth,Term/Type):-
  retract('$aleph_sat'(lastterm,Num)),
  retract('$aleph_sat'(lastvar,Var0)),
  TNo is Num + 1,
  Var is Var0 + 1,
  asserta('$aleph_sat'(lastterm,TNo)),
  asserta('$aleph_sat'(lastvar,Var)),
  asserta('$aleph_sat_vars'(Var,TNo,[],[])),
  asserta('$aleph_sat_terms'(TNo,Depth,Term,Type)).

% split_args(+Lit,?Mode,-Input,-Output,-Constants)
%       return term-places and types of +,-, and # args in Lit
%       by finding a matching mode declaration if Mode is given
%       otherwise first mode that matches is used
split_args(Lit,Mode,Input,Output,Constants):-
        functor(Lit,Psym,Arity),
  find_mode(mode,Psym/Arity,Mode),
        functor(Template,Psym,Arity),
  copy_modeterms(Mode,Template,Arity),
  Template = Lit,
  tp(Mode,TPList),
  split_tp(TPList,Input,Output,Constants).

% split_tp(+TPList,-Input,-Output,-Constants)
%  split term-place/type list into +,-,#
split_tp([],[],[],[]).
split_tp([(+Type)/Place|TP],[Place/Type|Input],Output,Constants):-
  !,
  split_tp(TP,Input,Output,Constants).
split_tp([(-Type)/Place|TP],Input,[Place/Type|Output],Constants):-
  !,
  split_tp(TP,Input,Output,Constants).
split_tp([(#Type)/Place|TP],Input,Output,[Place/Type|Constants]):-
  !,
  split_tp(TP,Input,Output,Constants).
split_tp([_|TP],Input,Output,Constants):-
  split_tp(TP,Input,Output,Constants).

% tp(+Literal,-TPList)
%  return terms and places in Literal
tp(Literal,TPList):-
  functor(Literal,_,Arity),
  tp_list(Literal,Arity,[],[],TPList).

tp_list(_,0,_,L,L):- !.
tp_list(Term,Pos,PlaceList,TpSoFar,TpList):-
  arg(Pos,Term,Arg),
  append(PlaceList, [Pos], Places),
  unwrap_term(Arg,Places,[Arg/Places|TpSoFar],L1),
  Pos1 is Pos - 1,
  tp_list(Term,Pos1,PlaceList,L1,TpList).

unwrap_term(Term,_,L,L):-
  var(Term), !.
unwrap_term(Term,Place,TpSoFar,TpList):-
  functor(Term,_,Arity),
  tp_list(Term,Arity,Place,TpSoFar,TpList).

get_determs(PSym/Arity,L):-
  findall(Pred,'$aleph_global'(determination,determination(PSym/Arity,Pred)),L).

get_modes(PSym/Arity,L):-
  functor(Lit,PSym,Arity),
  findall(Lit,'$aleph_global'(mode,mode(_,Lit)),L).



% SEARCH

% basic search engine for single clause search
search(S,Nodes):-
  arg(36,S,Time),
  Inf is 1e10,
  Time =\= Inf,
  SearchTime is integer(Time),
  SearchTime > 0, !,
  catch(time_bound_call(SearchTime,searchlimit,graphsearch(S,_)),
  searchlimit,
  p_message('Time limit reached')),
  '$aleph_search'(current,current(_,Nodes,_)).
search(S,Nodes):-
  graphsearch(S,Nodes).

% basic search engine for theory-based search
tsearch(S,Nodes):-
  arg(36,S,Time),
  Inf is 1e10,
  Time =\= Inf,
  SearchTime is integer(Time),
  SearchTime > 0, !,
  alarm(SearchTime,throw(searchlimit),Id),
  catch(theorysearch(S,Nodes),searchlimit,p_message('Time limit reached')),
  remove_alarm(Id).
tsearch(S,Nodes):-
  theorysearch(S,Nodes).

graphsearch(S,Nodes):-
  next_node(_),
  !,
  arg(3,S,RefineOp),
  arg(23,S,LazyPreds),
  repeat,
  next_node(NodeRef),
  once(retract('$aleph_search'(current,current(LastE,Last,BestSoFar)))),
  expand(RefineOp,S,NodeRef,Node,Path,MinLength,Succ,PosCover,NegCover,OVars,
  PrefixClause,PrefixTV,PrefixLength),
  (
    (
      LazyPreds = []
    ;
      RefineOp \= false
    )
  ->
    Succ1 = Succ;
    lazy_evaluate(Succ,LazyPreds,Path,PosCover,NegCover,Succ1)
  ),
  NextE is LastE + 1,
  get_gains(S,Last,BestSoFar,Path,PrefixClause,PrefixTV,PrefixLength, MinLength,Succ1,PosCover,NegCover,OVars,NextE,Last0,NextBest0),
  (
    RefineOp = false
  ->
    get_sibgains(S,Node,Last0,NextBest0,Path,PrefixClause, PrefixTV,PrefixLength,MinLength,PosCover,NegCover,OVars,NextE,Last1,NextBest)
  ;
    Last1 = Last0,
    NextBest = NextBest0
  ),
  asserta('$aleph_search'(current,current(NextE,Last1,NextBest))),
  NextL is Last + 1,
  asserta('$aleph_search_expansion'(NextE,Node,NextL,Last1)),
  (
    discontinue_search(S,NextBest,Last1)
  ->
    '$aleph_search'(current,current(_,Nodes,_))
  ;
    prune_open(S,BestSoFar,NextBest),
    get_nextbest(S,Next),
    Next = none,
    '$aleph_search'(current,current(_,Nodes,_))
  ),
  !.
graphsearch(_,Nodes):-
  '$aleph_search'(current,current(_,Nodes,_)).

theorysearch(S,Nodes):-
        next_node(_), !,
        '$aleph_global'(atoms,atoms(pos,Pos)),
        '$aleph_global'(atoms,atoms(neg,Neg)),
        interval_count(Pos,P),
        interval_count(Neg,N),
        repeat,
        next_node(NodeRef),
  '$aleph_search_node'(NodeRef,Theory,_,_,_,_,_,_),
        once(retract('$aleph_search'(current,current(_,Last,BestSoFar)))),
        get_theory_gain(S,Last,BestSoFar,Theory,Pos,Neg,P,N,NextBest,Last1),
        asserta('$aleph_search'(current,current(0,Last1,NextBest))),
        (discontinue_search(S,NextBest,Last1) ->
                '$aleph_search'(current,current(_,Nodes,_));
                prune_open(S,BestSoFar,NextBest),
                get_nextbest(S,Next),
                Next = none,
                '$aleph_search'(current,current(_,Nodes,_))),
   !.
theorysearch(_,Nodes):-
        '$aleph_search'(current,current(_,Nodes,_)).

next_node(NodeRef):-
  once('$aleph_search'(nextnode,NodeRef)), !.

get_search_settings(S):-
  functor(S,set,47),
  aleph5_setting(nodes,MaxNodes), arg(1,S,MaxNodes),
  aleph5_setting(explore,Explore), arg(2,S,Explore),
  aleph5_setting(refineop,RefineOp), arg(3,S,RefineOp),
  aleph5_setting(searchstrat,SearchStrat), aleph5_setting(evalfn,EvalFn),
  arg(4,S,SearchStrat/EvalFn),
  (aleph5_setting(greedy,Greedy)-> arg(5,S,Greedy); arg(5,S,false)),
  aleph5_setting(verbosity,Verbose), arg(6,S,Verbose),
  aleph5_setting(clauselength,CLength), arg(7,S,CLength),
  aleph5_setting(caching,Cache), arg(8,S,Cache),
  (aleph5_setting(prune_defs,Prune)-> arg(9,S,Prune); arg(9,S,false)),
  aleph5_setting(lazy_on_cost,LCost), arg(10,S,LCost),
  aleph5_setting(lazy_on_contradiction,LContra), arg(11,S,LContra),
  aleph5_setting(lazy_negs,LNegs), arg(12,S,LNegs),
  aleph5_setting(minpos,MinPos), arg(13,S,MinPos),
  aleph5_setting(depth,Depth), arg(14,S,Depth),
  aleph5_setting(cache_clauselength,CCLim), arg(15,S,CCLim),
  ('$aleph_global'(size,size(pos,PSize))-> arg(16,S,PSize); arg(16,S,0)),
  aleph5_setting(noise,Noise), arg(17,S,Noise),
  aleph5_setting(minacc,MinAcc), arg(18,S,MinAcc),
  aleph5_setting(minscore,MinScore), arg(19,S,MinScore),
  ('$aleph_global'(size,size(rand,RSize))-> arg(20,S,RSize); arg(20,S,0)),
  aleph5_setting(mingain,MinGain), arg(21,S,MinGain),
  aleph5_setting(search,Search), arg(22,S,Search),
  findall(PN/PA,'$aleph_global'(lazy_evaluate,lazy_evaluate(PN/PA)),LazyPreds),
  arg(23,S,LazyPreds),
  ('$aleph_global'(size,size(neg,NSize))-> arg(24,S,NSize); arg(24,S,0)),
  aleph5_setting(openlist,OSize), arg(25,S,OSize),
  aleph5_setting(check_redundant,RCheck), arg(26,S,RCheck),
  ('$aleph_sat'(eq,Eq) -> arg(27,S,Eq); arg(27,S,false)),
  ('$aleph_sat'(hovars,HOVars) -> arg(28,S,HOVars); arg(28,S,HOVars)),
  aleph5_setting(prooftime,PTime), arg(29,S,PTime),
  aleph5_setting(construct_bottom,CBott), arg(30,S,CBott),
  (get_ovars1(false,1,HIVars) ->  arg(31,S,HIVars); arg(31,S,[])),
  aleph5_setting(language,Lang), arg(32,S,Lang),
  aleph5_setting(splitvars,Split), arg(33,S,Split),
  aleph5_setting(proof_strategy,Proof), arg(34,S,Proof),
  aleph5_setting(portray_search,VSearch), arg(35,S,VSearch),
  aleph5_setting(searchtime,Time), arg(36,S,Time),
  aleph5_setting(optimise_clauses,Optim), arg(37,S,Optim),
  aleph5_setting(newvars,NewV), arg(38,S,NewV),
  (aleph5_setting(rls_type,RlsType) -> arg(39,S,RlsType);arg(39,S,false)),
  aleph5_setting(minposfrac,MinPosFrac), arg(40,S,MinPosFrac),
  (aleph5_setting(recursion,Recursion) -> true; Recursion = false),
  arg(41,S,swi),
  aleph5_setting(interactive,Interactive), arg(42,S,Interactive),
  aleph5_setting(lookahead,LookAhead), arg(43,S,LookAhead),
  (aleph5_setting(construct_features,Features)-> arg(44,S,Features); arg(44,S,false)),
  aleph5_setting(max_features,FMax), arg(45,S,FMax),
  aleph5_setting(subsample,SS), arg(46,S,SS),
  aleph5_setting(subsamplesize,SSize), arg(47,S,SSize).

% stop search from proceeding if certain
% conditions are reached. These are:
%  . minacc and minpos values reached in rrr search
%  . best hypothesis has accuracy 1.0 if evalfn=accuracy
%  . best hypothesis covers all positive examples
discontinue_search(S,[P,_,_,F|_]/_,_):-
  arg(39,S,RlsType),
  RlsType = rrr,
  arg(13,S,MinPos),
  P >= MinPos,
  arg(19,S,MinScore),
  F >= MinScore, !.
discontinue_search(S,_,Nodes):-
        arg(1,S,MaxNodes),
        Nodes >= MaxNodes, !,
  p_message('node limit reached').
discontinue_search(S,_,_):-
        arg(44,S,Features),
  Features = true,
  arg(45,S,FMax),
  '$aleph_search'(last_good,LastGood),
        LastGood >= FMax, !,
  p_message('feature limit reached').
discontinue_search(S,[_,_,_,F|_]/_,_):-
        arg(4,S,_/Evalfn),
  Evalfn = accuracy,
  F = 1.0, !.
discontinue_search(S,Best,_):-
  arg(2,S,Explore),
  Explore = false,
        arg(4,S,_/Evalfn),
  Evalfn \= user,
  Evalfn \= posonly,
  arg(22,S,Search),
  Search \= ic,
  Best = [P|_]/_,
  arg(16,S,P).

update_max_head_count(N,0):-
  retractall('$aleph_local'(max_head_count,_)),
  asserta('$aleph_local'(max_head_count,N)), !.
update_max_head_count(Count,Last):-
  '$aleph_search_node'(Last,LitNum,_,_,PosCover,_,_,_), !,
  asserta('$aleph_local'(head_lit,LitNum)),
  interval_count(PosCover,N),
  Next is Last - 1,
  (N > Count -> update_max_head_count(N,Next);
    update_max_head_count(Count,Next)).
update_max_head_count(Count,Last):-
  Next is Last - 1,
  update_max_head_count(Count,Next).

expand(false,S,NodeRef,NodeRef,Path1,Length,Descendents,PosCover,NegCover,OVars,C,TV,CL):-
  !,
        '$aleph_search_node'(NodeRef,LitNum,Path,Length/_,PCover,NCover,OVars,_),
  arg(46,S,SSample),
  (SSample = false -> PosCover = PCover, NegCover = NCover;
    get_sample_cover(S,PosCover,NegCover)),
        append(Path, [LitNum], Path1),
  get_pclause(Path1,[],C,TV,CL,_),
        '$aleph_sat_litinfo'(LitNum,_,_,_,_,Dependents),
        intersect1(Dependents,Path1,_,Succ),
        check_parents(Succ,OVars,Descendents,_).
expand(_,S,NodeRef,NodeRef,Path1,Length,[_],PosCover,NegCover,OVars,_,_,_):-
        retract('$aleph_search_node'(NodeRef,_,Path1,Length/_,_,_,OVars,_)),
  get_sample_cover(S,PosCover,NegCover).

get_sample_cover(S,PosCover,NegCover):-
        arg(5,S,Greedy),
        (Greedy = true ->
                '$aleph_global'(atoms_left,atoms_left(pos,PCover));
                arg(16,S,PSize),
                PCover = [1-PSize]),
        arg(4,S,_/Evalfn),
  (Evalfn = posonly ->
                '$aleph_global'(atoms_left,atoms_left(rand,NCover));
                arg(24,S,NSize),
                NCover = [1-NSize]),
  arg(46,S,SSample),
  (SSample = false -> PosCover = PCover, NegCover = NCover;
    arg(47,S,SampleSize),
    interval_sample(SampleSize,PCover,PosCover),
    interval_sample(SampleSize,NCover,NegCover)).

get_ovars([],_,V,V).
get_ovars([LitNum|Lits],K,VarsSoFar,Vars):-
  get_ovars1(K,LitNum,OVars),
  append(OVars, VarsSoFar, Vars1),
  get_ovars(Lits,K,Vars1,Vars).

get_ovars1(false,LitNum,OVars):-
  '$aleph_sat_ovars'(LitNum,OVars), !.
get_ovars1(false,LitNum,OVars):-
  !,
  '$aleph_sat_litinfo'(LitNum,_,Atom,_,O,_),
  get_vars(Atom,O,OVars).
get_ovars1(K,LitNum,OVars):-
  '$aleph_sat_ovars'(LitNum,K,OVars), !.
get_ovars1(K,LitNum,OVars):-
  '$aleph_sat_litinfo'(LitNum,K,_,Atom,_,O,_),
  get_vars(Atom,O,OVars).

% get set of vars at term-places specified
get_vars(not(Literal),Args,Vars):-
  !,
  get_vars(Literal,Args,Vars).
get_vars(_,[],[]).
get_vars(Literal,[ArgNo|Args],Vars):-
  (ArgNo = Pos/_ -> true; Pos = ArgNo),
  tparg(Pos,Literal,Term),
  get_vars_in_term([Term],TV1),
  get_vars(Literal,Args,TV2),
  update_list(TV2,TV1,Vars).

get_vars_in_term([],[]).
get_vars_in_term([Var|Terms],[Var|TVars]):-
  integer(Var), !,
  get_vars_in_term(Terms,TVars).
get_vars_in_term([Term|Terms],TVars):-
  Term =.. [_|Terms1],
  get_vars_in_term(Terms1,TV1),
  get_vars_in_term(Terms,TV2),
  update_list(TV2,TV1,TVars).

% get terms at term-places specified
% need not be variables
get_argterms(not(Literal),Args,TermsSoFar,Terms):-
        !,
        get_argterms(Literal,Args,TermsSoFar,Terms).
get_argterms(_,[],Terms,Terms).
get_argterms(Literal,[ArgNo|Args],TermsSoFar,Terms):-
  (ArgNo = Pos/_ -> true; Pos = ArgNo),
        tparg(Pos,Literal,Term),
        update(TermsSoFar,Term,T1),
        get_argterms(Literal,Args,T1,Terms).

% get list of terms at arg positions specified
get_args(not(Literal),Args,TermsSoFar,Terms):-
        !,
        get_args(Literal,Args,TermsSoFar,Terms).
get_args(_,[],Terms,Terms).
get_args(Literal,[ArgNo|Args],TermsSoFar,Terms):-
  (ArgNo = Pos/_ -> true; Pos = ArgNo),
        tparg(Pos,Literal,Term),
        get_args(Literal,Args,[Term|TermsSoFar],Terms).

get_ivars([],_,V,V).
get_ivars([LitNum|Lits],K,VarsSoFar,Vars):-
  get_ivars1(K,LitNum,IVars),
  append(IVars, VarsSoFar, Vars1),
  get_ivars(Lits,K,Vars1,Vars).

get_ivars1(false,LitNum,IVars):-
  '$aleph_sat_ivars'(LitNum,IVars), !.
get_ivars1(false,LitNum,IVars):-
  !,
  '$aleph_sat_litinfo'(LitNum,_,Atom,I,_,_),
  get_vars(Atom,I,IVars).
get_ivars1(K,LitNum,IVars):-
  '$aleph_sat_ivars'(LitNum,K,IVars), !.
get_ivars1(K,LitNum,IVars):-
  '$aleph_sat_litinfo'(LitNum,K,_,Atom,I,_,_),
  get_vars(Atom,I,IVars).

check_parents([],_,[],[]).
check_parents([LitNum|Lits],OutputVars,[LitNum|DLits],Rest):-
  get_ivars1(false,LitNum,IVars),
  subset(IVars,OutputVars), !,
  check_parents(Lits,OutputVars,DLits,Rest).
check_parents([LitNum|Lits],OutputVars,DLits,[LitNum|Rest]):-
  check_parents(Lits,OutputVars,DLits,Rest), !.

get_gains(S,Last,Best,_,_,_,_,_,_,_,_,_,_,Last,Best):-
  discontinue_search(S,Best,Last),
  !.
get_gains(_,Last,Best,_,_,_,_,_,[],_,_,_,_,Last,Best):-
  !.
get_gains(S,Last,Best,Path,C,TV,L,Min,[L1|Succ],Pos,Neg,OVars,E,Last1,NextBest):-
  get_gain(S,upper,Last,Best,Path,C,TV,L,Min,L1,Pos,Neg,OVars,E,Best1,Node1),
  !,
  get_gains(S,Node1,Best1,Path,C,TV,L,Min,Succ,Pos,Neg,OVars,E,Last1,NextBest).
get_gains(S,Last,BestSoFar,Path,C,TV,L,Min,[_|Succ],Pos,Neg,OVars,E,Last1,NextBest):-
  get_gains(S,Last,BestSoFar,Path,C,TV,L,Min,Succ,Pos,Neg,OVars,E,Last1,NextBest),
  !.

get_sibgains(S,Node,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Last1,NextBest):-
  '$aleph_search_node'(Node,LitNum,_,_,_,_,_,OldE),
  '$aleph_search_expansion'(OldE,_,_,LastSib),
  '$aleph_sat_litinfo'(LitNum,_,_,_,_,Desc),
  Node1 is Node + 1,
  arg(31,S,HIVars),
  aleph_delete_list(HIVars,OVars,LVars),
  get_sibgain(S,LVars,LitNum,Desc,Node1,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,NextBest,Last1),
  !.

get_sibgain(S,_,_,_,Node,Node1,Last,Best,_,_,_,_,_,_,_,_,_,Best,Last):-
  (
    Node > Node1
  ;
    discontinue_search(S,Best,Last)
  ),
  !.
get_sibgain(S,LVars,LitNum,Desc,Node,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,LBest,LNode):-
  arg(23,S,Lazy),
  get_sibpncover(Lazy,Node,Desc,Pos,Neg,Sib1,PC,NC),
  lazy_evaluate([Sib1],Lazy,Path,PC,NC,[Sib]),
  get_ivars1(false,Sib,SibIVars),
  (
    intersects(SibIVars,LVars)
  ->
    Flag = upper;
    get_ovars1(false,Sib,SibOVars),
    (
      intersects(SibOVars,LVars)
    ->
      Flag = upper
    ;
      Flag = exact
    )
  ),
  get_gain(S,Flag,Last,Best,Path,C,TV,L,Min,Sib,PC,NC,OVars,E,Best1,Node1),
  !,
  NextNode is Node + 1,
  get_sibgain(S,LVars,LitNum,Desc,NextNode,LastSib,Node1,Best1,Path,C,TV,L, Min,Pos,Neg,OVars,E,LBest,LNode),
  !.
get_sibgain(S,LVars,LitNum,Desc,Node,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Best1,Node1):-
  NextNode is Node + 1,
  get_sibgain(S,LVars,LitNum,Desc,NextNode,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Best1,Node1),
  !.
get_sibgain(S,LVars,LitNum,Node,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Best1,Node1):-
  NextNode is Node + 1,
  get_sibgain(S,LVars,LitNum,NextNode,LastSib,Last,Best,Path,C,TV,L,Min,Pos,Neg,OVars,E,Best1,Node1),
  !.

get_sibpncover(Lazy,NodeNum,Desc,Pos,Neg,Sib,PC,NC):-
  '$aleph_search_node'(NodeNum,Sib,_,_,Pos1,Neg1,_,_),
  '$aleph_sat_litinfo'(Sib,_,Atom,_,_,_),
  \+(memberchk(Sib,Desc)),
  functor(Atom,Name,Arity),
  (
    memberchk(Name/Arity,Lazy)
  ->
    PC = Pos, NC = Neg
  ;
    calc_intersection(Pos,Pos1,PC),
    calc_intersection(Neg,Neg1,NC)
  ).

% in some cases, it is possible to simply use the intersection of
% covers cached. The conditions under which this is possible was developed
% in discussions with James Cussens
calc_intersection(A1/[B1-L1],A2/[B2-L2],A/[B-L]):-
  !,
  intervals_intersection(A1,A2,A),
  B3 is max(B1,B2),
  (intervals_intersects(A1,[B2-L2],X3-_) -> true; X3 = B3),
  (intervals_intersects(A2,[B1-L1],X4-_) -> true; X4 = B3),
  B4 is min(X3,B3),
  B is min(X4,B4),
  L is max(L1,L2).
calc_intersection(A1/_,A2,A):-
  !,
  intervals_intersection(A1,A2,A).
calc_intersection(A1,A2/_,A):-
  !,
  intervals_intersection(A1,A2,A).
calc_intersection(A1,A2,A):-
  intervals_intersection(A1,A2,A).

get_gain(S,_,Last,Best,Path,_,_,_,MinLength,_,Pos,Neg,OVars,E,Best1,NewLast):-
  arg(3,S,RefineOp),
  RefineOp \= false,
  !,
  get_refine_gain(S,Last,Best,Path,MinLength,Pos,Neg,OVars,E,Best1,NewLast).
get_gain(S,Flag,Last,Best/Node,Path,C,TV,Len1,MinLen,L1,Pos,Neg,OVars,E,Best1,Last1):-
  arg(26,S,RCheck),
  arg(33,S,SplitVars),
  retractall('$aleph_search'(covers,_)),
  retractall('$aleph_search'(coversn,_)),
  get_pclause([L1],TV,Lit1,_,Len2,LastD),
  split_ok(SplitVars,C,Lit1),
  !,
  extend_clause(C,Lit1,Clause),
  (
    RCheck = true
  ->
    (
      redundant(Clause,Lit1)
    ->
      fail
    ;
      true
    )
  ;
    true
  ),
  CLen is Len1 + Len2,
  length_ok(S,MinLen,CLen,LastD,EMin,ELength),
  split_clause(Clause,Head,Body),
  assertz('$aleph_search'(pclause,pclause(Head,Body))),
  arg(6,S,Verbosity),
  (
    Verbosity >= 1
  ->
    pp_dclause(Clause)
  ;
    true
  ),
  get_gain1(S,Flag,Clause,CLen,EMin/ELength,Last,Best/Node,Path,L1,Pos,Neg,OVars,E,Best1),
  retractall('$aleph_search'(pclause,_)),
  Last1 is Last + 1.
get_gain(_,_,Last,Best,_,_,_,_,_,_,_,_,_,_,Best,Last).

get_refine_gain(S,Last,Best/Node,Path,MinLength,Pos,Neg,OVars,E,Best1,NewLast):-
  arg(3,S,RefineOp),
  RefineOp = rls,
  refine_prelims(Best/Node,Last),
  rls_refine(clauses,Path,Path1),
  get_refine_gain1(S,Path1,MinLength,Pos,Neg,OVars,E,Best1,NewLast),
  !.
get_refine_gain(S,Last,Best/Node,Path,MinLength,Pos,Neg,OVars,E,Best1,NewLast):-
  arg(3,S,RefineOp),
  RefineOp \= rls,
  refine_prelims(Best/Node,Last),
  Path = CL-[Example,Type,_,Clause],
  arg(30,S,ConstructBottom),
  arg(43,S,LookAhead),
  get_user_refinement(RefineOp,LookAhead,Clause,R,_),
  match_bot(ConstructBottom,R,R1,LitNums),
  Path1 = CL-[Example,Type,LitNums,R1],
  get_refine_gain1(S,Path1,MinLength,Pos,Neg,OVars,E,Best1,NewLast),
  !.
get_refine_gain(_,_,_,_,_,_,_,_,_,Best,Last):-
  retract('$aleph_search'(best_refinement,best_refinement(Best))),
  retract('$aleph_search'(last_refinement,last_refinement(Last))).

get_theory_gain(S,Last,BestSoFar,T0,Pos,Neg,P,N,Best1,NewLast):-
  refine_prelims(BestSoFar,Last),
  arg(3,S,RefineOp),
  (RefineOp = rls -> rls_refine(theories,T0,T1); fail),
  arg(23,S,LazyPreds),
  (LazyPreds = [] -> Theory = T1;
    lazy_evaluate_theory(T1,LazyPreds,Pos,Neg,Theory)),
  retract('$aleph_search'(best_refinement,best_refinement(OldBest))),
  retract('$aleph_search'(last_refinement,last_refinement(OldLast))),
        arg(6,S,Verbosity),
        (Verbosity >= 1 ->
                p_message('new refinement'),
                pp_dclauses(Theory);
        true),
  record_pclauses(Theory),
  get_theory_gain1(S,Theory,OldLast,OldBest,Pos,Neg,P,N,Best1),
  retractall('$aleph_search'(pclause,_)),
        NewLast is OldLast + 1,
  asserta('$aleph_search'(last_refinement,last_refinement(NewLast))),
        asserta('$aleph_search'(best_refinement,best_refinement(Best1))),
  (discontinue_search(S,Best1,NewLast) ->
    retract('$aleph_search'(last_refinement,last_refinement(_))),
    retract('$aleph_search'(best_refinement,best_refinement(_)));
    fail),
  !.
get_theory_gain(_,_,_,_,_,_,_,_,Best,Last):-
  '$aleph_search'(best_refinement,best_refinement(Best)),
  '$aleph_search'(last_refinement,last_refinement(Last)).

refine_prelims(Best,Last):-
  retractall('$aleph_search'(last_refinement,_)),
  retractall('$aleph_search'(best_refinement,_)),
        asserta('$aleph_search'(best_refinement,best_refinement(Best))),
  asserta('$aleph_search'(last_refinement,last_refinement(Last))).

get_refine_gain1(S,Path,MinLength,Pos,Neg,OVars,E,Best1,NewLast):-
        arg(23,S,LazyPreds),
  Path = CL-[Example,Type,Ids,Refine],
  (LazyPreds = [] -> Ids1 = Ids, Clause = Refine;
    lazy_evaluate_refinement(Ids,Refine,LazyPreds,Pos,Neg,Ids1,Clause)),
  retractall('$aleph_search'(covers,_)),
  retractall('$aleph_search'(coversn,_)),
  Path1 = CL-[Example,Type,Ids1,Clause],
  split_clause(Clause,Head,Body),
  nlits(Body,CLength0),
  CLength is CLength0 + 1,
  length_ok(S,MinLength,CLength,0,EMin,ELength),
  arg(41,S,Prolog),
  split_clause(Clause,Head,Body),
  (Prolog = yap ->
    assertz('$aleph_search'(pclause,pclause(Head,Body)),DbRef);
    assertz('$aleph_search'(pclause,pclause(Head,Body)))),
  retract('$aleph_search'(best_refinement,best_refinement(OldBest))),
  retract('$aleph_search'(last_refinement,last_refinement(OldLast))),
        arg(6,S,Verbosity),
        (Verbosity >= 1 ->
    p_message('new refinement'),
    pp_dclause(Clause);
  true),
  once(get_gain1(S,upper,Clause,CLength,EMin/ELength,OldLast,OldBest,
    Path1,[],Pos,Neg,OVars,E,Best1)),
  (Prolog = yap ->
    erase(DbRef);
    retractall('$aleph_search'(pclause,_))),
  NewLast is OldLast + 1,
  asserta('$aleph_search'(last_refinement,last_refinement(NewLast))),
        asserta('$aleph_search'(best_refinement,best_refinement(Best1))),
  (discontinue_search(S,Best1,NewLast) ->
    retract('$aleph_search'(last_refinement,last_refinement(_))),
    retract('$aleph_search'(best_refinement,best_refinement(_)));
    fail),
  !.

get_theory_gain1(S,Theory,Last,Best,Pos,Neg,P,N,Best1):-
        (false -> p_message('constraint violated'),
                Contradiction = true;
                Contradiction = false),
  Contradiction = false,
        Node1 is Last + 1,
  arg(32,S,Lang),
  theory_lang_ok(Theory,Lang),
  arg(38,S,NewVars),
  theory_newvars_ok(Theory,NewVars),
  arg(14,S,Depth),
  arg(29,S,Time),
  arg(34,S,Proof),
        prove(Depth/Time/Proof,pos,(X:-X),Pos,PCvr,TP),
        prove(Depth/Time/Proof,neg,(X:-X),Neg,NCvr,FP),
  arg(4,S,_/Evalfn),
  Correct is TP + (N - FP),
  Incorrect is FP + (P - TP),
  length(Theory,L),
  Label = [Correct,Incorrect,L],
  complete_label(Evalfn,Theory,Label,Label1),
  get_search_keys(heuristic,Label1,SearchKeys),
  arg(6,S,Verbosity),
  (Verbosity >= 1 -> p_message(Correct/Incorrect); true),
  asserta('$aleph_search_node'(Node1,Theory,[],0,PCvr,NCvr,[],0)),
  update_open_list(SearchKeys,Node1,Label1),
  update_best_theory(S,Theory,PCvr,NCvr,Best,Label1/Node1,Best1), !.
get_theory_gain1(_,_,_,Best,_,_,_,_,Best).

get_gain1(S,_,C,CL,_,Last,Best,Path,_,Pos,Neg,_,E,Best):-
        abandon_branch(S,C), !,
        Node1 is Last + 1,
        arg(3,S,RefineOp),
        arg(7,S,ClauseLength),
  arg(35,S,VSearch),
        (ClauseLength = CL -> true;
                (RefineOp = false  ->
                        asserta('$aleph_search_node'(Node1,0,Path,0,Pos,Neg,[],E));
      true)),
  (VSearch = true ->
    asserta('$aleph_search'(bad,Node1)),
    asserta('$aleph_search_node'(Node1,C));
    true).
get_gain1(S,_,Clause,_,_,_,Best,_,_,_,_,_,_,Best):-
        arg(8,S,Caching),
        Caching = true,
        skolemize(Clause,SHead,SBody,0,_),
        '$aleph_search_prunecache'([SHead|SBody]), !,
  arg(6,S,Verbosity),
        (Verbosity >= 1 -> p_message('in prune cache'); true).
get_gain1(S,Flag,C,CL,EMin/EL,Last,Best/Node,Path,L1,Pos,Neg,OVars,E,Best1):-
  split_clause(C,Head,Body),
  arg(22,S,Search),
        ((Search \== ic, false) -> p_message('constraint violated'),
                Contradiction = true;
                Contradiction = false),
        Node1 is Last + 1,
        arg(8,S,Caching),
        (Caching = true -> arg(15,S,CCLim),
    get_cache_entry(CCLim,C,Entry);
    Entry = false),
  arg(35,S,VSearch),
  (VSearch = true ->
    asserta('$aleph_search_node'(Node1,C));
    true),
        arg(3,S,RefineOp),
  refinement_ok(RefineOp,Entry),
  arg(32,S,Lang),
  lang_ok((Head:-Body),Lang),
  arg(38,S,NewVars),
  newvars_ok((Head:-Body),NewVars),
  arg(34,S,Proof),
  arg(37,S,Optim),
  rewrite_clause(Proof,Optim,(Head:-Body),(Head1:-Body1)),
  (Search = ic ->
    PCvr = [],
    Label = [_,_,CL],
    ccheck(S,(Head1:-Body1),NCvr,Label);
          prove_examples(S,Flag,Contradiction,Entry,Best,CL,EL,
        (Head1:-Body1),Pos,Neg,PCvr,NCvr,Label)
  ),
        arg(4,S,SearchStrat/Evalfn),
  arg(40,S,MinPosFrac),
  ((MinPosFrac > 0.0 ; Evalfn = wracc) ->
    reset_clause_prior(S,Head1);
    true
  ),
  arg(46,S,SSample),
  (SSample = true ->
    arg(47,S,SampleSize),
    estimate_label(SampleSize,Label,Label0);
    Label0 = Label),
  complete_label(Evalfn,C,Label0,Label1),
  compression_ok(Evalfn,Label1),
        get_search_keys(SearchStrat,Label1,SearchKeys),
        arg(6,S,Verbosity),
  arg(10,S,LCost),
  arg(11,S,LContra),
        ((Verbosity >= 1, LContra = false, LCost = false) ->
    Label = [A,B|_],
    p_message(A/B);
  true),
        arg(7,S,ClauseLength),
  (
    RefineOp = false
  ->
    get_ovars1(false,L1,OVars1),
    append(OVars, OVars1, OVars2)
  ;
    true
  ),
        ((ClauseLength=CL, RefineOp = false) -> true;
    (RefineOp = false ->
                  asserta('$aleph_search_node'(Node1,L1,Path,EMin/EL,PCvr,
          NCvr,OVars2,E));
                  asserta('$aleph_search_node'(Node1,0,Path,EMin/EL,PCvr,
          NCvr,[],E))),
                  update_open_list(SearchKeys,Node1,Label1)),
  (VSearch = true ->
    asserta('$aleph_search'(label,label(Node1,Label)));
    true),
        (((RefineOp \= false,Contradiction=false);
    (arg(28,S,HOVars),clause_ok(Contradiction,HOVars,OVars2))) ->
                update_best(S,C,PCvr,NCvr,Best/Node,Label1/Node1,Best1);
                Best1=Best/Node),
  !.
get_gain1(_,_,_,_,_,_,Best,_,_,_,_,_,_,Best).


abandon_branch(S,C):-
        arg(9,S,PruneDefined),
        PruneDefined = true,
        prune(C), !,
        arg(6,S,Verbosity),
        (Verbosity >= 1 -> p_message(pruned); true).

clause_ok(false,V1,V2):-
        subset(V1,V2).

% check to see if a clause is acceptable
%   unacceptable if it fails noise, minacc, or minpos settings
%  unacceptable if it fails search or language constraints
clause_ok(_,_):-
  false, !, fail.
clause_ok(_,Label):-
  extract_pos(Label,P),
  extract_neg(Label,N),
  Acc is P/(P+N),
  aleph5_setting(noise,Noise),
  aleph5_setting(minacc,MinAcc),
  aleph5_setting(minpos,MinPos),
  (N > Noise; Acc < MinAcc; P < MinPos), !, fail.
clause_ok(Clause,_):-
  prune(Clause), !, fail.
clause_ok(Clause,_):-
  aleph5_setting(language,Lang),
  \+ lang_ok(Clause,Lang), !, fail.
clause_ok(Clause,_):-
  aleph5_setting(newvars,NewVars),
  \+ newvars_ok(Clause,NewVars), !, fail.
clause_ok(_,_).

% check to see if refinement has been produced before
refinement_ok(false,_):- !.
refinement_ok(rls,_):- !.
refinement_ok(_,false):- !.
refinement_ok(_,Entry):-
  (check_cache(Entry,pos,_); check_cache(Entry,neg,_)), !,
  p_message('redundant refinement'),
  fail.
refinement_ok(_,_).


% specialised redundancy check with equality theory
% used only to check if equalities introduced by splitting vars make
% literal to be added redundant
split_ok(false,_,_):- !.
split_ok(_,Clause,Lit):-
  functor(Lit,Name,_),
  Name \= '=',
  copy_term(Clause/Lit,Clause1/Lit1),
  lit_redun(Lit1,Clause1), !,
  p_message('redundant literal'),
  current_stream(Stream),
  nl(Stream),
  fail.
split_ok(_,_,_).

lit_redun(Lit,(Head:-Body)):-
  !,
  lit_redun(Lit,(Head,Body)).
lit_redun(Lit,(L1,_)):-
  Lit == L1, !.
lit_redun(Lit,(L1,L2)):-
  !,
  execute_equality(L1),
  lit_redun(Lit,L2).
lit_redun(Lit,L):-
  Lit == L.

execute_equality(Lit):-
  functor(Lit,'=',2), !,
  Lit.
execute_equality(_).

theory_lang_ok([],_).
theory_lang_ok([_-[_,_,_,Clause]|T],Lang):-
        lang_ok(Lang,Clause),
        theory_lang_ok(Lang,T).

theory_newvars_ok([],_).
theory_newvars_ok([_-[_,_,_,Clause]|T],NewV):-
        newvars_ok(NewV,Clause),
        theory_newvars_ok(T,NewV).

lang_ok((Head:-Body),N):-
  !,
  (lang_ok(N,Head,Body) -> true;
    p_message('outside language bound'),
    fail).

lang_ok(N,_,_):- N is 1e10, !.
lang_ok(N,Head,Body):-
  get_psyms((Head,Body),PSymList),
  lang_ok1(PSymList,N).

newvars_ok((Head:-Body),N):-
  !,
  (newvars_ok(N,Head,Body) -> true;
    p_message('outside newvars bound'),
    fail).

newvars_ok(N,_,_):- N is 1e10, !.
newvars_ok(N,Head,Body):-
  vars_in_term([Head],[],HVars),
  goals_to_list(Body,BodyL),
  vars_in_term(BodyL,[],BVars),
        aleph_ord_subtract(BVars,HVars,NewVars),
  length(NewVars,N1),
  N1 =< N.

get_psyms((L,B),[N/A|Syms]):-
  !,
  functor(L,N,A),
  get_psyms(B,Syms).
get_psyms(true,[]):- !.
get_psyms(L,[N/A]):-
  functor(L,N,A).

lang_ok1([],_).
lang_ok1([Pred|Preds],N):-
        length(Preds,N0),
        aleph_delete_all(Pred,Preds,Preds1),
        length(Preds1,N1),
        PredOccurs is N0 - N1 + 1,
  PredOccurs =< N,
  lang_ok1(Preds1,N).

rewrite_clause(sld,_,_,(X:-X)):- !.
rewrite_clause(restricted_sld,true,(Head:-Body),(Head1:-Body1)):-
  !,
        optimise((Head:-Body),(Head1:-Body1)).
rewrite_clause(_,_,Clause,Clause).

record_pclauses([]).
record_pclauses([_-[_,_,_,Clause]|T]):-
        split_clause(Clause,Head,Body),
        assertz('$aleph_search'(pclause,pclause(Head,Body))),
        record_pclauses(T).

% get pos/neg distribution of clause head
reset_clause_prior(S,Head):-
  arg(3,S,Refine),
  Refine = false, !,
  ('$aleph_search'(clauseprior,_) -> true;
    get_clause_prior(S,Head,Prior),
    assertz('$aleph_search'(clauseprior,Prior))
  ).
reset_clause_prior(S,Head):-
  copy_term(Head,Head1),
  numbervars(Head1,0,_),
  ('$aleph_local'(clauseprior,prior(Head1,Prior)) ->
    true;
    get_clause_prior(S,Head,Prior),
    assertz('$aleph_local'(clauseprior,prior(Head1,Prior)))
  ),
  retractall('$aleph_search'(clauseprior,_)),
  assertz('$aleph_search'(clauseprior,Prior)).

get_clause_prior(S,Head,Total-[P-pos,N-neg]):-
  arg(5,S,Greedy),
  arg(14,S,Depth),
  arg(29,S,Time),
  arg(34,S,Proof),
  (Greedy = true ->
    '$aleph_global'(atoms_left,atoms_left(pos,Pos));
    '$aleph_global'(atoms,atoms(pos,Pos))
  ),
  '$aleph_global'(atoms_left,atoms_left(neg,Neg)),
  prove(Depth/Time/Proof,pos,(Head:-true),Pos,_,P),
  prove(Depth/Time/Proof,neg,(Head:-true),Neg,_,N),
  Total is P + N.

get_user_refinement(auto,L,Clause,Template,0):-
        auto_refine(L,Clause,Template).
get_user_refinement(user,_,Clause,Template,0):-
        refine(Clause,Template).

match_bot(false,Clause,Clause,[]).
match_bot(reduction,Clause,Clause1,Lits):-
  match_lazy_bottom(Clause,Lits),
  get_pclause(Lits,[],Clause1,_,_,_).
match_bot(saturation,Clause,Clause1,Lits):-
  once(get_aleph_clause(Clause,AlephClause)),
  match_bot_lits(AlephClause,[],Lits),
  get_pclause(Lits,[],Clause1,_,_,_).

match_bot_lits((Lit,Lits),SoFar,[LitNum|LitNums]):-
  !,
  match_bot_lit(Lit,LitNum),
  \+(member(LitNum,SoFar)),
  match_bot_lits(Lits,[LitNum|SoFar],LitNums).
match_bot_lits(Lit,SoFar,[LitNum]):-
  match_bot_lit(Lit,LitNum),
  \+(member(LitNum,SoFar)).

match_bot_lit(Lit,LitNum):-
  '$aleph_sat'(botsize,Last),
  '$aleph_sat_litinfo'(LitNum,_,Lit,_,_,_),
  LitNum >= 0,
  LitNum =< Last.

match_lazy_bottom(Clause,Lits):-
  once(get_aleph_clause(Clause,AlephClause)),
  copy_term(Clause,CClause),
  split_clause(CClause,CHead,CBody),
  example_saturated(CHead),
  store(stage),
  aleph5_set_setting(stage,saturation),
  match_lazy_bottom1(CBody),
  reinstate(stage),
  match_bot_lits(AlephClause,[],Lits).

match_lazy_bottom1(Body):-
  Body,
  match_body_modes(Body),
  fail.
match_lazy_bottom1(_):-
  flatten_matched_atoms(body).

match_body_modes((CLit,CLits)):-
        !,
        match_mode(body,CLit),
        match_body_modes(CLits).
match_body_modes(CLit):-
        match_mode(body,CLit).

match_mode(_,true):- !.
match_mode(Loc,CLit):-
  functor(CLit,Name,Arity),
        functor(Mode,Name,Arity),
  (Loc=head ->
    '$aleph_global'(modeh,modeh(_,Mode));
    '$aleph_global'(modeb,modeb(_,Mode))),
        split_args(Mode,Mode,I,O,C),
        (Loc = head ->
    update_atoms(CLit,mode(Mode,O,I,C));
    update_atoms(CLit,mode(Mode,I,O,C))),
  fail.
match_mode(_,_).

flatten_matched_atoms(Loc):-
        aleph5_setting(i,IVal),
        (retract('$aleph_sat'(botsize,BSize))-> true;  BSize = 0),
        (retract('$aleph_sat'(lastlit,Last))-> true ; Last = 0),
        (Loc = head ->
                flatten(0,IVal,BSize,BSize1);
                flatten(0,IVal,Last,BSize1)),
        asserta('$aleph_sat'(botsize,BSize1)),
  (Last < BSize1 ->
          asserta('$aleph_sat'(lastlit,BSize1));
          asserta('$aleph_sat'(lastlit,Last))), !.
flatten_matched_atoms(_).

% integrate head literal into lits database
% used during lazy evaluation of bottom clause
integrate_head_lit(HeadOVars):-
        example_saturated(Example),
  split_args(Example,_,_,Output,_),
  integrate_args(unknown,Example,Output),
        match_mode(head,Example),
  flatten_matched_atoms(head),
        get_ivars1(false,1,HeadOVars), !.
integrate_head_lit([]).


get_aleph_clause((Lit:-true),PLit):-
  !,
  get_aleph_lit(Lit,PLit).
get_aleph_clause((Lit:-Lits),(PLit,PLits)):-
  !,
  get_aleph_lit(Lit,PLit),
  get_aleph_lits(Lits,PLits).
get_aleph_clause(Lit,PLit):-
  get_aleph_lit(Lit,PLit).

get_aleph_lits((Lit,Lits),(PLit,PLits)):-
  !,
  get_aleph_lit(Lit,PLit),
  get_aleph_lits(Lits,PLits).
get_aleph_lits(Lit,PLit):-
  get_aleph_lit(Lit,PLit).

get_aleph_lit(Lit,PLit):-
  functor(Lit,Name,Arity),
  functor(PLit,Name,Arity),
  get_aleph_lit(Lit,PLit,Arity).

get_aleph_lit(_,_,0):- !.
get_aleph_lit(Lit,PLit,Arg):-
  arg(Arg,Lit,Term),
  (var(Term) -> arg(Arg,PLit,Term);arg(Arg,PLit,aleph_const(Term))),
  NextArg is Arg - 1,
  get_aleph_lit(Lit,PLit,NextArg), !.

% Claudien-style consistency checking as described by De Raedt and Dehaspe, 1996
% currently does not retain actual substitutions that result in inconsistencies
% also, only checks for constraints of the form false:- ...
% this simplifies the check of Body,not(Head) to just Body
ccheck(S,(false:-Body),[],[0,N|_]):-
  (Body = true ->
    N is 1e10;
    arg(11,S,LContra),
    (LContra = false ->
            arg(14,S,Depth),
            arg(29,S,Time),
      findall(X,(resource_bound_call(Time,Depth,Body),X=1),XL),
      length(XL,N);
      lazy_ccheck(S,Body,N)
    )
  ).

lazy_ccheck(S,Body,N):-
        arg(14,S,Depth),
        arg(17,S,Noise),
        arg(29,S,Time),
  retractall('$aleph_local'(subst_count,_)),
  asserta('$aleph_local'(subst_count,0)),
  resource_bound_call(Time,Depth,Body),
  retract('$aleph_local'(subst_count,N0)),
  N is N0 + 1,
  N > Noise, !.
lazy_ccheck(_,_,N):-
  retract('$aleph_local'(subst_count,N)).

% posonly formula as described by Muggleton, ILP-96
prove_examples(S,Flag,Contradiction,Entry,Best,CL,L2,Clause,Pos,Rand,PCover,RCover,[P,B,CL,I,G]):-
  arg(4,S,_/Evalfn),
  Evalfn = posonly, !,
        arg(11,S,LazyOnContra),
        ((LazyOnContra = true, Contradiction = true) ->
                prove_lazy_cached(S,Entry,Pos,Rand,PCover,RCover),
                interval_count(PCover,PC),
                interval_count(RCover,RC);
                prove_pos(S,Flag,Entry,Best,[PC,L2],Clause,Pos,PCover,PC),
                prove_rand(S,Flag,Entry,Clause,Rand,RCover,RC)),
        find_posgain(PCover,P),
        arg(16,S,M), arg(20,S,N),
        GC is (RC+1.0)/(N+2.0), % Laplace correction for small numbers
        A is log(P),
        B is log(GC),
        G is GC*M/P,
        C is CL/P,
        % Sz is CL*M/P,
        % D is M*G,
        %  I is M - D - Sz,
        I is A - B - C.
prove_examples(S,_,_,Entry,_,CL,_,_,Pos,Neg,Pos,Neg,[PC,NC,CL]):-
        arg(10,S,LazyOnCost),
        LazyOnCost = true, !,
        prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1),
        interval_count(Pos1,PC),
        interval_count(Neg1,NC).
prove_examples(S,_,true,Entry,_,CL,_,_,Pos,Neg,Pos,Neg,[PC,NC,CL]):-
        arg(11,S,LazyOnContra),
        LazyOnContra = true, !,
        prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1),
        interval_count(Pos1,PC),
        interval_count(Neg1,NC).
prove_examples(S,Flag,_,Ent,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,[PC,NC,CL]):-
  arg(3,S,RefineOp),
  (RefineOp = false; RefineOp = auto),
        arg(7,S,ClauseLength),
        ClauseLength = CL, !,
  interval_count(Pos,MaxPCount),
        prove_neg(S,Flag,Ent,Best,[MaxPCount,CL],Clause,Neg,NCover,NC),
        arg(17,S,Noise), arg(18,S,MinAcc),
        maxlength_neg_ok(Noise/MinAcc,Ent,MaxPCount,NC),
        prove_pos(S,Flag,Ent,Best,[PC,L2],Clause,Pos,PCover,PC),
        maxlength_neg_ok(Noise/MinAcc,Ent,PC,NC),
  !.
prove_examples(S,Flag,_,Ent,Best,CL,L2,Clause,Pos,Neg,PCover,NCover,[PC,NC,CL]):-
        prove_pos(S,Flag,Ent,Best,[PC,L2],Clause,Pos,PCover,PC),
        prove_neg(S,Flag,Ent,Best,[PC,CL],Clause,Neg,NCover,NC),
  !.

prove_lazy_cached(S,Entry,Pos,Neg,Pos1,Neg1):-
        arg(8,S,Caching),
  Caching = true, !,
  (check_cache(Entry,pos,Pos1)->
    true;
    add_cache(Entry,pos,Pos),
    Pos1 = Pos),
  (check_cache(Entry,neg,Neg1)->
    true;
    add_cache(Entry,neg,Neg),
    Neg1 = Neg).
prove_lazy_cached(_,_,Pos,Neg,Pos,Neg).

complete_label(posonly,_,L,L):- !.
complete_label(user,Clause,[P,N,L],[P,N,L,Val]):-
        cost(Clause,[P,N,L],Cost), !,
  Val is -Cost.
complete_label(entropy,_,[P,N,L],[P,N,L,Val]):-
  evalfn(entropy,[P,N,L],Entropy),
  Val is -Entropy, !.
complete_label(gini,_,[P,N,L],[P,N,L,Val]):-
  evalfn(gini,[P,N,L],Gini),
  Val is -Gini, !.
complete_label(EvalFn,_,[P,N,L],[P,N,L,Val]):-
  evalfn(EvalFn,[P,N,L],Val), !.
complete_label(_,_,_,_):-
  p_message1('error'), p_message('incorrect evaluation/cost function'),
  fail.

% estimate label based on subsampling
estimate_label(Sample,[P,N|Rest],[P1,N1|Rest]):-
  '$aleph_global'(atoms_left,atoms_left(pos,Pos)),
  '$aleph_global'(atoms_left,atoms_left(neg,Neg)),
  interval_count(Pos,PC), interval_count(Neg,NC),
  PFrac is P/Sample,
  NFrac is N/Sample,
  P1 is integer(PFrac*PC),
  N1 is integer(NFrac*NC).

% get primary and secondary search keys for search
% use [Primary|Secondary] notation as it is the most compact
get_search_keys(bf,[_,_,L,F|_],[L1|F]):-
  !,
  L1 is -1*L.
get_search_keys(df,[_,_,L,F|_],[L|F]):- !.
get_search_keys(_,[_,_,L,F|_],[F|L1]):-
  L1 is -1*L.

prove_pos(_,_,_,_,_,_,[],[],0):- !.
prove_pos(S,_,Entry,BestSoFar,PosSoFar,Clause,_,PCover,PCount):-
        '$aleph_search'(covers,covers(PCover,PCount)), !,
        pos_ok(S,Entry,BestSoFar,PosSoFar,Clause,PCover).
prove_pos(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Pos,PCover,PCount):-
        prove_cache(Flag,S,pos,Entry,Clause,Pos,PCover,PCount),
        pos_ok(S,Entry,BestSoFar,PosSoFar,Clause,PCover), !.

prove_neg(S,_,Entry,_,_,_,[],[],0):-
  arg(8,S,Caching),
  (Caching = true -> add_cache(Entry,neg,[]); true), !.
prove_neg(S,Flag,Entry,_,_,Clause,Neg,NCover,NCount):-
  arg(3,S,RefineOp),
  RefineOp = rls,  !,
        prove_cache(Flag,S,neg,Entry,Clause,Neg,NCover,NCount).
prove_neg(_,_,_,_,_,_,_,NCover,NCount):-
        '$aleph_search'(coversn,coversn(NCover,NCount)), !.
prove_neg(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Neg,NCover,NCount):-
        arg(12,S,LazyNegs),
        LazyNegs = true, !,
        lazy_prove_neg(S,Flag,Entry,BestSoFar,PosSoFar,Clause,Neg,NCover,NCount).
prove_neg(S,Flag,Entry,[P,0,L1|_],[P,L2],Clause,Neg,[],0):-
  arg(4,S,bf/coverage),
        L2 is L1 - 1,
  !,
        prove_cache(Flag,S,neg,Entry,Clause,Neg,0,[],0), !.
prove_neg(S,Flag,Entry,[P,N|_],[P,L1],Clause,Neg,NCover,NCount):-
  arg(4,S,bf/coverage),
        !,
        arg(7,S,ClauseLength),
        (ClauseLength = L1 ->
    arg(2,S,Explore),
    (Explore = true -> MaxNegs is N; MaxNegs is N - 1),
                MaxNegs >= 0,
                prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
    NCount =< MaxNegs;
                prove_cache(Flag,S,neg,Entry,Clause,Neg,NCover,NCount)),
        !.
prove_neg(S,Flag,Entry,_,[P1,L1],Clause,Neg,NCover,NCount):-
        arg(7,S,ClauseLength),
        ClauseLength = L1,  !,
        arg(17,S,Noise), arg(18,S,MinAcc),
        get_max_negs(Noise/MinAcc,P1,N1),
        prove_cache(Flag,S,neg,Entry,Clause,Neg,N1,NCover,NCount),
  NCount =< N1,
        !.
prove_neg(S,Flag,Entry,_,_,Clause,Neg,NCover,NCount):-
        prove_cache(Flag,S,neg,Entry,Clause,Neg,NCover,NCount),
        !.

prove_rand(S,Flag,Entry,Clause,Rand,RCover,RCount):-
        prove_cache(Flag,S,rand,Entry,Clause,Rand,RCover,RCount),
        !.

lazy_prove_neg(S,Flag,Entry,[P,N|_],[P,_],Clause,Neg,NCover,NCount):-
  arg(4,S,bf/coverage),
        !,
        MaxNegs is N + 1,
        prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
        !.
lazy_prove_neg(S,Flag,Entry,_,[P1,_],Clause,Neg,NCover,NCount):-
        arg(17,S,Noise), arg(18,S,MinAcc),
        get_max_negs(Noise/MinAcc,P1,N1),
        MaxNegs is N1 + 1,
        prove_cache(Flag,S,neg,Entry,Clause,Neg,MaxNegs,NCover,NCount),
        !.

% Bug reported by Daniel Fredouille
% For MiAcc =:= 0, Negs was being set to P1 + 1. Unclear why.
% This definition is as it was up to Aleph 2.
get_max_negs(Noise/MinAcc,P1,N):-
        number(P1),
  (MinAcc =:= 0.0 -> N is Noise;
          (N1 is integer((1-MinAcc)*P1/MinAcc),
    (Noise < N1 -> N is Noise; N is N1))
  ), !.
get_max_negs(Noise/_,_,Noise).


% update_open_list(+SearchKeys,+NodeRef,+Label)
% insert SearchKeys into openlist
update_open_list([K1|K2],NodeRef,Label):-
  assertz('$aleph_search_gain'(K1,K2,NodeRef,Label)),
  retract('$aleph_search'(openlist,OpenList)),
  uniq_insert(descending,[K1|K2],OpenList,List1),
  asserta('$aleph_search'(openlist,List1)).

pos_ok(S,_,_,_,_,_):-
  arg(3,S,RefineOp),
  (RefineOp = rls; RefineOp = user),  !.
pos_ok(S,Entry,_,[P,_],_,_):-
        arg(13,S,MinPos),
        P < MinPos, !,
        arg(8,S,Caching),
        (Caching = true ->
                add_prune_cache(Entry);
                true),
        fail.
pos_ok(S,Entry,_,[P,_],_,_):-
  arg(40,S,MinPosFrac),
  MinPosFrac > 0.0,
  '$aleph_search'(clauseprior,_-[P1-pos,_]),
  P/P1 < MinPosFrac, !,
        arg(8,S,Caching),
        (Caching = true ->
                add_prune_cache(Entry);
                true),
  fail.
pos_ok(S,_,[_,_,_,C1|_],[P,L],_,_):-
        arg(4,S,_/Evalfn),
  arg(2,S,Explore),
  ((Evalfn = user; Explore = true) -> true;
          evalfn(Evalfn,[P,0,L],C2),
    best_value(Evalfn,S,[P,0,L,C2],Max),
          Max > C1), !.


maxlength_neg_ok(Noise/MinAcc,Entry,P,N):-
  ((N > Noise); (P/(P+N) < MinAcc)), !,
        add_prune_cache(Entry),
  fail.
maxlength_neg_ok(_,_,_,_).

compression_ok(compression,[P,_,L|_]):-
  !,
  P - L + 1 > 0.
compression_ok(_,_).

length_ok(S,MinLen,ClauseLen,LastD,ExpectedMin,ExpectedCLen):-
        arg(3,S,RefineOp),
        (RefineOp = false  -> L1 = LastD; L1 = 0),
        (L1 < MinLen->ExpectedMin = L1;ExpectedMin = MinLen),
        ExpectedCLen is ClauseLen + ExpectedMin,
        arg(7,S,CLength),
        ExpectedCLen =< CLength, !.

update_best(S,_,_,_,Best,[P,_,_,F|_]/_,Best):-
        arg(13,S,MinPos),
        arg(19,S,MinScore),
  (P < MinPos;  F is -1e10; F < MinScore), !.
update_best(S,_,_,_,Best,[P|_]/_,Best):-
  arg(40,S,MinPosFrac),
  MinPosFrac > 0.0,
  '$aleph_search'(clauseprior,_-[P1-pos,_]),
  P/P1 < MinPosFrac, !.
update_best(S,_,_,_,Best,[P,N,_,_|_]/_,Best):-
        arg(4,S,_/Evalfn),
  Evalfn \= posonly,
  % Evalfn \= user,
        arg(17,S,Noise),
        arg(18,S,MinAcc),
  arg(22,S,Search),
  Total is P + N,
  ((N > Noise);(Search \= ic, Total > 0, P/Total < MinAcc)),   !.
update_best(S,Clause,PCover,NCover,Label/_,Label1/Node1,Label1/Node1):-
        Label = [_,_,_,Gain|_],
        Label1 = [_,_,_,Gain1|_],
        % (Gain1 = 1e10; Gain = -1e10; Gain1 > Gain), !,
  Gain1 > Gain, !,
  retractall('$aleph_search'(selected,_)),
        asserta('$aleph_search'(selected,selected(Label1,Clause,PCover,NCover))),
        arg(35,S,VSearch),
        (VSearch = true ->
    retractall('$aleph_search'(best,_)),
                asserta('$aleph_search'(best,Node1)),
                asserta('$aleph_search'(good,Node1));
                true),
  update_good(Label1,Clause),
        show_clause(newbest,Label1,Clause,Node1),
        record_clause(newbest,Label1,Clause,Node1),
        record_clause(good,Label1,Clause,Node1).
update_best(S,Clause,_,_,Label/Node,Label1/Node1,Label/Node):-
        arg(35,S,VSearch),
        (VSearch = true ->
                asserta('$aleph_search'(good,Node1));
                true),
  update_good(Label1,Clause),
        show_clause(good,Label1,Clause,Node1),
        record_clause(good,Label1,Clause,Node1).

update_good(Label,Clause):-
  aleph5_setting(good,true), !,
  Label = [_,_,L|_],
  aleph5_setting(check_good,Flag),
  update_good(Flag,L,Label,Clause).
update_good(_,_).

update_good(_,_,_,_):-
  aleph5_setting(goodfile,_), !.
update_good(true,L,Label,Clause):-
  '$aleph_good'(L,Label,Clause), !.
update_good(_,L,Label,Clause):-
  assertz('$aleph_good'(L,Label,Clause)),
  (retract('$aleph_search'(last_good,Good)) ->
    Good1 is Good + 1;
    Good1 is 1),
  assertz('$aleph_search'(last_good,Good1)).

update_best_theory(S,_,_,_,Best,[P,N,_,F|_]/_,Best):-
  arg(17,S,Noise),
  arg(18,S,MinAcc),
  arg(19,S,MinScore),
  (N > Noise; P/(P+N) < MinAcc; F < MinScore),  !.
update_best_theory(_,Theory,PCover,NCover,Label/_,Label1/Node1,Label1/Node1):-
  Label = [_,_,_,Gain|_],
  Label1 = [_,_,_,Gain1|_],
  Gain1 > Gain, !,
  retractall('$aleph_search'(selected,_)),
        asserta('$aleph_search'(selected,selected(Label1,Theory,PCover,NCover))),
  show_theory(newbest,Label1,Theory,Node1),
  record_theory(newbest,Label1,Theory,Node1),
  record_theory(good,Label1,Theory,Node1).
update_best_theory(_,Theory,_,_,Best,Label1/_,Best):-
  show_theory(good,Label1,Theory,Node1),
  record_theory(good,Label1,Theory,Node1).



% PRUNING CLAUSES

get_node([[K1|K2]|_],[K1|K2],Node):-
        '$aleph_search_gain'(K1,K2,Node,_).
get_node([_|Gains],Gain,Node):-
  get_node(Gains,Gain,Node).

prune_open(S,_,_):-
  arg(25,S,OSize),
  Inf is 1e10,
  OSize =\= Inf,
        retractall('$aleph_local'(in_beam,_)),
        asserta('$aleph_local'(in_beam,0)),
        '$aleph_search'(openlist,Gains),
        get_node(Gains,[K1|K2],NodeNum),
        '$aleph_local'(in_beam,N),
        (N < OSize->
          retract('$aleph_local'(in_beam,N)),
                N1 is N + 1,
                asserta('$aleph_local'(in_beam,N1));
    retract('$aleph_search_gain'(K1,K2,NodeNum,_)),
    arg(6,S,Verbose),
                (Verbose < 1 ->
      true;
      p1_message('non-admissible removal'),
      p_message(NodeNum))),
        fail.
prune_open(S,_,_):-
        arg(2,S,Explore),
        arg(3,S,RefineOp),
  (Explore = true; RefineOp = rls; RefineOp = user), !.
prune_open(_,_/N,_/N):- !.
prune_open(S,_,[_,_,_,Best|_]/_):-
        arg(4,S,_/Evalfn),
  built_in_prune(Evalfn),
        '$aleph_search_gain'(_,_,_,Label),
  best_value(Evalfn,S,Label,Best1),
  Best1 =< Best,
        retract('$aleph_search_gain'(_,_,_,Label)),
  fail.
prune_open(_,_,_).

built_in_prune(coverage).
built_in_prune(compression).
built_in_prune(posonly).
built_in_prune(laplace).
built_in_prune(wracc).
built_in_prune(mestimate).
built_in_prune(auto_m).

% pruning for posonly, laplace and m-estimates devised in
%  discussion with James Cussens
% pruning for weighted relative accuracy devised in
%  discussion with Steve Moyle
% corrections to best_value/4 after discussion with
% Mark Reid and James Cussens
best_value(gini,_,_,0.0):- !.
best_value(entropy,_,_,0.0):- !.
best_value(posonly,S,[P,_,L|_],Best):-
  arg(20,S,RSize),
  Best is log(P) + log(RSize+2.0) - (L+1)/P, !.
best_value(wracc,_,[P|_],Best):-
  ('$aleph_search'(clauseprior,Total-[P1-pos,_]) ->
    Best is P*(Total - P1)/(Total^2);
    Best is 0.25), !.
best_value(Evalfn,_,[P,_,L|Rest],Best):-
  L1 is L + 1,  % need at least 1 extra literal to achieve best value
  evalfn(Evalfn,[P,0,L1|Rest],Best).


get_nextbest(S,NodeRef):-
        arg(22,S,Search),
  select_nextbest(Search,NodeRef).

% Select the next best node
% Incorporates the changes made by Filip Zelezny to
% achieve the `randomised rapid restart' (or rrr) technique
% within randomised local search
select_nextbest(rls,NodeRef):-
  retractall('$aleph_search'(nextnode,_)),
        aleph5_setting(rls_type,Type),
        (retract('$aleph_search'(rls_parentstats,stats(PStats,_,_))) -> true; true),
        (rls_nextbest(Type,PStats,NodeRef,Label) ->
                asserta('$aleph_search'(rls_parentstats,stats(Label,[],[]))),
                aleph5_setting(rls_type,RlsType),
                (RlsType = rrr ->
                      true;
                      assertz('$aleph_search'(nextnode,NodeRef)));
                NodeRef = none), !.
select_nextbest(_,NodeRef):-
  retractall('$aleph_search'(nextnode,_)),
  get_nextbest(NodeRef), !.
select_nextbest(_,none).

get_nextbest(NodeRef):-
        '$aleph_search'(openlist,[H|_]),
  H = [K1|K2],
        retract('$aleph_search_gain'(K1,K2,NodeRef,_)),
        assertz('$aleph_search'(nextnode,NodeRef)).
get_nextbest(NodeRef):-
        retract('$aleph_search'(openlist,[_|T])),
        asserta('$aleph_search'(openlist,T)),
        get_nextbest(NodeRef), !.
get_nextbest(none).

rls_nextbest(rrr,_,NodeRef,_):-
        get_nextbest(NodeRef).
rls_nextbest(gsat,_,NodeRef,Label):-
        retract('$aleph_search'(openlist,[H|_])),
  H = [K1|K2],
  asserta('$aleph_search'(openlist,[])),
  findall(N-L,'$aleph_search_gain'(K1,K2,N,L),Choices),
  length(Choices,Last),
  get_random(Last,N),
  nth0(N,Choices,NodeRef-Label,_),
  retractall('$aleph_search_gain'(_,_,_,_)).
rls_nextbest(wsat,PStats,NodeRef,Label):-
  aleph5_setting(walk,WProb),
  aleph_random(P),
  P >= WProb, !,
  rls_nextbest(gsat,PStats,NodeRef,Label).
rls_nextbest(wsat,PStats,NodeRef,Label):-
  p_message('random walk'),
        retract('$aleph_search'(openlist,_)),
  asserta('$aleph_search'(openlist,[])),
  findall(N-L,'$aleph_search_gain'(_,_,N,L),AllNodes),
  potentially_good(AllNodes,PStats,Choices),
        length(Choices,Last),
        get_random(Last,N),
        nth0(N,Choices,NodeRef-Label,_),
  retractall('$aleph_search_gain'(_,_,_,_)).
rls_nextbest(anneal,[P,N|_],NodeRef,Label):-
  aleph5_setting(temperature,Temp),
        retract('$aleph_search'(openlist,_)),
  asserta('$aleph_search'(openlist,[])),
  findall(N-L,'$aleph_search_gain'(_,_,N,L),AllNodes),
  length(AllNodes,Last),
  get_random(Last,S),
  nth0(S,AllNodes,NodeRef-Label,_),
  Label = [P1,N1|_],
  Gain is (P1 - N1) - (P - N),
  ((P = 1); (Gain >= 0);(aleph_random(R), R < exp(Gain/Temp))).

potentially_good([],_,[]).
potentially_good([H|T],Label,[H|T1]):-
  H = _-Label1,
  potentially_good(Label,Label1), !,
  potentially_good(T,Label,T1).
potentially_good([_|T],Label,T1):-
  potentially_good(T,Label,T1).

potentially_good([1|_],[P1|_]):-
  !,
  P1 > 1.
potentially_good([P,_,L|_],[P1,_,L1|_]):-
  L1 =< L, !,
  P1 > P.
potentially_good([_,N|_],[_,N1|_]):-
  N1 < N.



% PROVE

% prove with caching
% if entry exists in cache, then return it
% otherwise find and cache cover
% if ``exact'' flag is set then only check proof for examples
% in the part left over due to lazy theorem-proving
% ideas in caching developed in discussions with James Cussens

prove_cache(exact,S,Type,Entry,Clause,Intervals,IList,Count):-
  !,
  (Intervals = Exact/Left ->
          arg(14,S,Depth),
          arg(29,S,Time),
          arg(34,S,Proof),
          prove(Depth/Time/Proof,Type,Clause,Left,IList1,Count1),
    append(Exact, IList1, IList),
    interval_count(Exact,Count0),
    Count is Count0 + Count1;
    IList = Intervals,
    interval_count(IList,Count)),
        arg(8,S,Caching),
        (Caching = true -> add_cache(Entry,Type,IList); true).
prove_cache(upper,S,Type,Entry,Clause,Intervals,IList,Count):-
        arg(8,S,Caching),
        Caching = true, !,
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
        (check_cache(Entry,Type,Cached)->
                prove_cached(S,Type,Entry,Cached,Clause,Intervals,IList,Count);
                prove_intervals(Depth/Time/Proof,Type,Clause,Intervals,IList,Count),
                add_cache(Entry,Type,IList)).
prove_cache(upper,S,Type,_,Clause,Intervals,IList,Count):-
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
  (Intervals = Exact/Left ->
    append(Exact, Left, IList1),
          prove(Depth/Time/Proof,Type,Clause,IList1,IList,Count);
          prove(Depth/Time/Proof,Type,Clause,Intervals,IList,Count)).

prove_intervals(DepthTime,Type,Clause,I1/Left,IList,Count):-
  !,
  append(I1, Left, Intervals),
  prove(DepthTime,Type,Clause,Intervals,IList,Count).
prove_intervals(DepthTime,Type,Clause,Intervals,IList,Count):-
  prove(DepthTime,Type,Clause,Intervals,IList,Count).

prove_cached(S,Type,Entry,I1/Left,Clause,Intervals,IList,Count):-
        !,
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
        prove(Depth/Time/Proof,Type,Clause,Left,I2,_),
        append(I1, I2, I),
        (Type = pos ->
                arg(5,S,Greedy),
                (Greedy = true ->
                        intervals_intersection(I,Intervals,IList);
                        IList = I);
                IList = I),
        interval_count(IList,Count),
        update_cache(Entry,Type,IList).
prove_cached(S,Type,Entry,I1,_,Intervals,IList,Count):-
  (Type = pos -> arg(5,S,Greedy),
    (Greedy = true ->
      intervals_intersection(I1,Intervals,IList);
      IList = I1);
    IList = I1),
  interval_count(IList,Count),
  update_cache(Entry,Type,IList).

% prove at most Max atoms
prove_cache(exact,S,Type,Entry,Clause,Intervals,Max,IList,Count):-
  !,
  (Intervals = Exact/Left ->
    interval_count(Exact,Count0),
    Max1 is Max - Count0,
          arg(12,S,LNegs),
          arg(14,S,Depth),
          arg(29,S,Time),
          arg(34,S,Proof),
          prove(LNegs/false,Depth/Time/Proof,Type,Clause,Left,Max1,IList1,Count1),
    append(Exact, IList1, Exact1),
    find_lazy_left(S,Type,Exact1,Left1),
    IList = Exact1/Left1,
    Count is Count0 + Count1;
    IList = Intervals,
    interval_count(Intervals,Count)),
        arg(8,S,Caching),
        (Caching = true -> add_cache(Entry,Type,IList); true).
prove_cache(upper,S,Type,Entry,Clause,Intervals,Max,IList,Count):-
        arg(8,S,Caching),
        Caching = true, !,
        (check_cache(Entry,Type,Cached)->
                prove_cached(S,Type,Entry,Cached,Clause,Intervals,Max,IList,Count);
                (prove_intervals(S,Type,Clause,Intervals,Max,IList1,Count)->
                        find_lazy_left(S,Type,IList1,Left1),
                        add_cache(Entry,Type,IList1/Left1),
      IList = IList1/Left1,
                        retractall('$aleph_local'(example_cache,_));
                        collect_example_cache(IList),
                        add_cache(Entry,Type,IList),
                        fail)).
prove_cache(upper,S,Type,_,Clause,Intervals,Max,IList/Left1,Count):-
        arg(8,S,Caching),
        arg(12,S,LNegs),
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
  (Intervals = Exact/Left ->
    append(Exact, Left, IList1),
          prove(LNegs/Caching,Depth/Time/Proof,Type,Clause,IList1,Max,IList,Count);
          prove(LNegs/Caching,Depth/Time/Proof,Type,Clause,Intervals,Max,IList,Count)),
  find_lazy_left(S,Type,IList,Left1).

prove_intervals(S,Type,Clause,I1/Left,Max,IList,Count):-
        !,
        arg(8,S,Caching),
        arg(12,S,LNegs),
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
        append(I1, Left, Intervals),
        prove(LNegs/Caching,Depth/Time/Proof,Type,Clause,Intervals,Max,IList,Count).
prove_intervals(S,Type,Clause,Intervals,Max,IList,Count):-
        arg(8,S,Caching),
        arg(12,S,LNegs),
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
        prove(LNegs/Caching,Depth/Time/Proof,Type,Clause,Intervals,Max,IList,Count).


prove_cached(S,Type,Entry, I1/Left,Clause,_,Max,IList/Left1,Count):-
        !,
        arg(8,S,Caching),
        arg(12,S,LNegs),
        arg(14,S,Depth),
        arg(29,S,Time),
        arg(34,S,Proof),
        interval_count(I1,C1),
        Max1 is Max - C1,
        Max1 >= 0,
        (prove(LNegs/Caching,Depth/Time/Proof,Type,Clause,Left,Max1,I2,C2)->
                append(I1, I2, IList),
                Count is C2 + C1,
                find_lazy_left(S,Type,IList,Left1),
                update_cache(Entry,Type,IList/Left1),
                retractall('$aleph_local'(example_cache,_));
                collect_example_cache(I2/Left1),
                append(I1, I2, IList),
                update_cache(Entry,Type,IList/Left1),
                fail).
prove_cached(_,neg,_, I1/L1,_,_,_,I1/L1,C1):-
  !,
  interval_count(I1,C1).
prove_cached(S,_,_,I1,_,_,Max,I1,C1):-
  interval_count(I1,C1),
  arg(12,S,LNegs),
  (LNegs = true ->true; C1 =< Max).

collect_example_cache(Intervals/Left):-
  retract('$aleph_local'(example_cache,[Last|Rest])),
  reverse([Last | Rest], IList),
  list_to_intervals1(IList,Intervals),
  Next is Last + 1,
  '$aleph_global'(size,size(neg,LastN)),
  (Next > LastN -> Left = []; Left = [Next-LastN]).

find_lazy_left(S,_,_,[]):-
        arg(12,S,LazyNegs),
        LazyNegs = false, !.
find_lazy_left(_,_,[],[]).
find_lazy_left(S,Type,[_-F],Left):-
        !,
        F1 is F + 1,
  (Type = pos -> arg(16,S,Last);
    (Type = neg -> arg(24,S,Last);
      (Type = rand -> arg(20,S,Last); Last = F))),
        (F1 > Last -> Left = []; Left = [F1-Last]).
find_lazy_left(S,Type,[_|T1],Left):-
        find_lazy_left(S,Type,T1,Left).


% prove atoms specified by Type and index set using Clause.
% dependent on data structure used for index set:
% currently index set is a list of intervals
% return atoms proved and their count
% if tail-recursive version is needed see below

prove(_,_,_,[],[],0).
prove(Flags,Type,Clause,[Interval|Intervals],IList,Count):-
  index_prove(Flags,Type,Clause,Interval,I1,C1),
  prove(Flags,Type,Clause,Intervals,I2,C2),
  append(I1, I2, IList),
  Count is C1 + C2.



% TAIL-RECURSIVE PROVE/6

% use this rather than the prove/6 above for tail recursion
% written by James Cussens

% code for tail recursive cover testing starts here
% when we know that Sofar is a variable.
prove2([],_,_,_,Count,[],Count).
prove2([Current-Finish|Intervals],Depth/Time/Proof,Type,(Head:-Body),InCount,Sofar,OutCount):-
  example(Current,Type,Example),
  \+ prove1(Proof,Depth/Time,Example,(Head:-Body)), %uncovered
        !,
        (Current>=Finish ->
            prove2(Intervals,Depth/Time/Proof,Type,(Head:-Body),InCount,Sofar,OutCount);
            Next is Current+1,!,
            prove2([Next-Finish|Intervals],Depth/Time/Proof,Type,(Head:-Body),InCount,Sofar,OutCount)
        ).
prove2([Current-Finish|Intervals],ProofFlags,Type,Clause,InCount,Sofar,OutCount):-
        (Current>=Finish ->
            Sofar=[Current-Current|Rest],
            MidCount is InCount+1,!,
            prove2(Intervals,ProofFlags,Type,Clause,MidCount,Rest,OutCount);
            Next is Current+1,
            Sofar=[Current-_Last|_Rest],!,
            prove3([Next-Finish|Intervals],ProofFlags,Type,Clause,InCount,Sofar,OutCount)
        ).

%when Sofar is not a variable
prove3([Current-Finish|Intervals],Depth/Time/Proof,Type,(Head:-Body),InCount,Sofar,OutCount):-
  example(Current,Type,Example),
  \+ prove1(Proof,Depth/Time,Example,(Head:-Body)), %uncovered
        !,
        Last is Current-1, %found some previously
        Sofar=[Start-Last|Rest], %complete found interval
        MidCount is InCount+Current-Start,
        (Current>=Finish ->
            prove2(Intervals,Depth/Time/Proof,Type,(Head:-Body),MidCount,Rest,OutCount);
            Next is Current+1,!,
            prove2([Next-Finish|Intervals],Depth/Time/Proof,Type,(Head:-Body),MidCount,Rest,OutCount)
        ).
prove3([Current-Finish|Intervals],ProofFlags,Type,Clause,InCount,Sofar,OutCount):-
        (Current>=Finish ->
            Sofar=[Start-Finish|Rest],
            MidCount is InCount+Finish-Start+1,!,
            prove2(Intervals,ProofFlags,Type,Clause,MidCount,Rest,OutCount);
            Next is Current+1,!,
            prove3([Next-Finish|Intervals],ProofFlags,Type,Clause,InCount,Sofar,OutCount)
        ).

% code for tail recursive cover testing ends here

index_prove(_,_,_,Start-Finish,[],0):-
  Start > Finish, !.
index_prove(ProofFlags,Type,Clause,Start-Finish,IList,Count):-
  index_prove1(ProofFlags,Type,Clause,Start,Finish,Last),
  Last0 is Last - 1 ,
  Last1 is Last + 1,
  (Last0 >= Start->
    index_prove(ProofFlags,Type,Clause,Last1-Finish,Rest,Count1),
    IList = [Start-Last0|Rest],
    Count is Last - Start + Count1;
    index_prove(ProofFlags,Type,Clause,Last1-Finish,IList,Count)).

prove1(G):-
  depth_bound_call(G), !.

prove1(user,_,Example,Clause):-
  prove(Clause,Example), !.
prove1(restricted_sld,Depth/Time,Example,(Head:-Body)):-
  \+((\+(((Example = Head),resource_bound_call(Time,Depth,Body))))), !.
prove1(sld,Depth/Time,Example,_):-
  \+(\+(resource_bound_call(Time,Depth,Example))), !.

index_prove1(_,_,_,Num,Last,Num):-
  Num > Last, !.
index_prove1(Depth/Time/Proof,Type,Clause,Num,Finish,Last):-
  example(Num,Type,Example),
  prove1(Proof,Depth/Time,Example,Clause), !,
  Num1 is Num + 1,
  index_prove1(Depth/Time/Proof,Type,Clause,Num1,Finish,Last).
index_prove1(_,_,_,Last,_,Last).


% proves at most Max atoms using Clause.

prove(_,_,_,_,[],_,[],0).
prove(Flags,ProofFlags,Type,Clause,[Interval|Intervals],Max,IList,Count):-
        index_prove(Flags,ProofFlags,Type,Clause,Interval,Max,I1,C1), !,
        Max1 is Max - C1,
        prove(Flags,ProofFlags,Type,Clause,Intervals,Max1,I2,C2),
        append(I1, I2, IList),
        Count is C1 + C2.


index_prove(_,_,_,_,Start-Finish,_,[],0):-
        Start > Finish, !.
index_prove(Flags,ProofFlags,Type,Clause,Start-Finish,Max,IList,Count):-
        index_prove1(Flags,ProofFlags,Type,Clause,Start,Finish,0,Max,Last),
        Last0 is Last - 1 ,
        Last1 is Last + 1,
        (Last0 >= Start->
                Max1 is Max - Last + Start,
    ((Max1 = 0, Flags = true/_) ->
                        Rest = [], Count1 = 0;
                  index_prove(Flags,ProofFlags,Type,Clause,Last1-Finish,
          Max1,Rest,Count1)),
                IList = [Start-Last0|Rest],
                Count is Last - Start + Count1;
                index_prove(Flags,ProofFlags,Type,Clause,Last1-Finish,Max,IList,Count)).

index_prove1(false/_,_,_,_,_,_,Proved,Allowed,_):-
        Proved > Allowed, !, fail.
index_prove1(_,_,_,_,Num,Last,_,_,Num):-
        Num > Last, !.
index_prove1(true/_,_,_,_,Num,_,Allowed,Allowed,Num):- !.
index_prove1(LNegs/Caching,Depth/Time/Proof,Type,Clause,Num,Finish,Proved,Allowed,Last):-
  example(Num,Type,Example),
  prove1(Proof,Depth/Time,Example,Clause), !,
        Num1 is Num + 1,
        Proved1 is Proved + 1,
        (Caching = true ->
                (retract('$aleph_local'(example_cache,L)) ->
                        asserta('$aleph_local'(example_cache,[Num|L]));
                        asserta('$aleph_local'(example_cache,[Num])));
                true),
        index_prove1(LNegs/Caching,Depth/Time/Proof,Type,Clause,Num1,Finish,Proved1,Allowed,Last).
index_prove1(_,_,_,_,Last,_,_,_,Last).

% resource_bound_call(Time,Depth,Goals)
%  attempt to prove Goals using depth bounded theorem-prover
%  in at most Time secs
resource_bound_call(T,Depth,Goals):-
  Inf is 1e10,
  T =:= Inf,
  !,
  depth_bound_call(Goals,Depth).
resource_bound_call(T,Depth,Goals):-
        catch(time_bound_call(T,prooflimit,depth_bound_call(Goals,Depth)),
    prooflimit,fail).

time_bound_call(T,Exception,Goal):-
  alarm(T,throw(Exception),X),
        (Goal -> remove_alarm(X); remove_alarm(X), fail).



% CACHING

clear_cache:-
  retractall('$aleph_search_cache'(_)),
  retractall('$aleph_search_prunecache'(_)).

check_cache(Entry,Type,I):-
  Entry \= false,
        '$aleph_search_cache'(Entry), !,
        functor(Entry,_,Arity),
        (Type = pos -> Arg is Arity - 1; Arg is Arity),
        arg(Arg,Entry,I),
  nonvar(I).

add_cache(false,_,_):- !.
add_cache(Entry,Type,I):-
        (retract('$aleph_search_cache'(Entry))-> true ; true),
        functor(Entry,_,Arity),
        (Type = pos -> Arg is Arity - 1; Arg is Arity),
        (arg(Arg,Entry,I)-> asserta('$aleph_search_cache'(Entry));
                        true), !.

update_cache(Entry,Type,I):-
        Entry \= false,
        functor(Entry,Name,Arity),
        (Type = pos -> Arg is Arity - 1; Arg is Arity),
        arg(Arg,Entry,OldI),
        OldI = _/_,
        retract('$aleph_search_cache'(Entry)),
        functor(NewEntry,Name,Arity),
        Arg0 is Arg - 1,
        copy_args(Entry,NewEntry,1,Arg0),
        arg(Arg,NewEntry,I),
        Arg1 is Arg + 1,
        copy_args(Entry,NewEntry,Arg1,Arity),
        asserta('$aleph_search_cache'(NewEntry)), !.
update_cache(_,_,_).

add_prune_cache(false):-
  !.
add_prune_cache(Entry):-
  (
    setting(caching, true)
  ->
    functor(Entry,_,Arity),
    A1 is Arity - 2,
    arg(A1,Entry,Clause),
    asserta('$aleph_search_prunecache'(Clause))
  ;
    true
  ).

get_cache_entry(Max,Clause,Entry):-
        skolemize(Clause,Head,Body,0,_),
  length(Body,L1),
  Max >= L1 + 1,
        aleph_hash_term([Head|Body],Entry), !.
get_cache_entry(_,_,false).

% upto 3-argument indexing using predicate names in a clause
aleph_hash_term([L0,L1,L2,L3,L4|T],Entry):-
        !,
        functor(L1,P1,_), functor(L2,P2,_),
        functor(L3,P3,_), functor(L4,P4,_),
        functor(Entry,P4,6),
        arg(1,Entry,P2), arg(2,Entry,P3),
        arg(3,Entry,P1), arg(4,Entry,[L0,L1,L2,L3,L4|T]).
aleph_hash_term([L0,L1,L2,L3],Entry):-
        !,
        functor(L1,P1,_), functor(L2,P2,_),
        functor(L3,P3,_),
        functor(Entry,P3,5),
        arg(1,Entry,P2), arg(2,Entry,P1),
        arg(3,Entry,[L0,L1,L2,L3]).
aleph_hash_term([L0,L1,L2],Entry):-
        !,
        functor(L1,P1,_), functor(L2,P2,_),
        functor(Entry,P2,4),
        arg(1,Entry,P1), arg(2,Entry,[L0,L1,L2]).
aleph_hash_term([L0,L1],Entry):-
        !,
        functor(L1,P1,_),
        functor(Entry,P1,3),
        arg(1,Entry,[L0,L1]).
aleph_hash_term([L0],Entry):-
        functor(L0,P0,_),
        functor(Entry,P0,3),
        arg(1,Entry,[L0]).



% TREES

construct_tree(Type):-
  aleph5_setting(searchtime,Time),
  Inf is 1e10,
        Time =\= Inf,
        SearchTime is integer(Time),
        SearchTime > 0, !,
  catch(time_bound_call(SearchTime,searchlimit,find_tree(Type)),
    searchlimit,p_message('Time limit reached')).
construct_tree(Type):-
  find_tree(Type).

% find_tree(Type) where Type is one of
%      classification, regression, class_probability
find_tree(Type):-
  retractall('$aleph_search'(tree,_)),
  retractall('$aleph_search'(tree_besterror,_)),
  retractall('$aleph_search'(tree_gain,_)),
  retractall('$aleph_search'(tree_lastleaf,_)),
  retractall('$aleph_search'(tree_leaf,_)),
  retractall('$aleph_search'(tree_newleaf,_)),
  retractall('$aleph_search'(tree_startdistribution,_)),
  get_start_distribution(Type,Distribution),
  asserta('$aleph_search'(tree_startdistribution,d(Type,Distribution))),
  '$aleph_global'(atoms_left,atoms_left(pos,Pos)),
  aleph5_setting(dependent,Argno),
  p_message('constructing tree'),
  stopwatch(StartClock),
  get_search_settings(S),
  auto_refine(false,Head),
  gen_leaf(Leaf),
  eval_treenode(S,Type,(Head:-true),[Argno],Pos,Examples,N,Cost),
  asserta('$aleph_search'(tree_leaf,l(Leaf,Leaf,[Head,Cost,N],Examples))),
  find_tree1([Leaf],S,Type,[Argno]),
  prune_rules(S,Type,[Argno]),
  stopwatch(StopClock),
  add_tree(S,Type,[Argno]),
  Time is StopClock - StartClock,
  p1_message('construction time'),
  p_message(Time).

get_start_distribution(regression,0-[0,0]):- !.
get_start_distribution(model,0-[0,0]):-
  aleph5_setting(evalfn,mse), !.
get_start_distribution(model,0-Distribution):-
  aleph5_setting(evalfn,accuracy), !,
  (aleph5_setting(classes,Classes) -> true;
    !,
    p_message('missing setting for classes'),
    fail),
  initialise_distribution(Classes,Distribution), !.
get_start_distribution(Tree,0-Distribution):-
  (Tree = classification; Tree = class_probability),
  (aleph5_setting(classes,Classes) -> true;
    !,
    p_message('missing setting for classes'),
    fail),
  initialise_distribution(Classes,Distribution), !.
get_start_distribution(_,_):-
  p_message('incorrect/missing setting for tree_type or evalfn'),
  fail.

initialise_distribution([],[]).
initialise_distribution([Class|Classes],[0-Class|T]):-
  initialise_distribution(Classes,T).

laplace_correct([],[]).
laplace_correct([N-Class|Classes],[N1-Class|T]):-
  N1 is N + 1,
  laplace_correct(Classes,T).

find_tree1([],_,_,_).
find_tree1([Leaf|Leaves],S,Type,Predict):-
  can_split(S,Type,Predict,Leaf,Left,Right), !,
  split_leaf(Leaf,Left,Right,NewLeaves),
  append(Leaves, NewLeaves, LeavesLeft),
  find_tree1(LeavesLeft,S,Type,Predict).
find_tree1([_|LeavesLeft],S,Type,Predict):-
  find_tree1(LeavesLeft,S,Type,Predict).

prune_rules(S,Tree,Predict):-
  aleph5_setting(prune_tree,true),
  prune_rules1(Tree,S,Predict), !.
prune_rules(_,_,_).

% pessimistic pruning by employing corrections to observed errors
prune_rules1(class_probability,_,_):-
  p_message('no pruning for class probability trees'), !.
prune_rules1(model,_,_):-
  p_message('no pruning for model trees'), !.
prune_rules1(Tree,S,Predict):-
  p_message('pruning clauses'),
  '$aleph_search'(tree_leaf,l(Leaf,Parent,Clause,Examples)),
  prune_rule(Tree,S,Predict,Clause,Examples,NewClause,NewExamples),
  retract('$aleph_search'(tree_leaf,l(Leaf,Parent,Clause,Examples))),
  asserta('$aleph_search'(tree_newleaf,l(Leaf,Parent,NewClause,NewExamples))),
  fail.
prune_rules1(_,_,_):-
  retract('$aleph_search'(tree_newleaf,l(Leaf,Parent,NewClause,NewExamples))),
  asserta('$aleph_search'(tree_leaf,l(Leaf,Parent,NewClause,NewExamples))),
  fail.
prune_rules1(_,_,_).

prune_rule(Tree,S,PredictArg,[Clause,_,N],Examples,[PrunedClause,E1,NCov],NewEx):-
  node_stats(Tree,Examples,PredictArg,Total-Distribution),
  leaf_prediction(Tree,Total-Distribution,_,Incorrect),
  estimate_error(Tree,Incorrect,Total,Upper),
  split_clause(Clause,Head,Body),
  goals_to_list(Body,BodyL),
  arg(14,S,Depth),
  arg(29,S,Time),
  arg(34,S,Proof),
  greedy_prune_rule(Tree,Depth/Time/Proof,PredictArg,[Head|BodyL],Upper,C1L,E1),
  list_to_clause(C1L,PrunedClause),
  (E1 < Upper ->
    '$aleph_global'(atoms_left,atoms_left(pos,Pos)),
          prove(Depth/Time/Proof,pos,PrunedClause,Pos,NewEx,NCov);
    NewEx = Examples,
    NCov = N).


% estimate error using binomial distribution as done in C4.5
estimate_error(classification,Incorrect,Total,Error):-
  aleph5_setting(confidence,Conf),
  estimate_error(1.0/0.0,0.0/1.0,Conf,Total,Incorrect,Error).

% estimate upper bound on sample std deviation by
% assuming the n values in a leaf are normally distributed.
% In this case, a (1-alpha)x100 confidence interval for the
% variance is (n-1)s^2/X^2(alpha/2) =< var =< (n-1)s^2/X^2(1-alpha/2)
estimate_error(regression,Sd,1,Sd):- !.
estimate_error(regression,Sd,N,Upper):-
  (aleph5_setting(confidence,Conf) -> true; Conf = 0.95),
  Alpha is 1.0 - Conf,
  DF is N - 1,
  Prob is 1 - Alpha/2,
  chi_square(DF,Prob,ChiSq),
  Upper is Sd*sqrt((N-1)/ChiSq).

bound_error(classification,Error,Total,Lower,Upper):-
  (aleph5_setting(confidence,Alpha) -> true; Alpha = 0.95),
  approx_z(Alpha,Z),
  Lower is Error - Z*sqrt(Error*(1-Error)/Total),
  Upper is Error + Z*sqrt(Error*(1-Error)/Total).

approx_z(P,2.58):- P >= 0.99, !.
approx_z(P,Z):- P >= 0.98, !, Z is 2.33 + (P-0.98)*(2.58-2.33)/(0.99-0.98).
approx_z(P,Z):- P >= 0.95, !, Z is 1.96 + (P-0.95)*(2.33-1.96)/(0.98-0.95).
approx_z(P,Z):- P >= 0.90, !, Z is 1.64 + (P-0.90)*(1.96-1.64)/(0.95-0.90).
approx_z(P,Z):- P >= 0.80, !, Z is 1.28 + (P-0.80)*(1.64-1.28)/(0.90-0.80).
approx_z(P,Z):- P >= 0.68, !, Z is 1.00 + (P-0.68)*(1.28-1.00)/(0.80-0.68).
approx_z(P,Z):- P >= 0.50, !, Z is 0.67 + (P-0.50)*(1.00-0.67)/(0.68-0.50).
approx_z(_,0.67).

greedy_prune_rule(Tree,Flags,PredictArg,Clause,Err0,NewClause,BestErr):-
  greedy_prune_rule1(Tree,Flags,PredictArg,Clause,Err0,Clause1,Err1),
  Clause \= Clause1, !,
  greedy_prune_rule(Tree,Flags,PredictArg,Clause1,Err1,NewClause,BestErr).
greedy_prune_rule(_,_,_,C,E,C,E).

greedy_prune_rule1(Tree,Flags,PredictArg,[Head|Body],Err0,_,_):-
  retractall('$aleph_search'(tree_besterror,_)),
  asserta('$aleph_search'(tree_besterror,besterror([Head|Body],Err0))),
  '$aleph_global'(atoms_left,atoms_left(pos,Pos)),
  aleph_delete(_,Body,Left),
  strip_negs(Left,Body1),
  aleph_mode_linked([Head|Body1]),
  list_to_clause([Head|Left],Clause),
        prove(Flags,pos,Clause,Pos,Ex1,_),
  node_stats(Tree,Ex1,PredictArg,Total-Distribution),
  leaf_prediction(Tree,Total-Distribution,_,Incorrect),
  estimate_error(Tree,Incorrect,Total,Upper),
  '$aleph_search'(tree_besterror,besterror(_,BestError)),
  Upper =< BestError,
  retract('$aleph_search'(tree_besterror,besterror(_,BestError))),
  asserta('$aleph_search'(tree_besterror,besterror([Head|Left],Upper))),
  fail.
greedy_prune_rule1(_,_,_,_,_,Clause1,Err1):-
  retract('$aleph_search'(tree_besterror,besterror(Clause1,Err1))).

strip_negs([],[]).
strip_negs([not(L)|T],[L|T1]):-
  !,
  strip_negs(T,T1).
strip_negs([L|T],[L|T1]):-
  strip_negs(T,T1).

add_tree(_,Tree,Predict):-
  retract('$aleph_search'(tree_leaf,l(_,_,Leaf,Examples))),
  Leaf = [Clause,Cost,P],
  add_prediction(Tree,Clause,Predict,Examples,Clause1),
  p_message('best clause'),
  pp_dclause(Clause1),
        nlits(Clause,L),
  Gain is -Cost,
        asserta('$aleph_global'(hypothesis,hypothesis([P,0,L,Gain],Clause1,Examples,[]))),
  addhyp,
  fail.
add_tree(_,_,_).

add_prediction(Tree,Clause,PredictArg,Examples,Clause1):-
  split_clause(Clause,Head,_),
  (Tree = model ->
    aleph5_setting(evalfn,Evalfn),
    add_model(Evalfn,Clause,PredictArg,Examples,Clause1,_,_);
    node_stats(Tree,Examples,PredictArg,Distribution),
    leaf_prediction(Tree,Distribution,Prediction,Error),
    tparg(PredictArg,Head,Var),
    add_prediction(Tree,Clause,Var,Prediction,Error,Clause1)).

add_prediction(classification,Clause,Var,Prediction,_,Clause1):-
  extend_clause(Clause,(Var = Prediction),Clause1).
add_prediction(class_probability,Clause,Var,Prediction,_,Clause1):-
  extend_clause(Clause,(random(Var,Prediction)),Clause1).
add_prediction(regression,Clause,Var,Mean,Sd,Clause1):-
  extend_clause(Clause,(random(Var,normal(Mean,Sd))),Clause1).

add_model(Evalfn,Clause,PredictArg,Examples,_,_,_):-
  retractall('$aleph_local'(tree_model,_,_,_)),
  Best is 1e10,
  split_clause(Clause,Head,_),
  tparg(PredictArg,Head,Var),
  asserta('$aleph_local'(tree_model,false,0,Best)),
  '$aleph_global'(model,model(Name/Arity)),
  functor(Model,Name,Arity),
  auto_extend(Clause,Model,C),
  leaf_predicts(Arity,Model,Var),
  lazy_evaluate_refinement([],C,[Name/Arity],Examples,[],[],C1),
  find_model_error(Evalfn,Examples,C1,PredictArg,Total,Error),
  '$aleph_local'(tree_model,_,_,BestSoFar),
  (Error < BestSoFar ->
    retract('$aleph_local'(tree_model,_,_,_)),
    asserta('$aleph_local'(tree_model,C1,Total,Error));
    true),
  fail.
add_model(_,_,_,_,Clause,Total,Error):-
  retract('$aleph_local'(tree_model,Clause,Total,Error)).


find_model_error(Evalfn,Examples,(Head:-Body),[PredictArg],T,E):-
  functor(Head,_,Arity),
  findall(Actual-Pred,
      (member(Interval,Examples),
      aleph_member3(N,Interval),
      example(N,pos,Example),
      copy_iargs(Arity,Example,Head,PredictArg),
      once(Body),
      arg(PredictArg,Head,Pred),
      arg(PredictArg,Example,Actual)
      ),
    L),
  sum_model_errors(L,Evalfn,0,0.0,T,E), !.

sum_model_errors([],_,N,E,N,E).
sum_model_errors([Act-Pred|T],Evalfn,NSoFar,ESoFar,N,E):-
  get_model_error(Evalfn,Act,Pred,E1),
  E1SoFar is ESoFar + E1,
  N1SoFar is NSoFar + 1,
  sum_model_errors(T,Evalfn,N1SoFar,E1SoFar,N,E).

get_model_error(mse,Act,Pred,E):-
  E is (Act-Pred)^2.
get_model_error(accuracy,Act,Pred,E):-
  (Act = Pred -> E is 0.0; E is 1.0).

leaf_predicts(0,_,_):- !, fail.
leaf_predicts(Arg,Model,Var):-
  arg(Arg,Model,Var1),
  var(Var1),
  Var1 == Var, !.
leaf_predicts(Arg,Model,Var):-
  Arg1 is Arg - 1,
  leaf_predicts(Arg1,Model,Var).

leaf_prediction(classification,Total-Distribution,Class,Incorrect):-
  find_maj_class(Distribution,N-Class),
  Incorrect is Total - N.
leaf_prediction(class_probability,T1-D1,NDistr,0):-
  length(D1,NClasses),
  laplace_correct(D1,LaplaceD1),
  LaplaceTotal is T1 + NClasses,
  normalise_distribution(LaplaceD1,LaplaceTotal,NDistr).
leaf_prediction(regression,_-[Mean,Sd],Mean,Sd).

find_maj_class([X],X):- !.
find_maj_class([N-Class|Rest],MajClass):-
  find_maj_class(Rest,N1-C1),
  (N > N1 -> MajClass = N-Class; MajClass = N1-C1).

can_split(S,Type,Predict,Leaf,Left,Right):-
  arg(21,S,MinGain),
  '$aleph_search'(tree_leaf,l(Leaf,_,[Clause,Cost,N],Examples)),
  Cost >= MinGain,
  get_best_subtree(S,Type,Predict,[Clause,Cost,N],Examples,Gain,Left,Right),
  Gain >= MinGain,
  p_message('found clauses'),
  Left = [ClF,CostF|_], Right = [ClS,CostS|_],
  arg(4,S,_/Evalfn),
  pp_dclause(ClS),
  print_eval(Evalfn,CostS),
  pp_dclause(ClF),
  print_eval(Evalfn,CostF),
  p1_message('expected cost reduction'),
  p_message(Gain).

get_best_subtree(S,Type,Predict,[Clause,Cost,N],Examples,Gain,Left,Right):-
  arg(42,S,Interactive),
  arg(43,S,LookAhead),
  retractall('$aleph_search'(tree_gain,_)),
  MInf is -1e10,
  (Interactive = false ->
    asserta('$aleph_search'(tree_gain,tree_gain(MInf,[],[])));
    true),
  split_clause(Clause,Head,Body),
  arg(4,S,_/Evalfn),
  arg(13,S,MinPos),
  auto_refine(LookAhead,Clause,ClS),
  tree_refine_ok(Type,ClS),
  eval_treenode(S,Type,ClS,Predict,Examples,ExS,NS,CostS),
  NS >= MinPos,
  rm_intervals(ExS,Examples,ExF),
  split_clause(ClS,Head,Body1),
  get_goaldiffs(Body,Body1,Diff),
  extend_clause(Clause,not(Diff),ClF),
  eval_treenode(S,Type,ClF,Predict,ExF,NF,CostF),
  NF >= MinPos,
  AvLeafCost is (NS*CostS + NF*CostF)/N,
  CostReduction is Cost - AvLeafCost,
  (Interactive = false ->
    pp_dclause(ClS), print_eval(Evalfn,CostS),
    pp_dclause(ClF), print_eval(Evalfn,CostF),
    p1_message('expected cost reduction'),
  p_message(CostReduction),
    '$aleph_search'(tree_gain,tree_gain(BestSoFar,_,_)),
    CostReduction > BestSoFar,
    retract('$aleph_search'(tree_gain,tree_gain(BestSoFar,_,_))),
    asserta('$aleph_search'(tree_gain,tree_gain(CostReduction,
              [ClF,CostF,NF,ExF],
              [ClS,CostS,NS,ExS])));
    asserta('$aleph_search'(tree_gain,tree_gain(CostReduction,
              [ClF,CostF,NF,ExF],
              [ClS,CostS,NS,ExS])))),

  AvLeafCost =< 0.0,
  !,
  get_best_subtree(Interactive,Clause,Gain,Left,Right).
get_best_subtree(S,_,_,[Clause|_],_,Gain,Left,Right):-
  arg(42,S,Interactive),
  get_best_subtree(Interactive,Clause,Gain,Left,Right).

get_best_subtree(false,_,Gain,Left,Right):-
  retract('$aleph_search'(tree_gain,tree_gain(Gain,Left,Right))), !.
get_best_subtree(true,Clause,Gain,Left,Right):-
  current_stream(Stream),
  format(Stream, '\nExtending path: \n---------------\n', []),
  pp_dclause(Stream, Clause),
  findall(
    MCR-[Left,Right],
    (
      '$aleph_search'(tree_gain,tree_gain(CostReduction,Left,Right)),
      MCR is -1*CostReduction
    ),
    SplitsList
  ),
  keysort(SplitsList,Sorted),
  get_best_split(Clause,Sorted,Gain,Left,Right),
  retractall('$aleph_search'(tree_gain,_)).

get_best_split(Clause,Splits,Gain,Left,Right):-
  current_stream(Stream),
  show_split_list(Stream,Clause,Splits),
  ask_best_split(Splits,Gain,Left,Right).

show_split_list(Stream,Clause,Splits):-
  format(
    Stream,
    '\t\t\t\tSplit Information\n\t\t\t\t-----------------\n\n\t\t\t\tNo.\t\t\t\tSplit\n\t\t\t\t---\t\t\t\t-----\n',
    []
  ),
  show_split_list(Stream,Splits,1,Clause).

show_split_list(_Stream, [], _, _).
show_split_list(
  Stream,
  [MCR-[[_,_,NF,_],[CLS,_,NS,_]]|Rest],
  SplitNum,
  Clause
):-
  copy_term(Clause,ClauseCopy),
  split_clause(ClauseCopy,Head,Body),
  copy_term(CLS,CLSCopy),
  numbervars(CLSCopy,0,_),
  split_clause(CLSCopy,Head,Body1),
  get_goaldiffs(Body,Body1,Diff),
  Gain is -1*MCR,
  format(Stream, '\t\t\t\t~w\t\t\t\t~w\n', [SplitNum, Diff]),
  tab(Stream, 12),
  format(Stream, 'Succeeded (Right Branch): ~w\n', [NS]),
  tab(Stream, 12),
  format(Stream, 'Failed    (Left Branch) : ~w\n', [NF]),
  tab(Stream, 12),
  format(Stream, 'Cost Reduction          : ~w\n\n', [Gain]),
  NextSplit is SplitNum + 1,
  show_split_list(Rest,NextSplit,Clause).

ask_best_split(Splits,Gain,Left,Right):-
  repeat,
  current_stream(Stream),
  format(Stream, '\t\t\t\t-> Select Split Number (or "none.")\n', []),
  read(Answer),
  (
    Answer = none
  ->
    Gain is -1e10,
    Left = [],
    Right = [];
    SplitNum is integer(Answer),
    nth0(SplitNum,Splits,MCR-[Left,Right],_),
    Gain is -1*MCR
  ),
  !.

tree_refine_ok(model,Clause):-
        '$aleph_global'(model,model(Name/Arity)),
  functor(Model,Name,Arity),
  in(Clause,Model), !,
  fail.
tree_refine_ok(_,_).


eval_treenode(S,Tree,Clause,PredictArg,PCov,N,Cost):-
  arg(4,S,_/Evalfn),
  treenode_cost(Tree,Evalfn,Clause,PCov,PredictArg,N,Cost).

eval_treenode(S,Tree,Clause,PredictArg,Pos,PCov,N,Cost):-
  arg(4,S,_/Evalfn),
  arg(13,S,MinPos),
  arg(14,S,Depth),
  arg(29,S,Time),
  arg(34,S,Proof),
        prove(Depth/Time/Proof,pos,Clause,Pos,PCov,PCount),
  PCount >= MinPos,
  treenode_cost(Tree,Evalfn,Clause,PCov,PredictArg,N,Cost).

treenode_cost(model,Evalfn,Clause,Covered,PredictArg,Total,Cost):-
  !,
  add_model(Evalfn,Clause,PredictArg,Covered,_,Total,Cost).
treenode_cost(Tree,Evalfn,_,Covered,PredictArg,Total,Cost):-
  node_stats(Tree,Covered,PredictArg,Total-Distribution),
  Total > 0,
  impurity(Tree,Evalfn,Total-Distribution,Cost).

node_stats(Tree,Covered,PredictArg,D):-
        '$aleph_search'(tree_startdistribution,d(Tree,D0)),
        (Tree = regression ->
                cont_distribution(Covered,PredictArg,D0,D);
                discr_distribution(Covered,PredictArg,D0,D)).

discr_distribution([],_,D,D).
discr_distribution([S-F|Intervals],PredictArg,T0-D0,D):-
  discr_distribution(S,F,PredictArg,T0-D0,T1-D1),
  discr_distribution(Intervals,PredictArg,T1-D1,D).

discr_distribution(N,F,_,D,D):- N > F, !.
discr_distribution(N,F,PredictArg,T0-D0,D):-
  example(N,pos,Example),
  tparg(PredictArg,Example,Actual),
  N1 is N + 1,
  T1 is T0 + 1,
  (aleph_delete(C0-Actual,D0,D1) ->
    C1 is C0 + 1,
    discr_distribution(N1,F,PredictArg,T1-[C1-Actual|D1],D);
    discr_distribution(N1,F,PredictArg,T1-[1-Actual|D0],D)).

cont_distribution([],_,T-[S,SS],T-[Mean,Sd]):-
  (T = 0 -> Mean = 0, Sd = 0;
    Mean is S/T,
    Sd is sqrt(SS/T - Mean*Mean)).
cont_distribution([S-F|Intervals],PredictArg,T0-D0,D):-
        cont_distribution(S,F,PredictArg,T0-D0,T1-D1),
        cont_distribution(Intervals,PredictArg,T1-D1,D).

cont_distribution(N,F,_,D,D):- N > F, !.
cont_distribution(N,F,PredictArg,T0-[S0,SS0],D):-
        example(N,pos,Example),
        tparg(PredictArg,Example,Actual),
  N1 is N + 1,
        T1 is T0 + 1,
  S1 is S0 + Actual,
  SS1 is SS0 + Actual*Actual,
        cont_distribution(N1,F,PredictArg,T1-[S1,SS1],D).

impurity(regression,sd,_-[_,Sd],Sd):- !.
impurity(classification,entropy,Total-Distribution,Cost):-
  sum_entropy(Distribution,Total,S),
  Cost is -S/(Total*log(2)), !.
impurity(classification,gini,Total-Distribution,Cost):-
  sum_gini(Distribution,Total,Cost), !.
impurity(class_probability,entropy,Total-Distribution,Cost):-
  sum_entropy(Distribution,Total,S),
  Cost is -S/(Total*log(2)), !.
impurity(class_probability,gini,Total-Distribution,Cost):-
  sum_gini(Distribution,Total,Cost), !.
impurity(_,_,_,_):-
  err_message('inappropriate settings for tree_type and/or evalfn'),
  fail.


sum_gini([],_,0).
sum_gini([N-_|Rest],Total,Sum):-
  N > 0, !,
  sum_gini(Rest,Total,C0),
  P is N/Total,
  Sum is P*(1-P) + C0.
sum_gini([_|Rest],Total,Sum):-
  sum_gini(Rest,Total,Sum).

sum_entropy([],_,0).
sum_entropy([N-_|Rest],Total,Sum):-
  N > 0, !,
  sum_entropy(Rest,Total,C0),
  Sum is N*log(N/Total) + C0.
sum_entropy([_|Rest],Total,Sum):-
  sum_entropy(Rest,Total,Sum).

% only binary splits
% left = condition at node fails
% right = condition at node succeeds
split_leaf(Leaf,LeftTree,RightTree,[Left,Right]):-
  retract('$aleph_search'(tree_leaf,l(Leaf,Parent,
            [Clause,Cost,N],Examples))),
  gen_leaf(Left),
  gen_leaf(Right),
  LeftTree = [ClF,CostF,NF,ExF],
  RightTree = [ClS,CostS,NS,ExS],
  asserta('$aleph_search'(tree,t(Leaf,Parent,[Clause,Cost,N],
          Examples,Left,Right))),
  asserta('$aleph_search'(tree_leaf,l(Left,Leaf,[ClF,CostF,NF],ExF))),
  asserta('$aleph_search'(tree_leaf,l(Right,Leaf,[ClS,CostS,NS],ExS))).

gen_leaf(Leaf1):-
  retract('$aleph_search'(tree_lastleaf,Leaf0)), !,
  Leaf1 is Leaf0 + 1,
  asserta('$aleph_search'(tree_lastleaf,Leaf1)).
gen_leaf(0):-
        asserta('$aleph_search'(tree_lastleaf,0)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% G C W S

% examine list of clauses to be specialised
% generate an exception theory for each clause that covers negative examples
gcws:-
  aleph5_setting(evalfn,EvalFn),
  repeat,
  retract('$aleph_search'(sphyp,hypothesis([P,N,L|T],Clause,PCover,NCover))),
  (PCover = _/_ -> label_create(pos,Clause,Label1),
    extract_pos(Label1,PCover1),
    interval_count(PCover1,P1);
    PCover1 = PCover,
    P1 = P),
  (NCover = _/_ -> label_create(neg,Clause,Label2),
    extract_neg(Label2,NCover1),
    interval_count(NCover1,N1);
    NCover1 = NCover,
    N1 = N),
  (N1 = 0 -> NewClause = Clause, NewLabel = [P1,N1,L|T];
    MinAcc is P1/(2*P1 - 1),
    aleph5_set_setting(minacc,MinAcc),
    aleph5_set_setting(noise,N1),
    gcws(Clause,PCover1,NCover1,NewClause),
    L1 is L + 1,
    complete_label(EvalFn,NewClause,[P,0,L1],NewLabel)),
  assertz('$aleph_search'(gcwshyp,hypothesis(NewLabel,NewClause,PCover1,[]))),
  \+('$aleph_search'(sphyp,hypothesis(_,_,_,_))), !.


% gcws(+Clause,+PCvr,+NCvr,-Clause1)
%  specialise Clause that covers pos examples PCvr and neg examples NCvr
%  result is is Clause extended with a single negated literal
% clauses in exception theory are added to list for specialisation
gcws(Clause,PCover,NCover,Clause1):-
  gen_absym(AbName),
  split_clause(Clause,Head,Body),
  functor(Head,_,Arity),
  add_determinations(AbName/Arity,true),
  add_modes(AbName/Arity),
  gen_ab_examples(AbName/Arity,PCover,NCover),
  cwinduce,
  Head =.. [_|Args],
  AbLit =.. [AbName|Args],
  (Body = true -> Body1 = not(AbLit) ; app_lit(not(AbLit),Body,Body1)),
  Clause1 = (Head:-Body1).

% greedy set-cover based construction of abnormality theory
% starts with the first exceptional example
% each clause obtained is added to list of clauses to be specialised
cwinduce:-
  store(greedy),
        aleph5_set_setting(greedy,true),
        '$aleph_global'(atoms_left,atoms_left(pos,PosSet)),
        PosSet \= [],
        repeat,
  '$aleph_global'(atoms_left,atoms_left(pos,[Num-X|Y])),
  sat(Num),
  reduce,
  retract('$aleph_global'(hypothesis,hypothesis(Label,H,PCover,NCover))),
  asserta('$aleph_search'(sphyp,hypothesis(Label,H,PCover,NCover))),
        rm_seeds1(PCover,[Num-X|Y],NewPosLeft),
  retract('$aleph_global'(atoms_left,atoms_left(pos,[Num-X|Y]))),
        asserta('$aleph_global'(atoms_left,atoms_left(pos,NewPosLeft))),
  NewPosLeft = [],
        retract('$aleph_global'(atoms_left,atoms_left(pos,NewPosLeft))),
  reinstate(greedy), !.
cwinduce.


% gen_ab_examples(+Ab,+PCover,+NCover)
% obtain examples for abnormality predicate Ab by
%  pos examples are copies of neg examples in NCover
%  neg examples are copies of pos examples in PCover
% writes new examples to temporary ".f" and ".n" files
% to ensure example/3 remains a static predicate
% alters search parameters accordingly
gen_ab_examples(Ab/_,PCover,NCover):-
  PosFile = '.alephtmp.f',
  NegFile = '.alephtmp.n',
  create_examples(PosFile,Ab,neg,NCover,pos,PCover1),
  create_examples(NegFile,Ab,pos,PCover,neg,NCover1),
  aleph_consult(PosFile),
  aleph_consult(NegFile),
  retractall('$aleph_global'(atoms_left,_)),
  retractall('$aleph_global'(size,_)),
  asserta('$aleph_global'(atoms_left,atoms_left(pos,PCover1))),
  asserta('$aleph_global'(atoms_left,atoms_left(neg,NCover1))),
  interval_count(PCover1,PSize),
  interval_count(NCover1,NSize),
  asserta('$aleph_global'(size,size(pos,PSize))),
  asserta('$aleph_global'(size,size(neg,NSize))),
  delete_file(PosFile),
  delete_file(NegFile).

% create_examples(+File,+OldType,+OldE,+NewType,-NewE)
% copy OldE examples of OldType to give NewE examples of NewType
% copy stored in File
create_examples(File,Ab,OldT,OldE,NewT,[Next-Last]):-
  '$aleph_global'(last_example,last_example(NewT,OldLast)),
  aleph_open(File,write,Stream),
  set_output(Stream),
  create_copy(OldE,OldT,NewT,Ab,OldLast,Last),
  close(Stream),
  set_output(user_output),
  Last > OldLast, !,
  retract('$aleph_global'(last_example,last_example(NewT,OldLast))),
  Next is OldLast + 1,
  asserta('$aleph_global'(last_example,last_example(NewT,Last))).
create_examples(_,_,_,_,_,[]).

create_copy([],_,_,_,L,L).
create_copy([X-Y|T],OldT,NewT,Ab,Num,Last):-
  create_copy(X,Y,OldT,NewT,Ab,Num,Num1),
  create_copy(T,OldT,NewT,Ab,Num1,Last).

create_copy(X,Y,_,_,_,L,L):- X > Y, !.
create_copy(X,Y,OldT,NewT,Ab,Num,Last):-
  example(X,OldT,Example),
  Example =.. [_|Args],
  NewExample =.. [Ab|Args],
  Num1 is Num + 1,
  aleph_writeq(example(Num1,NewT,NewExample)),
  current_stream(Stream),
  format(Stream, '.\n', []),
  X1 is X + 1,
  create_copy(X1,Y,OldT,NewT,Ab,Num1,Last).

% gen_absym(-Name)
% generate new abnormality predicate symbol
gen_absym(Name):-
  (retract('$aleph_global'(last_ab,last_ab(N))) ->
    N1 is N + 1;
    N1 is 0),
  asserta('$aleph_global'(last_ab,last_ab(N1))),
  atomic_concat(ab, N1, Name).



% CLAUSE OPTIMISATIONS

optimise(Clause,Clause1):-
  remove_redundant(Clause,Clause0),
  reorder_clause(Clause0,Clause1).

remove_redundant((Head:-Body),(Head1:-Body1)):-
  goals_to_list((Head,Body),ClauseL),
  remove_subsumed(ClauseL,[Head1|Body1L]),
  (Body1L = [] -> Body1 = true; list_to_goals(Body1L,Body1)).

reorder_clause((Head:-Body), Clause):-
        % term_variables(Head,LHead),
        vars_in_term([Head],[],LHead),
        number_goals_and_get_vars(Body,LHead,1,_,[],Conj),
        calculate_independent_sets(Conj,[],BSets),
        compile_clause(BSets,Head,Clause).

number_goals_and_get_vars((G,Body),LHead,I0,IF,L0,[g(I0,LVF,NG)|LGs]):- !,
        I is I0+1,
        get_goal_vars(G,LHead,LVF,NG),
        number_goals_and_get_vars(Body,LHead,I,IF,L0,LGs).
number_goals_and_get_vars(G,LHead,I,I,L0,[g(I,LVF,NG)|L0]):-
        get_goal_vars(G,LHead,LVF,NG).

get_goal_vars(G,LHead,LVF,G):-
        % term_variables(G,LV0),
        vars_in_term([G],[],LVI),
        aleph_ord_subtract(LVI,LHead,LVF).

calculate_independent_sets([],BSets,BSets).
calculate_independent_sets([G|Ls],BSets0,BSetsF):-
        add_goal_to_set(G,BSets0,BSetsI),
        calculate_independent_sets(Ls,BSetsI,BSetsF).

add_goal_to_set(g(I,LV,G),Sets0,SetsF):-
        add_to_sets(Sets0,LV,[g(I,LV,G)],SetsF).

add_to_sets([],LV,Gs,[[LV|Gs]]).
add_to_sets([[LV|Gs]|Sets0],LVC,GsC,[[LV|Gs]|SetsF]):-
        aleph_ord_disjoint(LV,LVC), !,
        add_to_sets(Sets0,LVC,GsC,SetsF).
add_to_sets([[LV|Gs]|Sets0],LVC,GsC,SetsF):-
        aleph_ord_union(LV,LVC,LVN),
        join_goals(Gs,GsC,GsN),
        add_to_sets(Sets0,LVN,GsN,SetsF).

join_goals([],L,L):- !.
join_goals(L,[],L):- !.
join_goals([g(I1,VL1,G1)|T],[g(I2,VL2,G2)|T2],Z):-
        I1 < I2, !,
        Z = [g(I1,VL1,G1)|TN],
        join_goals(T,[g(I2,VL2,G2)|T2],TN).
join_goals([H|T],[g(I2,VL2,G2)|T2],Z):-
        Z = [g(I2,VL2,G2)|TN],
        join_goals(T,[H|T2],TN).

compile_clause(Goals,Head,(Head:-Body)):-
        compile_clause2(Goals,Body).

compile_clause2([[_|B]], B1):-
  !,
        glist_to_goals(B,B1).
compile_clause2([[_|B]|Bs],(B1,!,NB)):-
        glist_to_goals(B,B1),
        compile_clause2(Bs,NB).

glist_to_goals([g(_,_,Goal)],Goal):- !.
glist_to_goals([g(_,_,Goal)|Goals],(Goal,Goals1)):-
        glist_to_goals(Goals,Goals1).

% remove literals subsumed in the body of a clause
remove_subsumed([Head|Lits],Lits1):-
  delete(Lit,Lits,Left),
  \+(\+(redundant(Lit,[Head|Lits],[Head|Left]))), !,
  remove_subsumed([Head|Left],Lits1).
remove_subsumed(L,L).

% determine if Lit is subsumed by a body literal
redundant(Lit,Lits,[Head|Body]):-
  copy_term([Head|Body],Rest1),
  member(Lit1,Body),
  Lit = Lit1,
  aleph_subsumes(Lits,Rest1).

aleph_subsumes(Lits,Lits1):-
  \+(
    \+(
      (
        numbervars(Lits,0,_),
        numbervars(Lits1,0,_),
        subset(Lits,Lits1)
      )
    )
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S A T  /  R E D U C E

sat(Num):-
  integer(Num),
  example(Num,pos,_),
  sat(pos,Num), !.
sat(Example):-
  record_example(check,uspec,Example,Num),
  sat(uspec,Num), !.

sat(Type,Num):-
        aleph5_setting(construct_bottom,false), !,
        sat_prelims,
  example(Num,Type,Example),
  broadcast(start(sat(Num))),
  p1_message('sat'), p_message(Num), p_message(Example),
  record_sat_example(Num),
  asserta('$aleph_sat'(example,example(Num,Type))),
  asserta('$aleph_sat'(hovars,[])),
  broadcast(end(sat(Num, 0, 0.0))).
sat(Type,Num):-
  aleph5_setting(construct_bottom,reduction), !,
  sat_prelims,
  example(Num,Type,Example),
  broadcast(start(sat(Num))),
  p1_message('sat'), p_message(Num), p_message(Example),
  record_sat_example(Num),
  asserta('$aleph_sat'(example,example(Num,Type))),
  integrate_head_lit(HeadOVars),
  asserta('$aleph_sat'(hovars,HeadOVars)),
  broadcast(end(sat(Num, 0, 0.0))).
sat(Type,Num):-
  aleph5_set_setting(stage,saturation),
  sat_prelims,
  example(Num,Type,Example),
  broadcast(start(sat(Num))),
  p1_message('sat'), p_message(Num), p_message(Example),
  record_sat_example(Num),
  asserta('$aleph_sat'(example,example(Num,Type))),
  split_args(Example,Mode,Input,Output,Constants),
  integrate_args(unknown,Example,Output),
  stopwatch(StartClock),
  assertz('$aleph_sat_atom'(Example,mode(Mode,Output,Input,Constants))),
  setting(i, Ival),
  flatten(0,Ival,0,Last1),
  '$aleph_sat_litinfo'(1,_,Atom,_,_,_),
  get_vars(Atom,Output,HeadOVars),
  asserta('$aleph_sat'(hovars,HeadOVars)),
  get_vars(Atom,Input,HeadIVars),
  asserta('$aleph_sat'(hivars,HeadIVars)),
  functor(Example,Name,Arity),
  get_determs(Name/Arity,L),
  ('$aleph_global'(determination,determination(Name/Arity,'='/2))->
    asserta('$aleph_sat'(eq,true));
    asserta('$aleph_sat'(eq,false))),
  get_atoms(L,1,Ival,Last1,Last),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  asserta('$aleph_sat'(lastlit,Last)),
  asserta('$aleph_sat'(botsize,Last)),
  update_generators,
  rm_moderepeats(Last,Repeats),
  rm_commutative(Last,Commutative),
  rm_symmetric(Last,Symmetric),
  rm_redundant(Last,Redundant),
  rm_uselesslits(Last,NotConnected),
  rm_nreduce(Last,NegReduced),
  TotalLiterals is
    Last-Repeats-NotConnected-Commutative-Symmetric-Redundant-NegReduced,
  show(bottom),
  p1_message('literals'), p_message(TotalLiterals),
  p1_message('saturation time'), p_message(Time),
  broadcast(end(sat(Num, TotalLiterals, Time))),
  store(bottom),
  aleph5_restore_setting(stage).
sat(_,_):-
  aleph5_restore_setting(stage).

reduce:-
  aleph5_setting(search,Search),
  catch(reduce(Search),abort,reinstate_values), !.

% no search: add bottom clause as hypothesis
reduce(false):-
  !,
  add_bottom.
% iterative beam search as described by Ross Quinlan+MikeCameron-Jones,IJCAI-95
reduce(ibs):-
  !,
  retractall('$aleph_search'(ibs_rval,_)),
  retractall('$aleph_search'(ibs_nodes,_)),
  retractall('$aleph_search'(ibs_selected,_)),
  store_values([openlist,caching,explore]),
  aleph5_set_setting(openlist,1),
  aleph5_set_setting(caching,true),
  aleph5_set_setting(explore,true),
  asserta('$aleph_search'(ibs_rval,1.0)),
  asserta('$aleph_search'(ibs_nodes,0)),
  aleph5_setting(evalfn,Evalfn),
  get_start_label(Evalfn,Label),
  ('$aleph_sat'(example,example(Num,Type)) ->
    example(Num,Type,Example),
    asserta('$aleph_search'(ibs_selected,selected(Label,(Example:-true),
        [Num-Num],[])));
    asserta('$aleph_search'(ibs_selected,selected(Label,(false:-true),
        [],[])))),
  stopwatch(Start),
  repeat,
  aleph5_setting(openlist,OldOpen),
  p1_message('ibs beam width'), p_message(OldOpen),
  find_clause(bf),
  '$aleph_search'(current,current(_,Nodes0,[PC,NC|_]/_)),
  N is NC + PC,
  estimate_error_rate(Nodes0,0.5,N,NC,NewR),
  p1_message('ibs estimated error'), p_message(NewR),
  retract('$aleph_search'(ibs_rval,OldR)),
  retract('$aleph_search'(ibs_nodes,Nodes1)),
        '$aleph_search'(selected,selected(BL,RCl,PCov,NCov)),
  NewOpen is 2*OldOpen,
  Nodes2 is Nodes0 + Nodes1,
  aleph5_set_setting(openlist,NewOpen),
  asserta('$aleph_search'(ibs_rval,NewR)),
  asserta('$aleph_search'(ibs_nodes,Nodes2)),
  ((NewR >= OldR; NewOpen > 512) -> true;
    retract('$aleph_search'(ibs_selected,selected(_,_,_,_))),
    asserta('$aleph_search'(ibs_selected,selected(BL,RCl,PCov,NCov))),
    fail),
  !,
  stopwatch(Stop),
  Time is Stop - Start,
  retractall('$aleph_search'(ibs_rval,_)),
  retract('$aleph_search'(ibs_nodes,Nodes)),
        retract('$aleph_search'(ibs_selected,selected(BestLabel,RClause,PCover,NCover))),
  add_hyp(BestLabel,RClause,PCover,NCover),
  p1_message('ibs clauses constructed'), p_message(Nodes),
  p1_message('ibs search time'), p_message(Time),
  p_message('ibs best clause'),
  pp_dclause(RClause),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RClause,Nodes,Time),
  reinstate_values([openlist,caching,explore]).
% iterative deepening search
reduce(id):-
  !,
  retractall('$aleph_search'(id_nodes,_)),
  retractall('$aleph_search'(id_selected,_)),
  store_values([caching,clauselength]),
  aleph5_setting(clauselength,MaxCLen),
  aleph5_set_setting(clauselength,1),
  aleph5_set_setting(caching,true),
  asserta('$aleph_search'(id_nodes,0)),
  aleph5_setting(evalfn,Evalfn),
  get_start_label(Evalfn,Label),
  ('$aleph_sat'(example,example(Num,Type)) ->
    example(Num,Type,Example),
    asserta('$aleph_search'(id_selected,selected(Label,(Example:-true),
        [Num-Num],[])));
    asserta('$aleph_search'(id_selected,selected(Label,(false:-true),
        [],[])))),
  stopwatch(Start),
  repeat,
  aleph5_setting(clauselength,OldCLen),
  p1_message('id clauselength setting'), p_message(OldCLen),
  find_clause(df),
  '$aleph_search'(current,current(_,Nodes0,_)),
  retract('$aleph_search'(id_nodes,Nodes1)),
        '$aleph_search'(selected,selected([P,N,L,F|T],RCl,PCov,NCov)),
  '$aleph_search'(id_selected,selected([_,_,_,F1|_],_,_,_)),
  NewCLen is OldCLen + 1,
  Nodes2 is Nodes0 + Nodes1,
  aleph5_set_setting(clauselength,NewCLen),
  '$aleph_search'(id_nodes,Nodes2),
  (F1 >= F -> true;
    retract('$aleph_search'(id_selected,selected([_,_,_,F1|_],_,_,_))),
    asserta('$aleph_search'(id_selected,selected([P,N,L,F|T],RCl,PCov,NCov))),
    aleph5_set_setting(best,[P,N,L,F|T])),
  NewCLen > MaxCLen,
  !,
  stopwatch(Stop),
  Time is Stop - Start,
  retract('$aleph_search'(id_nodes,Nodes)),
        retract('$aleph_search'(id_selected,selected(BestLabel,RClause,PCover,NCover))),
  add_hyp(BestLabel,RClause,PCover,NCover),
  p1_message('id clauses constructed'), p_message(Nodes),
  p1_message('id search time'), p_message(Time),
  p_message('id best clause'),
  pp_dclause(RClause),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RClause,Nodes,Time),
  aleph5_restore_setting(best),
  reinstate_values([caching,clauselength]).
% iterative language search as described by Rui Camacho, 1996
reduce(ils):-
  !,
  retractall('$aleph_search'(ils_nodes,_)),
  retractall('$aleph_search'(ils_selected,_)),
  store_values([caching,language]),
  aleph5_set_setting(searchstrat,bf),
  aleph5_set_setting(language,1),
  aleph5_set_setting(caching,true),
  asserta('$aleph_search'(ils_nodes,0)),
  aleph5_setting(evalfn,Evalfn),
  get_start_label(Evalfn,Label),
  ('$aleph_sat'(example,example(Num,Type)) ->
    example(Num,Type,Example),
    asserta('$aleph_search'(ils_selected,selected(Label,(Example:-true),
        [Num-Num],[])));
    asserta('$aleph_search'(ils_selected,selected(Label,(false:-true),
        [],[])))),
  stopwatch(Start),
  repeat,
  aleph5_setting(language,OldLang),
  p1_message('ils language setting'), p_message(OldLang),
  find_clause(bf),
  '$aleph_search'(current,current(_,Nodes0,_)),
  retract('$aleph_search'(ils_nodes,Nodes1)),
        '$aleph_search'(selected,selected([P,N,L,F|T],RCl,PCov,NCov)),
  '$aleph_search'(ils_selected,selected([_,_,_,F1|_],_,_,_)),
  NewLang is OldLang + 1,
  Nodes2 is Nodes0 + Nodes1,
  aleph5_set_setting(language,NewLang),
  asserta('$aleph_search'(ils_nodes,Nodes2)),
  (F1 >= F -> true;
    retract('$aleph_search'(ils_selected,selected([_,_,_,F1|_],_,_,_))),
    asserta('$aleph_search'(ils_selected,selected([P,N,L,F|T],RCl,PCov,NCov))),
    aleph5_set_setting(best,[P,N,L,F|T]),
    fail),
  !,
  stopwatch(Stop),
  Time is Stop - Start,
  retract('$aleph_search'(ils_nodes,Nodes)),
        retract('$aleph_search'(ils_selected,selected(BestLabel,RClause,PCover,NCover))),
  add_hyp(BestLabel,RClause,PCover,NCover),
  p1_message('ils clauses constructed'), p_message(Nodes),
  p1_message('ils search time'), p_message(Time),
  p_message('ils best clause'),
  pp_dclause(RClause),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RClause,Nodes,Time),
  aleph5_restore_setting(best),
  reinstate_values([caching,language]).
% implementation of a randomised local search for clauses
% currently, this can use either: simulated annealing with a fixed temp
% or a GSAT-like algorithm
% the choice of these is specified by the parameter: rls_type
% both annealing and GSAT employ random multiple restarts
% and a limit on the number of moves
%  the number of restarts is specified by aleph5_set_setting(tries,...)
%  the number of moves is specified by aleph5_set_setting(moves,...)
% annealing currently restricted to using a fixed temperature
%  the temperature is specified by aleph5_set_setting(temperature,...)
%  the use of a fixed temp. makes it equivalent to the Metropolis alg.
% GSAT if given a ``random-walk probability'' performs Selman et als walksat
%  the walk probability is specified by aleph5_set_setting(walk,...)
%  a walk probability of 0 is equivalent to doing standard GSAT
reduce(rls):-
  !,
  aleph5_setting(tries,MaxTries),
  MaxTries >= 1,
  store_values([caching,refine,refineop]),
  aleph5_set_setting(searchstrat,heuristic),
  aleph5_set_setting(caching,true),
  aleph5_setting(refine,Refine),
  (Refine \= false  -> true; aleph5_set_setting(refineop,rls)),
  aleph5_setting(threads,Threads),
  rls_search(Threads, MaxTries, Time, Nodes, selected(BestLabel,
          RBest,PCover,NCover)),
  add_hyp(BestLabel,RBest,PCover,NCover),
  p1_message('rls nodes constructed'), p_message(Nodes),
  p1_message('rls search time'), p_message(Time),
  p_message('rls best result'),
  pp_dclause(RBest),
  aleph5_setting(evalfn,Evalfn),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RBest,Nodes,Time),
  aleph5_restore_setting(best),
  reinstate_values([caching,refine,refineop]).
% stochastic clause selection based on ordinal optimisation
% see papers by Y.C. Ho and colleagues for more details
reduce(scs):-
  !,
  store_values([tries,moves,rls_type,clauselength_distribution]),
  stopwatch(Start),
  (
    aleph5_setting(scs_sample,SampleSize)
  ->
    true
  ;
    aleph5_setting(scs_percentile,K),
    K > 0.0,
    aleph5_setting(scs_prob,P),
    P < 1.0,
    SampleSize is integer(log(1-P)/log(1-K/100) + 1)
  ),
  (
    aleph5_setting(scs_type,informed)
  ->
    (
      aleph5_setting(clauselength_distribution,D)
    ->
      true
    ;
      aleph5_setting(clauselength,CL),
      estimate_clauselength_distribution(CL,100,K,D),
      p1_message('using clauselength distribution'),
      p_message(D),
      aleph5_set_setting(clauselength_distribution,D)
    )
  ;
    true
  ),
  aleph5_set_setting(tries,SampleSize),
  aleph5_set_setting(moves,0),
  aleph5_set_setting(rls_type,gsat),
  reduce(rls),
  stopwatch(Stop),
  Time is Stop - Start,
  '$aleph_search'(rls_nodes,Nodes),
  '$aleph_search'(rls_selected,selected(BestLabel,RBest,_,_)),
  p1_message('scs nodes constructed'), p_message(Nodes),
  p1_message('scs search time'), p_message(Time),
  p_message('scs best result'),
  pp_dclause(RBest),
  aleph5_setting(evalfn,Evalfn),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RBest,Nodes,Time),
  p1_message('scs search time'), p_message(Time),
  reinstate_values([tries,moves,rls_type,clauselength_distribution]).
% simple association rule search
% For a much more sophisticated approach see: L. Dehaspe, PhD Thesis, 1998
% Here, simply find all rules within search that cover at least
% a pre-specificed fraction of the positive examples
reduce(ar):-
  !,
  clear_cache,
  (aleph5_setting(pos_fraction,PFrac) -> true;
    p_message('value required for pos_fraction parameter'),
    fail),
        '$aleph_global'(atoms_left,atoms_left(pos,Pos)),
  retract('$aleph_global'(atoms_left,atoms_left(neg,Neg))),
  interval_count(Pos,P),
  MinPos is PFrac*P,
  store_values([minpos,evalfn,explore,caching,minacc,good]),
  aleph5_set_setting(searchstrat,bf),
  aleph5_set_setting(minpos,MinPos),
  aleph5_set_setting(evalfn,coverage),
  aleph5_set_setting(explore,true),
  aleph5_set_setting(caching,true),
  aleph5_set_setting(minacc,0.0),
  aleph5_set_setting(good,true),
  asserta('$aleph_global'(atoms_left,atoms_left(neg,[]))),
  find_clause(bf),
  show(good),
  retract('$aleph_global'(atoms_left,atoms_left(neg,[]))),
  asserta('$aleph_global'(atoms_left,atoms_left(neg,Neg))),
  reinstate_values([minpos,evalfn,explore,caching,minacc,good]).
% search for integrity constraints
% modelled on the work by L. De Raedt and L. Dehaspe, 1996
reduce(ic):-
  !,
  store_values([minpos,minscore,evalfn,explore,refineop]),
  aleph5_setting(refineop,RefineOp),
  (RefineOp = false -> aleph5_set_setting(refineop,auto); true),
  aleph5_set_setting(minpos,0),
  aleph5_set_setting(searchstrat,bf),
  aleph5_set_setting(evalfn,coverage),
  aleph5_set_setting(explore,true),
  aleph5_setting(noise,N),
  MinScore is -N,
  aleph5_set_setting(minscore,MinScore),
  find_clause(bf),
  reinstate_values([minpos,minscore,evalfn,explore,refineop]).
reduce(bf):-
  !,
  find_clause(bf).
reduce(df):-
  !,
  find_clause(df).
reduce(heuristic):-
  !,
  find_clause(heuristic).

% find_clause(Search) where Search is one of bf, df, heuristic
find_clause(Search):-
  aleph5_set_setting(stage,reduction),
  aleph5_set_setting(searchstrat,Search),
  p_message('reduce'),
  reduce_prelims(L,P,N),
  asserta('$aleph_search'(openlist,[])),
  get_search_settings(S),
  arg(4,S,_/Evalfn),
  get_start_label(Evalfn,Label),
  (
    '$aleph_sat'(example,example(Num,Type))
  ->
    example(Num,Type,Example),
    asserta(
      '$aleph_search'(selected,selected(Label,(Example:-true), [Num-Num],[]))
    )
  ;
    asserta('$aleph_search'(selected,selected(Label,(false:-true),[],[])))
  ),
  arg(13,S,MinPos),
  interval_count(P,PosLeft),
  PosLeft >= MinPos,
  '$aleph_search'(selected,selected(L0,C0,P0,N0)),
  add_hyp(L0,C0,P0,N0),
  (
    '$aleph_global'(max_set,max_set(Type,Num,Label1,ClauseNum))
  ->
    BestSoFar = Label1/ClauseNum;
    (
      setting(best, Label2)
    ->
      BestSoFar = Label2/0
    ;
      BestSoFar = Label/0
    )
  ),
  asserta('$aleph_search'(best_label,BestSoFar)),
  p1_message('best label so far'),
  p_message(BestSoFar),
  arg(3,S,RefineOp),
  stopwatch(StartClock),
  (
    RefineOp = false
  ->
    get_gains(S,0,BestSoFar,[],false,[],0,L,[1],P,N,[],1,Last,NextBest),
    update_max_head_count(0,Last)
  ;
    clear_cache,
    interval_count(P,MaxPC),
    asserta('$aleph_local'(max_head_count,MaxPC)),
    StartClause = 0-[Num,Type,[],false],
    get_gains(S,0,BestSoFar,StartClause,_,_,_,L,[StartClause],P,N,[],1,Last,NextBest)
  ),
  asserta('$aleph_search_expansion'(1,0,1,Last)),
  get_nextbest(S,_),
  asserta('$aleph_search'(current,current(1,Last,NextBest))),
  search(S,Nodes),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  '$aleph_search'(selected,selected(BestLabel,RClause,PCover,NCover)),
  retract('$aleph_search'(openlist,_)),
  add_hyp(BestLabel,RClause,PCover,NCover),
  p1_message('clauses constructed'),
  p_message(Nodes),
  p1_message('search time'),
  p_message(Time),
  p_message('best clause'),
  pp_dclause(RClause),
  show_stats(Evalfn,BestLabel),
  update_search_stats(Nodes,Time),
  record_search_stats(RClause,Nodes,Time),
  aleph5_restore_setting(stage),
  !.
find_clause(_):-
  '$aleph_search'(selected,selected(BestLabel,RClause,PCover,NCover)),
  retract('$aleph_search'(openlist,_)),
  add_hyp(BestLabel,RClause,PCover,NCover),
  p_message('best clause'),
  pp_dclause(RClause),
  (
    aleph5_setting(evalfn,Evalfn)
  ->
    true
  ;
    Evalfn = coverage
  ),
  show_stats(Evalfn,BestLabel),
  aleph5_restore_setting(stage),
  !.

% find_theory(Search) where Search is rls only at present
find_theory(rls):-
  !,
  retractall('$aleph_search'(rls_move,_)),
  retractall('$aleph_search'(rls_nodes,_)),
  retractall('$aleph_search'(rls_parentstats,_)),
  retractall('$aleph_search'(rls_selected,_)),
  aleph5_setting(tries,MaxTries),
  MaxTries >= 1,
  store_values([caching,store_bottom]),
  aleph5_set_setting(caching,false),
  aleph5_set_setting(store_bottom,true),
        '$aleph_global'(atoms,atoms(pos,PosSet)),
        '$aleph_global'(atoms,atoms(neg,NegSet)),
        interval_count(PosSet,P0),
        interval_count(NegSet,N0),
  aleph5_setting(evalfn,Evalfn),
        complete_label(Evalfn,[0-[0,0,[],false]],[P0,N0,1],Label),
  asserta('$aleph_search'(rls_selected,selected(Label,[0-[0,0,[],false]],
            PosSet,NegSet))),
  asserta('$aleph_search'(rls_nodes,0)),
  asserta('$aleph_search'(rls_restart,1)),
  get_search_settings(S),
  aleph5_set_setting(best,Label),
  stopwatch(Start),
  repeat,
  retractall('$aleph_search'(rls_parentstats,_)),
  retractall('$aleph_search'(rls_move,_)),
  retractall('$aleph_search_seen'(_,_)),
  asserta('$aleph_search'(rls_move,1)),
  asserta('$aleph_search'(rls_parentstats,stats(Label,PosSet,NegSet))),
  '$aleph_search'(rls_restart,R),
  p1_message('restart'), p_message(R),
  find_theory1(rls),
  '$aleph_search'(current,current(_,Nodes0,_)),
  retract('$aleph_search'(rls_nodes,Nodes1)),
        '$aleph_search'(selected,selected([P,N,L,F|T],RCl,PCov,NCov)),
  '$aleph_search'(rls_selected,selected([_,_,_,F1|_],_,_,_)),
  retract('$aleph_search'(rls_restart,R)),
  R1 is R + 1,
  asserta('$aleph_search'(rls_restart,R1)),
  Nodes2 is Nodes0 + Nodes1,
  asserta('$aleph_search'(rls_nodes,Nodes2)),
  (F1 >= F -> true;
    retract('$aleph_search'(rls_selected,selected([_,_,_,F1|_],_,_,_))),
    asserta('$aleph_search'(rls_selected,selected([P,N,L,F|T],RCl,PCov,NCov))),
    aleph5_set_setting(best,[P,N,L,F|T])),
  aleph5_setting(best,BestSoFar),
  (R1 > MaxTries;discontinue_search(S,BestSoFar/_,Nodes2)),
  !,
  stopwatch(Stop),
  Time is Stop - Start,
  '$aleph_search'(rls_nodes,Nodes),
        '$aleph_search'(rls_selected,selected(BestLabel,RBest,PCover,NCover)),
  add_hyp(BestLabel,RBest,PCover,NCover),
  p1_message('nodes constructed'), p_message(Nodes),
  p1_message('search time'), p_message(Time),
  p_message('best theory'),
  pp_dclauses(RBest),
  show_stats(Evalfn,BestLabel),
  record_search_stats(RBest,Nodes,Time),
  aleph5_restore_setting(best),
  reinstate_values([caching,refine,refineop,store_bottom]).

find_theory1(_):-
  clean_up_reduce,
        '$aleph_global'(atoms,atoms(pos,Pos)),
        '$aleph_global'(atoms,atoms(neg,Neg)),
        asserta('$aleph_search'(openlist,[])),
  asserta('$aleph_search'(nextnode,none)),
        stopwatch(StartClock),
        get_search_settings(S),
  arg(4,S,_/Evalfn),
        interval_count(Pos,P),
        interval_count(Neg,N),
        complete_label(Evalfn,[0-[0,0,[],false]],[P,N,1],Label),
  asserta('$aleph_search'(selected,selected(Label,[0-[0,0,[],false]],Pos,Neg))),
  get_theory_gain(S,0,Label/0,[0-[0,0,[],false]],Pos,Neg,P,N,NextBest,Last),
  asserta('$aleph_search'(current,current(0,Last,NextBest))),
  get_nextbest(S,_),
  tsearch(S,Nodes),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  '$aleph_search'(selected,selected(BestLabel,RTheory,PCover,NCover)),
  retract('$aleph_search'(openlist,_)),
  add_hyp(BestLabel,RTheory,PCover,NCover),
  p1_message('theories constructed'), p_message(Nodes),
  p1_message('search time'), p_message(Time),
  p_message('best theory'),
  pp_dclauses(RTheory),
  show_stats(Evalfn,BestLabel),
  update_search_stats(Nodes,Time),
  record_tsearch_stats(RTheory,Nodes,Time).

estimate_error_rate(H,Del,N,E,R):-
  TargetProb is 1-exp(log(1-Del)/H),
  estimate_error(1.0/0.0,0.0/1.0,TargetProb,N,E,R).

estimate_error(L/P1,U/P2,P,N,E,R):-
  M is (L+U)/2,
  binom_lte(N,M,E,P3),
  ADiff is abs(P - P3),
  (ADiff < 0.00001 ->
    R is M;
    (P3 > P ->
      estimate_error(L/P1,M/P3,P,N,E,R);
      estimate_error(M/P3,U/P2,P,N,E,R)
    )
  ).


zap_rest(Lits):-
  retract('$aleph_sat_litinfo'(LitNum,Depth,Atom,I,O,D)),
  (memberchk(LitNum,Lits) ->
    intersect1(Lits,D,D1,_),
    asserta('$aleph_sat_litinfo'(LitNum,Depth,Atom,I,O,D1));
    true),
  fail.
zap_rest(_).

sat_prelims:-
  clean_up_sat,
  clean_up_hypothesis,
  reset_counts,
  set_up_builtins.


reduce_prelims(L,P,N):-
  clean_up_reduce,
  check_posonly,
  check_auto_refine,
  ('$aleph_sat'(lastlit,L) -> true;
    L = 0, asserta('$aleph_sat'(lastlit,L))),
  ('$aleph_sat'(botsize,B) -> true;
    B = 0, asserta('$aleph_sat'(botsize,B))),
        (('$aleph_global'(lazy_evaluate,lazy_evaluate(_));aleph5_setting(greedy,true))->
                '$aleph_global'(atoms_left,atoms_left(pos,P));
                '$aleph_global'(atoms,atoms(pos,P))),
  aleph5_setting(evalfn,E),
  (E = posonly -> NType = rand; NType = neg),
  '$aleph_global'(atoms_left,atoms_left(NType,N)),
  asserta('$aleph_search'(nextnode,none)).

set_up_builtins:-
  gen_nlitnum(Cut),
  asserta('$aleph_sat_litinfo'(Cut,0,'!',[],[],[])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% T H R E A D S

% multi-threaded randomised local search
rls_search(1, MaxTries, Time, Nodes, Selected):-
   !,
  retractall('$aleph_search'(rls_restart,_)),
  retractall('$aleph_search'(rls_nodes,_)),
  retractall('$aleph_search'(rls_selected,_)),
  asserta('$aleph_search'(rls_restart,1)),
  aleph5_setting(evalfn,Evalfn),
  get_start_label(Evalfn,Label),
  aleph5_set_setting(best,Label),
  get_search_settings(S),
  arg(4,S,SearchStrat/_),
  ('$aleph_sat'(example,example(Num,Type)) ->
    example(Num,Type,Example),
    asserta('$aleph_search'(rls_selected,selected(Label,
            (Example:-true),[Num-Num],[])));
    asserta('$aleph_search'(rls_selected,selected(Label,
            (false:-true),[],[])))
  ),
   asserta('$aleph_search'(rls_nodes,0)),
   stopwatch(Start),
  estimate_numbers(_),
   repeat,
   retract('$aleph_search'(rls_restart,R)),
   R1 is R + 1,
   asserta('$aleph_search'(rls_restart,R1)),
   rls_thread(R, SearchStrat, Label, Nodes0, selected(Best,RCl,PCov,NCov)),
   Best = [_,_,_,F|_],
   '$aleph_search'(rls_selected,selected([_,_,_,F1|_],_,_,_)),
   (F1 >= F -> true;
     retract('$aleph_search'(rls_selected,selected([_,_,_,F1|_],
              _,_,_))),
     asserta('$aleph_search'(rls_selected,selected(Best,RCl,
              PCov,NCov))),
     aleph5_set_setting(best,Best)
   ),
   aleph5_setting(best,BestSoFar),
   retract('$aleph_search'(rls_nodes,Nodes1)),
   Nodes2 is Nodes0 + Nodes1,
   asserta('$aleph_search'(rls_nodes,Nodes2)),
   (R1 > MaxTries; discontinue_search(S,BestSoFar/_,Nodes2)),
   !,
   stopwatch(Stop),
   Time is Stop - Start,
   retractall('$aleph_search'(rls_restart,_)),
  retract('$aleph_search'(rls_nodes,Nodes)),
        retract('$aleph_search'(rls_selected,Selected)).
rls_search(N, MaxTries, Time, Nodes, Selected):-
  retractall('$aleph_search'(rls_restart,_)),
  retractall('$aleph_search'(rls_nodes,_)),
  retractall('$aleph_search'(rls_selected,_)),
  aleph5_setting(evalfn,Evalfn),
  get_start_label(Evalfn,Label),
  aleph5_set_setting(best,Label),
  get_search_settings(S),
  arg(4,S,SearchStrat/_),
  ('$aleph_sat'(example,example(Num,Type)) ->
    example(Num,Type,Example),
    asserta('$aleph_search'(rls_selected,selected(Label,
            (Example:-true),[Num-Num],[])));
    asserta('$aleph_search'(rls_selected,selected(Label,
            (false:-true),[],[])))
  ),
   asserta('$aleph_search'(rls_nodes,0)),
  estimate_numbers(_),  % so all threads can use same estimates
  thread_self(Master),
  message_queue_create(Queue),
  create_worker_pool(N, Master, Queue, WorkerIds),
  forall(between(1, MaxTries, R),
         thread_send_message(Queue, rls_restart(R, SearchStrat, Label))),
  collect_results(rls_restart,MaxTries,[0,S],[Time|_]),
  kill_worker_pool(Queue, WorkerIds),
   retractall('$aleph_search'(rls_restart,_)),
  retract('$aleph_search'(rls_nodes,Nodes)),
        retract('$aleph_search'(rls_selected,Selected)).

rls_thread(R, SearchStrat, Label, Nodes0, selected(Best,RCl,PCov,NCov)):-
  retractall('$aleph_search'(best_refinement,_)),
  retractall('$aleph_search'(last_refinement,_)),
        retractall('$aleph_search'(rls_move,_)),
        retractall('$aleph_search'(rls_parentstats,_)),
  retractall('$aleph_search_seen'(_,_)),
        asserta('$aleph_search'(rls_move,1)),
        asserta('$aleph_search'(rls_parentstats,stats(Label,[],[]))),
        p1_message('restart'), p_message(R),
        find_clause(SearchStrat),
  '$aleph_search'(current,current(_,Nodes0,_)),
  '$aleph_search'(selected,selected(Best,RCl,PCov,NCov)),
  retractall('$aleph_search'(best_refinement,_)),
  retractall('$aleph_search'(last_refinement,_)),
        retractall('$aleph_search'(rls_move,_)),
        retractall('$aleph_search'(rls_parentstats,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% T H R E A D S

create_worker_pool(N, Master, Queue, WorkerIds):-
  create_worker_pool(1, N, Master, Queue, WorkerIds).

create_worker_pool(I, N, _, _, []):-
  I > N, !.
create_worker_pool(I, N, Master, Queue, [Id|T]):-
  atom_concat(worker_, I, Alias),
  thread_create(worker(Queue, Master), Id, [alias(Alias)]),
  I2 is I + 1,
  create_worker_pool(I2, N, Master, Queue, T).

kill_worker_pool(Queue, WorkerIds):-
  p_message('Killing workers'),
  forall(member(Worker, WorkerIds),
         kill_worker(Queue, Worker)),
  p_message('Waiting for workers'),
  forall(member(Worker, WorkerIds),
         thread_join(Worker, _)),
  message_queue_destroy(Queue),
  p_message('Ok, all done').

kill_worker(Queue, Worker):-
  thread_send_message(Queue, all_done),
  thread_signal(Worker, throw(surplus_to_requirements)).

worker(Queue, Master):-
  thread_get_message(Queue, Message),
  work(Message, Master),
  worker(Queue, Master).

work(rls_restart(R, SearchStrat, Label), Master):-
  statistics(cputime, CPU0),
  rls_thread(R, SearchStrat, Label, Nodes, Selected),
  statistics(cputime, CPU1),
  CPU is CPU1 - CPU0,
  thread_send_message(Master, done(CPU, Nodes, Selected)).
work(all_done, _):-
  thread_exit(done).

collect_results(rls_restart,NResults,In,Out):-
        collect_results(0,NResults,rls_restart,In,Out).

collect_results(R0,MaxR,Flag,In,Out):-
        thread_get_message(Message),
        collect(Flag,Message,In,Out1,Done),
        R1 is R0 + 1,
        (   (Done == false,
            R1 < MaxR)
        ->  collect_results(R1,MaxR,Flag,Out1,Out)
        ;   Out = Out1
        ).

collect(rls_restart,done(CPU, Nodes, selected(Best,RCl,PCov,NCov)),[T0,S], [T1,S],Done):-
  T1 is CPU + T0,
  Best = [_,_,_,F|_],
  '$aleph_search'(rls_selected,selected([_,_,_,F1|_],_,_,_)),
  (
    F1 >= F
  ->
    true
  ;
    retract('$aleph_search'(rls_selected, selected([_,_,_,F1|_],_,_,_))),
    asserta('$aleph_search'(rls_selected, selected(Best, RCl, PCov, NCov))),
    aleph5_set_setting(best, Best)
  ),
  aleph5_setting(best, BestSoFar),
  retract('$aleph_search'(rls_nodes,Nodes1)),
  Nodes2 is Nodes + Nodes1,
  asserta('$aleph_search'(rls_nodes,Nodes2)),
  (
    discontinue_search(S,BestSoFar/_,Nodes2)
  ->
    Done = true
  ;
    Done = false
  ).



% Control %

% induce_clauses/0: the basic theory construction predicate
% constructs theories 1 clause at a time
induce_clauses:-
  aleph5_setting(interactive, true),
  !,
  induce_incremental.
induce_clauses:-
  induce.

% induce/0: non-interactive theory construction
% constructs theories 1 clause at a time
% does greedy cover removal after each clause found.

induce:-
  clean_up,
  aleph5_set_setting(greedy, true),
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  '$aleph_global'(atoms_left,atoms_left(pos,PosSet)),
  PosSet \= [],
  store(portray_search),
  aleph5_set_setting(portray_search,false),
  aleph5_setting(samplesize, S),
  aleph5_setting(abduce, Abduce),
  record_settings,
  stopwatch(StartClock),
  repeat,
  gen_sample(pos,S),
  retractall('$aleph_global'(besthyp,besthyp(_,_,_,_,_))),
  asserta(
    '$aleph_global'(besthyp,besthyp([-1e10,0,1,-1e10],0,(false),[],[]))
  ),
  get_besthyp(Abduce),
  (
    aleph5_setting(gcws, true)
  ->
    sphyp,
    addgcws
  ;
    addhyp
  ),
  show_atoms_left,
  record_atoms_left,
  '$aleph_global'(atoms_left,atoms_left(pos,[])),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(theory),
  record_theory(Time),
  aleph5_restore_setting(greedy),
  reinstate(portray_search),
  stream_message('[time taken] ~w seconds\n', [Time]),
  show_total_stats,
  record_total_stats,
  !.
induce.

% construct theories 1 clause at a time
% does not perform greedy cover removal after each clause found
% constructs unique `maximum cover set' solution
% by obtaining the best clause covering each example
% slow

induce_max:-
  clean_up,
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  '$aleph_global'(atoms,atoms(pos,PosSet)),
  PosSet \= [],
  store(portray_search),
  aleph5_set_setting(portray_search,false),
  record_settings,
  stopwatch(StartClock),
  aleph5_set_setting(maxcover,true),
  induce_max(PosSet),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(theory),
  record_theory(Time),
  aleph5_restore_setting(maxcover),
  reinstate(portray_search),
  reinstate(greedy),
  stream_message('[time taken] ~w seconds\n', [Time]),
  show_total_stats,
  record_total_stats, !.
induce_max.

induce_max([]).
induce_max([Start-Finish|Intervals]):-
  asserta('$aleph_local'(counter,Start)),
  induce_max1(Finish),
  induce_max(Intervals).

induce_max1(Finish):-
  '$aleph_local'(counter,S),
  S =< Finish,
  !,
  (
    aleph5_setting(resample, Resample)
  ->
    true
  ;
    Resample = 1
  ),
  repeat,
  retract('$aleph_local'(counter,Start)),
  gen_sample(Resample,pos,Start),
  get_besthyp(false),
  update_coverset(pos,Start),
  Next is Start + 1,
  assertz('$aleph_local'(counter,Next)),
  Next > Finish,
  !,
  retract('$aleph_local'(counter,Next)).
induce_max1(_).

% construct theories 1 clause at a time
% does not perform greedy cover removal after each clause found
induce_cover:-
  clean_up,
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  '$aleph_global'(atoms,atoms(pos,PosSet)),
  PosSet \= [],
  store(portray_search),
  aleph5_set_setting(portray_search,false),
  aleph5_setting(samplesize,S),
  aleph5_setting(abduce,Abduce),
  record_settings,
  stopwatch(StartClock),
  repeat,
  gen_sample(pos,S),
  asserta('$aleph_global'(besthyp,besthyp([-1e10,0,1,-1e10],0,(false),[],[]))),
  get_besthyp(Abduce),
  addhyp,
  '$aleph_global'(atoms_left,atoms_left(pos,[])),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(theory),
  record_theory(Time),
  reinstate(portray_search),
  reinstate(greedy),
  stream_message('[time taken] ~w seconds\n', [Time]),
  show_total_stats,
  record_total_stats,
  !.
induce_cover.

% rudimentary version of an interactive, incremental rule learner
% repeatedly does the following:
%  1. ask the user for an example
%    default is to use a new positive example from previous search
%    if user responds with Ctrl-d (eof) then search stops
%    if user responds with "ok" then default is used
%    otherwise user has to provide an example
%  2. construct bottom clause using that example
%    expects to have appropriate mode declarations
%  3. search for the best clause C
%  4. ask the user about C who can respond with
%    ok: clause added to theory
%    prune: statement added to prevent future
%        clauses that are subsumed by C
%    overgeneral: constraint added to prevent future
%        clauses that subsume C
%    overgeneral because not(E): E is added as a negative example
%    overspecific: C is added as new positive example
%    overspecific because E: E is added as a new positive example
%    X: where X is some aleph command like "covers"
%    Ctrl-d (eof): return to Step 1
induce_incremental:-
  clean_up,
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  store_values([interactive,portray_search,proof_strategy,mode]),
  aleph5_set_setting(portray_search,false),
  aleph5_set_setting(proof_strategy,sld),
  aleph5_set_setting(interactive,true),
  record_settings,
  stopwatch(StartClock),
  repeat,
  ask_example(E),
  (
    (
      E = end_of_file
    ;
      E = none
    )
  ->
    true
  ;
    once(record_example(check,pos,E,N)),
    retractall('$aleph_global'(example_selected, example_selected(_,_))),
    asserta('$aleph_global'(example_selected, example_selected(pos,N))),
    once(sat(N)),
    once(reduce),
    current_stream(Stream),
    once(process_hypothesis(Stream)),
    fail
  ),
  !,
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(theory),
  show(pos),
  show(neg),
  show(false/0),
  show(prune/1),
  record_theory(Time),
  reinstate_values([interactive,portray_search,proof_strategy,mode]),
  stream_message('[time taken] ~w seconds\n', [Time]).

% induce_theory/0: does theory-level search
% currently only with search = rls; and evalfn = accuracy

induce_theory:-
  aleph5_setting(search,Search),
  induce_theory(Search).

% induce entire theories from batch data
% using a randomised local search
%   currently, this can use either: simulated annealing with a fixed temp,
%   GSAT, or a WSAT-like algorithm
%   the choice of these is specified by the parameter: rls_type
%   all methods employ random multiple restarts
%   and a limit on the number of moves
%         the number of restarts is specified by aleph5_set_setting(tries,...)
%         the number of moves is specified by aleph5_set_setting(moves,...)
%   annealing currently restricted to using a fixed temperature
%         the temperature is specified by aleph5_set_setting(temperature,...)
%         the fixed temp. makes it equivalent to the Metropolis alg.
%   WSAT requires a ``random-walk probability''
%         the walk probability is specified by aleph5_set_setting(walk,...)
%         a walk probability of 0 is equivalent to doing standard GSAT
%   theory accuracy is the evaluation function

induce_theory(rls):-
  clean_up,
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  store(evalfn),
  aleph5_set_setting(evalfn,accuracy),
  record_settings,
  find_theory(rls),
  reinstate(evalfn),
  show_total_stats,
  record_total_stats,
  !.
induce_theory(_).

% induce_constraints/0: search for logical constraints that
% hold in the background knowledge
% A constraint is a clause of the form false:-...
% This is modelled on the Claudien program developed by
% L. De Raedt and his colleagues in Leuven
% Constraints that are ``nearly true'' can be obtained
% by altering the noise setting
% All constraints found are stored as `good clauses'.

induce_constraints:-
  clean_up,
  retractall('$aleph_global'(search_stats,search_stats(_,_))),
  store_values([portray_search,search,construct_bottom,good,goodfile]),
  aleph5_restore_setting(goodfile),
  aleph5_set_setting(portray_search,false),
  aleph5_set_setting(construct_bottom,false),
  aleph5_set_setting(search,ic),
  aleph5_set_setting(good,true),
  sat(uspec,0),
  reduce,
  show(constraints),
  reinstate_values([portray_search,search,construct_bottom,good,goodfile]),
  show_total_stats,
  record_total_stats,
  !.
induce_constraints.

% induce_modes/0: search for an acceptable set of mode declarations

induce_modes:-
  clean_up,
  store_values([typeoverlap]),
  search_modes,
  reinstate_values([typeoverlap]),
  show(modes).

% induce_features/0: search for interesting boolean features
% each good clause found in a search constitutes a new boolean feature
% the maximum number of features is controlled by aleph5_set_setting(max_features,F)
% the features are constructed by doing the following:
% while (number of features =< F) do:
%       (a) randomly select an example;
%       (b) search for good clauses using the example selected;
%       (c) construct new features using good clauses

induce_features:-
  clean_up,
  store_values([good,check_good,updateback,construct_features,samplesize,greedy,explore,lazy_on_contradiction]),
  aleph5_set_setting(good,true),
  aleph5_set_setting(check_good,true),
  aleph5_set_setting(updateback,false),
  aleph5_set_setting(construct_features,true),
  aleph5_set_setting(lazy_on_contradiction,true),
  (
    aleph5_setting(feature_construction, exhaustive)
  ->
    aleph5_set_setting(explore, true)
  ;
    true
  ),
  aleph5_setting(max_features,FMax),
  record_settings,
  stopwatch(StartClock),
  '$aleph_global'(atoms_left,atoms_left(pos,AtomsLeft)),
  repeat,
  gen_sample(pos,0),
  retractall('$aleph_global'(besthyp,besthyp(_,_,_,_,_))),
  asserta('$aleph_global'(besthyp,besthyp([-1e10,0,1,-1e10],0,(false),[],[]))),
  get_besthyp(false),
  addhyp,
  show_atoms_left,
  record_atoms_left,
  (
    '$aleph_search'(last_good,LastGood),
    LastGood >= FMax
  ;
    '$aleph_global'(atoms_left,atoms_left(pos,[]))
  ),
  !,
  gen_features,
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(features),
  record_features(Time),
  retract('$aleph_global'(atoms_left,atoms_left(pos,_))),
  assertz('$aleph_global'(atoms_left,atoms_left(pos,AtomsLeft))),
  reinstate_values([good,check_good,updateback,construct_features,samplesize,greedy,explore,lazy_on_contradiction]), !.
induce_features.

% induce_tree/0: construct a theory using recursive partitioning
% rules are obtained by building a tree
% the tree constructed can be one of 4 types
%        classification, regression, class_probability or model
%        the type is set by aleph5_set_setting(tree_type,...)
% In addition, the following parameters are relevant
%        aleph5_set_setting(classes,ListofClasses): when tree_type is classification or
%                                    or class_probability
%        aleph5_set_setting(prune_tree,Flag): for pruning rules from a tree
%        aleph5_set_setting(confidence,C): for pruning of rules as described by
%                           J R Quinlan in the C4.5 book
%        aleph5_set_setting(lookahead,L): lookahead for the refinement operator to avoid
%                          local zero-gain literals
%        aleph5_set_setting(dependent,A): argument of the dependent variable in the examples
% The basic procedure attempts to construct a tree to predict the dependent
% variable in the examples. Note that the mode declarations must specify the
% variable as an output argument. Paths from root to leaf constitute clauses.
% Tree-construction is viewed as a refinement operation: any leaf can currently
% be refined by extending the corresponding clause. The extension is done using
% Aleph's automatic refinement operator that extends clauses within the mode
% language. A lookahead option allows additions to include several literals.
% Classification problems currently use entropy gain to measure worth of additions.
% Regression and model trees use reduction in standard deviation to measure
% worth of additions. This is not quite correct for the latter.
% Pruning for classification is done on the final set of clauses from the tree.
% The technique used here is the reduced-error pruning method.
% For classification trees, this is identical to the one proposed by
% Quinlan in C4.5: Programs for Machine Learning, Morgan Kauffmann.
% For regression and model trees, this is done by using a pessimistic estimate
% of the sample standard deviation. This assumes normality of observed values
% in a leaf. This method and others have been studied by L. Torgo in
% "A Comparative Study of Reliable Error Estimators for Pruning Regression
% Trees"
% Following work by F Provost and P Domingos, pruning is not employed
% for class probability prediction.
% Currently no pruning is performed for model trees.
induce_tree:-
  clean_up,
  aleph5_setting(tree_type,Type),
  store_values([refine]),
  aleph5_set_setting(refine,auto),
  aleph5_setting(mingain,MinGain),
  (MinGain =< 0.0 ->
    err_message('inappropriate setting for mingain'),
    fail;
    true
  ),
  record_settings,
  stopwatch(StartClock),
  construct_tree(Type),
  stopwatch(StopClock),
  Time is StopClock - StartClock,
  show(theory),
  record_theory(Time),
  reinstate_values([refine]), !.
induce_tree.

% utilities for the induce predicates

% randomly pick a positive example and construct bottom clause
%  example is from those uncovered by current theory
%  and whose bottom clause has not been stored away previously
%   makes at most 100 attempts to find such an example
rsat:-
        '$aleph_global'(atoms_left,atoms_left(pos,PosSet)),
        PosSet \= [],
  store(resample),
  aleph5_set_setting(resample,1),
  rsat(100),
  reinstate(resample).

rsat(0):- !.
rsat(N):-
        gen_sample(pos,1),
  '$aleph_global'(example_selected,example_selected(pos,Num)),
  (\+('$aleph_sat'(stored,stored(Num,pos,_))) ->
    !,
    retract('$aleph_global'(example_selected,
          example_selected(pos,Num))),
    sat(pos,Num);
    N1 is N - 1,
    rsat(N1)).

get_besthyp(AbduceFlag):-
  retract('$aleph_global'(example_selected, example_selected(pos,Num))),
  reset_best_label,
  sat(Num),
  reduce,
  update_besthyp(Num),
  (
    AbduceFlag = true
  ->
    example(Num,pos,Atom),
    abgen(Atom,AbGen),
    once(
      retract('$aleph_global'(hypothesis, hypothesis(Label,_,PCover,NCover)))
    ),
    assert(
      '$aleph_global'(hypothesis, hypothesis(Label,AbGen,PCover,NCover))
    ),
    update_besthyp(Num)
  ;
    true
  ),
  fail.
get_besthyp(_):-
  retract('$aleph_global'(besthyp,besthyp(L,Num,H,PC,NC))),
  H \= false, !,
  (
    aleph5_setting(samplesize,S),
    S > 1
  ->
    aleph5_setting(nodes,Nodes),
    show_clause(sample,L,H,Nodes),
    record_clause(sample,L,H,Nodes)
  ;
    true
  ),
  add_hyp(L,H,PC,NC),
  asserta('$aleph_global'(example_selected, example_selected(pos,Num))),
  !.
get_besthyp(_).

reset_best_label:-
  '$aleph_global'(besthyp,besthyp(Label1,_,Clause,P,N)),
  '$aleph_search'(best_label,Label/_),
  Label = [_,_,L,Gain|_],
  Label1 = [_,_,L1,Gain1|_],
        % Gain1 > Gain, !,
        ((Gain1 > Gain);(Gain1 =:= Gain, L1 < L)), !,
  retract('$aleph_search'(best_label,Label/_)),
  asserta('$aleph_search'(best_label,Label1/0)),
  retractall('$aleph_search'(selected,_)),
  asserta('$aleph_search'(selected,selected(Label1,Clause,P,N))).
reset_best_label.

update_besthyp(Num):-
  '$aleph_global'(hypothesis,hypothesis(Label,H,PCover,NCover)),
  '$aleph_global'(besthyp,besthyp(Label1,_,_,_,_)),
  Label = [_,_,L,Gain|_],
  Label1 = [_,_,L1,Gain1|_],
        % Gain > Gain1, !,
        ((Gain > Gain1);(Gain =:= Gain1, L < L1)), !,
  retract('$aleph_global'(besthyp,besthyp(Label1,_,_,_,_))),
  assertz('$aleph_global'(besthyp,besthyp(Label,Num,H,PCover,NCover))).
update_besthyp(_).

% generate a new feature from a good clause
gen_features:-
  aleph_abolish('$aleph_feature'/2),
  (aleph5_setting(dependent,PredictArg) -> true; PredictArg is 0),
        (aleph5_setting(minscore,FMin) -> true; FMin = -1e10),
  '$aleph_good'(_,Label,Clause),
  Label = [_,_,_,F|_],
  F >= FMin,
  split_clause(Clause,Head,Body),
  Body \= true,
  functor(Head,Name,Arity),
  functor(Template,Name,Arity),
  copy_iargs(Arity,Head,Template,PredictArg),
  get_feature_class(PredictArg,Head,Body,Class),
  gen_feature((Template:-Body),Label,Class),
  fail.
gen_features:-
  (aleph5_setting(dependent,PredictArg) -> true; PredictArg is 0),
  aleph5_setting(good,true),
  aleph5_setting(goodfile,File),
  aleph_open(File,read,Stream),
        (aleph5_setting(minscore,FMin) -> true; FMin = -1e10),
  repeat,
  read(Stream,Fact),
  (Fact = '$aleph_good'(_,Label,Clause) ->
    Label = [_,_,_,F|_],
    F >= FMin,
    split_clause(Clause,Head,Body),
    Body \= true,
    functor(Head,Name,Arity),
    functor(Template,Name,Arity),
    copy_iargs(Arity,Head,Template,PredictArg),
    get_feature_class(PredictArg,Head,Body,Class),
    gen_feature((Template:-Body),Label,Class),
    fail;
    close(Stream), !
  ).
gen_features.

get_feature_class(Argno,Head,Body,Class):-
  has_class(Argno,Head,Body,Class), !.
get_feature_class(_,_,_,_).

has_class(Argno,Head,_,Class):-
  arg(Argno,Head,Class),
  ground(Class), !.
has_class(Argno,Head,Body,Class):-
  arg(Argno,Head,DepVar),
  in((DepVar=Class),Body),
  ground(Class), !.

ask_example(E):-
  current_stream(Stream),
  ('$aleph_global'(example_selected,example_selected(pos,N)) ->
    example(N,pos,E1);
    E1 = none),
  !,
  show_options(Stream, example_selection),
  format(Stream, '\t\t\t\tResponse ', []),
  p1_message(default:E1),
  format(Stream, '?\n', []),
  read(Response),
  (Response = ok  -> E = E1; E = Response).

process_hypothesis(Stream):-
  show(hypothesis),
  repeat,
  show_options(Stream, hypothesis_selection),
  format(Stream, '\t\t\t\tResponse?\n', []),
  read(Response),
  process_hypothesis(Stream, Response),
  (Response = end_of_file; Response = none), !.

process_hypothesis(Stream, end_of_file):-
  format(Stream, '\n\n', []),
  !.
process_hypothesis(Stream, none):-
  format(Stream, '\n\n', []),
  !.
process_hypothesis(Stream, ok):-
  !,
  update_theory(_),
  format(Stream, '\n', []),
  p_message('added new clause').
process_hypothesis(Stream, prune):-
  !,
  retract('$aleph_global'(hypothesis,hypothesis(_,H,_,_))),
  Prune = (
    hypothesis(Head,Body,_),
    goals_to_list(Body,BodyL),
    clause_to_list(H,HL),
    aleph_subsumes(HL,[Head|BodyL])
  ),
  assertz((prune(H):- Prune)),
  nl(Stream),
  p_message('added new prune statement').
process_hypothesis(Stream, overgeneral):-
  !,
  retract('$aleph_global'(hypothesis,hypothesis(_,H,_,_))),
  Constraint = (
    hypothesis(Head,Body,_),
    goals_to_list(Body,BodyL),
    clause_to_list(H,HL),
    aleph_subsumes([Head|BodyL],HL)
  ),
  assertz((false:- Constraint)),
  nl(Stream),
  p_message('added new constraint').
process_hypothesis(Stream, overgeneral because not(E)):-
  !,
  record_example(check,neg,E,_),
  nl(Stream),
  p_message('added new negative example').
process_hypothesis(Stream, overspecific):-
  !,
  retract('$aleph_global'(hypothesis,hypothesis(_,H,_,_))),
  (
    retract('$aleph_global'(example_selected,example_selected(_,_)))
  ->
    true
  ;
    true
  ),
  record_example(check,pos,H,N),
  asserta('$aleph_global'(example_selected,example_selected(pos,N))),
  nl(Stream),
  p_message('added new positive example').
process_hypothesis(Stream, overspecific because E):-
  !,
  retract('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
  (
    retract('$aleph_global'(example_selected,example_selected(_,_)))
  ->
    true
  ;
    true
  ),
  record_example(check,pos,E,N),
  asserta('$aleph_global'(example_selected,example_selected(pos,N))),
  nl(Stream),
  p_message('added new positive example').
process_hypothesis(_Stream, AlephCommand):-
  AlephCommand.

show_options(Stream, example_selection):-
  current_stream(Stream),
  format(Stream, '\n\t\t\t\tOptions:\n', []),
  format(Stream, '\t\t\t\t\t\t\t\t-> "ok." to accept default example\n', []),
  format(Stream, '\t\t\t\t\t\t\t\t-> Enter an example\n', []),
  format(Stream, '\t\t\t\t\t\t\t\t-> ctrl-D or "none." to end\n\n', []).
show_options(Stream, hypothesis_selection):-
  current_stream(Stream),
  format(Stream, '\n\t\t\t\tOptions:\n', []),
  format(Stream, '\t\t\t\t\t\t\t\t-> "ok." to accept clause\n', []),
  format(
    Stream,
    '\t\t\t\t\t\t\t\t-> "prune." to prune clause and its refinements from the search\n',
    []
  ),
  format(
    Stream,
    '\t\t\t\t\t\t\t\t-> "overgeneral." to add clause as a constraint\n',
    []
  ),
  format(
    Stream,
    '\t\t\t\t\t\t\t\t-> "overgeneral because not(E)." to add E as a negative example\n',
    []
  ),
  format(
    Stream,
    '\t\t\t\t\t\t\t\t-> "overspecific." to add clause as a positive example\n',
    []
  ),
  format(
    Stream,
    '\t\t\t\t\t\t\t\t-> "overspecific because E." to add E as a positive example\n',
    []
  ),
  format(Stream, '\t\t\t\t\t\t\t\t-> any Aleph command\n', []),
  format(Stream, '\t\t\t\t\t\t\t\t-> ctrl-D or "none." to end\n\n', []).

get_performance(_Stream):-
  aleph5_setting(evalfn,Evalfn),
  (Evalfn = sd; Evalfn = mse), !.
get_performance(Stream):-
  (aleph5_setting(train_pos,PFile) ->
    test(PFile,noshow,Tp,TotPos),
    Fn is TotPos - Tp;
    TotPos = 0, Tp = 0, Fn = 0),
  (aleph5_setting(train_neg,NFile) ->
    test(NFile,noshow,Fp,TotNeg),
    Tn is TotNeg - Fp;
    TotNeg = 0, Tn = 0, Fp = 0),
  TotPos + TotNeg > 0,
  p_message(Stream, 'Training set performance'),
  write_cmatrix(Stream, [Tp,Fp,Fn,Tn]),
  p1_message(Stream, 'Training set summary'),
  p_message(Stream, [Tp,Fp,Fn,Tn]),
  fail.
get_performance(Stream):-
  (aleph5_setting(test_pos,PFile) ->
    test(PFile,noshow,Tp,TotPos),
    Fn is TotPos - Tp;
    TotPos = 0, Tp = 0, Fn = 0),
  (aleph5_setting(test_neg,NFile) ->
    test(NFile,noshow,Fp,TotNeg),
    Tn is TotNeg - Fp;
    TotNeg = 0, Tn = 0, Fp = 0),
  TotPos + TotNeg > 0,
  p_message(Stream, 'Test set performance'),
  write_cmatrix(Stream, [Tp,Fp,Fn,Tn]),
  p1_message(Stream, 'Test set summary'),
  p_message(Stream, [Tp,Fp,Fn,Tn]),
  fail.
get_performance(_Stream).

write_cmatrix(Stream, [Tp,Fp,Fn,Tn]):-
  P is Tp + Fn, N is Fp + Tn,
  PP is Tp + Fp, PN is Fn + Tn,
  Total is PP + PN,
  (Total = 0 -> Accuracy is 0.5; Accuracy is (Tp + Tn)/Total),
  find_max_width([Tp,Fp,Fn,Tn,P,N,PP,PN,Total],0,W1),
  W is W1 + 2,
  tab(Stream, 5), format(Stream, ' ', []),
  tab(Stream, W), format(Stream, 'Actual', []), nl(Stream),
  tab(Stream, 5), format(Stream, ' ', []), write_entry(Stream, W,'+'),
  tab(Stream, 6), write_entry(Stream, W,'-'), nl(Stream),
  tab(Stream, 5), format(Stream, '+', []), write_entry(Stream, W,Tp),
  tab(Stream, 6), write_entry(Stream, W,Fp),
  tab(Stream, 6), write_entry(Stream, W,PP), nl(Stream),
  format(Stream, 'Pred ', []), nl(Stream),
  tab(Stream, 5), format(Stream, '-', []), write_entry(Stream, W,Fn),
  tab(Stream, 6), write_entry(Stream, W,Tn),
  tab(Stream, 6), write_entry(Stream, W,PN), nl(Stream), nl(Stream),
  tab(Stream, 5), format(Stream, ' ', []), write_entry(Stream, W,P),
  tab(Stream, 6), write_entry(Stream, W,N),
  tab(Stream, 6), write_entry(Stream, W,Total), nl(Stream), nl(Stream),
  format(Stream, 'Accuracy = ~w\n', [Accuracy]).

find_max_width([],W,W).
find_max_width([V|T],W1,W):-
  name(V,VList),
  length(VList,VL),
  (VL > W1 -> find_max_width(T,VL,W);
          find_max_width(T,W1,W)).

write_entry(Stream, W, V):-
  name(V,VList),
  length(VList,VL),
  Y is integer((W-VL)/2),
  tab(Stream, Y),
  format(Stream, '~w', [V]),
  tab(Stream, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A B D U C T I O N

% Generalisation of an abductive explanation for a fact.
% The basic procedure is a simplified variant of S. Moyle's Alecto
% program. Alecto is described in some detail in S. Moyle,
% "Using Theory Completion to Learn a Navigation Control Program",
% Proceedings of the Twelfth International Conference on ILP (ILP2002),
% S. Matwin and C.A. Sammut (Eds), LNAI 2583, pp 182-197,
% 2003.
% Alecto does the following: for each positive example,  an
% abductive explanation is obtained. This explanation is set of
% ground atoms. The union of abductive explanations from all
% positive examples is formed (this is also a set of ground atoms).
% These are then generalised to give the final theory. The
% ground atoms in an abductive explanation are obtained using
% Yamamoto's SOLD resolution or SOLDR (Skip Ordered Linear resolution for
% Definite clauses).
% One complication with abductive learning is this: for a given
% positive example to be provable, we require all the ground atoms
% in its abductive explanation to be true. Correctly therefore,
% we would need to assert the abductive explanation before
% checking the utility of any hypothesis. To avoid unnecessary
% asserts and retracts, the "pclause" trick is used here (see
% record_testclause/0).

abgen(Fact):-
  abgen(Fact,_).

abgen(Fact,AbGen):-
  retractall('$aleph_search'(abgenhyp,hypothesis(_,_,_,_))),
  Minf is -1e10,
  asserta('$aleph_search'(abgenhyp,
        hypothesis([Minf,0,1,Minf],[false],[],[]))),
  aleph5_setting(max_abducibles,Max),
  abgen(Fact,Max,AbGen),
  '$aleph_global'(hypothesis,hypothesis(Label,_,PCover,NCover)),
  Label = [_,_,L,Gain|_],
  '$aleph_search'(abgenhyp,hypothesis(Label1,_,_,_)),
  Label1 = [_,_,L1,Gain1|_],
  once(((Gain > Gain1); (Gain =:= Gain1, L < L1))),
  once(retract('$aleph_search'(abgenhyp,hypothesis(_,_,_,_)))),
  asserta('$aleph_search'(abgenhyp,
      hypothesis(Label,AbGen,PCover,NCover))),
  fail.
abgen(_,AbGen):-
  retractall('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
  '$aleph_search'(abgenhyp,hypothesis(Label,AbGen,PCover,NCover)),
  asserta('$aleph_global'(hypothesis,
      hypothesis(Label,AbGen,PCover,NCover))).

abgen(Fact,Max,AbGen):-
  sold_prove(Fact,AbAtoms),
  ground(AbAtoms),
  length(AbAtoms,N),
  N =< Max,
  store_abduced_atoms(AbAtoms),
  store(proof_strategy),
  aleph5_set_setting(proof_strategy,sld),
  gen_abduced_atoms(AbAtoms,AbGen),
  reinstate(proof_strategy),
  remove_abduced_atoms(AbAtoms).

gen_abduced_atoms([],[]).
gen_abduced_atoms([AbAtom|AbAtoms],[AbGen|AbGens]):-
  functor(AbAtom,Name,Arity),
  add_determinations(Name/Arity,true),
  sat(AbAtom),
  reduce,
  '$aleph_global'(hypothesis,hypothesis(_,AbGen,_,_)),
  remove_explained(AbAtoms,AbGen,AbAtoms1),
  gen_abduced_atoms(AbAtoms1,AbGens).

remove_explained([],_,[]).
remove_explained([AbAtom|AbAtoms],(Head:-Body),Rest):-
  \+((\+ ((AbAtom = Head), Body))), !,
  remove_explained(AbAtoms,(Head:-Body),Rest).
remove_explained([AbAtom|AbAtoms],(Head:-Body),[AbAtom|Rest]):-
  remove_explained(AbAtoms,(Head:-Body),Rest).

store_abduced_atoms([],[]).
store_abduced_atoms([AbAtom|AbAtoms],[DbRef|DbRefs]):-
  assertz('$aleph_search'(abduced,pclause(AbAtom,true)),DbRef),
  store_abduced_atoms(AbAtoms,DbRefs).

store_abduced_atoms([]).
store_abduced_atoms([AbAtom|AbAtoms]):-
  assertz('$aleph_search'(abduced,pclause(AbAtom,true))),
  store_abduced_atoms(AbAtoms).

remove_abduced_atoms([]).
remove_abduced_atoms([AbAtom|AbAtoms]):-
  retract('$aleph_search'(abduced,pclause(AbAtom,true))),
  remove_abduced_atoms(AbAtoms).


%    sold_prove(+G,-A)
% Where G is an input goal (comma separated conjunction of atoms)
% and A is a list of atoms (containing the abductive explanation).
% This procedure is due to S.Moyle
sold_prove(Goal,SkippedGoals):-
  soldnf_solve(Goal,Skipped),
  sort(Skipped,SkippedGoals).

soldnf_solve(Goal,Skipped):-
  soldnf_solve(Goal,true,[],Skipped).

soldnf_solve((Goal,Goals),Status,SkippedSoFar,Skipped):-
  !,
  soldnf_solve(Goal,Status1,SkippedSoFar,Skipped1),
  soldnf_solve(Goals,Status2,Skipped1,Skipped),
  conj_status(Status1,Status2,Status).
soldnf_solve(not(Goal),true,SkippedSoFar,Skipped):-
  soldnf_solve(Goal,false,SkippedSoFar,Skipped).
soldnf_solve(not(Goal),false,SkippedSoFar,Skipped):-
  !,
  soldnf_solve(Goal,true,SkippedSoFar,Skipped).
soldnf_solve(Goal,Status,SkippedSoFar,SkippedSoFar):-
  soldnf_builtin(Goal), !,
  soldnfcall(Goal,Status).
soldnf_solve(Goal,Status,SkippedSoFar,Skipped):-
  soldnf_clause(Goal,Body),
  soldnf_solve(Body,Status,SkippedSoFar,Skipped).
soldnf_solve(Goal,true,SkippedSoFar,[Goal|SkippedSoFar]):-
  skippable(Goal).

soldnf_clause(Goal,_Body):-soldnf_builtin(Goal),!,fail.
soldnf_clause(Goal,Body):-
  clause(Goal,Body).

soldnf_builtin(not(_Goal)):-!,fail.
soldnf_builtin(A):-predicate_property(A,built_in).

soldnfcall(Goal,true):-
  Goal, !.
soldnfcall(_,false).

conj_status(true,true,true):- !.
conj_status(_,_,false).

skippable(Pred):-
  functor(Pred,Name,Arity),
  '$aleph_global'(abducible,abducible(Name/Arity)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L A Z Y  E V A L U A T I O N

% lazy_evaluate_theory(+Clauses,+Lazy,+Pos,+Neg,-Theory)
% evaluate lazy preds in a set of clauses
% untested
lazy_evaluate_theory([],_,_,_,[]).
lazy_evaluate_theory([Refine|T],LazyPreds,Pos,Neg,[Refine1|T1]):-
  Refine = A-[B,C,D,Clause],
  lazy_evaluate_refinement(D,Clause,LazyPreds,Pos,Neg,D1,Clause1),
  Refine1 = A-[B,C,D1,Clause1],
  lazy_evaluate_theory(T,LazyPreds,Pos,Neg,T1).

% lazy evaluation of literals in a refinement operation
lazy_evaluate_refinement([],Refine,Lazy,Pos,Neg,[],NewRefine):-
  clause_to_list(Refine,Lits),
  lazy_evaluate_refinement(Lits,Lazy,[],Pos,Neg,Lits1),
  list_to_clause(Lits1,NewRefine), !.
lazy_evaluate_refinement(Lits,_,Lazy,Pos,Neg,Lits1,NewRefine):-
  Lits \= [],
  lazy_evaluate_refinement(Lits,Lazy,[],Pos,Neg,Lits1),
  get_pclause(Lits1,[],NewRefine,_,_,_), !.
lazy_evaluate_refinement(Lits,Refine,_,_,_,Lits,Refine).

lazy_evaluate_refinement([],_,L,_,_,L):-
  !.
lazy_evaluate_refinement([Lit|Lits],LazyPreds,Path,PosCover,NegCover,Refine):-
  lazy_evaluate([Lit],LazyPreds,Path,PosCover,NegCover,[Lit1]),
  append(Path, [Lit1], Path1),
  !,
  lazy_evaluate_refinement(Lits,LazyPreds,Path1,PosCover,NegCover,Refine).

% lazy evaluation of specified literals
% all #'d arguments of these literals are evaluated at reduction-time
% From Version 5 (dated Sat Nov 29 13:02:36 GMT 2003), collects both
% input and output args (previously only collected input args)
lazy_evaluate(Lits,[],_,_,_,Lits):- !.
lazy_evaluate([],_,_,_,_,[]):- !.
lazy_evaluate([LitNum|LitNums],LazyPreds,Path,PosCover,NegCover,Lits):-
  (integer(LitNum) ->
    BottomExists = true,
    '$aleph_sat_litinfo'(LitNum,Depth,Atom,I,O,D),
    functor(Atom,Name,Arity),
    memberchk(Name/Arity,LazyPreds), !,
    get_pclause([LitNum|Path],[],(Lit:-(Goals)),_,_,_);
    BottomExists = false,
    Atom = LitNum,
    Depth = 0,
    functor(Atom,Name,Arity),
    memberchk(Name/Arity,LazyPreds), !,
    split_args(LitNum,_,I,O,C),
    D = [],
    list_to_clause([LitNum|Path],(Lit:-(Goals)))),
  goals_to_clause(Goals,Clause),
  lazy_prove(pos,Lit,Clause,PosCover),
  ('$aleph_global'(positive_only,positive_only(Name/Arity))->
    true;
    lazy_prove_negs(Lit,Clause,NegCover)),
  functor(LazyLiteral,Name,Arity),
  collect_args(I,LazyLiteral),
  collect_args(O,LazyLiteral),
  lazy_evaluate1(BottomExists,Atom,Depth,I,O,C,D,LazyLiteral,NewLits),
  retractall('$aleph_local'(lazy_evaluate,_)),
  lazy_evaluate(LitNums,LazyPreds,Path,PosCover,NegCover,NewLits1),
  update_list(NewLits1,NewLits,Lits).
lazy_evaluate([LitNum|LitNums],LazyPreds,Path,PosCover,NegCover,[LitNum|Lits]):-
  lazy_evaluate(LitNums,LazyPreds,Path,PosCover,NegCover,Lits).

lazy_prove_negs(Lit,Clause,_):-
  setting(lazy_negs, true),
  !,
  '$aleph_global'(atoms,atoms(neg,NegCover)),
  lazy_prove(neg,Lit,Clause,NegCover).
lazy_prove_negs(Lit,Clause,NegCover):-
  lazy_prove(neg,Lit,Clause,NegCover).

collect_args([],_).
collect_args([Argno/_|Args],Literal):-
  findall(Term,
      ('$aleph_local'(lazy_evaluate,eval(pos,Lit)),
      tparg(Argno,Lit,Term)),
    PTerms),
  findall(Term,
      ('$aleph_local'(lazy_evaluate,eval(neg,Lit)),
      tparg(Argno,Lit,Term)),
    NTerms),
  tparg(Argno,Literal,[PTerms,NTerms]),
  collect_args(Args,Literal).

% when construct_bottom = false
% currently do not check if user's definition of lazily evaluated
% literal corresponds to recall number in the modes
lazy_evaluate1(false,Atom,_,I,O,C,_,Lit,NewLits):-
  functor(Atom,Name,Arity),
  p1_message('lazy evaluation'), p_message(Name),
  functor(NewLit,Name,Arity),
  findall(NewLit,(Lit,copy_args(Lit,NewLit,C)),NewLits),
  copy_io_args(NewLits,Atom,I,O).

lazy_evaluate1(true,Atom,Depth,I,O,_,D,Lit,NewLits):-
  % '$aleph_sat'(lastlit,_),
  call_library_pred(Atom,Depth,Lit,I,O,D),
  findall(LitNum,(retract('$aleph_local'(lazy_evaluated,LitNum))),NewLits).

call_library_pred(OldLit,Depth,Lit,I,O,D):-
  functor(OldLit,Name,Arity),
  '$aleph_global'(lazy_recall,lazy_recall(Name/Arity,Recall)),
  asserta('$aleph_local'(callno,1)),
  p1_message('lazy evaluation'), p_message(Name),
  repeat,
  evaluate(OldLit,Depth,Lit,I,O,D),
  retract('$aleph_local'(callno,CallNo)),
  NextCall is CallNo + 1,
  asserta('$aleph_local'(callno,NextCall)),
  NextCall > Recall,
  !,
  p_message('completed'),
  retract('$aleph_local'(callno,NextCall)).

evaluate(OldLit,_,Lit,I,O,D):-
  functor(OldLit,Name,Arity),
  functor(NewLit,Name,Arity),
  Lit,
  copy_args(OldLit,NewLit,I),
  copy_args(OldLit,NewLit,O),
  copy_consts(Lit,NewLit,Arity),
  update_lit(LitNum,false,NewLit,I,O,D),
  \+('$aleph_local'(lazy_evaluated,LitNum)),
  asserta('$aleph_local'(lazy_evaluated,LitNum)), !.
evaluate(_,_,_,_,_,_).

copy_io_args([],_,_,_).
copy_io_args([New|NewL],Old,I,O):-
  copy_args(Old,New,I),
  copy_args(Old,New,O),
  copy_io_args(NewL,Old,I,O).

copy_args(_,_,[]).
copy_args(Old,New,[Arg/_|T]):-
  tparg(Arg,Old,Term),
  tparg(Arg,New,Term),
  copy_args(Old,New,T), !.

copy_consts(_,_,0):- !.
copy_consts(Old,New,Arg):-
  arg(Arg,Old,Term),
  arg(Arg,New,Term1),
  var(Term1), !,
  Term1 = aleph_const(Term),
  Arg0 is Arg - 1,
  copy_consts(Old,New,Arg0).
copy_consts(Old,New,Arg):-
  Arg0 is Arg - 1,
  copy_consts(Old,New,Arg0).

% copy_modeterm(+Old,-New)
%  copy term structure from Old to New
%  by finding an appropriate mode declaration
copy_modeterm(Lit1,Lit2):-
  functor(Lit1,Name,Arity),
  find_mode(mode,Name/Arity,Mode),
  functor(Lit2,Name,Arity),
  copy_modeterms(Mode,Lit2,Arity),
  \+((\+ (Lit1 = Lit2))).

% find_mode(+modetype,+Name/+Arity,-Mode)
% find a mode for Name/Arity of type modetype
find_mode(mode,Name/Arity,Mode):-
  !,
  functor(Mode,Name,Arity),
  '$aleph_global'(mode,mode(_,Mode)).
find_mode(modeh,Name/Arity,Mode):-
  !,
  functor(Mode,Name,Arity),
  '$aleph_global'(modeh,modeh(_,Mode)).
find_mode(modeb,Name/Arity,Mode):-
  !,
  functor(Mode,Name,Arity),
  '$aleph_global'(modeb,modeb(_,Mode)).

%! copy_modeterms(+Mode, +Lit, +Arity)
% Copy all term structures in a mode template.
%
% Example of a mode template:
% ~~~{.pl}
% father(+person,+person)
% ~~~
%
% Example of a literal:
% ~~~{.pl}
% father(_G406,_G407)
% ~~~
%
% The arity for the above example is _2_.

copy_modeterms(_Mode, _Lit, 0):-
  !.
copy_modeterms(Mode, Lit, Arg):-
  % Take one of the mode arguments, this is a modetype.
  arg(Arg, Mode, Term),
  nonvar(Term),
  % Take the prefix of the simple modetype.
  functor(Term, Name, Arity),
  \+(
    (
      Name = '+'
    ;
      Name = '-'
    ;
      Name = '#'
    )
  ),
  !,
  functor(NewTerm,Name,Arity),
  arg(Arg,Lit,NewTerm),
  copy_modeterms(Term,NewTerm,Arity),
  Arg0 is Arg - 1,
  copy_modeterms(Mode,Lit,Arg0).
copy_modeterms(Mode,Lit,Arg):-
  Arg0 is Arg - 1,
  copy_modeterms(Mode,Lit,Arg0).

% theorem-prover for lazy evaluation of literals
lazy_prove(Type,Lit,Clause,Intervals):-
        (Clause = (Head:-Body)->
    lazy_prove(Intervals,Type,Lit,Head,Body);
    lazy_prove(Intervals,Type,Lit,Clause,true)).

lazy_prove([],_,_,_,_).
lazy_prove([Interval|Intervals],Type,Lit,Head,Body):-
        lazy_index_prove(Interval,Type,Lit,Head,Body),
        lazy_prove(Intervals,Type,Lit,Head,Body).

lazy_index_prove(Start-Finish,_,_,_,_):-
        Start > Finish, !.
lazy_index_prove(Start-Finish,Type,Lit,Head,Body):-
        lazy_index_prove1(Type,Lit,Head,Body,Start),
        Start1 is Start + 1,
        lazy_index_prove(Start1-Finish,Type,Lit,Head,Body).

% bind input args of lazy literal
% each example gives an set of input bindings
% this is different from Aleph 2 where only a single binding was obtained
lazy_index_prove1(Type,Lit,Head,Body,Num):-
        depth_bound_call((example(Num,Type,Head),Body)),
  \+('$aleph_local'(lazy_evaluate,eval(Type,Lit))),
        asserta('$aleph_local'(lazy_evaluate,eval(Type,Lit))),
        fail.
lazy_index_prove1(_,_,_,_,_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S L P
% implemented as described by Muggleton, ILP-96

condition_target:-
  setting(condition, true),
  add_generator,
  '$aleph_global'(modeh,modeh(_,Pred)),
  functor(Pred,Name,Arity),
  p_message('conditioning'),
  make_sname(Name,SName),
  functor(SPred,SName,Arity),
  SPred =.. [_|Args],
  functor(Fact,Name,Arity),
  example(_,_,Fact),
  Fact =.. [_|Args],
  condition(SPred),
  fail.
condition_target:-
  setting(condition, false),
  add_generator, !.

add_generator:-
  '$aleph_global'(modeh,modeh(_,Pred)),
  functor(Pred,Name,Arity),
  make_sname(Name,SName),
  functor(SPred,SName,Arity),
  (clause(SPred,_)->
    true;
    add_generator(Name/Arity),
    p1_message('included generator'), p_message(SName/Arity)),
  fail.
add_generator.

add_generator(Name/Arity):-
  make_sname(Name,SName),
  functor(SPred,SName,Arity),
  find_mode(modeh,Name/Arity,Mode),
  once(copy_modeterms(Mode,SPred,Arity)),
  split_args(Mode,Mode,Input,Output,Constants),
  range_restrict(Input,SPred,[],B1),
  range_restrict(Output,SPred,B1,B2),
  range_restrict(Constants,SPred,B2,B3),
  list_to_goals(B3,Body),
  \+(clause(SPred,Body)),
  asserta((SPred:-Body)),
  fail.
add_generator(_).

make_sname(Name,SName):-
  atomic_concat('*', Name, SName).

range_restrict([],_,R,R).
range_restrict([Pos/Type|T],Pred,R0,R):-
  functor(TCheck,Type,1),
  tparg(Pos,Pred,X),
  arg(1,TCheck,X),
  range_restrict(T,Pred,[TCheck|R0],R).


condition(Fact):-
  slprove(condition,Fact), !.
condition(_).

sample(_,0,[]):- !.
sample(Name/Arity,N,S):-
  functor(Pred,Name,Arity),
  retractall('$aleph_local'(slp_samplenum,_)),
  retractall('$aleph_local'(slp_sample,_)),
  asserta('$aleph_local'(slp_samplenum,1)),
  repeat,
  slprove(stochastic,Pred),
  asserta('$aleph_local'(slp_sample,Pred)),
  retract('$aleph_local'(slp_samplenum,N1)),
  N2 is N1 + 1,
  asserta('$aleph_local'(slp_samplenum,N2)),
  N2 > N,
  !,
  retract('$aleph_local'(slp_samplenum,N2)),
  functor(Fact,Name,Arity),
  findall(Fact,(retract('$aleph_local'(slp_sample,Fact))),S).

gsample(Name/Arity,_):-
        make_sname(Name,SName),
        functor(SPred,SName,Arity),
        clause(SPred,Body),
        ground((SPred:-Body)), !,
        update_gsample(Name/Arity,_).
gsample(_,0):- !.
gsample(Name/Arity,N):-
  functor(Pred,Name,Arity),
  make_sname(Name,SName),
  functor(SPred,SName,Arity),
  Pred =.. [_|Args],
  retractall('$aleph_local'(slp_samplenum,_)),
  asserta('$aleph_local'(slp_samplenum,0)),
  repeat,
  slprove(stochastic,SPred),
  SPred =..[_|Args],
  retract('$aleph_local'(slp_samplenum,N1)),
  N2 is N1 + 1,
  asserta('$aleph_local'(slp_samplenum,N2)),
  assertz(example(N2,rand,Pred)),
  N2 >= N,
  !,
  retract('$aleph_local'(slp_samplenum,N2)),
  asserta('$aleph_global'(size,size(rand,N))),
  asserta('$aleph_global'(last_example,last_example(rand,N))),
  asserta('$aleph_global'(atoms,atoms(rand,[1-N]))),
  asserta('$aleph_global'(atoms_left,atoms_left(rand,[1-N]))).

update_gsample(Name/Arity,_):-
        functor(Pred,Name,Arity),
        make_sname(Name,SName),
        functor(SPred,SName,Arity),
        retractall('$aleph_global'(gsample,gsample(_))),
  retractall('$aleph_local'(slp_samplenum,_)),
        asserta('$aleph_local'(slp_samplenum,0)),
        SPred =.. [_|Args],
        Pred =.. [_|Args],
        clause(SPred,Body),
        ground((SPred:-Body)),
  record_example(check,rand,(Pred:-Body),N1),
        retract('$aleph_local'(slp_samplenum,_)),
        asserta('$aleph_local'(slp_samplenum,N1)),
        fail.
update_gsample(_,N):-
        '$aleph_local'(slp_samplenum,N),
        N > 0, !,
        retract('$aleph_local'(slp_samplenum,N)),
        aleph5_set_setting(gsamplesize,N),
        retract('$aleph_global'(atoms,atoms(rand,_))),
        retract('$aleph_global'(atoms_left,atoms_left(rand,_))),
        retract('$aleph_global'(last_example,last_example(rand,_))),
        assert('$aleph_global'(atoms,atoms(rand,[1-N]))),
        assert('$aleph_global'(atoms_left,atoms_left(rand,[1-N]))),
        assert('$aleph_global'(last_example,last_example(rand,N))).
update_gsample(_,_).


slprove(_,true):-
  !.
slprove(Mode,not(Goal)):-
  slprove(Mode,Goal),
  !,
  fail.
slprove(Mode,(Goal1,Goal2)):-
  !,
  slprove(Mode,Goal1),
  slprove(Mode,Goal2).
slprove(Mode,(Goal1;Goal2)):-
  !,
  slprove(Mode,Goal1);
  slprove(Mode,Goal2).
slprove(_,Goal):-
  predicate_property(Goal,built_in), !,
  Goal.
slprove(stochastic,Goal):-
  findall(Count/Clause,
    (clause(Goal,Body),Clause=(Goal:-Body),find_count(Clause,Count)),
    ClauseCounts),
  renormalise(ClauseCounts,Normalised),
  aleph_random(X),
  rselect_clause(X,Normalised,(Goal:-Body)),
  slprove(stochastic,Body).
slprove(condition,Goal):-
  functor(Goal,Name,Arity),
  functor(Head,Name,Arity),
  clause(Head,Body),
  \+(\+((Head=Goal,slprove(condition,Body)))),
  inc_count((Head:-Body)).

renormalise(ClauseCounts,Normalised):-
  sum_counts(ClauseCounts,L),
  L > 0,
  renormalise(ClauseCounts,L,Normalised).

sum_counts([],0).
sum_counts([N/_|T],C):-
  sum_counts(T,C1),
  C is N + C1.

renormalise([],_,[]).
renormalise([Count/Clause|T],L,[Prob/Clause|T1]):-
  Prob is Count/L,
  renormalise(T,L,T1).

rselect_clause(X,[P/C|_],C):- X =< P, !.
rselect_clause(X,[P/_|T],C):-
  X1 is X - P,
  rselect_clause(X1,T,C).

find_count(Clause,N):-
  copy_term(Clause,Clause1),
  '$aleph_global'(slp_count,Clause1,N), !.
find_count(_,1).

inc_count(Clause):-
  retract('$aleph_global'(slp_count,Clause,N)), !,
  N1 is N + 1,
  asserta('$aleph_global'(slp_count,Clause,N1)).
inc_count(Clause):-
  asserta('$aleph_global'(slp_count,Clause,2)).

find_posgain(PCover,P):-
  setting(greedy, true),
  !,
  interval_count(PCover,P).
find_posgain(PCover,P):-
  '$aleph_global'(atoms_left,atoms_left(pos,PLeft)),
  intervals_intersection(PLeft,PCover,PC),
  interval_count(PC,P).



% SEARCH I/O

record_clause(good,Label,Clause,_):-
  aleph5_setting(good,true),
  aleph5_setting(goodfile_stream,GoodfileStream), !,
  set_output(GoodfileStream),
  Label = [_,_,L|_],
  aleph_writeq('$aleph_good'(L,Label,Clause)),
  current_stream(Stream),
  format(Stream, '.\n', []),
  flush_output(GoodfileStream),
  set_output(user_output).
record_clause(Flag,Label,Clause,Nodes):-
  Flag \= good,
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show_clause(Flag,Label,Clause,Nodes),
  flush_output(Stream),
  set_output(user_output).
record_clause(_,_,_,_).

record_theory(Flag,Label,Clauses,Nodes):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show_theory(Label,Clauses,Nodes,Flag),
  flush_output(Stream),
        set_output(user_output).
record_theory(_,_,_,_).

record_theory(Flag,Label,Clauses,Nodes):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show_theory(Label,Clauses,Nodes,Flag),
  flush_output(Stream),
  set_output(user_output).
record_theory(_,_,_,_).

record_sat_example(N):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  p1_message('sat'), p_message(N),
  flush_output(Stream),
  set_output(user_output).
record_sat_example(_).

record_search_stats(Clause,Nodes,Time):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  p1_message('clauses constructed'), p_message(Nodes),
  p1_message('search time'), p_message(Time),
  p_message('best clause'),
  pp_dclause(Clause),
  % show(hypothesis),
  flush_output(Stream),
  set_output(user_output).
record_search_stats(_,_,_).

record_tsearch_stats(Theory,Nodes,Time):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  p1_message('theories constructed'), p_message(Nodes),
  p1_message('search time'), p_message(Time),
  p_message('best theory'),
  pp_dclauses(Theory),
  flush_output(Stream),
  set_output(user_output).
record_tsearch_stats(_,_,_).

record_theory(Time):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show(theory),
  stream_message('[time taken] ~w seconds\n', [Time]),
  (
    setting(maxcover, true)
  ->
    show(aleph,theory/5), nl(Stream),
    show(aleph,max_set/4), nl(Stream),
    show(aleph,rules/1)
  ;
    true
  ),
  flush_output(Stream),
  set_output(user_output).
record_theory(_).

record_features(Time):-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show(features),
  stream_message('[time taken] ~w seconds\n', [Time]),
  flush_output(Stream),
  set_output(user_output).
record_features(_).

record_settings:-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  (
    is_unix
  ->
    execute(date),
    execute(hostname)
  ;
    true
  ),
  show(settings),
  flush_output(Stream),
  set_output(user_output).
record_settings.

show_clause(Flag,Label,Clause,Nodes):-
  broadcast(clause(Flag,Label,Clause,Nodes)),
  p_message('-------------------------------------'),
  (
    Flag=good
  ->
    p_message('good clause')
  ;
    (
      Flag = sample
    ->
      p_message('selected from sample')
    ;
      p_message('found clause')
    )
  ),
  pp_dclause(Clause),
  (
    aleph5_setting(evalfn,Evalfn)
  ->
    true
  ;
    Evalfn = coverage
  ),
  show_stats(Evalfn,Label),
  p1_message('clause label'),
  p_message(Label),
  p1_message('clauses constructed'),
  p_message(Nodes),
  p_message('-------------------------------------').

show_theory(Flag,Label,Clauses,Nodes):-
        p_message('-------------------------------------'),
        (Flag=good -> p_message('good theory');
                (Flag=sample-> p_message('selected from sample');
                        p_message('found theory'))),
        pp_dclauses(Clauses),
        (aleph5_setting(evalfn,Evalfn)-> true; Evalfn = accuracy),
        show_stats(Evalfn,Label),
        p1_message('theory label'), p_message(Label),
        p1_message('theories constructed'), p_message(Nodes),
        p_message('-------------------------------------').

update_search_stats(N,T):-
  (retract('$aleph_global'(search_stats,search_stats(N0,T0))) ->
      N1 is N0 + N,
      T1 is T0 + T;
      N1 is N,
      T1 is T),
  asserta('$aleph_global'(search_stats,search_stats(N1,T1))).

record_total_stats:-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show_total_stats,
  flush_output(Stream),
  set_output(user_output).
record_total_stats.

record_atoms_left:-
  aleph5_setting(recordfile_stream, Stream),
  is_stream(Stream),
  !,
  set_output(Stream),
  show_atoms_left,
  flush_output(Stream),
  set_output(user_output).
record_atoms_left.

show_total_stats:-
  '$aleph_global'(search_stats,search_stats(Nodes,_)), !,
  p1_message('total clauses constructed'),
  p_message(Nodes).
show_total_stats.

show_atoms_left:-
  '$aleph_global'(atoms_left,atoms_left(pos,PLeft)),
  interval_count(PLeft,NLeft),
  '$aleph_global'(size,size(pos,NPos)),
  '$aleph_global'(search_stats,search_stats(_,Time)),
  EstTime is (Time*NLeft)/(NPos - NLeft),
  p1_message('positive examples left'), p_message(NLeft),
  p1_message('estimated time to finish (secs)'),
  p_message(EstTime),
  !.
show_atoms_left.

show_stats(Evalfn,[P,N,_,F|_]):-
  ((Evalfn = user; Evalfn = entropy; Evalfn = gini) ->
    Value is -F;
    Value is F
  ),
  format(atom(Message), 'pos cover = ~w neg cover = ~w', [P, N]),
  p1_message(Message),
  print_eval(Evalfn,Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A U T O  -- R E F I N E
%
% built-in refinement operator

gen_auto_refine:-
  aleph5_setting(autorefine, true),
  !.
gen_auto_refine:-
  aleph5_set_setting(autorefine, true),
  process_modes,
  process_determs.

process_modes:-
  once(aleph_abolish('$aleph_link_vars'/2)),
  once(aleph_abolish('$aleph_has_vars'/3)),
  once(aleph_abolish('$aleph_has_ovar'/4)),
  once(aleph_abolish('$aleph_has_ivar'/4)),
  '$aleph_global'(modeb,modeb(_,Mode)),
  process_mode(Mode),
  fail.
process_modes:-
  '$aleph_global'(determination,determination(Name/Arity,_)),
  find_mode(modeh,Name/Arity,Mode),
  split_args(Mode,Mode,I,O,_),
  functor(Lit,Name,Arity),
  copy_modeterms(Mode,Lit,Arity),
  add_ivars(Lit,I),
  add_ovars(Lit,O),
  add_vars(Lit,I,O),
  fail.
process_modes.

process_determs:-
  once(aleph_abolish('$aleph_determination'/2)),
  '$aleph_global'(determination,determination(Name/Arity,Name1/Arity1)),
  functor(Pred,Name1,Arity1),
  find_mode(modeb,Name1/Arity1,Mode),
  copy_modeterms(Mode,Pred,Arity1),
  Determ = '$aleph_determination'(Name/Arity,Pred),
  (Determ -> true; assert(Determ)),
  fail.
process_determs.

process_mode(Mode):-
  functor(Mode,Name,Arity),
  split_args(Mode,Mode,I,O,C),
  functor(Lit,Name,Arity),
  copy_modeterms(Mode,Lit,Arity),
  add_ioc_links(Lit,I,O,C),
  add_ovars(Lit,O),
  add_vars(Lit,I,O).

add_ioc_links(Lit,I,O,C):-
  Clause = ('$aleph_link_vars'(Lit,Lits):-
      var_types(Lits,VT),
      Body),
  get_o_links(O,Lit,VT,true,OGoals),
  get_i_links(I,Lit,VT,OGoals,IOGoals),
  get_c_links(C,Lit,IOGoals,Body),
  assert(Clause).

add_ovars(Lit,O):-
  member(Pos/Type,O),
  tparg(Pos,Lit,V),
  ('$aleph_has_ovar'(Lit,V,Type,Pos)->true;
    assert('$aleph_has_ovar'(Lit,V,Type,Pos))),
  fail.
add_ovars(_,_).

add_ivars(Lit,I):-
  member(Pos/Type,I),
  tparg(Pos,Lit,V),
  ('$aleph_has_ivar'(Lit,V,Type,Pos)->true;
    assert('$aleph_has_ivar'(Lit,V,Type,Pos))),
  fail.
add_ivars(_,_).

add_vars(Lit,I,O):-
        get_var_types(I,Lit,IVarTypes),
        get_var_types(O,Lit,OVarTypes),
        ('$aleph_has_vars'(Lit,IVarTypes,OVarTypes) -> true;
          assert('$aleph_has_vars'(Lit,IVarTypes,OVarTypes))).

get_var_types([],_,[]).
get_var_types([Pos/Type|PlaceTypes],Lit,[Var/Type|Rest]):-
        tparg(Pos,Lit,Var),
        get_var_types(PlaceTypes,Lit,Rest).

get_o_links([],_,_,Goals,Goals).
get_o_links([Pos/Type|T],Lit,VarTypes,GoalsSoFar,Goals):-
  tparg(Pos,Lit,V),
  Goal = (aleph_output_var(V,Type,VarTypes);
    aleph_output_var(V,Type,Lit,Pos)),
  prefix_lits((Goal),GoalsSoFar,G1),
  get_o_links(T,Lit,VarTypes,G1,Goals).


get_i_links([],_,_,Goals,Goals).
get_i_links([Pos/Type|T],Lit,VarTypes,GoalsSoFar,Goals):-
  tparg(Pos,Lit,V),
  Goal = aleph_input_var(V,Type,VarTypes),
  prefix_lits((Goal),GoalsSoFar,G1),
  get_i_links(T,Lit,VarTypes,G1,Goals).

get_c_links([],_,Goals,Goals).
get_c_links([Pos/Type|T],Lit,GoalsSoFar,Goals):-
  tparg(Pos,Lit,V),
  TypeFact =.. [Type,C],
  Goal = (TypeFact,V=C),
  prefix_lits((Goal),GoalsSoFar,G1),
  get_c_links(T,Lit,G1,Goals).

aleph_input_var(Var,Type,VarTypes):-
        member(Var/Type1,VarTypes),
  nonvar(Type1),
  Type = Type1.

aleph_output_var(Var,Type,VarTypes):-
        member(Var/Type1,VarTypes),
  nonvar(Type1),
  Type = Type1.
aleph_output_var(_,_,_).

aleph_output_var(Var,Type,Lit,ThisPos):-
  '$aleph_has_ovar'(Lit,Var,Type,Pos),
  Pos @< ThisPos.

var_types([Head|Body],VarTypes):-
  hvar_types(Head,HVarTypes),
  bvar_types(Body,HVarTypes,BVarTypes),
  append(HVarTypes, BVarTypes, VarTypesList),
  sort(VarTypesList,VarTypes).

hvar_types(Head,HVarTypes):-
  '$aleph_has_vars'(Head,IVarTypes,OVarTypes),
  append(OVarTypes, IVarTypes, HVarTypes).

bvar_types([],V,V).
bvar_types([Lit|Lits],VTSoFar,BVarTypes):-
  '$aleph_has_vars'(Lit,IVarTypes,OVarTypes),
  consistent_vartypes(IVarTypes,VTSoFar),
  \+(inconsistent_vartypes(OVarTypes,VTSoFar)),
  append(VTSoFar, OVarTypes, VT1),
  bvar_types(Lits,VT1,BVarTypes).

consistent_vartypes([],_).
consistent_vartypes([Var/Type|VarTypes],VTSoFar):-
  memberchk_eq(Var/Type,VTSoFar),
  consistent_vartypes(VarTypes,VTSoFar).

inconsistent_vartypes([Var/Type|_],VTSoFar):-
  member(Var1/Type1,VTSoFar),
  Var == Var1,
  Type \== Type1,
  !.
inconsistent_vartypes([_|VarTypes],VTSoFar):-
  inconsistent_vartypes(VarTypes,VTSoFar).

aleph_get_hlit(Name/Arity,Head):-
  functor(Head,Name,Arity),
  find_mode(modeh,Name/Arity,Mode),
  once(split_args(Mode,Mode,_,_,C)),
  copy_modeterms(Mode,Head,Arity),
  get_c_links(C,Head,true,Equalities),
  Equalities.

aleph_get_lit(Lit,[H|Lits]):-
  functor(H,Name,Arity),
  aleph_get_lit(Lit,Name/Arity),
  '$aleph_link_vars'(Lit,[H|Lits]),
  \+(memberchk_eq(Lit,[H|Lits])).

aleph_get_lit(Lit,Target):-
  '$aleph_determination'(Target,Lit).

% aleph_mode_linked(+Lits)
% checks to see if a sequence of literals are within mode language
% using information compiled by process_modes/0
aleph_mode_linked([H|B]):-
  aleph_mode_linked(B,[H]).

aleph_mode_linked([],_):- !.
aleph_mode_linked([Lit|Lits],LitsSoFar):-
  '$aleph_link_vars'(Lit,LitsSoFar),
  append(LitsSoFar, [Lit], L1),
  aleph_mode_linked(Lits,L1).

auto_refine(false,Head):-
  example_saturated(Example),
  functor(Example,Name,Arity),
        aleph_get_hlit(Name/Arity,Head),
  Head \== false.
auto_refine(false,Head):-
        '$aleph_global'(modeh,modeh(_,Pred)),
  functor(Pred,Name,Arity),
        aleph_get_hlit(Name/Arity,Head),
  Head \== false.
auto_refine((H:-B),(H1:-B1)):-
        !,
        goals_to_list((H,B),LitList),
        aleph5_setting(clauselength,L),
        length(LitList,ClauseLength),
        ClauseLength < L,
        aleph_get_lit(Lit,LitList),
        append(LitList, [Lit], LitList1),
        list_to_goals(LitList1,(H1,B1)),
  \+(prune((H1:-B1))),
  \+(tautology((H1:-B1))),
  (aleph5_setting(language,Lang) ->
    lang_ok(Lang,H1,B1);
    true),
  (aleph5_setting(newvars,NewVars) ->
    newvars_ok(NewVars,H1,B1);
    true).
auto_refine(Head,Clause):-
        auto_refine((Head:-true),Clause).

% refinement with lookahead
auto_refine(1,Clause1,Clause2):-
  !,
  auto_refine(Clause1,Clause2).
auto_refine(L,Clause1,Clause2):-
  L1 is L - 1,
  auto_refine(L1,Clause1,Clause),
  (Clause2 = Clause;
    auto_refine(Clause,Clause2)).

auto_extend((H:-B),Lit,(H1:-B1)):-
  !,
  goals_to_list((H,B),LitList),
  aleph5_setting(clauselength,L),
  length(LitList,ClauseLength),
  ClauseLength < L,
  aleph_get_lit(Lit,LitList),
  append(LitList, [Lit], LitList1),
  list_to_goals(LitList1,(H1,B1)),
  (
    aleph5_setting(language,Lang)
  ->
    lang_ok(Lang,H1,B1)
  ;
    true
  ),
  (
    aleph5_setting(newvars,NewVars)
  ->
    newvars_ok(NewVars,H1,B1)
  ;
    true
  ),
  \+(tautology((H1:-B1))),
  \+(prune((H1:-B1))).

tautology((false:-Body)):-
  !,
  in(Body,L1,Rest),
  in(Rest,not(L2)),
  L1 == L2.
tautology((Head:-Body)):-
  in(Body,Lit),
  Head == Lit, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A U T O -- M O D E

% automatic inference of mode declarations given a set of
% determinations. The procedure works in two parts: (i) finding
% equivalence classes of types; and (ii) finding an input/output
% assignment.
%
% Finding equivalence classes of types is similar to
% the work of McCreath and Sharma, Proc of the 8th Australian
% Joint Conf on AI pages 75-82, 1995. However, unlike there
% types in the same equivalence class are given the same name only if
% they "overlap" significantly (the overlap of type1 with type2
% is the proportion of elements of type1 that are also elements of type2).
% Significantly here means an overlap at least some threshold
% T (set using typeoverlap, with default 0.95).
% Since this may not be perfect, modes are also produced
% for equality statements that re-introduce co-referencing amongst
% differently named types in the same equivalence class.
% The user has to however explicitly include a determination declaration for
% the equality predicate.
%
% The i/o assignment is not straightforward, as we may be dealing
% with non-functional definitions. The assignment sought here is one
% that maximises the number of input args as this gives the
% largest bottom clause. This assignment is
% is sought by means of a search procedure over mode sequences.
% Suppose we have a mode sequence M = <m1,m2,..m{i-1}> that uses the types T.
% An argument of type t in mode m{i} is an input iff t overlaps
% significantly (used in the same sense as earlier) with some type in T.
% Otherwise the argument is an output.
% The utility of each mode sequence M is f(M) = g(M) + h(M) where
% g(M) is the number of input args in M; and h(M) is a (lower) estimate
% of the number of input args in any mode sequence of which M is a prefix.
% The search strategy adopted is a simple hill-climbing one.
%
% All very complicated: there must be a simpler approach.
% Requires generative background predicates.

search_modes:-
  '$aleph_global'(targetpred,targetpred(N/A)),
  findall(N1/A1,determinations(N/A,N1/A1),L),
  number_types([N/A|L],0,TypedPreds,Last),
  get_type_elements(TypedPreds),
  interval_to_list(1-Last,Types),
  get_type_equivalences(Types,Equiv1),
  merge_equivalence_classes(Equiv1,Equiv),
  store_type_equivalences(Equiv),
  aleph5_setting(typeoverlap,Thresh),
  infer_modes(TypedPreds,Thresh,Types,Modes),
  infer_equalities(EqModes),
  Modes = [_|BodyModes],
  infer_negations(BodyModes,NegModes),
  (aleph5_setting(updateback,Update) -> true; Update = true),
  p_message('found modes'),
  add_inferred_modes(Modes,Update),
  add_inferred_modes(EqModes,Update),
  add_inferred_modes(NegModes,Update),
  fail.
search_modes.

number_types([],Last,[],Last).
number_types([N/A|T],L0,[Pred|T1],L1):-
  functor(Pred,N,A),
  L is L0 + A,
  number_types(A,L,Pred),
  number_types(T,L,T1,L1).

number_types(0,_,_):- !.
number_types(A,N,Pred):-
  arg(A,Pred,N),
  A1 is A - 1,
  N1 is N - 1,
  number_types(A1,N1,Pred).

get_type_elements([]).
get_type_elements([Pred|Preds]):-
  functor(Pred,Name,Arity),
  functor(Template,Name,Arity),
  interval_to_list(1-Arity,AL),
  get_type_elements(example(_,_,Template),Template,Pred,AL),
  get_type_elements(Template,Template,Pred,AL),
  get_type_elements(Preds).

get_type_elements(Fact,Template,Pred,AL):-
  member(Arg,AL),
  findall(Val,(Fact,ground(Fact),arg(Arg,Template,Val)),Vals),
  arg(Arg,Pred,Type),
  sort(Vals,SVals),
  (retract('$aleph_search'(modes,type(Type,_,OtherVals))) ->
    aleph_ord_union(SVals,OtherVals,ArgVals);
    ArgVals = SVals),
  length(ArgVals,N),
  asserta('$aleph_search'(modes,type(Type,N,ArgVals))),
  fail.
get_type_elements(_,_,_,_).

get_type_equivalences([],[]).
get_type_equivalences([First|Rest],[Class|Classes]):-
  get_type_equivalence(Rest,[First],Class,Left),
  get_type_equivalences(Left,Classes).

get_type_equivalence([],Class1,Class,[]):-
  sort(Class1,Class).
get_type_equivalence([Type|Rest],Class1,Class,Left):-
  type_equivalent(Class1,Type), !,
  get_type_equivalence(Rest,[Type|Class1],Class,Left).
get_type_equivalence([Type|Rest],Class1,Class,[Type|Left]):-
  get_type_equivalence(Rest,Class1,Class,Left).

merge_equivalence_classes([Class],[Class]):- !.
merge_equivalence_classes(Classes1,Classes2):-
        aleph_delete(Class1,Classes1,Left),
        aleph_delete(Class2,Left,Left1),
        class_equivalent(Class1,Class2), !,
        aleph_ord_union(Class1,Class2,NewClass),
        merge_equivalence_classes([NewClass|Left1],Classes2).
merge_equivalence_classes(Classes,Classes).

class_equivalent(Class1,Class2):-
        member(Type1,Class1),
        type_equivalent(Class2,Type1), !.

type_equivalent([T1|_],T2):-
  '$aleph_search'(modes,type(T1,_,E1)),
  '$aleph_search'(modes,type(T2,_,E2)),
  intersects(E1,E2), !.
type_equivalent([_|T],T2):-
  type_equivalent(T,T2).

store_type_equivalences([]).
store_type_equivalences([[CType|Class]|Classes]):-
  length([CType|Class],N),
  store_type_equivalence([CType|Class],CType,N),
  store_type_equivalences(Classes).

store_type_equivalence([],_,_).
store_type_equivalence([Type|Types],CType,Neq):-
  retract('$aleph_search'(modes,type(Type,N,Elements))),
  store_type_overlaps(Types,Type,Elements,N),
  asserta('$aleph_search'(modes,type(Type,CType,Neq,N,Elements))),
  store_type_equivalence(Types,CType,Neq).

store_type_overlaps([],_,_,_).
store_type_overlaps([T1|Types],T,E,N):-
  '$aleph_search'(modes,type(T1,N1,E1)),
  aleph_ord_intersection(E1,E,Int),
  length(Int,NInt),
  O is NInt/N,
  O1 is NInt/N1,
  asserta('$aleph_search'(modes,typeoverlap(T,T1,O,O1))),
  store_type_overlaps(Types,T,E,N).

infer_modes([Head|Rest],Thresh,Types,[Head1|Rest1]):-
  infer_mode(Head,Thresh,head,[],Head1,Seen),
  aleph_delete_list(Seen,Types,TypesLeft),
  infer_ordered_modes(Rest,Thresh,body,Seen,TypesLeft,Rest1).

infer_ordered_modes([],_,_,_,_,[]):- !.
infer_ordered_modes(L,Thresh,Loc,Seen,Left,[Mode|Rest]):-
  score_modes(L,Thresh,Seen,Left,ScoredPreds),
  keysort(ScoredPreds,[_-Pred|_]),
  infer_mode(Pred,Thresh,Loc,Seen,Mode,Seen1),
  aleph_delete(Pred,L,L1),
  aleph_delete_list(Seen1,Left,Left1),
  infer_ordered_modes(L1,Thresh,Loc,Seen1,Left1,Rest).

score_modes([],_,_,_,[]).
score_modes([Pred|Preds],Thresh,Seen,Left,[Cost-Pred|Rest]):-
  Pred =.. [_|Types],
  evaluate_backward(Types,Thresh,Seen,G),
  aleph_delete_list(Types,Left,Left1),
  estimate_forward(Seen,Thresh,Left1,H0),
  estimate_forward(Types,Thresh,Left1,H1),
  Diff is H1 - H0,
  (Diff < 0 -> H is 0; H is Diff),
  Cost is -(G + H),
  score_modes(Preds,Thresh,Seen,Left,Rest).

evaluate_backward([],_,_,0.0).
evaluate_backward([Type|Types],Thresh,Seen,Score):-
  best_overlap(Seen,Type,_,Overlap),
  (Overlap >= Thresh -> Score1 = 1.0; Score1 = 0.0),
  evaluate_backward(Types,Thresh,Seen,Score2),
  Score is Score1 + Score2.

estimate_forward([],_,_,0.0).
estimate_forward([Type|Types],Thresh,Left,Score):-
        estimate_forward1(Left,Thresh,Type,S1),
        estimate_forward(Types,Thresh,Left,S2),
        Score is S1 + S2.

estimate_forward1([],_,_,0.0).
estimate_forward1([T1|Types],Thresh,T,Score):-
        type_overlap(T1,T,O1),
  (O1 >= Thresh -> S1 is 1.0; S1 is 0.0),
        estimate_forward1(Types,Thresh,T,S2),
        Score is S1 + S2.

infer_mode(Pred,Thresh,Loc,Seen0,InferredMode,Seen):-
  Pred =.. [Name|Types],
  infer_mode1(Types,Thresh,Loc,Seen0,Modes),
  Mode =.. [Name|Modes],
  length(Types,Arity),
  ('$aleph_global'(targetpred,targetpred(Name/Arity)) ->
    InferredMode = modeh(*,Mode);
    InferredMode = mode(*,Mode)),
  aleph_ord_union(Seen0,Types,Seen).

infer_mode1([],_,_,_,[]).
infer_mode1([Type|Types],Thresh,Loc,Seen,[Mode|Modes]):-
  best_overlap(Seen,Type,Best,Overlap),
  (Overlap >= Thresh ->
    '$aleph_search'(modes,typemapped(Best,_,NewType)),
    asserta('$aleph_search'(modes,typemapped(Type,Best,NewType))),
    atomic_concat(type, NewType, Name),
    Mode = +Name;
    (Overlap > 0.0 ->
      asserta('$aleph_search'(modes,typemapped(Type,Best,Type)));
      asserta('$aleph_search'(modes,typemapped(Type,Type,Type)))),
    atomic_concat(type, Type, Name),
    (Loc = head -> Mode = +Name; Mode = -Name)
  ),
  infer_mode1(Types,Thresh,Loc,Seen,Modes).


best_overlap([T1],T,T1,O):-
  !,
  type_overlap(T,T1,O).
best_overlap([T1|Types],T,Best,O):-
  type_overlap(T,T1,O1),
  best_overlap(Types,T,T2,O2),
  (O2 > O1 -> O is O2, Best = T2; O is O1, Best = T1).
best_overlap([],T,T,0.0).

type_overlap(T,T1,O):-
  T > T1, !,
  ('$aleph_search'(modes,typeoverlap(T1,T,_,O)) -> true; O = 0.0).
type_overlap(T,T1,O):-
  ('$aleph_search'(modes,typeoverlap(T,T1,O,_)) -> true; O = 0.0).


infer_equalities(EqModes):-
  findall(mode(1,(Eq)),(pairwise_equality(Eq);grounding_equality(Eq)),
    EqL),
  sort(EqL,EqModes).

infer_negations([],[]).
infer_negations([mode(_,Pred)|Modes],NegModes):-
  Pred =.. [_|Args],
  memberchk(-_,Args), !,
  infer_negations(Modes,NegModes).
infer_negations([mode(_,Pred)|Modes],[mode(1,not(Pred))|NegModes]):-
  infer_negations(Modes,NegModes).


pairwise_equality((+N1 = +N2)):-
  '$aleph_search'(modes,typemapped(_,Best,T1)),
  '$aleph_search'(modes,typemapped(Best,_,T2)),
  T1 \== T2,
  format(atom(N1), 'type~w', [T1]),
  format(atom(N2), 'type~w', [T2]).
grounding_equality((+N1 = #N1)):-
  '$aleph_search'(modes,typemapped(T1,_,T1)),
  concat([type,T1],N1).

add_inferred_modes([],_).
add_inferred_modes([Mode|Modes],Flag):-
  format(Stream, Mode), nl(Stream),
  (Flag = true -> Mode; true),
  add_inferred_modes(Modes,Flag).



% STOCHASTIC SEARCH

%! sample_clauses(+N,-Clauses)
%  return sample of at most N legal clauses from hypothesis space
%  If a bottom clause exists then
%    Each clause is drawn randomly. The length of the clause is
%    determined by:
%      (a) user-specified distribution over clauselengths
%          using aleph5_set_setting(clauselength_distribution,Distribution);
%          Distribution is a list of the form p1-1, p2-2,...
%          specifying that clauselength 1 has prob p1, etc.
%          Note: sum pi must = 1. This is not checked; or
%      (b) uniform distribution over all legal clauses.
%          (if clauselength_distribution is not set)
%          this uses a Monte-Carlo estimate of the number of
%          legal clauses in the hypothesis space
%  If a bottom clause does not exist, then legal clauses are constructed
%  using the mode declarations. Only option (a) is allowed. If
%  clauselength_distribution is not set, then a uniform distribution over
%  lengths is assumed.
%  Each element of Clauses is of the form L-[E,T,Lits,Clause] where
%  L is the clauselength; E,T are example number and type (pos, neg) used
%  to build the bottom clause; Lits contains the literal numbers in the
%  bottom clause for Clause. If no bottom clause then E,T = 0 and Lits = []
%  Clauses is in ascending order of clause length.

sample_clauses(N,Clauses):-
  aleph5_setting(construct_bottom,Bottom),
  sample_nclauses(Bottom,N,Clauses).

sample_nclauses(false,N,Clauses):-
  !,
  gen_auto_refine,
  (aleph5_setting(clauselength_distribution,D) -> true;
    aleph5_setting(clauselength,CL),
    Uniform is 1.0/CL,
    distrib(1-CL,Uniform,D)),
  sample_nclauses_using_modes(N,D,CList),
  remove_alpha_variants(CList,CList1),
  keysort(CList1,Clauses).
sample_nclauses(_,N,Clauses):-
  retractall('$aleph_sat'(random,rselect(_))),
  ('$aleph_sat'(example,example(_,_)) -> true; rsat),
  aleph5_setting(clauselength,CL),
  (aleph5_setting(clauselength_distribution,Universe) ->
    Sample is N;
    estimate_numbers(CL,1,400,Universe),
    (N > Universe -> Sample is Universe; Sample is N)),
  get_clause_sample(Sample,Universe,CL,CList),
  keysort(CList,Clauses).

% sample_nclauses_using_modes(+N,+D,-Clauses)
%   get upto N legal clauses using mode declarations
%  and distribution D over clauselengths

sample_nclauses_using_modes(0,_,[]):- !.
sample_nclauses_using_modes(N,D,[Clause|Rest]):-
  legal_clause_using_modes(100,D,Clause),
  N1 is N - 1,
  sample_nclauses_using_modes(N1,D,Rest).

% legal_clause_using_modes(+N,+D,-Clause)
%  make at most N attempts to obtain a legal clause Clause
%  from mode language using distribution D over clauselengths
%  if all N attempts fail, then just return most general clause
legal_clause_using_modes(N,D,L-[0,0,[],Clause]):-
  N > 0,
  sample_clause_using_modes(D,L,Clause),
  \+(prune(Clause)),
  split_clause(Clause,Head,Body),
  (aleph5_setting(language,Lang) ->
          lang_ok(Lang,Head,Body);
    true),
  (aleph5_setting(newvars,NewVars) ->
    newvars_ok(NewVars,Head,Body);
    true),
  !.
legal_clause_using_modes(N,D,Clause):-
  N > 1,
  N1 is N - 1,
  legal_clause_using_modes(N1,D,Clause), !.
legal_clause_using_modes(_,_,1-[0,0,[],Clause]):-
  sample_clause_using_modes([1.0-1],1,Clause).

sample_clause_using_modes(D,L,Clause):-
  findall(H,auto_refine(false,H),HL),
  HL \= [],
  random_select(Head,HL,_),
  draw_element(D,L),
  (L = 1 -> Clause = Head;
    L1 is L - 1,
    sample_clause_using_modes(L1,Head,Clause)).

sample_clause_using_modes(N,ClauseSoFar,Clause):-
  findall(C,auto_refine(ClauseSoFar,C),CL),
  CL \= [], !,
  (N = 1 -> random_select(Clause,CL,_);
    random_select(C1,CL,_),
    N1 is N - 1,
    sample_clause_using_modes(N1,C1,Clause)).
sample_clause_using_modes(_,Clause,Clause).


% get_clause_sample(+N,+U,+CL,-Clauses)
%   get upto N legal clauses of at most length CL drawn from universe U
%  U is either the total number of legal clauses
%    or a distribution over clauselengths
%  the clauses are constructed by drawing randomly from bottom
get_clause_sample(0,_,_,[]):- !.
get_clause_sample(N,Universe,CL,[L-[E,T,C1,C]|Clauses]):-
        (number(Universe) ->
    get_rrandom(Universe,ClauseNum),
    num_to_length(ClauseNum,CL,L),
    UpperLim is CL;
    draw_element(Universe,L),
    UpperLim is L),
  draw_legalclause_wo_repl(L,UpperLim,C,C1), !,
  '$aleph_sat'(example,example(E,T)),
  N1 is N - 1,
  get_clause_sample(N1,Universe,CL,Clauses).
get_clause_sample(N,Universe,CL,Clauses):-
  N1 is N - 1,
  get_clause_sample(N1,Universe,CL,Clauses).

% draw_legalclause_wo_repl(+L,+CL,-C,-Lits)
%  randomly draw without replacement a legal clause of length >= L and =< CL
%  also returns literals from bottom used to construct clause
draw_legalclause_wo_repl(L,CL,C,C1):-
  L =< CL,
  randclause_wo_repl(L,C,legal,C1), !.
draw_legalclause_wo_repl(L,CL,C,C1):-
  L < CL,
  L1 is L + 1,
  draw_legalclause_wo_repl(L1, CL,C,C1).

% estimate_clauselength_distribution(+L,+T,+K,-D)
%  for each clauselength l <= L, estimate the probability of
%  drawing a good clause
%  here, a ``good clause'' is one that is in the top K-percentile of clauses
%  estimation is by Monte Carlo using at most T trials
%  probabilities are normalised to add to 1
estimate_clauselength_distribution(L,T,K,D):-
  '$aleph_sat'(example,example(Type,Example)),
  '$aleph_sat'(random,clauselength_distribution(Type,Example,L,T,K,D)), !.
estimate_clauselength_distribution(L,T,K,D):-
  aleph5_setting(evalfn,Evalfn),
  estimate_clauselength_scores(L,T,Evalfn,[],S),
  select_good_clauses(S,K,Good),
  estimate_frequency(L,Good,Freq),
  normalise_distribution(Freq,D),
  ('$aleph_sat'(example,example(Type,Example)) ->
    asserta('$aleph_sat'(random,clauselength_distribution(Type,
            Example,L,T,K,D)));
    true).

estimate_clauselength_scores(0,_,_,S,S):- !.
estimate_clauselength_scores(L,T,Evalfn,S1,S):-
  aleph5_set_setting(clauselength_distribution,[1.0-L]),
  p1_message('Estimate scores of clauses with length'),
  p_message(L),
  sample_clauses(T,Clauses),
  estimate_scores(Clauses,Evalfn,S1,S2),
  L1 is L - 1,
  estimate_clauselength_scores(L1,T,Evalfn,S2,S).

estimate_scores([],_,S,S):- !.
estimate_scores([L-[_,_,_,C]|Rest],Evalfn,S1,S):-
  label_create(C,Label),
  extract_count(pos,Label,PC),
  extract_count(neg,Label,NC),
  complete_label(Evalfn,C,[PC,NC,L],[_,_,_,Val|_]),
  estimate_scores(Rest,Evalfn,[-Val-L|S1],S).

% ``good'' clauses are defined to be those in the top K-percentile
%  policy on ties is to include them
select_good_clauses(S,K,Good):-
  keysort(S,S1),
  length(S1,Total),
  N is integer(K*Total/100),
  select_good_clauses(S1,N,[],Good).

select_good_clauses([],_,Good,Good):- !.
select_good_clauses(_,N,Good,Good):- N =< 0, !.
select_good_clauses([Score-X|T],N,GoodSoFar,Good):-
  select_good_clauses(T,Score,N,[Score-X|GoodSoFar],N0,Good1,T1),
  N1 is N0 - 1,
  select_good_clauses(T1,N1,Good1,Good).

select_good_clauses([],_,N,G,N,G,[]):- !.
select_good_clauses([Score-X|T],Score,N,GoodSoFar,N0,Good1,T1):-
  !,
  N1 is N - 1,
  select_good_clauses(T,Score,N1,[Score-X|GoodSoFar],N0,Good1,T1).
select_good_clauses(L,_,N,G,N,G,L).

estimate_frequency(0,_,[]).
estimate_frequency(L,Good,[N-L|T]):-
  count_frequency(Good,L,N),
  L1 is L - 1,
  estimate_frequency(L1,Good,T).

count_frequency([],_,0).
count_frequency([Entry|T],X,N):-
  count_frequency(T,X,N1),
  (Entry = _-X -> N is N1 + 1; N is N1).

%   estimate total number of legal clauses in space
%  bounded by bot
estimate_numbers(Total):-
  ('$aleph_sat'(example,example(_,_)) -> true; rsat),
  aleph5_setting(clauselength,CL),
  estimate_numbers(CL,1,400,Total).

% estimate_numbers(+L,+Trials,+Sample,-T)
%   estimate total number of legal clauses of length <= L in space
%  bounded by bot
%  estimated number is cached for future use
%  estimation is by Monte Carlo, averaged over Trials trials
%  with given sample size
estimate_numbers(L,Trials,Sample,Total):-
  '$aleph_sat'(example,example(Type,Example)),
  '$aleph_sat'(random,sample(Type,Example,L,Trials,Sample)),
  '$aleph_sat'(random,hypothesis_space(Total)), !.
estimate_numbers(L,Trials,Sample,Total):-
  retractall('$aleph_sat'(random,sample(_,_,_,_,_))),
  retractall('$aleph_sat'(random,hypothesis_space(_))),
  estimate_numbers(L,Trials,Sample,0,Total),
  asserta('$aleph_sat'(random,hypothesis_space(Total))),
  '$aleph_sat'(example,example(Type,Example)),
  asserta('$aleph_sat'(random,sample(Type,Example,L,Trials,Sample))).

% estimate_numbers(+L,+Trials,+Sample,+TotalSoFar,-Total)
%  estimate the number of legal clauses of length <= L
%  estimated number of legal clauses at each length are cached for future use
%  TotalSoFar is an accumulator of the number legal clauses so far
%  Total is the cumulative total of the number of legal clauses
estimate_numbers(0,_,_,T,T):- !.
estimate_numbers(L,Trials,Sample,TotalSoFar,T):-
  retractall('$aleph_sat'(random,number_of_clauses(L,_))),
  estimate_number(Trials,Sample,L,T0),
  asserta('$aleph_sat'(random,number_of_clauses(L,T0))),
  L1 is L - 1,
  T1 is T0 + TotalSoFar,
  estimate_numbers(L1,Trials,Sample,T1,T).

% estimate_number(+T,+S,+L,-N)
%  monte carlo estimate of number of legal clauses of length L
%  estimate formed from average over T trials with sample S
estimate_number(_,_,L,0):-
  '$aleph_sat'(lastlit,Last),
  Last < L, !.
estimate_number(T,S,L,N):-
  T > 0,
  p1_message('Estimate legal clauses with length'),
  p_message(L),
  estimate_number(T,S,0,L,Total),
  N is float(Total/T),
  format(atom(Message), 'trials=~w sample=~w estimate=~w', [T, S, N]),
  p_message(Message).

estimate_number(1,S,Total,L,N):-
  !,
  estimate_number(L,S,N1),
  N is Total + N1.
estimate_number(T,S,Total,L,N):-
  p_message('New Trial'),
  estimate_number(L,S,N1),
  Total1 is Total + N1,
  T1 is T - 1,
  estimate_number(T1,S,Total1,L,N).

% estimate_number(+L,+S,-N)
%  estimate the number of legal clauses of length L in the search space
%  estimation based on sample size S
estimate_number(1,_,1):- !.
estimate_number(L,S,N):-
  estimate_proportion(S,L,legal,P,_),
  '$aleph_sat'(lastlit,Last),
  total_clauses(L,Last,Total),
  N is float(P*Total).

% estimate_proportion(+N,+L,+S,-P,-Clauses)
%  estimate prop. of at most N random clauses of length L and status S
%  clauses are generated without replacement
%  S is one of legal or illegal depending on whether C is inside or
%    outside the mode language provided
%  Clauses is the list of at most N def. clauses
%  If S is a variable then clauses can be legal or illegal
%  Thus estimate_proportion(10000,2,S,P,C) returns the
%    proportion and list of 2 literal clauses which are either
%    legal or illegal in a sample of at most 10000
%  Keeps legal clauses obtained in rselect_legal for later use
estimate_proportion(0,_,_,0,[]):- !.
estimate_proportion(N,L,S,P,Clauses):-
  retractall('$aleph_sat'(random,rselect(_))),
  retractall('$aleph_sat'(random,rselect_legal(L,_,_,_,_))),
  get_random_wo_repl(N,L,Clauses),
  length(Clauses,Total),
  count_clause_status(Clauses,S,A,_),
  (Total = 0 -> P = 0; P is A/Total),
  '$aleph_sat'(example,example(E,T)),
  retractall('$aleph_sat'(random,rselect(_))),
  store_legal_clauses(Clauses,L,E,T).

% get_random_wo_repl(+N,+L,-List)
%  randomly construct at most N definite clauses of length L
%  returns Status/Clause list where Status is one of legal/illegal
get_random_wo_repl(0,_,[]):- !.
get_random_wo_repl(N,L,[S/[C,C1]|Clauses]):-
  randclause_wo_repl(L,C,S,C1), !,
  N1 is N - 1,
  get_random_wo_repl(N1,L,Clauses).
get_random_wo_repl(_,_,[]).

% print_distribution(+Steam)
print_distribution(Stream):-
  format(
    Stream,
    'Clause Length\t\t\t\t\t\t\t\tEstimated number of clauses\n_____________\t\t\t\t\t\t\t\t___________________________\n',
    []
  ),
  findall(
    L-N,
    '$aleph_sat'(random,number_of_clauses(L,N)),
    List
  ),
  sort(List,List1),
  member(L-N,List1),
  format(Stream, '~w', [L]),
  tab(Stream, 20),
  format(Stream, '~w\n', [N]),
  fail.
print_distribution(Stream):-
  (
    '$aleph_sat'(random,hypothesis_space(S))
  ->
    true
  ;
    S = 0
  ),
  format(Stream, '\nEstimated size of hypothesis space = ~w clauses\n', [S]).

% count_clause_status(+List,+Status,-C1,-C2)
%  count number of clauses in List with status Status
%  C1 is the number of such clauses
%  C2 is the number of clauses with some other status
count_clause_status(_,S,_,0):-
  var(S), !.
count_clause_status(Clauses,S,A,B):-
  count_clause_status1(Clauses,S,A,B).

count_clause_status1([],_,0,0):- !.
count_clause_status1([S1/_|T],S,A,B):-
  count_clause_status1(T,S,A1,B1),
  (S == S1 -> A is A1 + 1, B is B1; A is A1, B is B1 + 1).

% store_legal_clauses(+List,+L,+E,+T)
% store all legal clauses of length L obtained with bottom clause for
% example E of type T
% useful later when a random legal clause of length L is required
store_legal_clauses([],_,_,_).
store_legal_clauses([S/[C,C1]|Clauses],L,E,T):-
  (S == legal ->
    asserta('$aleph_sat'(random,rselect_legal(L,E,T,C,C1)));
    true),
  store_legal_clauses(Clauses,L,E,T).

% randclause_wo_repl(+L,-C,-S,-Lits)
% as randclause/4 but ensures that clause obtained is without replacement
%  only makes at most 100 attempts to find such a clause
%  also returns lits from bottom clause selected
%  if all attempts fail, then return the most general clause
randclause_wo_repl(L,C,S,C1):-
  randclause_wo_repl(100,L,C,S,C1).

randclause_wo_repl(N,L,C,S,C1):-
  N > 0,
  randclause(L,C,S,C1),  % if not accounting for variable renamings
  % copy_term(C,C1),  % if accounting for variable renamings
  % numbervars(C1,0,_),  % if accounting for variable renamings
  \+(prune(C)),
  split_clause(C,Head,Body),
  (aleph5_setting(language,Lang) ->
    lang_ok(Lang,Head,Body);
    true),
  (aleph5_setting(newvars,NewVars) ->
    newvars_ok(NewVars,Head,Body);
    true),
  \+('$aleph_sat'(random,rselect(C1))), !,
  asserta('$aleph_sat'(random,rselect(C1))).
randclause_wo_repl(N,L,C,S,C1):-
  N > 0,
  N1 is N - 1,
  randclause_wo_repl(N1,L,C,S,C1), !.
randclause_wo_repl(_,1,C,S,C1):-
  randclause(1,C,S,C1).  % if not accounting for variable renamings
  % copy_term(C,C1),  % if accounting for variable renamings
  % numbervars(C1,0,_),  % if accounting for variable renamings

% randclause(+L,-C,-S,-Lits)
%  returns definite clause C of length L with status S comprised of Lits
%  drawn at random from the bottom clause
%  also returns the literals in the bottom clause that were selected
%  body literals of C are randomly selected from the bottom clause
%  S is one of legal or illegal depending on whether C is inside or
%    outside the mode language provided
% needs a bottom clause to be constructed before it is meaningful
% this can be done with the sat predicate for eg: sat(1)
% if aleph5_set_setting(store_bottom,true) then use stored bottom clause instead
% if S is legal, then checks to see if previously generated legal
% clauses exist for this bottom clause (these would have been generated
% when trying to estimate the number of legal clause at each length)
randclause(1,C,legal,[1]):-
  !,
  bottom_key(_,_,Key,_),
        (Key = false ->
    get_pclause([1],[],C,_,_,_);
    get_pclause([1],Key,[],C,_,_,_)).
randclause(L,C,Status,Lits):-
  Status == legal,
  '$aleph_sat'(example,example(E,T)),
  retract('$aleph_sat'(random,rselect_legal(L,E,T,C,Lits))).
% can do things more efficiently if we want to generate legal clauses only
randclause(L,C,Status,Lits):-
  Status == legal, !,
  bottom_key(_,_,Key,_),
        (Key = false ->
          '$aleph_sat_litinfo'(1,_,_,_,_,D);
    '$aleph_sat_litinfo'(1,Key,_,_,_,_,D)),
        L1 is L - 1,
        repeat,
        randselect1(L1,Key,D,[1],BodyLits),
        Lits = [1|BodyLits],
  clause_status(Lits,Key,[],legal,legal), !,
        (Key = false ->
          get_pclause(Lits,[],C,_,_,_);
          get_pclause(Lits,Key,[],C,_,_,_)).
randclause(L,C,Status,Lits):-
  L1 is L - 1,
  bottom_key(_,_,Key,_),
  (
    Key = false
  ->
    '$aleph_sat'(lastlit,Last)
  ;
    '$aleph_sat'(lastlit,Key,Last)
  ),
  repeat,
  randselect(L1,Last,Key,[],BodyLits),
  append([1], BodyLits, Lits),
  clause_status(Lits,Key,[],legal,Status1),
  Status1 = Status,
  !,
  (
    Key = false
  ->
    get_pclause(Lits,[],C,_,_,_)
  ;
    get_pclause(Lits,Key,[],C,_,_,_)
  ).

% clause_status(+Lits,+LitsSoFar,+StatusSoFar,-Status)
% compute status of a clause
%  Lits is the lits left to add to the clause
%  LitsSoFar is the lits in the clause so far
%  StatusSoFar is the Status of the clause so far
%    if a literal to be added contains unbound input vars then
%    status is illegal
clause_status(Lits,LitsSoFar,Status1,Status2):-
  bottom_key(_,_,Key,_),
  clause_status(Lits,Key,LitsSoFar,Status1,Status2).

clause_status([],_,_,S,S):-
  !.
clause_status([Lit|Lits],Key,LitsSoFar,S,S1):-
  get_ovars(LitsSoFar,Key,[],OVars),
  get_ivars([Lit],Key,[],IVars),
  subset(IVars,OVars),
  !,
  append(LitsSoFar, [Lit], Lits1),
  clause_status(Lits,Key,Lits1,S,S1).
clause_status(_,_,_,_,illegal).

% randselect(+L,+Last,+Key,+LitsSoFar,-Lits)
% randomly select L distinct literals to give Lits
% Last is the last literal number in the bottom clause
% LitsSoFar is the literals selected so far
randselect(0,_,_,_,[]):- !.
randselect(_,Last,_,LitsSoFar,[]):-
        length(LitsSoFar,L1),
        L1 is Last - 1, !.
randselect(L,Last,Key,LitsSoFar,[LitNum|Lits]):-
  get_rand_lit(Last,Key,LitsSoFar,LitNum),
  L1 is L - 1,
  randselect(L1,Last,Key,[LitNum|LitsSoFar],Lits).

% randselect1(+L,+Key,+Avail,+LitsSoFar,-Lits)
% randomly select L distinct literals from Avail to give Lits
% LitsSoFar is the literals selected so far
randselect1(0,_,_,_,[]):- !.
randselect1(_,_,[],_,[]):- !.
randselect1(L,Key,Avail,LitsSoFar,[LitNum|Lits]):-
  random_select(LitNum,Avail,Left),
  (Key = false ->
          '$aleph_sat_litinfo'(LitNum,_,_,_,_,D);
          '$aleph_sat_litinfo'(LitNum,Key,_,_,_,_,D)),
        update_list(D,Left,Left1),
        aleph_delete_list([LitNum|LitsSoFar],Left1,Avail1),
        L1 is L - 1,
        randselect1(L1,Key,Avail1,[LitNum|LitsSoFar],Lits).

% get_rand_lit(+Last,+Key,+LitsSoFar,-LitNum)
% randomly select a literal number from 2 - Last
% and not in list LitsSoFar
%  2 because 1 is reserved for head literal
get_rand_lit(Last,Key,LitsSoFar,LitNum):-
  repeat,
  get_rand_lit(Last,Key,LitNum),
  \+(member(LitNum,LitsSoFar)),
  !.

% have to use repeat/0 in case literal number from random no generator
%  no longer exists in lits database
get_rand_lit(Last,Key,LitNum):-
  repeat,
  get_random(Last,LitNum),
  LitNum > 1,
  (Key = false ->
          '$aleph_sat_litinfo'(LitNum,_,_,_,_,_);
          '$aleph_sat_litinfo'(LitNum,Key,_,_,_,_,_)), !.

% total_clauses(+L,+N1,-N2)
%  total number of clauses of length L is N2
%  constructed from bottom clause of length N1
total_clauses(1,_,1.0):- !.
total_clauses(L,Bot,N):-
  L1 is L - 1,
  Bot1 is Bot - 1,
  total_clauses(L1,Bot1,N1),
  N is N1*Bot1.

% num_to_length(+N,+CL,-L)
%  find length of clause numbered N
%  clause length should be =< CL

num_to_length(N,_,1):- N =< 1.0, !.
num_to_length(N,CL,L):-
  num_to_length1(2,CL,N,1.0,L).

num_to_length1(L,CL,_,_,CL):-
  L >= CL, !.
num_to_length1(L,CL,N,TotalSoFar,Length):-
  '$aleph_sat'(random,number_of_clauses(L,T)),
  NClauses is TotalSoFar + T,
  (N =< NClauses ->
    (T < 1.0 -> Length is L - 1; Length = L) ;
    L1 is L + 1,
    num_to_length1(L1,CL,N,NClauses,Length)).

% refinement operator for randomised local search
%  Type is one of clauses or theories
rls_refine(clauses,_-[_,_,_,false],Clause):-
  !,
  sample_clauses(1,[Clause]),
  \+(old_move(clauses,Clause)).
rls_refine(clauses,Clause1,Clause2):-
  aleph5_setting(moves,Max),
  MaxMoves is Max,
  once(retract('$aleph_search'(rls_move,M))),
  M =< MaxMoves,
  p1_message('move'), p_message(M),
  M1 is M + 1,
  asserta('$aleph_search'(rls_move,M1)),
  clause_move(Move,Clause1,Clause2),
  p_message(Move),
  \+(old_move(clauses,Clause2)).

rls_refine(theories,[_-[_,_,_,false]],Theory):-
  !,
  once(theory_move(add_clause,[],Theory)),
  \+(old_move(theories,Theory)).
rls_refine(theories,Theory1,Theory2):-
  aleph5_setting(moves,MaxMoves),
  once(retract('$aleph_search'(rls_move,M))),
  M =< MaxMoves,
  p1_message('move'), p_message(M),
  M1 is M + 1,
  asserta('$aleph_search'(rls_move,M1)),
  theory_move(_,Theory1,Theory2),
  \+(old_move(theories,Theory2)).

% clause_move(+Type,+C1,-C2)
% local moves from clause C1 to give C2
%  A move is:
%  a) delete a literal from C1 (Type = delete_lit)
%  b) add a legal literal to C1 (Type = add_lit)
clause_move(delete_lit,C1,C2):-
  C1 = L-[E,T,Lits,Clause],
  (Lits = [H|Rest] ->
    aleph_delete(_,Rest,Left),
    Lits1 = [H|Left],
    bottom_key(E,T,Key,_),
    clause_status(Lits1,Key,[],legal,legal),
    L1 is L - 1,
    (Key = false ->
            get_pclause(Lits1,[],Clause1,_,_,_);
            get_pclause(Lits1,Key,[],Clause1,_,_,_)),
    \+(prune(Clause1)) ;
    clause_to_list(Clause,[Head|Body]),
    aleph_delete(_,Body,Left),
    aleph_mode_linked([Head|Left]),
    list_to_clause([Head|Left],Clause1),
    \+(prune(Clause1)),
    L1 is L - 1,
    Lits1 = []),
  C2 = L1-[E,T,Lits1,Clause1].
clause_move(add_lit,C1,C2):-
  C1 = L-[E,T,Lits,Clause],
  aleph5_setting(clauselength,CL),
  L < CL,
  (Lits = [] ->
    auto_refine(Clause,Clause1),
    L1 is L + 1,
    Lits1 = [];
    aleph_delete(Lit,Lits,Left),
    bottom_key(E,T,Key,_),
    (Key = false ->
            '$aleph_sat_litinfo'(Lit,_,_,_,_,D);
            '$aleph_sat_litinfo'(Lit,Key,_,_,_,_,D)),
    member(Lit1,D),
    \+(member(Lit1,Left)),
    append(Lits, [Lit1], Lits1),
          clause_status(Lits1,Key,[],legal,legal),
    L1 is L + 1,
    (Key = false ->
            get_pclause(Lits1,[],Clause1,_,_,_);
            get_pclause(Lits1,Key,[],Clause1,_,_,_)),
    \+(prune(Clause1))),
  C2 = L1-[E,T,Lits1,Clause1].

% theory_move(+Type,+T1,-T2)
% local moves from theory T1 to give T2
%  A move is:
%  a) delete a clause from T1 (Type = delete_clause)
%  b) add a legal clause to  T1  (Type = add_clause)
%  c) delete a literal from a clause in T1 (Type = delete_lit)
%  d) add a legal literal to a clause in T1 (Type = add_lit)
theory_move(delete_clause,T1,T2):-
  aleph_delete(_,T1,T2),
  T2 \= [].
theory_move(add_clause,T1,T2):-
  aleph5_setting(clauses,Max),
  length(T1,L),
  L < Max,
  sample_clauses(1,[Clause]),
  append(T1, [Clause], T2).
theory_move(delete_lit,T1,T2):-
  aleph_delete(Clause,T1,T),
  clause_move(delete_lit,Clause,Clause1),
  append(T, [Clause1], T2).
theory_move(add_lit,T1,T2):-
  aleph_delete(Clause,T1,T),
  clause_move(add_lit,Clause,Clause1),
  append(T, [Clause1], T2).

old_move(clauses,N-[_,_,L,C]):-
  (aleph5_setting(cache_clauselength,N1) -> true; N1 = 3),
  N =< N1,
  (L = [] ->
    clause_to_list(C,C1),
    sort(C1,Hash),
    numbervars(Hash,0,_);
    sort(L,Hash)),
  ('$aleph_search_seen'(N,Hash) ->
    p_message('old move'),
    true;
    asserta('$aleph_search_seen'(N,Hash)), !,
    fail).
old_move(theories,T):-
  % remove_alpha_variants(T,T1),
  numbervars(T,0,_),
  length(T,N),
  ('$aleph_search_seen'(N,Hash) ->
    p_message('old move'),
    true;
    asserta('$aleph_search_seen'(N,Hash)), !,
    fail).

extract_clauses_with_length([],[]).
extract_clauses_with_length([L-[_,_,_,C]|T],[L-C|T1]):-
  extract_clauses_with_length(T,T1).



% UTILITIES

split_clause((Head:-true),Head,true):- !.
split_clause((Head:-Body1),Head,Body2):- !, Body1 = Body2.
split_clause([Head|T],Head,T):- !.
split_clause([Head],Head,[true]):- !.
split_clause(Head,Head,true).

strip_true((Head:-true),Head):- !.
strip_true(Clause,Clause).

% pretty print a definite clause
pp_dclause(Clause):-
  current_stream(Stream),
  pp_dclause(Stream, Clause).

pp_dclause(Stream, Clause):-
  (
    setting(portray_literals, true)
  ->
    pp_dclause(Stream, Clause, true)
  ;
    pp_dclause(Stream, Clause, false)
  ).

% pretty print a set of definite clauses
pp_dclauses(Stream, Theory):-
  member(_-[_,_,_,Clause],Theory),
  pp_dclause(Stream, Clause),
  fail.
pp_dclauses(Stream, _):-
  nl(Stream).

pp_dclause(Stream, (H:-true),Pretty):-
  !,
  pp_dclause(Stream, H,Pretty).
pp_dclause(Stream, (H:-B),Pretty):-
  !,
  copy_term((H:-B),(Head:-Body)),
  numbervars((Head:-Body),0,_),
  aleph_portray(Head,Pretty),
  (
    Pretty = true
  ->
    format(Stream, ' if:', [])
  ;
    format(Stream, ':-', [])
  ),
  nl(Stream),
  setting(print, N),
  print_lits(Stream, Body,Pretty,1,N).
pp_dclause(Stream, (Lit),Pretty):-
  copy_term(Lit,Lit1),
  numbervars(Lit1,0,_),
  aleph_portray(Lit1,Pretty),
  format(Stream, '.\n', []).

% pretty print a definite clause list: head of list is + literal
pp_dlist(_Stream, []):- !.
pp_dlist(Stream, Clause):-
  (
    setting(portray_literals, true)
  ->
    pp_dlist(Stream, Clause,true)
  ;
    pp_dlist(Stream, Clause,false)
  ).

pp_dlist(Stream, Clause,Pretty):-
  copy_term(Clause,[Head1|Body1]),
  numbervars([Head1|Body1],0,_),
  aleph_portray(Head1,Pretty),
  (
    Body1 = []
  ->
    format(Stream, '.\n', [])
  ;
    (
      Pretty = true
    ->
      format(Stream, ' if:', [])
    ;
      format(Stream, ':-', [])
    ),
    nl(Stream),
    setting(print, N),
    print_litlist(Stream, Body1,Pretty,1,N)
  ).

print_litlist(_Stream, [],_,_,_).
print_litlist(Stream, [Lit],Pretty,LitNum,_):-
  !,
  print_lit(Stream, Lit,Pretty,LitNum,LitNum,'.',_).
print_litlist(Stream, [Lit|Lits],Pretty,LitNum,LastLit):-
  print_lit(Stream, Lit,Pretty,LitNum,LastLit,', ',NextLit),
  print_litlist(Stream, Lits,Pretty,NextLit,LastLit).

print_lits(Stream, (Lit,Lits),Pretty,LitNum,LastLit):-
  !,
  (Pretty = true ->
          Sep = ' and ';
          Sep = ', '),
  print_lit(Stream, Lit,Pretty,LitNum,LastLit,Sep,NextLit),
  print_lits(Stream, Lits,Pretty,NextLit,LastLit).
print_lits(Stream, (Lit),Pretty,LitNum,_):-
  print_lit(Stream, Lit,Pretty,LitNum,LitNum,'.',_).

print_lit(Stream, Lit,Pretty,LitNum,LastLit,Sep,NextLit):-
  (LitNum = 1 -> tab(Stream, 3);true),
  aleph_portray(Lit,Pretty), format(Stream, '~w', Sep),
  (LitNum=LastLit-> nl(Stream),NextLit=1; NextLit is LitNum + 1).

p1_message(Mess):-
  current_stream(Stream),
  p1_message(Stream, Mess).

p1_message(Stream, Message):-
  format(Stream, '[~w]', [Message]).

p_message(Mess):-
  current_stream(Stream),
  p_message(Stream, Mess).

p_message(Stream, Message):-
  format(Stream, '[~w]\n', [Message]).

stream_message(Format, Arguments):-
  current_stream(Stream),
  format(Stream, Format, Arguments).

err_message(Mess):-
  current_stream(Stream),
  err_message(Stream, Mess).

err_message(Stream, Mess):-
  p1_message(Stream, error),
  p_message(Stream, Mess).

aleph_delete_all(_,[],[]).
aleph_delete_all(X,[Y|T],T1):-
        X == Y, !,
        aleph_delete_all(X,T,T1).
aleph_delete_all(X,[Y|T],[Y|T1]):-
        aleph_delete_all(X,T,T1).

aleph_delete_list([],L,L).
aleph_delete_list([H1|T1],L1,L):-
  aleph_delete(H1,L1,L2), !,
  aleph_delete_list(T1,L2,L).
aleph_delete_list([_|T1],L1,L):-
  aleph_delete_list(T1,L1,L).

aleph_delete(H,[H|T],T).
aleph_delete(H,[H1|T],[H1|T1]):-
  aleph_delete(H,T,T1).

% aleph_rpermute(+List1,-List2)
%  randomly permute the elements of List1 into List2
aleph_rpermute(List1,List2):-
  length(List1,N1),
  aleph_rpermute(List1,N1,List2).

aleph_rpermute([],0,[]):- !.
aleph_rpermute(L1,N1,[X|Rest]):-
  get_random(N1,R),
  nth0(R,L1,X,L2),
  N2 is N1 - 1,
  aleph_rpermute(L2,N2,Rest).

% aleph_rsample(+N,+List1,-List2)
%  randomly sample N elements from List1 into List2
aleph_rsample(N,List1,List2):-
  length(List1,N1),
  aleph_rsample(N,N1,List1,List2).

aleph_rsample(N,N1,L,L):- N >= N1, !.
aleph_rsample(SampleSize,Total,[X|L1],[X|L2]):-
  get_random(Total,R),
  R =< SampleSize, !,
  SampleSize0 is SampleSize - 1,
  Total0 is Total - 1,
  aleph_rsample(SampleSize0,Total0,L1,L2).
aleph_rsample(SampleSize,Total,[_|L1],L2):-
  Total0 is Total - 1,
  aleph_rsample(SampleSize,Total0,L1,L2).

% get_first_n(+N,+List1,-List2)
%  get the first n elements in List1
get_first_n(0,_,[]):- !.
get_first_n(_,[],[]):- !.
get_first_n(N,[H|T],[H|T1]):-
  N1 is N - 1,
  get_first_n(N1,T,T1).

% erase_refs(+List)
%  erase database references: only works for Yap
erase_refs([]).
erase_refs([DbRef|DbRefs]):-
  erase(DbRef),
  erase_refs(DbRefs).


% max_in_list(+List,-Max)
%  return largest element in a list
max_in_list([X],X):- !.
max_in_list([X|T],Z):-
  max_in_list(T,Y),
  (X @> Y -> Z = X; Z = Y).

% min_in_list(+List,-Max)
%  return largest element in a list
min_in_list([X],X):- !.
min_in_list([X|T],Z):-
  min_in_list(T,Y),
  (X @> Y -> Z = Y; Z = X).

% remove_alpha_variants(+List1,-List2):-
%  remove alphabetic variants from List1 to give List2
remove_alpha_variants([],[]).
remove_alpha_variants([X|Y],L):-
  member(X1,Y),
  alphabetic_variant(X,X1), !,
  remove_alpha_variants(Y,L).
remove_alpha_variants([X|Y],[X|L]):-
  remove_alpha_variants(Y,L).

% alphabetic_variant(+Term1,+Term2)
%  true if Term1 is the alphabetic variant of Term2
alphabetic_variant(Term1,Term2):-
  copy_term(Term1/Term2,T1/T2),
  numbervars(T1,0,_),
  numbervars(T2,0,_),
  T1 = T2.

% tparg(+TermPlace,+Term1,?Term2)
% return Term2 at position specified by TermPlace in Term1
tparg([Place],Term,Arg):-
        !,
        arg(Place,Term,Arg).
tparg([Place|Places],Term,Arg):-
        arg(Place,Term,Term1),
        tparg(Places,Term1,Arg).

aleph_member3(A,A-B):- A =< B.
aleph_member3(X,A-B):-
  A < B,
  A1 is A + 1,
  aleph_member3(X,A1-B).

goals_to_clause((Head,Body),(Head:-Body)):- !.
goals_to_clause(Head,Head).

clause_to_list((Head:-true),[Head]):- !.
clause_to_list((Head:-Body),[Head|L]):-
        !,
        goals_to_list(Body,L).
clause_to_list(Head,[Head]).

extend_clause(false,Lit,(Lit)):- !.
extend_clause((Head:-Body),Lit,(Head:-Body1)):-
        !,
        app_lit(Lit,Body,Body1).
extend_clause(Head,Lit,(Head:-Lit)).

app_lit(L,(L1,L2),(L1,L3)):-
        !,
        app_lit(L,L2,L3).
app_lit(L,L1,(L1,L)).

prefix_lits(L,true,L):- !.
prefix_lits(L,L1,((L),L1)).

get_goaldiffs((G1,G2),(G1,G3),Diffs):-
  !,
  get_goaldiffs(G2,G3,Diffs).
get_goaldiffs(true,G,G):- !.
get_goaldiffs(G1,(G1,G2),G2).

nlits((_:-B),N):-
  !,
  nlits(B,N1),
  N is N1 + 1.
nlits((_,Lits),N):-
  !,
  nlits(Lits,N1),
  N is N1 + 1.
nlits(_,1).

list_to_clause([Goal],(Goal:-true)):- !.
list_to_clause([Head|Goals],(Head:-Body)):-
  list_to_goals(Goals,Body).

list_to_goals([Goal],Goal):- !.
list_to_goals([Goal|Goals],(Goal,Goals1)):-
  list_to_goals(Goals,Goals1).

goals_to_list((true,Goals),T):-
  !,
  goals_to_list(Goals,T).
goals_to_list((Goal,Goals),[Goal|T]):-
  !,
  goals_to_list(Goals,T).
goals_to_list(true,[]):- !.
goals_to_list(Goal,[Goal]).

% get_litnums(+First,+Last,-LitNums)
%  get list of Literal numbers in the bottom clause
get_litnums(LitNum,Last,[]):-
        LitNum > Last, !.
get_litnums(LitNum,Last,[LitNum|LitNums]):-
        '$aleph_sat_litinfo'(LitNum,_,_,_,_,_), !,
        NextLit is LitNum + 1,
        get_litnums(NextLit,Last,LitNums).
get_litnums(LitNum,Last,LitNums):-
        NextLit is LitNum + 1,
        get_litnums(NextLit,Last,LitNums).

get_clause(LitNum,Last,_,[]):-
        LitNum > Last, !.
get_clause(LitNum,Last,TVSoFar,[FAtom|FAtoms]):-
        '$aleph_sat_litinfo'(LitNum,_,Atom,_,_,_), !,
        get_flatatom(Atom,TVSoFar,FAtom,TV1),
        NextLit is LitNum + 1,
        get_clause(NextLit,Last,TV1,FAtoms).
get_clause(LitNum,Last,TVSoFar,FAtoms):-
        NextLit is LitNum + 1,
        get_clause(NextLit,Last,TVSoFar,FAtoms).

get_flatatom(not(Atom),TVSoFar,not(FAtom),TV1):-
        !,
        get_flatatom(Atom,TVSoFar,FAtom,TV1).
get_flatatom(Atom,TVSoFar,FAtom,TV1):-
        functor(Atom,Name,Arity),
        functor(FAtom,Name,Arity),
        flatten_args(Arity,Atom,FAtom,TVSoFar,TV1).

get_pclause([LitNum],TVSoFar,Clause,TV,Length,LastDepth):-
        !,
        get_pclause1([LitNum],TVSoFar,TV,Clause,Length,LastDepth).
get_pclause([LitNum|LitNums],TVSoFar,Clause,TV,Length,LastDepth):-
        get_pclause1([LitNum],TVSoFar,TV1,Head,Length1,_),
        get_pclause1(LitNums,TV1,TV,Body,Length2,LastDepth),
  Clause = (Head:-Body),
        Length is Length1 + Length2.

get_pclause1([LitNum],TVSoFar,TV1,Lit,Length,LastDepth):-
        !,
        '$aleph_sat_litinfo'(LitNum,LastDepth,Atom,_,_,_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        functor(Lit,Name,_),
        (Name = '='-> Length = 0; Length = 1).
get_pclause1([LitNum|LitNums],TVSoFar,TV2,(Lit,Lits1),Length,LastDepth):-
        '$aleph_sat_litinfo'(LitNum,_,Atom,_,_,_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        get_pclause1(LitNums,TV1,TV2,Lits1,Length1,LastDepth),
        functor(Lit,Name,_),
        (Name = '='-> Length = Length1; Length is Length1 + 1).

get_pclause([LitNum],Key,TVSoFar,Clause,TV,Length,LastDepth):-
        !,
        get_pclause1([LitNum],Key,TVSoFar,TV,Clause,Length,LastDepth).
get_pclause([LitNum|LitNums],Key,TVSoFar,Clause,TV,Length,LastDepth):-
        get_pclause1([LitNum],Key,TVSoFar,TV1,Head,Length1,_),
        get_pclause1(LitNums,Key,TV1,TV,Body,Length2,LastDepth),
  Clause = (Head:-Body),
        Length is Length1 + Length2.

get_pclause1([LitNum],Key,TVSoFar,TV1,Lit,Length,LastDepth):-
        !,
        '$aleph_sat_litinfo'(LitNum,Key,LastDepth,Atom,_,_,_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        functor(Lit,Name,_),
        (Name = '='-> Length = 0; Length = 1).
get_pclause1([LitNum|LitNums],Key,TVSoFar,TV2,(Lit,Lits1),Length,LastDepth):-
        '$aleph_sat_litinfo'(LitNum,Key,_,Atom,_,_,_),
        get_flatatom(Atom,TVSoFar,Lit,TV1),
        get_pclause1(LitNums,Key,TV1,TV2,Lits1,Length1,LastDepth),
        functor(Lit,Name,_),
        (Name = '='-> Length = Length1; Length is Length1 + 1).


flatten_args(0,_,_,TV,TV):- !.
flatten_args(Arg,Atom,FAtom,TV,TV1):-
        arg(Arg,Atom,Term),
        Arg1 is Arg - 1,
        (Term = aleph_const(Const) ->
                arg(Arg,FAtom,Const),
                flatten_args(Arg1,Atom,FAtom,TV,TV1);
                (integer(Term) ->
                        update(TV,Term/Var,TV0),
                        arg(Arg,FAtom,Var),
                        flatten_args(Arg1,Atom,FAtom,TV0,TV1);
                        (functor(Term,Name,Arity),
                         functor(FTerm,Name,Arity),
                         arg(Arg,FAtom,FTerm),
                         flatten_args(Arity,Term,FTerm,TV,TV0),
                         flatten_args(Arg1,Atom,FAtom,TV0,TV1)
                        )
                )
        ).


% returns intersection of S1, S2 and S1-Intersection
intersect1(Elems,[],[],Elems):- !.
intersect1([],_,[],[]):- !.
intersect1([Elem|Elems],S2,[Elem|Intersect],ElemsLeft):-
  memberchk(Elem,S2), !,
  intersect1(Elems,S2,Intersect,ElemsLeft).
intersect1([Elem|Elems],S2,Intersect,[Elem|ElemsLeft]):-
  intersect1(Elems,S2,Intersect,ElemsLeft).

% two sets are equal

equal_set([],[]).
equal_set([H|T], S):-
  select(H, S, S1),
  equal_set(T, S1),
  !.

uniq_insert(_,X,[],[X]).
uniq_insert(descending,H,[H1|T],[H,H1|T]):-
  H @> H1, !.
uniq_insert(ascending,H,[H1|T],[H,H1|T]):-
  H @< H1, !.
uniq_insert(_,H,[H|T],[H|T]):- !.
uniq_insert(Order,H,[H1|T],[H1|T1]):-
  !,
  uniq_insert(Order,H,T,T1).

quicksort(_,[],[]).
quicksort(Order,[X|Tail],Sorted):-
  partition(X,Tail,Small,Big),
  quicksort(Order,Small,SSmall),
  quicksort(Order,Big,SBig),
  (
    Order = ascending
  ->
    append(SSmall, [X | SBig], Sorted)
  ;
    append(SBig, [X | SSmall], Sorted)
  ).

partition(_,[],[],[]).
partition(X,[Y|Tail],[Y|Small],Big):-
  X @> Y, !,
  partition(X,Tail,Small,Big).
partition(X,[Y|Tail],Small,[Y|Big]):-
  partition(X,Tail,Small,Big).

update_list([],L,L).
update_list([H|T],L,Updated):-
  update(L,H,L1), !,
  update_list(T,L1,Updated).

update([],H,[H]).
update([H|T],H,[H|T]):- !.
update([H1|T],H,[H1|T1]):-
  update(T,H,T1).

% checks if 2 sets intersect
intersects(S1,S2):-
  member(Elem,S1), memberchk(Elem,S2), !.

% checks if bitsets represented as lists of intervals intersect
intervals_intersects([L1-L2|_],I):-
  intervals_intersects1(L1-L2,I), !.
intervals_intersects([_|I1],I):-
  intervals_intersects(I1,I).

intervals_intersects1(L1-_,[M1-M2|_]):-
  L1 >= M1, L1 =< M2, !.
intervals_intersects1(L1-L2,[M1-_|_]):-
  M1 >= L1, M1 =< L2, !.
intervals_intersects1(L1-L2,[_|T]):-
  intervals_intersects1(L1-L2,T).

% checks if bitsets represented as lists of intervals intersect
% returns first intersection
intervals_intersects([L1-L2|_],I,I1):-
  intervals_intersects1(L1-L2,I,I1), !.
intervals_intersects([_|ILeft],I,I1):-
  intervals_intersects(ILeft,I,I1).

intervals_intersects1(I1,[I2|_],I):-
  interval_intersection(I1,I2,I), !.
intervals_intersects1(I1,[_|T],I):-
  intervals_intersects1(I1,T,I).

interval_intersection(L1-L2,M1-M2,L1-L2):-
  L1 >= M1, L2 =< M2, !.
interval_intersection(L1-L2,M1-M2,M1-M2):-
  M1 >= L1, M2 =< L2, !.
interval_intersection(L1-L2,M1-M2,L1-M2):-
  L1 >= M1, M2 >= L1, M2 =< L2, !.
interval_intersection(L1-L2,M1-M2,M1-L2):-
  M1 >= L1, M1 =< L2, L2 =< M2, !.

%most of the time no intersection, so optimise on that
% optimisation by James Cussens
intervals_intersection([],_,[]).
intervals_intersection([A-B|T1],[C-D|T2],X):-
        !,
        (A > D ->
            intervals_intersection([A-B|T1],T2,X);
            (C > B ->
                intervals_intersection(T1,[C-D|T2],X);
                (B > D ->
                    (C > A ->
                        X=[C-D|Y];
                        X=[A-D|Y]
                    ),
                    intervals_intersection([A-B|T1],T2,Y);
                    (C > A ->
                        X=[C-B|Y];
                        X=[A-B|Y]
                    ),
                    intervals_intersection(T1,[C-D|T2],Y)
                )
            )
        ).
intervals_intersection(_,[],[]).


% finds length of intervals in a list
interval_count([],0).
interval_count([L1-L2|T],N):-
  N1 is L2 - L1 + 1,
  interval_count(T,N2),
  N is N1 + N2.
interval_count(I/_,N):-
  interval_count(I,N).

% interval_select(+N,+List1,-Elem)
%       select the Nth elem from an interval list
interval_select(N,[A-B|_],X):-
        N =< B - A + 1, !,
        X is A + N - 1.
interval_select(N,[A-B|T],X):-
        N1 is N - (B - A + 1),
        interval_select(N1,T,X).

% interval_sample(+N,List1,-List2)
%  get a random sample of N elements from List1
interval_sample(N,List1,List2):-
  intervals_to_list(List1,L1),
  aleph_rsample(N,L1,L2),
  list_to_intervals(L2,List2).

% convert list to intervals
list_to_intervals(List,Intervals):-
  sort(List,List1),
        list_to_intervals1(List1,Intervals).

list_to_intervals1([],[]).
list_to_intervals1([Start|T],[Start-Finish|I1]):-
        list_to_interval(Start,T,Finish,T1),
        list_to_intervals1(T1,I1).

list_to_interval(Finish,[],Finish,[]).
list_to_interval(Finish,[Next|T],Finish,[Next|T]):-
        Next - Finish > 1,
        !.
list_to_interval(_,[Start|T],Finish,Rest):-
        list_to_interval(Start,T,Finish,Rest).

% converts an interval-list into a list of (sorted) numbers
intervals_to_list(L,L1):-
  intervals_to_list(L,[],L0),
  sort(L0,L1), !.

intervals_to_list([],L,L).
intervals_to_list([Interval|Intervals],L1,L2):-
  interval_to_list(Interval,L1,L),
  intervals_to_list(Intervals,L,L2).

% converts an interval into a list
interval_to_list(Start-Finish,[]):-
  Start > Finish, !.
interval_to_list(Start-Finish,[Start|T]):-
  Start1 is Start+1,
  interval_to_list(Start1-Finish,T).

% converts an interval into a list
%  with an accumulator list. Result will be in reverse order
interval_to_list(Start-Finish,L,L):-
  Start > Finish, !.
interval_to_list(Start-Finish,L,L1):-
  Start1 is Start+1,
  interval_to_list(Start1-Finish,[Start|L],L1).

% interval_subsumes(+I1,+I2)
%  checks to see if interval I1 subsumes I2
interval_subsumes(Start1-Finish1,Start2-Finish2):-
  Start1 =< Start2,
  Finish1 >= Finish2.

interval_subtract(Start1-Finish1,Start1-Finish1,[]):- !.
interval_subtract(Start1-Finish1,Start1-Finish2,[S2-Finish1]):-
  !,
  S2 is Finish2 + 1.
interval_subtract(Start1-Finish1,Start2-Finish1,[Start1-S1]):-
  !,
  S1 is Start2 - 1.
interval_subtract(Start1-Finish1,Start2-Finish2,[Start1-S1,S2-Finish1]):-
  S1 is Start2 - 1,
  S2 is Finish2 + 1,
  S1 >= Start1, Finish1 >= S2, !.


% code for set manipulation utilities
% taken from the Yap library
% aleph_ord_subtract(+Set1,+Set2,?Difference)
% is true when Difference contains all and only the elements of Set1
% which are not also in Set2.
aleph_ord_subtract(Set1,[],Set1):- !.
aleph_ord_subtract([],_,[]):- !.
aleph_ord_subtract([Head1|Tail1],[Head2|Tail2],Difference):-
        compare(Order,Head1,Head2),
        aleph_ord_subtract(Order,Head1,Tail1,Head2,Tail2,Difference).

aleph_ord_subtract(=,_,    Tail1,_,    Tail2,Difference):-
  aleph_ord_subtract(Tail1,Tail2,Difference).
aleph_ord_subtract(<,Head1,Tail1,Head2,Tail2,[Head1|Difference]):-
        aleph_ord_subtract(Tail1,[Head2|Tail2],Difference).
aleph_ord_subtract(>,Head1,Tail1,_,    Tail2,Difference):-
        aleph_ord_subtract([Head1|Tail1],Tail2,Difference).

% aleph_ord_disjoint(+Set1,+Set2)
% is true when the two ordered sets have no element in common.  If the
% arguments are not ordered,I have no idea what happens.
aleph_ord_disjoint([],_):- !.
aleph_ord_disjoint(_,[]):- !.
aleph_ord_disjoint([Head1|Tail1],[Head2|Tail2]):-
        compare(Order,Head1,Head2),
        aleph_ord_disjoint(Order,Head1,Tail1,Head2,Tail2).

aleph_ord_disjoint(<,_,Tail1,Head2,Tail2):-
        aleph_ord_disjoint(Tail1,[Head2|Tail2]).
aleph_ord_disjoint(>,Head1,Tail1,_,Tail2):-
        aleph_ord_disjoint([Head1|Tail1],Tail2).


% aleph_ord_union(+Set1,+Set2,?Union)
% is true when Union is the union of Set1 and Set2.  Note that when
% something occurs in both sets,we want to retain only one copy.
aleph_ord_union(Set1,[],Set1):- !.
aleph_ord_union([],Set2,Set2):- !.
aleph_ord_union([Head1|Tail1],[Head2|Tail2],Union):-
        compare(Order,Head1,Head2),
        aleph_ord_union(Order,Head1,Tail1,Head2,Tail2,Union).

aleph_ord_union(=,Head, Tail1,_,    Tail2,[Head|Union]):-
        aleph_ord_union(Tail1,Tail2,Union).
aleph_ord_union(<,Head1,Tail1,Head2,Tail2,[Head1|Union]):-
        aleph_ord_union(Tail1,[Head2|Tail2],Union).
aleph_ord_union(>,Head1,Tail1,Head2,Tail2,[Head2|Union]):-
        aleph_ord_union([Head1|Tail1],Tail2,Union).

% aleph_ord_union(+Set1,+Set2,?Union,?Difference)
% is true when Union is the union of Set1 and Set2 and Difference is the
% difference between Set2 and Set1.
aleph_ord_union(Set1,[],Set1,[]):- !.
aleph_ord_union([],Set2,Set2,Set2):- !.
aleph_ord_union([Head1|Tail1],[Head2|Tail2],Union,Diff):-
        compare(Order,Head1,Head2),
        aleph_ord_union(Order,Head1,Tail1,Head2,Tail2,Union,Diff).

aleph_ord_union(=,Head, Tail1,_, Tail2,[Head|Union],Diff):-
        aleph_ord_union(Tail1,Tail2,Union,Diff).
aleph_ord_union(<,Head1,Tail1,Head2,Tail2,[Head1|Union],Diff):-
        aleph_ord_union(Tail1,[Head2|Tail2],Union,Diff).
aleph_ord_union(>,Head1,Tail1,Head2,Tail2,[Head2|Union],[Head2|Diff]):-
        aleph_ord_union([Head1|Tail1],Tail2,Union,Diff).

aleph_ord_intersection(_,[],[]):- !.
aleph_ord_intersection([],_,[]):- !.
aleph_ord_intersection([Head1|Tail1],[Head2|Tail2],Intersection):-
        compare(Order,Head1,Head2),
        aleph_ord_intersection(Order,Head1,Tail1,Head2,Tail2,Intersection).

aleph_ord_intersection(=,Head,Tail1,_,Tail2,[Head|Intersection]):-
        aleph_ord_intersection(Tail1,Tail2,Intersection).
aleph_ord_intersection(<,_,Tail1,Head2,Tail2,Intersection):-
        aleph_ord_intersection(Tail1,[Head2|Tail2],Intersection).
aleph_ord_intersection(>,Head1,Tail1,_,Tail2,Intersection):-
        aleph_ord_intersection([Head1|Tail1],Tail2,Intersection).

aleph_ord_subset([], _):- !.
aleph_ord_subset([Head1|Tail1], [Head2|Tail2]):-
        compare(Order, Head1, Head2),
        aleph_ord_subset(Order, Head1, Tail1, Head2, Tail2).

aleph_ord_subset(=, _, Tail1, _, Tail2):-
        aleph_ord_subset(Tail1, Tail2).
aleph_ord_subset(>, Head1, Tail1, _, Tail2):-
        aleph_ord_subset([Head1|Tail1], Tail2).

vars_in_term([],Vars,Vars1):- sort(Vars,Vars1), !.
vars_in_term([Var|T],VarsSoFar,Vars):-
        var(Var), !,
        vars_in_term(T,[Var|VarsSoFar],Vars).
vars_in_term([Term|T],VarsSoFar,Vars):-
        Term =.. [_|Terms], !,
        vars_in_term(Terms,VarsSoFar,V1),
        vars_in_term(T,V1,Vars).
vars_in_term([_|T],VarsSoFar,Vars):-
        vars_in_term(T,VarsSoFar,Vars).

occurs_in(Vars,(Lit,_)):-
  occurs_in(Vars,Lit), !.
occurs_in(Vars,(_,Lits)):-
  !,
  occurs_in(Vars,Lits).
occurs_in(Vars,Lit):-
  functor(Lit,_,Arity),
  occurs1(Vars,Lit,1,Arity).

occurs1(Vars,Lit,Argno,MaxArgs):-
  Argno =< MaxArgs,
  arg(Argno,Lit,Term),
  vars_in_term([Term],[],Vars1),
  member(X,Vars), member(Y,Vars1),
  X == Y, !.
occurs1(Vars,Lit,Argno,MaxArgs):-
  Argno < MaxArgs,
  Next is Argno + 1,
  occurs1(Vars,Lit,Next,MaxArgs).


declare_dynamic(Name/Arity):-
  dynamic Name/Arity.

aleph_abolish(Name/Arity):-
  functor(Pred,Name,Arity),
  (predicate_property(Pred,dynamic) ->
    retractall(Pred);
    abolish(Name/Arity)).

aleph_open(File, Mode, Stream):-
  catch(open(File, Mode, Stream, [type(text)]), _Error, fail).

clean_up:-
  clean_up_init,
  clean_up_sat,
  clean_up_reduce.

clean_up_init:-
  aleph_abolish('$aleph_good'/3),
  retractall('$aleph_search'(last_good,_)),
  aleph_abolish('$aleph_feature'/2).

clean_up_sat:-
  aleph_abolish('$aleph_sat'/2),
  aleph_abolish('$aleph_local'/2),
  aleph_abolish('$aleph_sat_atom'/2),
  aleph_abolish('$aleph_sat_ovars'/2),
  aleph_abolish('$aleph_sat_ivars'/2),
  aleph_abolish('$aleph_sat_varscopy'/3),
  aleph_abolish('$aleph_sat_varequiv'/3),
  aleph_abolish('$aleph_sat_terms'/4),
  aleph_abolish('$aleph_sat_vars'/4),
  aleph_abolish('$aleph_sat_litinfo'/6),
  retractall('$aleph_search'(pclause,_)),
  garbage_collect.

clean_up_reduce:-
  aleph_abolish('$aleph_local'/2),
  clean_up_search,
  retractall('$aleph_search'(pclause,_)),
  garbage_collect.

clean_up_search:-
  retractall('$aleph_search'(bad,_)),
  retractall('$aleph_search'(best,_)),
  retractall('$aleph_search'(best_label,_)),
  retractall('$aleph_search'(clauseprior,_)),
  retractall('$aleph_search'(covers,_)),
  retractall('$aleph_search'(coversn,_)),
  retractall('$aleph_search'(current,_)),
  retractall('$aleph_search'(label,_)),
  retractall('$aleph_search'(modes,_)),
  retractall('$aleph_search'(nextnode,_)),
  retractall('$aleph_search'(openlist,_)),
  retractall('$aleph_search'(pclause,_)),
  retractall('$aleph_search'(selected,_)),
  retractall('$aleph_search_seen'(_,_)),
  retractall('$aleph_search_expansion'(_,_,_,_)),
  retractall('$aleph_search_gain'(_,_,_,_)),
  retractall('$aleph_search_node'(_,_,_,_,_,_,_,_)).

clean_up_examples:-
  clean_up_examples(pos),
  clean_up_examples(neg),
  clean_up_examples(rand).

clean_up_tree:-
  retractall('$aleph_search'(tree,_)),
  retractall('$aleph_search'(tree_startdistribution,_)),
  retractall('$aleph_search'(tree_leaf,_)),
  retractall('$aleph_search'(tree_lastleaf,_)),
  retractall('$aleph_search'(tree_newleaf,_)),
  retractall('$aleph_search'(tree_besterror,_)),
  retractall('$aleph_search'(tree_gain,_)).

clean_up_examples(Type):-
  retractall('$aleph_global'(size,size(Type,_))),
  retractall('$aleph_global'(atoms,atoms(Type,_))),
  retractall('$aleph_global'(atoms_left,atoms_left(Type,_))),
  retractall('$aleph_global'(last_example,last_example(Type,_))).

clean_up_hypothesis:-
        retractall('$aleph_global'(hypothesis,hypothesis(_,_,_,_))).

depth_bound_call(G):-
  setting(depth, D),
  call_with_depth_bound(G,D).

call_with_depth_bound((H:-B),D):-
  !,
  call_with_depth_bound((H,B),D).
call_with_depth_bound((A,B),D):-
  !,
  depth_bound_call(A,D),
  call_with_depth_bound(B,D).
call_with_depth_bound(A,D):-
  depth_bound_call(A,D).

binom_lte(_,_,O,0.0):- O < 0, !.
binom_lte(N,P,O,Prob):-
        binom(N,P,O,Prob1),
        O1 is O - 1,
        binom_lte(N,P,O1,Prob2),
        Prob is Prob1 + Prob2, !.

binom(N,_,O,0.0):- O > N, !.
binom(N,P,O,Prob):-
        aleph_choose(N,O,C),
        E1 is P^O,
        P2 is 1 - P,
        O2 is N - O,
        E2 is P2^O2,
        Prob is C*E1*E2, !.

aleph_choose(N,I,V):-
        NI is N-I,
        (NI > I -> pfac(N,NI,I,V) ; pfac(N,I,NI,V)).

pfac(0,_,_,1).
pfac(1,_,_,1).
pfac(N,N,_,1).
pfac(N,I,C,F):-
        N1 is N-1,
        C1 is C-1,
        pfac(N1,I,C1,N1F),
        F1 is N/C,
        F is N1F*F1.

% record_example(+Check,+Type,+Example,-N)
%  records Example of type Type
%  if Check = check, then checks to see if example exists
%    also updates number of related databases accordingly
%  if Check = nocheck then no check is done
%  returns example number N and Flag
%  if Flag = new then example is a new example of Type
record_example(check,Type,Example,N1):-
  (once(example(N1,Type,Example)) -> true;
    record_example(nocheck,Type,Example,N1),
    (retract('$aleph_global'(atoms,atoms(Type,Atoms))) ->
        true;
        Atoms = []),
    (retract('$aleph_global'(atoms_left,atoms_left(Type,AtomsLeft)))->
        true;
        AtomsLeft = []),
    (retract('$aleph_global'(last_example,last_example(Type,_))) ->
        true;
        true),
    update(Atoms,N1-N1,NewAtoms),
    update(AtomsLeft,N1-N1,NewAtomsLeft),
    asserta('$aleph_global'(atoms,atoms(Type,NewAtoms))),
    asserta('$aleph_global'(atoms_left,atoms_left(Type,
            NewAtomsLeft))),
    asserta('$aleph_global'(last_example,last_example(Type,N1)))),
  !.
record_example(nocheck,Type,Example,N1):-
  (retract('$aleph_global'(size,size(Type,N)))->
    true;
    N is 0),
  N1 is N + 1,
  asserta('$aleph_global'(size,size(Type,N1))),
  (Type \= neg ->
    aleph5_setting(skolemvars,Sk1),
    skolemize(Example,Fact,Body,Sk1,SkolemVars),
    record_skolemized(Type,N1,SkolemVars,Fact,Body),
    (Sk1 = SkolemVars -> true;
      aleph5_set_setting(skolemvars,SkolemVars));
    split_clause(Example,Head,Body),
    record_nskolemized(Type,N1,Head,Body)), !.


record_targetpred:-
  retract('$aleph_local'(backpred,Name/Arity)),
  once('$aleph_global'(determination,determination(Name/Arity,_))),
  asserta('$aleph_global'(targetpred,targetpred(Name/Arity))),
  record_testclause(Name/Arity),
  fail.
record_targetpred.

check_recursive_calls:-
  '$aleph_global'(targetpred,targetpred(Name/Arity)),
  '$aleph_global'(determination,determination(Name/Arity,Name/Arity)),
  record_recursive_sat_call(Name/Arity),
  aleph5_set_setting(recursion, true),
  fail.
check_recursive_calls.

check_posonly:-
  '$aleph_global'(size,size(rand,N)),
  N > 0, !.
check_posonly:-
  aleph5_setting(evalfn,posonly),
  \+('$aleph_global'(modeh,modeh(_,_))),
  p1_message('error'),
  p_message('missing modeh declaration in posonly mode'), !,
  fail.
check_posonly:-
  retractall('$aleph_global'(slp_count,_,_)),
  retractall('$aleph_local'(slp_sample,_)),
  retractall('$aleph_local'(slp_samplenum,_)),
  aleph5_setting(evalfn,posonly),
  aleph5_setting(gsamplesize,S),
  condition_target,
  '$aleph_global'(targetpred,targetpred(Name/Arity)),
  gsample(Name/Arity,S), !.
check_posonly.

check_prune_defs:-
  clause(prune(_),_), !,
  aleph5_set_setting(prune_defs, true).
check_prune_defs.

check_auto_refine:-
  (
    aleph5_setting(construct_bottom, reduction)
  ;
    aleph5_setting(construct_bottom, false)
  ),
  aleph5_setting(autorefine, false),
  unless(
    aleph5_setting(refine, user),
    aleph5_set_setting(refine, auto)
  ).
check_auto_refine.

check_user_search:-
  aleph5_setting(evalfn,user),
  \+(cost_cover_required),
  aleph5_set_setting(lazy_on_cost,true), !.
check_user_search.

check_abducibles:-
  '$aleph_global'(abducible,abducible(Name/Arity)),
  record_testclause(Name/Arity),
  record_abclause(Name/Arity),
  fail.
check_abducibles.

cost_cover_required:-
  clause(cost(_,Label,Cost),Body),
  vars_in_term([Label],[],Vars),
  (occurs_in(Vars,p(Cost)); occurs_in(Vars,Body)), !.

set_lazy_recalls:-
  '$aleph_global'(lazy_evaluate,lazy_evaluate(Name/Arity)),
  functor(Pred,Name,Arity),
  % asserta('$aleph_global'(lazy_recall,lazy_recall(Name/Arity,1))),
  asserta('$aleph_global'(lazy_recall,lazy_recall(Name/Arity,0))),
  '$aleph_global'(mode,mode(Recall,Pred)),
  '$aleph_global'(lazy_recall,lazy_recall(Name/Arity,N)),
  (Recall = '*' -> RecallNum = 100; RecallNum = Recall),
  RecallNum > N,
  retract('$aleph_global'(lazy_recall,lazy_recall(Name/Arity,N))),
  asserta('$aleph_global'(lazy_recall,lazy_recall(Name/Arity,RecallNum))),
  fail.
set_lazy_recalls.

set_lazy_on_contradiction(_,_):-
  '$aleph_global'(lazy_on_contradiction,aleph5_set_setting(lazy_on_contradiction,false)), !.
set_lazy_on_contradiction(P,N):-
  Tot is P + N,
  Tot >= 100, !,
  aleph5_set_setting(lazy_on_contradiction,true).
set_lazy_on_contradiction(_,_).

% The "pclause" trick: much more effective with the use of recorded/3
% clause for testing partial clauses obtained in search
% only needed when learning recursive theories or
% proof_strategy is not restricted_sld.
record_testclause(Name/Arity):-
        functor(Head,Name,Arity),
        Clause = (Head:-
                        '$aleph_search'(pclause,pclause(Head,Body)),
                        Body, !),
        assertz(Clause).

% The "pclause" trick for abducible predicates
record_abclause(Name/Arity):-
        functor(Head,Name,Arity),
        Clause = (Head:-
                        '$aleph_search'(abduced,pclause(Head,Body)),
                        Body, !),
        assertz(Clause).

% clause for incorporating recursive calls into bottom clause
% this is done by allowing calls to the positive examples
record_recursive_sat_call(Name/Arity):-
        functor(Head,Name,Arity),
  Clause = (Head:-
      setting(stage, saturation),
      '$aleph_sat'(example,example(Num,Type)),
      example(Num1,Type,Head),
      Num1 \= Num, !),  % to prevent tautologies
  assertz(Clause).

skolemize((Head:-Body),SHead,SBody,Start,SkolemVars):-
  !,
  copy_term((Head:-Body),(SHead:-Body1)),
  numbervars((SHead:-Body1),Start,SkolemVars),
  goals_to_list(Body1,SBody).
skolemize(UnitClause,Lit,[],Start,SkolemVars):-
  copy_term(UnitClause,Lit),
  numbervars(Lit,Start,SkolemVars).
skolemize(UnitClause,Lit):-
  skolemize(UnitClause,Lit,[],0,_).

record_nskolemized(Type,N1,Head,true):-
  !,
  assertz(example(N1,Type,Head)).
record_nskolemized(Type,N1,Head,Body):-
  assertz((example(N1,Type,Head):-Body)).

record_skolemized(Type,N1,SkolemVars,Head,Body):-
  assertz(example(N1,Type,Head)),
        functor(Head,Name,Arity),
        update_backpreds(Name/Arity),
  add_backs(Body),
  add_skolem_types(SkolemVars,Head,Body).

add_backs([]).
add_backs([Lit|Lits]):-
  asserta('$aleph_global'(back,back(Lit))),
  functor(Lit,Name,Arity),
  declare_dynamic(Name/Arity),
  assertz(Lit),
  add_backs(Lits).

add_skolem_types(10000,_,_):- !.  % no new skolem variables
add_skolem_types(_,Head,Body):-
  add_skolem_types([Head]),
  add_skolem_types(Body).

add_skolem_types([]).
add_skolem_types([Lit|Lits]):-
  functor(Lit,PSym,Arity),
  get_modes(PSym/Arity,L),
  add_skolem_types1(L,Lit),
  add_skolem_types(Lits).

add_skolem_types1([],_).
add_skolem_types1([Lit|Lits],Fact):-
  split_args(Lit,_,I,O,C),
  add_skolem_types2(I,Fact),
  add_skolem_types2(O,Fact),
  add_skolem_types2(C,Fact),
  add_skolem_types1(Lits,Fact).

add_skolem_types2([],_).
add_skolem_types2([Pos/Type|Rest],Literal):-
  tparg(Pos,Literal,Arg),
  SkolemType =.. [Type,Arg],
  ('$aleph_global'(back,back(SkolemType))-> true;
    asserta('$aleph_global'(back,back(SkolemType))),
    asserta(SkolemType)),
  add_skolem_types2(Rest,Literal).


copy_args(_,_,Arg,Arity):-
  Arg > Arity, !.
copy_args(Lit,Lit1,Arg,Arity):-
  arg(Arg,Lit,T),
  arg(Arg,Lit1,T),
  NextArg is Arg + 1,
  copy_args(Lit,Lit1,NextArg,Arity).

copy_iargs(0,_,_,_):- !.
copy_iargs(Arg,Old,New,Arg):-
        !,
        Arg1 is Arg - 1,
        copy_iargs(Arg1,Old,New,Arg).
copy_iargs(Arg,Old,New,Out):-
        arg(Arg,Old,Val),
        arg(Arg,New,Val),
        Arg1 is Arg - 1,
        copy_iargs(Arg1,Old,New,Out).


index_clause((Head:-true),NextClause,(Head)):-
  !,
  retract('$aleph_global'(last_clause,last_clause(ClauseNum))),
  NextClause is ClauseNum + 1,
  asserta('$aleph_global'(last_clause,last_clause(NextClause))).
index_clause(Clause,NextClause,Clause):-
  retract('$aleph_global'(last_clause,last_clause(ClauseNum))),
  NextClause is ClauseNum + 1,
  asserta('$aleph_global'(last_clause,last_clause(NextClause))).

update_backpreds(Name/Arity):-
  '$aleph_local'(backpred,Name/Arity), !.
update_backpreds(Name/Arity):-
  assertz('$aleph_local'(backpred,Name/Arity)).

reset_counts:-
  retractall('$aleph_sat'(lastterm,_)),
  retractall('$aleph_sat'(lastvar,_)),
  asserta('$aleph_sat'(lastterm,0)),
  asserta('$aleph_sat'(lastvar,0)), !.

% reset the number of successes for a literal: cut to avoid useless backtrack
reset_succ:-
        retractall('$aleph_local'(last_success,_)),
        asserta('$aleph_local'(last_success,0)), !.

skolem_var(Var):-
  atomic(Var), !,
  name(Var,[36|_]).
skolem_var(Var):-
  gen_var(Num),
  name(Num,L),
  name(Var,[36|L]).

gen_var(Var1):-
  retract('$aleph_sat'(lastvar,Var0)), !,
        Var1 is Var0 + 1,
  asserta('$aleph_sat'(lastvar,Var1)).
gen_var(0):-
  asserta('$aleph_sat'(lastvar,0)).

copy_var(OldVar,NewVar,Depth):-
  gen_var(NewVar),
  '$aleph_sat_vars'(OldVar,TNo,_,_),
  asserta('$aleph_sat_vars'(NewVar,TNo,[],[])),
  asserta('$aleph_sat_varscopy'(NewVar,OldVar,Depth)).

gen_litnum(Lit1):-
  retract('$aleph_sat'(lastlit,Lit0)), !,
        Lit1 is Lit0 + 1,
  asserta('$aleph_sat'(lastlit,Lit1)).
gen_litnum(0):-
  asserta('$aleph_sat'(lastlit,0)).

gen_nlitnum(Lit1):-
  retract('$aleph_sat'(lastnlit,Lit0)), !,
        Lit1 is Lit0 - 1,
  asserta('$aleph_sat'(lastnlit,Lit1)).
gen_nlitnum(-1):-
  asserta('$aleph_sat'(lastnlit,-1)).

% generate a new feature number
% provided it is less than the maximum number of features allowed
gen_featurenum(Feature1):-
        '$aleph_feature'(last_feature,Feature0), !,
        Feature1 is Feature0 + 1,
  aleph5_setting(max_features,FMax),
  Feature1 =< FMax,
        retract('$aleph_feature'(last_feature,Feature0)),
        asserta('$aleph_feature'(last_feature,Feature1)).
gen_featurenum(1):-
        asserta('$aleph_feature'(last_feature,1)).

gen_lits([],[]).
gen_lits([Lit|Lits],[LitNum|Nums]):-
  gen_litnum(LitNum),
  asserta('$aleph_sat_litinfo'(LitNum,0,Lit,[],[],[])),
  gen_lits(Lits,Nums).

update_theory(ClauseIndex):-
        retract('$aleph_global'(hypothesis,hypothesis(OldLabel,Hypothesis,
        OldPCover,OldNCover))),
  index_clause(Hypothesis,ClauseIndex,Clause),
        ('$aleph_global'(example_selected,example_selected(_,Seed))-> true;
                PCover = [Seed-_|_]),
  (aleph5_setting(lazy_on_cost,true) ->
          nlits(Clause,L),
    label_create(Clause,Label),
          extract_pos(Label,PCover),
          extract_neg(Label,NCover),
          interval_count(PCover,PC),
          interval_count(NCover,NC),
    aleph5_setting(evalfn,Evalfn),
    complete_label(Evalfn,Clause,[PC,NC,L],NewLabel),
          assertz('$aleph_global'(theory,theory(ClauseIndex,
          NewLabel/Seed,Clause,
          PCover,NCover)));
          assertz('$aleph_global'(theory,theory(ClauseIndex,
          OldLabel/Seed,Clause,
          OldPCover,OldNCover)))),
  add_clause_to_background(ClauseIndex).

add_clause_to_background(ClauseIndex):-
        '$aleph_global'(theory,theory(ClauseIndex,Label/_,Clause,_,_)),
  (aleph5_setting(minpos,PMin) -> true; PMin = 1),
  Label = [PC,_,_,F|_],
  PC >= PMin,
  aleph5_setting(minscore,MinScore),
  F >= MinScore, !,
        (retract('$aleph_global'(rules,rules(Rules)))->
                asserta('$aleph_global'(rules,rules([ClauseIndex|Rules])));
                asserta('$aleph_global'(rules,rules([ClauseIndex])))),
  (aleph5_setting(updateback,Update) -> true; Update = true),
        (Update = true -> assertz(Clause); true),  !.
add_clause_to_background(_).


rm_seeds:-
  update_theory(ClauseIndex), !,
  '$aleph_global'(theory,theory(ClauseIndex,_,_,PCover,NCover)),
  rm_seeds(pos,PCover),
  (aleph5_setting(evalfn,posonly) -> rm_seeds(rand,NCover); true),
  '$aleph_global'(atoms_left,atoms_left(pos,PLeft)),
  interval_count(PLeft,PL),
  p1_message('atoms left'), p_message(PL),
  !.
rm_seeds.

rm_seeds(pos,PCover):-
  aleph5_setting(construct_features,true),
  aleph5_setting(feature_construction,exhaustive), !,
        retract('$aleph_global'(atoms_left,atoms_left(pos,OldIntervals))),
        ('$aleph_global'(example_selected,example_selected(_,Seed))-> true;
                PCover = [Seed-_|_]),
        rm_seeds1([Seed-Seed],OldIntervals,NewIntervals),
        assertz('$aleph_global'(atoms_left,atoms_left(pos,NewIntervals))).
rm_seeds(Type,RmIntervals):-
        retract('$aleph_global'(atoms_left,atoms_left(Type,OldIntervals))),
        rm_seeds1(RmIntervals,OldIntervals,NewIntervals),
        assertz('$aleph_global'(atoms_left,atoms_left(Type,NewIntervals))).

rm_seeds1([],Done,Done).
rm_seeds1([Start-Finish|Rest],OldIntervals,NewIntervals):-
        rm_interval(Start-Finish,OldIntervals,MidIntervals),!,
        rm_seeds1(Rest,MidIntervals,NewIntervals).

% update lower estimate on maximum size cover set for an atom
update_coverset(Type,_):-
        '$aleph_global'(hypothesis,hypothesis(Label,_,PCover,_)),
  Label = [_,_,_,Gain|_],
        worse_coversets(PCover,Type,Gain,Worse),
        (Worse = [] -> true;
                update_theory(NewClause),
                update_coversets(Worse,NewClause,Type,Label)).

% revise coversets of previous atoms
worse_coversets(_,_,_,[]):-
  setting(maxcover, false),
  !.
worse_coversets([],_,_,[]).
worse_coversets([Interval|Intervals],Type,Gain,Worse):-
  worse_coversets1(Interval,Type,Gain,W1),
  worse_coversets(Intervals,Type,Gain,W2),
  append(W1, W2, Worse),
  !.

worse_coversets1(Start-Finish,_,_,[]):-
        Start > Finish, !.
worse_coversets1(Start-Finish,Type,Gain,Rest):-
        '$aleph_global'(max_set,max_set(Type,Start,Label1,_)),
  Label1 = [_,_,_,Gain1|_],
        Gain1 >= Gain, !,
        Next is Start + 1,
        worse_coversets1(Next-Finish,Type,Gain,Rest), !.
worse_coversets1(Start-Finish,Type,Gain,[Start|Rest]):-
        Next is Start + 1,
        worse_coversets1(Next-Finish,Type,Gain,Rest), !.

update_coversets([],_,_,_).
update_coversets([Atom|Atoms],ClauseNum,Type,Label):-
  (retract('$aleph_global'(max_set,max_set(Type,Atom,_,_)))->
    true;
    true),
  asserta('$aleph_global'(max_set,max_set(Type,Atom,Label,ClauseNum))),
  update_coversets(Atoms,ClauseNum,Type,Label), !.

rm_intervals([],I,I).
rm_intervals([I1|I],Intervals,Result):-
  rm_interval(I1,Intervals,Intervals1),
  rm_intervals(I,Intervals1,Result), !.

rm_interval(_,[],[]).
rm_interval(I1,[Interval|Rest],Intervals):-
  interval_intersection(I1,Interval,I2), !,
  interval_subtract(Interval,I2,I3),
  rm_interval(I1,Rest,I4),
  append(I3, I4, Intervals).
rm_interval(I1,[Interval|Rest],[Interval|Intervals]):-
  rm_interval(I1,Rest,Intervals).

% gen_sample(+Type,+N)
% select N random samples from the set of examples uncovered. Type is one of pos/neg
% if N = 0 returns first example in Set
% resamples the same example R times where aleph5_set_setting(resample,R)
gen_sample(Type,0):-
  !,
  '$aleph_global'(atoms_left,atoms_left(Type,[ExampleNum-_|_])),
  retractall('$aleph_global'(example_selected,example_selected(_,_))),
  p1_message('select example'), p_message(ExampleNum),
  (aleph5_setting(resample,Resample) -> true; Resample = 1),
  gen_sample(Resample,Type,ExampleNum).
gen_sample(Type,SampleSize):-
  '$aleph_global'(atoms_left,atoms_left(Type,Intervals)),
  interval_count(Intervals,AtomsLeft),
  N is min(AtomsLeft,SampleSize),
  assertz('$aleph_local'(sample_num,0)),
  retractall('$aleph_global'(example_selected,example_selected(_,_))),
  (aleph5_setting(resample,Resample) -> true; Resample = 1),
  repeat,
  '$aleph_local'(sample_num,S1),
  S is S1 + 1,
  (S =< N ->
    get_random(AtomsLeft,INum),
    select_example(INum,0,Intervals,ExampleNum),
    \+('$aleph_global'(example_selected,
        example_selected(Type,ExampleNum))),
    p1_message('select example'), p_message(ExampleNum),
    retract('$aleph_local'(sample_num,S1)),
    assertz('$aleph_local'(sample_num,S)),
    gen_sample(Resample,Type,ExampleNum),
    fail;
    retract('$aleph_local'(sample_num,S1))), !.

gen_sample(0,_,_):- !.
gen_sample(R,Type,ExampleNum):-
  assertz('$aleph_global'(example_selected,
      example_selected(Type,ExampleNum))),
  R1 is R - 1,
  gen_sample(R1,Type,ExampleNum).

select_example(Num,NumberSoFar,[Start-Finish|_],ExampleNum):-
  Num =< NumberSoFar + Finish - Start + 1, !,
  ExampleNum is Num - NumberSoFar + Start - 1.
select_example(Num,NumberSoFar,[Start-Finish|Rest],ExampleNum):-
  N1 is NumberSoFar + Finish - Start + 1,
  select_example(Num,N1,Rest,ExampleNum).

% get_random(+Last,-Num)
%   get a random integer between 1 and Last
get_random(Last,INum):-
  aleph_random(X),
  INum1 is integer(X*Last + 0.5),
  (INum1 = 0 ->
    INum = 1;
    (INum1 > Last ->
      INum = Last;
      INum = INum1
    )
  ).

% get_rrandom(+Last,-Num)
%   get a random floating point number between 1 and Last
get_rrandom(Last,Num):-
  aleph_random(X),
  Num is X*Last.

% distrib(+Interval,+Prob,-Distrib)
%  generate discrete distribution Distrib
%  by assigning all elements in Interval the probability Prob
distrib(X-Y,_,[]):-  X > Y, !.
distrib(X-Y,P,[P-X|D]):-
  X1 is X + 1,
  distrib(X1-Y,P,D).

% draw_element(+D,-E)
%  draw element E using distribution D
%  D is a list specifying the probability of each element E
%    in the form p1-e1, p2-e2, ... ,pn-en
%           proportions pi are normalised to add to 1
draw_element(D,E):-
  normalise_distribution(D,Distr),
  aleph_random(X),
  draw_element(Distr,0,X,E).

draw_element([P1-E1|T],CumProb,X,E):-
  CumProb1 is CumProb + P1,
  (X =< CumProb1 -> E = E1;
    draw_element(T,CumProb1,X,E)).

normalise_distribution(D,Distr):-
  key_sum(D,Sum),
  (0.0 is float(Sum) -> Distr = D;
    normalise_distribution(D,Sum,D1),
    keysort(D1,Distr)).

key_sum([],0.0).
key_sum([K1-_|T],Sum):-
  key_sum(T,S1),
  Sum is float(K1 + S1).

normalise_distribution([],_,[]).
normalise_distribution([K1-X1|T],Sum,[K2-X1|T1]):-
  K2 is K1/Sum,
  normalise_distribution(T,Sum,T1).

% random_select(-Num,+List1,-List2)
%       randomly remove an element Num from List1 to give List2
random_select(X,[X],[]):- !.
random_select(X,L,Left):-
        length(L,N),
        N > 0,
        get_random(N,I),
        nth0(I,L,X,Left).

% random_nselect(+Num,+List1,-List2)
%       randomly remove Num elements from List1 to give List2
random_nselect(0,_,[]):- !.
random_nselect(_,[],[]):- !.
random_nselect(N,List1,[X|List2]):-
        random_select(X,List1,Left),
        N1 is N - 1,
        random_nselect(N1,Left,List2).

% random_select_from_intervals(-Num,+IList)
%   randomly select an element from an interval list
random_select_from_intervals(N,IList):-
  interval_count(IList,L),
  get_random(L,X),
  interval_select(X,IList,N).


normal(Mean,Sigma,X):-
  std_normal(X1),
  X is Mean + Sigma*X1.

get_normal(0,_,_,[]):- !.
get_normal(N,Mean,Sigma,[X|Xs]):-
        N > 0,
        normal(Mean,Sigma,X),
        N1 is N - 1,
        get_normal(N1,Mean,Sigma,Xs).

% Polar method for generating random variates
% from a standard normal distribution.
% From A.M. Law and W.D. Kelton, "Simulation Modeling and Analysis",
%   McGraw-Hill,2000
std_normal(X):-
  aleph_random(U1),
  aleph_random(U2),
  V1 is 2*U1 - 1,
  V2 is 2*U2 - 1,
  W is V1^2 + V2^2,
  (W > 1 -> std_normal(X);
    Y is sqrt((-2.0*log(W))/W),
    X is V1*Y).

% Approximate method for computing the chi-square value
% given the d.f. and probability (to the right). Uses
% a normal approximation and Monte-Carlo simulation.
% The normal approximation used is the one proposed by
% E.B. Wilson and M.M. Hilferty (1931). "The distribution of chi-square"
%    PNAS, 17, 684.
% Monte-Carlo simulation uses 1000 trials.
chi_square(DF,Prob,ChisqVal):-
  DF > 0,
  Mean is 1 - 2/(9*DF),
  Sigma is sqrt(2/(9*DF)),
  NTrials is 1000,
  get_normal(NTrials,Mean,Sigma,X),
  sort(X,Z),
  ProbLeft is 1.0 - Prob,
  Index is integer(ProbLeft*NTrials),
  (Index > NTrials ->
    nth0(NTrials,Z,Val,_);
    nth0(Index,Z,Val,_)),
  ChisqVal is DF*(Val^3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% L A B E L S   A N D    E V A L F N S
%

label_create(Clause,Label):-
        '$aleph_global'(last_example,last_example(pos,Last1)),
  Type1 = pos,
  (aleph5_setting(evalfn,posonly) ->
          '$aleph_global'(last_example,last_example(rand,Last2)),
    Type2 = rand;
          '$aleph_global'(last_example,last_example(neg,Last2)),
    Type2 = neg),
  label_create(Clause,Type1,[1-Last1],Type2,[1-Last2],Label).

label_create(Type,Clause,Label):-
        '$aleph_global'(last_example,last_example(Type,Last)),
  label_create(Clause,Type,[1-Last],Label).

label_create(Clause,Type1,Set1,Type2,Set2,Label):-
        split_clause(Clause,Head,Body),
  nlits((Head,Body),Length),
        assertz('$aleph_search'(pclause,pclause(Head,Body))),
  aleph5_setting(depth,Depth),
  aleph5_setting(prooftime,Time),
  aleph5_setting(proof_strategy,Proof),
        prove(Depth/Time/Proof,Type1,(Head:-Body),Set1,Cover1,_),
        prove(Depth/Time/Proof,Type2,(Head:-Body),Set2,Cover2,_),
  retractall('$aleph_search'(pclause,_)),
        assemble_label(Cover1,Cover2,Length,Label), !.

label_create(Clause,Type,Set,Label):-
        split_clause(Clause,Head,Body),
        assertz('$aleph_search'(pclause,pclause(Head,Body))),
  aleph5_setting(depth,Depth),
  aleph5_setting(prooftime,Time),
  aleph5_setting(proof_strategy,Proof),
        prove(Depth/Time/Proof,Type,(Head:-Body),Set,Cover,_),
  retractall('$aleph_search'(pclause,_)),
  (Type = pos ->
          assemble_label(Cover,unknown,unknown,Label);
          assemble_label(unknown,Cover,unknown,Label)).

label_pcover(Label,P):-
  extract_cover(pos,Label,P).
label_ncover(Label,N):-
  extract_cover(neg,Label,N).

label_union([],Label,Label):- !.
label_union(Label,[],Label):- !.
label_union(Label1,Label2,Label):-
        extract_cover(pos,Label1,Pos1),
        extract_cover(pos,Label2,Pos2),
        extract_cover(neg,Label1,Neg1),
        extract_cover(neg,Label2,Neg2),
        extract_length(Label1,L1),
        extract_length(Label2,L2),
        update_list(Pos2,Pos1,Pos),
        update_list(Neg2,Neg1,Neg),
        Length is L1 + L2,
        list_to_intervals(Pos,PCover),
        list_to_intervals(Neg,NCover),
        assemble_label(PCover,NCover,Length,Label).

label_print_examples(Type,Label):-
  extract_cover(Type,Label,C),
  examples(Type,C).

label_print_eval([]):- !.
label_print_eval(Label):-
  Eval = coverage,
  evalfn(Eval,Label,Val),
  print_eval(Eval,Val).

print_eval(Evalfn,Val):-
  evalfn_name(Evalfn,Name),
  p1_message(Name), p_message(Val).

eval_rule(Stream, 0,Label):-
  '$aleph_global'(hypothesis,hypothesis(_,Clause,_,_)), !,
  label_create(Clause,Label),
  p_message(Stream, 'Rule 0'),
  pp_dclause(Stream, Clause),
  extract_count(pos,Label,PC),
  extract_count(neg,Label,NC),
  extract_length(Label,L),
  label_print_eval([PC,NC,L]),
  nl.
eval_rule(Stream, ClauseNum, Label):-
  integer(ClauseNum),
  ClauseNum > 0,
  '$aleph_global'(theory,theory(ClauseNum,_,Clause,_,_)),
  !,
  label_create(Clause,Label),
  extract_count(pos,Label,PC),
  extract_count(neg,Label,NC),
  format(atom(RuleTag), 'Rule ~w', [ClauseNum]),
  (
    aleph5_setting(evalfn,posonly)
  ->
    format(atom(CoverTag), 'Pos cover = ~w Rand cover = ~w',[PC, NC])
  ;
    format(atom(CoverTag), 'Pos cover = ~w Neg cover = ~w', [PC, NC])
  ),
  p1_message(Stream, RuleTag),
  p_message(Stream, CoverTag),
  pp_dclause(Stream, Clause),
  aleph5_setting(verbosity,V),
  (
    V >= 2
  ->
    p_message(Stream, 'positive examples covered'),
    label_print_examples(pos,Label),
    p_message(Stream, 'negative examples covered'),
    label_print_examples(neg,Label)
  ;
    true
  ),
  nl(Stream).
eval_rule(_Stream, _, _).


evalfn(Label,Val):-
  (aleph5_setting(evalfn,Eval)->true;Eval=coverage),
  evalfn(Eval,Label,Val).

evalfn_name(compression,'compression').
evalfn_name(coverage,'pos-neg').
evalfn_name(accuracy,'accuracy').
evalfn_name(wracc,'novelty').
evalfn_name(laplace,'laplace estimate').
evalfn_name(pbayes,'pseudo-bayes estimate').
evalfn_name(auto_m,'m estimate').
evalfn_name(mestimate,'m estimate').
evalfn_name(mse,'mse').
evalfn_name(posonly,'posonly bayes estimate').
evalfn_name(entropy,'entropy').
evalfn_name(gini,'gini value').
evalfn_name(sd,'standard deviation').
evalfn_name(user,'user defined cost').

evalfn(compression,[P,N,L|_],Val):-
  (P = -1e10 -> Val is -1e10;
          Val is P - N - L + 1), !.
evalfn(coverage,[P,N,_|_],Val):-
  (P = -1e10 -> Val is -1e10;
    Val is P - N), !.
evalfn(laplace,[P,N|_],Val):-
  (P = -1e10 -> Val is 0.5;
    Val is (P + 1) / (P + N + 2)), !.
% the evaluation function below is due to Steve Moyle's implementation
% of the work by Lavrac, Flach and Zupan
evalfn(wracc,[P,N|_],Val):-
  ('$aleph_search'(clauseprior,Total-[P1-pos,_]) ->
    Val is P/Total - (P1/Total)*((P+N)/Total);
    Val is -0.25), !.
evalfn(entropy,[P,N|_],Val):-
  (P = -1e10 ->  Val is 1.0;
    ((P is 0); (N is 0) -> Val is 0.0;
      Total is P + N,
      P1 is P/Total,
      Q1 is 1-P1,
      Val is -(P1*log(P1) + Q1*log(Q1))/log(2)
    )
  ), !.
evalfn(gini,[P,N|_],Val):-
  (P = -1e10 -> Val is 1.0;
    Total is P + N,
    P1 is P/Total,
    Val is 2*P1*(1-P1)), !.
evalfn(accuracy,[P,N|_],Val):-
  (P = -1e10 -> Val is 0.5;
    Val is P / (P + N)), !.
% the evaluation functions below are due to James Cussens
evalfn(pbayes,[P,N|_],Val):-
        (P = -1e10 -> Val is 0.5;
                Acc is P/(P+N),
                aleph5_setting(prior,PriorD),
    normalise_distribution(PriorD,NPriorD),
    memberchk(Prior-pos,NPriorD),
                (0 is Prior-Acc ->
                    Val=Prior;
                K is (Acc*(1 - Acc)) / ((Prior-Acc)^2 ),
                Val is (P + K*Prior) / (P + N + K))), !.
evalfn(posonly,[P,0,L|_],Val):-
        '$aleph_global'(size,size(rand,RSize)),
        Val is log(P) + log(RSize+2.0) - (L+1)/P, !.
evalfn(auto_m,[P,N|_],Val):-
        (P = -1e10 -> Val is 0.5;
                Cover is P + N,
                aleph5_setting(prior,PriorD),
    normalise_distribution(PriorD,NPriorD),
    memberchk(Prior-pos,NPriorD),
                K is sqrt(Cover),
                Val is (P + K*Prior) / (Cover+K)), !.
evalfn(mestimate,[P,N|_],Val):-
        (P = -1e10 -> Val is 0.5;
                Cover is P + N,
                aleph5_setting(prior,PriorD),
    normalise_distribution(PriorD,NPriorD),
    memberchk(Prior-pos,NPriorD),
                (aleph5_setting(m,M) -> K = M; K is sqrt(Cover)),
                Val is (P + K*Prior) / (Cover+K)), !.
evalfn(_,_,X):- X is -1e10.


assemble_label(P,N,L,[P,N,L]).

extract_cover(pos,[P,_,_],P1):-
  intervals_to_list(P,P1), !.
extract_cover(neg,[_,N,_],N1):-
  intervals_to_list(N,N1),!.
extract_cover(_,[]).

extract_count(pos,[P,_,_],P1):-
  interval_count(P,P1), !.
extract_count(neg,[_,N,_],N1):-
  interval_count(N,N1), !.
extract_count(neg,_,0).


extract_pos([P|_],P).
extract_neg([_,N|_],N).
extract_length([_,_,L|_],L).

get_start_label(_,[0,0,0,F]):-
  (aleph5_setting(interactive,true); aleph5_setting(search,ic)), !,
  F is -1e10.
get_start_label(user,[1,0,2,F]):- !, F is -1e10.
get_start_label(entropy,[1,0,2,-0.5]):- !.
get_start_label(gini,[1,0,2,-0.5]):- !.
get_start_label(wracc,[1,0,2,-0.25]):- !.
get_start_label(Evalfn,[1,0,2,Val]):-
  evalfn(Evalfn,[1,0,2],Val).


%! read_all(+Base:atom) is det.
% Read ILP files with the given name. Background knowledge and
% examples all have the same base name.
%
% @arg Base An atomic base name.

read_all(Base):-
  read_all(Base, Base, Base).

%! read_all(+BackgroundBase:atom, +ExamplesBase:atom) is det.
% Read ILP files with the given name. The examples files have
% the same base name.
%
% @arg BackgroundBase An atomic base name.
% @arg ExamplesBase An atomic base names.

read_all(BackgroundBase, ExamplesBase):-
  read_all(BackgroundBase, ExamplesBase, ExamplesBase).

%! read_all(
%!   +BackgroundBase:atom,
%!   +PositiveExamplesBase:atom,
%!   +NegativeExamplesBase:atom
%! ) is det.
% Read ILP files with the given names.
%
% @arg BackgroundBase An atomic base name.
% @arg PositiveExamplesBase An atomic base name.
% @arg NegativeExamplesBase An atomic base name.
% @tbd Fix the lacking determinism of this predicate
%       (now enforced by the green cut).

read_all(BackgroundBase, PositiveExamplesBase, NegativeExamplesBase):-
  clean_up,
  reset,
  read_background(BackgroundBase),
  read_examples(PositiveExamplesBase, NegativeExamplesBase),
  record_targetpred,
  check_recursive_calls,
  check_prune_defs,
  check_user_search,
  check_posonly,
  check_auto_refine,
  check_abducibles,
  !.

%! read_background(+BackgroundBase:atom) is det.
% Reads background knowledge from the background file with the
% given base name.
%
% @arg BackgroundBase The atomic name of a background file's base.

read_background(BackgroundBase):-
  absolute_file_name(
    data(BackgroundBase),
    BackgroundFile,
    [access(read), file_type(background_knowledge)]
  ),
  % The background file is allowed to include multiple occurrences of
  % singleton variables (of the form
  % '_' + <uppercase-letter> + <lowercase-letter>*).
  % We therefore disable the style checks on singletons while consulting
  % the background knowledge file.
  style_check(-singleton),
  consult(BackgroundFile),
  style_check(+singleton),
  broadcast(background(loaded)).

%! read_examples(
%!   +PositiveExamplesBase:atom,
%!   +NegativeExamplesBase:atom
%! ) is det.
% Reads examples from files that have the given base names.
%
% @arg PositiveExamplesBase The atomic base name of a positive
%        examples file.
% @arg NegativeExamplesBase The atomic base name of a negative
%        examples file.

read_examples(PositiveExamplesBase, NegativeExamplesBase):-
  read_positive_examples(PositiveExamplesBase),
  read_negative_examples(NegativeExamplesBase),
  '$aleph_global'(size,size(pos,P)),
  '$aleph_global'(size,size(neg,N)),
  set_lazy_recalls,
  (
    aleph5_setting(prior, _)
  ->
    true
  ;
    normalise_distribution([P-pos, N-neg], Prior),
    aleph5_set_setting(prior, Prior)
  ),
  reset_counts,
  asserta('$aleph_global'(last_clause, last_clause(0))),
  broadcast(examples(loaded)).

% The files with positive training examples have been set internally.
read_positive_examples(_PositiveExamplesBase):-
  aleph5_setting(train_pos, PositiveExamplesFiles),
  PositiveExamplesFiles \== '',
  !,
  read_examples_files(pos, PositiveExamplesFiles, _Dummy1).
read_positive_examples(PositiveExamplesBase):-
  read_examples_files(pos, [PositiveExamplesBase], [PositiveExamplesFile]),
  aleph5_set_setting(train_pos, PositiveExamplesFile).

% The files with negative training examples have been set internally.
read_negative_examples(_NegativeExamplesBase):-
  aleph5_setting(train_neg, NegativeExamplesFiles),
  NegativeExamplesFiles \== '',
  !,
  read_examples_files(neg, NegativeExamplesFiles, _Dummy2).
read_negative_examples(NegativeExamplesBase):-
  read_examples_files(neg, [NegativeExamplesBase], [NegativeExamplesFile]),
  aleph5_set_setting(train_neg, NegativeExamplesFile).

%! read_examples_files(
%!   +Type:atom,
%!   +ExamplesBases:list(atom),
%!   -ExamplesFiles:list(atom)
%! ) is det.
% Reads examples from the given bases and returns their absolute files.
% The exampels are loaded as being of the given type.
%
% @arg Type The atomic type of the examples file. Either
%        'pos' or 'neg'.
% @arg ExamplesBases Either an atomic base or a list of such bases.
% @arg ExamplesFiles Either an atomic abolsute file name or a list
%        of such file names.

read_examples_files(Type, ExamplesBases, ExamplesFiles):-
  clean_up_examples(Type),
  asserta('$aleph_global'(size, size(Type, 0))),
  maplist(read_examples_from_file(Type), ExamplesBases, ExamplesFiles),
  '$aleph_global'(size, size(Type, N)),
  (
    N > 0
  ->
    Ex = [1-N]
  ;
    Ex = []
  ),
  asserta('$aleph_global'(atoms, atoms(Type, Ex))),
  asserta('$aleph_global'(atoms_left, atoms_left(Type, Ex))),
  asserta('$aleph_global'(last_example, last_example(Type, N))).

%! read_examples_from_file(Type, ExamplesBase, ExamplesFile) is det.
% Reads the examples file with the given base name that is of the given type.
%
% @arg Type The atomic type of examples file. This is either
%        'pos' or 'neg'.
% @arg ExamplesBase The atomic base of the examples file.
% @arg ExamplesFile The atomic absolute examples file name.

read_examples_from_file(Type, ExamplesBase, ExamplesFile):-
  absolute_file_name(data(ExamplesBase), ExamplesFile, [access(read), file_type(Type)]),
  (
    aleph_open(ExamplesFile, read, Stream)
  ->
    stream_message('Consulting ~w from ~w.\n', [Type, ExamplesFile])
  ;
    stream_message('Cannot open ~w.\n', [ExamplesFile]),
    fail
  ),
  repeat,
  read(Stream, Example),
  (
    Example = end_of_file
  ->
    close(Stream)
  ;
    record_example(nocheck, Type, Example, _),
    fail
  ),
  !.
read_examples_from_file(_Type, _ExamplesBase, '?').



% MISC. DEFINITIONS

execute(C):-
  shell(C),
  !.
execute(_).

% store critical values of current search state
store(searchstate):-
  !,
  retractall('$aleph_global'(save,save(searchstate,_))),
  ('$aleph_global'(atoms_left,atoms_left(pos,PosLeft)) ->
    asserta('$aleph_global'(save,
        save(searchstate,atoms_left(pos,PosLeft))));
    true),
  ('$aleph_global'(atoms_left,atoms_left(neg,NegLeft)) ->
    asserta('$aleph_global'(save,
        save(searchstate,atoms_left(neg,NegLeft))));
    true),
  ('$aleph_global'(size,size(pos,PSize)) ->
    asserta('$aleph_global'(save,
        save(searchstate,size(pos,PSize))));
    true),
  ('$aleph_global'(size,size(neg,NSize)) ->
    asserta('$aleph_global'(save,
        save(searchstate,size(neg,NSize))));
    true),
  (
    setting(noise, Noise)
  ->
    asserta('$aleph_global'(save, save(searchstate,aleph5_set_setting(noise,Noise))))
  ;
    true
  ),
  (
    setting(minacc, MinAcc)
  ->
    asserta('$aleph_global'(save, save(searchstate,aleph5_set_setting(minacc,MinAcc))))
  ;
    true
  ).

% Store the current bottom clause.
store(bottom):-
  !,
  ('$aleph_global'(store_bottom,aleph5_set_setting(store_bottom,true)) ->
    store_bottom;
    true).
% Store the current value of a setting.
store(Parameter):-
  nonvar(Parameter),
  setting(Parameter, Value),
  retractall('$aleph_global'(save,save(Parameter,_))),
  asserta('$aleph_global'(save,save(Parameter,Value))).

% store values of a list of parameters
store_values([]).
store_values([Parameter|T]):-
  store(Parameter),
  store_values(T).

% store all relevant info related to current bottom
%  details are stored in 5 idbs:
%  1. bottom: points to 2 other idbs sat_X_n and lits_X_N
%  2. sat_X_N: where X is the type of the current example and N the number
%    this contains misc stuff recorded by sat/2 for use by reduce/1
%  3. lits_X_N: contains the lits in bottom
%  4. ovars_X_N: contains output vars of lits in bottom
%  5. ivars_X_N: contains input vars of lits in bottom
store_bottom:-
  bottom_key(Num,Type,Key,true),
  asserta('$aleph_sat'(stored,stored(Num,Type,Key))),
  '$aleph_sat'(lastterm,LastTerm),
  asserta('$aleph_sat'(lasterm,Key,LastTerm)),
  '$aleph_sat'(lastvar,LastVar),
  asserta('$aleph_sat'(lastvar,Key,LastVar)),
  '$aleph_sat'(botsize,BotSize),
  asserta('$aleph_sat'(botsize,Key,BotSize)),
  '$aleph_sat'(lastlit,LastLit),
  asserta('$aleph_sat'(lastlit,Key,LastLit)),
  '$aleph_sat'(hovars,HOVars),
  asserta('$aleph_sat'(hovars,Key,HOVars)),
  '$aleph_sat'(hivars,HIVars),
  asserta('$aleph_sat'(hivars,Key,HIVars)),
  '$aleph_sat'(eq,Eq),
  asserta('$aleph_sat'(eq,Key,Eq)),
  '$aleph_sat_ivars'(Lit,IVars),
  asserta('$aleph_sat_ivars'(Lit,Key,IVars)),
  '$aleph_sat_ovars'(Lit,OVars),
  asserta('$aleph_sat_ovars'(Lit,Key,OVars)),
  '$aleph_sat_litinfo'(Lit,Depth,Atom,I,O,D),
  asserta('$aleph_sat_litinfo'(Lit,Key,Depth,Atom,I,O,D)),
  fail.
store_bottom.


reinstate(searchstate):-
  !,
  retractall('$aleph_global'(atoms_left,atoms_left(_,_))),
  retractall('$aleph_global'(size,size(_,_))),
  ('$aleph_global'(save,save(searchstate,atoms_left(pos,PosLeft))) ->
    asserta('$aleph_global'(atoms_left,atoms_left(pos,PosLeft)));
    true),
  ('$aleph_global'(save,save(searchstate,atoms_left(neg,NegLeft))) ->
    asserta('$aleph_global'(atoms_left,atoms_left(neg,NegLeft)));
    true),
  ('$aleph_global'(save,save(searchstate,size(pos,PSize))) ->
    asserta('$aleph_global'(size,size(pos,PSize)));
    true),
  ('$aleph_global'(save,save(searchstate,size(neg,NSize))) ->
    asserta('$aleph_global'(size,size(neg,NSize)));
    true),
  ('$aleph_global'(save,save(searchstate,aleph5_set_setting(noise,Noise))) ->
    aleph5_set_setting(noise,Noise);
    true),
  ('$aleph_global'(save,save(searchstate,aleph5_set_setting(minacc,MinAcc))) ->
    aleph5_set_setting(minacc,MinAcc);
    true),
  retractall('$aleph_global'(save,save(searchstate,_))).
reinstate(Parameter):-
  retract('$aleph_global'(save,save(Parameter,Value))), !,
  (Value = unknown -> aleph5_restore_setting(Parameter); aleph5_set_setting(Parameter,Value)).
reinstate(_).

% reinstate list of values of parameters
reinstate_values([]).
reinstate_values([Parameter|T]):-
  reinstate(Parameter),
  reinstate_values(T).

% reinstate all saved values
reinstate_values:-
  reinstate_file_streams,
  '$aleph_global'(save,save(_,_)),
  repeat,
  retract('$aleph_global'(save,save(Parameter,Value))),
  (Value = unknown -> aleph5_restore_setting(Parameter) ; aleph5_set_setting(Parameter,Value)),
  \+('$aleph_global'(save,save(_,_))),
  !.
reinstate_values.

% @tbd ?
reinstate_file_streams:-
  aleph5_setting(recordfile, File),
  aleph5_set_setting(recordfile, File),
  fail.
reinstate_file_streams:-
  aleph5_setting(goodfile, File),
  aleph5_set_setting(goodfile, File),
  fail.
reinstate_file_streams.

% bottom_key(?N,?T,-Key,-Flag)
%  returns key that indexes bottom clause info for example N of type T
%  Flag is one of "true" or "false" depending on whether bottom
%  requires storing
bottom_key(N,T,Key,Flag):-
  ((var(N),var(T)) ->
    '$aleph_sat'(example,example(N,T));
    true),
  (aleph5_setting(store_bottom,true) ->
    ('$aleph_sat'(stored,stored(N,T,Key)) ->
      Flag = false;
      format(atom(Key), '~w_~w', [T, N]),
      Flag = true
    );
    Key = false,
    Flag = false).

% @tbd Support for streams.
%aleph5_setting(Setting, Value):-
%  setting_property(Setting, type(stream)),
%  !,
%  setting(Setting, Value),
%  is_stream(Value).
% @tbd Support for files.
%aleph5_setting(Setting, Value):-
%  setting_property(Setting, type(file)),
%  !,
%  setting(Setting, Value),
%  exists_file(Value).
aleph5_setting(Setting, Value):-
  nonvar(Setting),
  setting(Setting, Value1),
  !,
  Value = Value1.
aleph5_setting(Setting, DefaultValue):-
  setting_property(Setting, default(DefaultValue)).

aleph5_restore_setting(Setting):-
  nonvar(Setting),
  setting(Setting, OldValue),
  !,
  rm_special_consideration(Setting, OldValue),
  restore_setting(Setting).

determinations(Pred1,Pred2):-
        '$aleph_global'(determination,determination(Pred1,Pred2)).

determination(Pred1,Pred2):-
  nonvar(Pred1),
  '$aleph_global'(determination,determination(Pred1,Pred2)), !.
determination(Pred1,Pred2):-
  aleph5_restore_setting(autorefine),
  assertz('$aleph_global'(determination,determination(Pred1,Pred2))),
  (nonvar(Pred1) ->
    update_backpreds(Pred1);
    true).

abducible(Name/Arity):-
  assertz('$aleph_global'(abducible,abducible(Name/Arity))).

commutative(Name/Arity):-
  assertz('$aleph_global'(commutative,commutative(Name/Arity))).

symmetric(Name/Arity):-
  assertz('$aleph_global'(symmetric,symmetric(Name/Arity))).

lazy_evaluate(Name/Arity):-
        assertz('$aleph_global'(lazy_evaluate,lazy_evaluate(Name/Arity))).

model(Name/Arity):-
        assertz('$aleph_global'(model,model(Name/Arity))).

positive_only(Name/Arity):-
  assertz('$aleph_global'(positive_only,positive_only(Name/Arity))).

mode(Recall,Pred):-
  modeh(Recall,Pred),
  modeb(Recall,Pred).

modes(N/A,Mode):-
        Mode = modeh(_,Pred),
        '$aleph_global'(modeh,Mode),
        functor(Pred,N,A).
modes(N/A,Mode):-
        Mode = modeb(_,Pred),
        '$aleph_global'(modeb,Mode),
        functor(Pred,N,A).

modeh(Recall,Pred):-
  ('$aleph_global'(mode,mode(Recall,Pred)) -> true;
    aleph5_restore_setting(autorefine),
    assertz('$aleph_global'(modeh,modeh(Recall,Pred))),
    assertz('$aleph_global'(mode,mode(Recall,Pred))),
          functor(Pred,Name,Arity),
          update_backpreds(Name/Arity)).

modeb(Recall,Pred):-
  ('$aleph_global'(modeb,modeb(Recall,Pred)) -> true;
    aleph5_restore_setting(autorefine),
    assertz('$aleph_global'(modeb,modeb(Recall,Pred))),
    ('$aleph_global'(mode,mode(Recall,Pred)) -> true;
      assertz('$aleph_global'(mode,mode(Recall,Pred))))).

% add_determinations(+PSym,Stratified)
% add determination declarations for a background predicate
% these are obtained from the determinations of the target predicate
% If Stratified is true then only stratified definitions are allowed
add_determinations(PSym,Stratified):-
  '$aleph_global'(targetpred,targetpred(Target)),
  determinations(Target,OtherPred),
  (Stratified = true -> OtherPred \= Target; true),
  determination(PSym,OtherPred),
  fail.
add_determinations(_,_).

% add_modes(+PSym)
% add modes declarations for a (new) predicate
% these are obtained from the modes of the target predicate
add_modes(Name/_):-
  '$aleph_global'(targetpred,targetpred(Target)),
  modes(Target,Mode),
  Mode =.. [ModeType,Recall,TargetMode],
  TargetMode =.. [_|Args],
  PredMode =.. [Name|Args],
  NewMode =.. [ModeType,Recall,PredMode],
  call(NewMode),
  fail.
add_modes(_).

feature(Id,Feature):-
  '$aleph_feature'(feature,feature(Id,_,_,Template,Body)),
  Feature = (Template:-Body).

gen_feature(Feature,Label,Class):-
  nonvar(Feature), !,
  (var(Id) -> gen_featurenum(Id); true),
  split_clause(Feature,Template,Body),
  assertz('$aleph_feature'(feature,feature(Id,Label,Class,Template,Body))).

show(Category):-
  current_stream(Stream),
  show(Stream, Category).

show(Stream, settings):-
  nl(Stream),
  p_message(Stream, settings),
  findall(
    P-V,
    (
      current_setting(P),
      setting(P, V)
    ),
    L
  ),
  sort(L,L1),
  member(Parameter-Value,L1),
  format(Stream, '\t\t\t\t\t\t\t\t~w=~w\n', [Parameter, Value]),
  fail.
show(Stream, determinations):-
  nl(Stream),
  p_message(Stream, determinations),
  show_global(determination,determination(_,_)).
show(Stream, modes):-
  nl(Stream),
  p_message(Stream, modes),
  show_global(mode,mode(_,_)).
show(Stream, modehs):-
  nl(Stream),
  p_message(Stream, modehs),
  show_global(modeh,modeh(_,_)).
show(Stream, modebs):-
  nl(Stream),
  p_message(Stream, modebs),
  show_global(modeb,modeb(_,_)).
show(Stream, sizes):-
  nl(Stream),
  p_message(Stream, sizes),
  show_global(size,size(_,_)).
show(Stream, bottom):-
  nl(Stream),
  p_message(Stream, 'bottom clause'),
  aleph5_setting(verbosity,V),
  V > 0,
  '$aleph_sat'(lastlit,Last),
  get_clause(1,Last,[],FlatClause),
  pp_dlist(Stream, FlatClause).
show(Stream, theory):-
  nl(Stream),
  p_message(Stream, theory),
  nl(Stream),
  '$aleph_global'(rules,rules(L)),
  reverse(L, L1),
  member(ClauseNum,L1),
  '$aleph_global'(theory,theory(ClauseNum,_,_,_,_)),
  eval_rule(Stream, ClauseNum, _),
  % pp_dclause(Clause),
  fail.
show(Stream, theory):-
  get_performance(Stream).
show(Stream, pos):-
  nl(Stream),
  p_message(Stream, positives),
  store(greedy),
  examples(pos,_),
  reinstate(greedy),
  fail.
show(Stream, posleft):-
  nl(Stream),
  p_message(Stream, 'positives left'),
  example(_,pos,Atom),
  \+(Atom),
  format(Stream, '~w.\n', [Atom]),
  fail.
show(Stream, neg):-
  nl(Stream),
  p_message(Stream, negatives),
  store(greedy),
  examples(neg,_),
  reinstate(greedy),
  fail.
show(Stream, rand):-
  nl(Stream),
  p_message(Stream, random),
  examples(rand,_),
  fail.
show(Stream, uspec):-
  nl(Stream),
  p_message(Stream, uspec),
  examples(uspec,_),
  fail.
show(Stream, gcws):-
  nl(Stream),
  p_message(Stream, 'gcws hypothesis'),
  '$aleph_search'(gcwshyp,hypothesis(_,C,_,_)),
  pp_dclause(C),
  fail.
show(Stream, abgen):-
  nl(Stream),
  p_message(Stream, 'abduced hypothesis'),
  '$aleph_search'(abgenhyp,hypothesis(_,AbGen,_,_)),
  member(C,AbGen),
  pp_dclause(C),
  fail.
show(_Stream, hypothesis):-
  aleph5_setting(portray_hypothesis,Pretty),
  aleph_portray(hypothesis,Pretty),
  fail.
show(_Stream, search):-
  aleph5_setting(portray_search,Pretty),
  aleph_portray(search,Pretty).
show(Stream, good):-
  aleph5_setting(good,true),
  nl(Stream),
  p_message(Stream, 'good clauses'),
  (
    aleph5_setting(minscore,FMin)
  ->
    true
  ;
    FMin is -1e10
  ),
  aleph5_setting(evalfn,Evalfn),
  '$aleph_good'(_,Label,Clause),
  Label = [_,_,_,F|_],
  F >= FMin,
  pp_dclause(Stream, Clause),
  show_stats(Evalfn,Label),
  fail.
show(Stream, good):-
  aleph5_setting(good,true),
  aleph5_setting(goodfile,File),
  aleph_open(File,read,ReadStream),
  (aleph5_setting(minscore,FMin) -> true; FMin is -1e10),
  aleph5_setting(evalfn,Evalfn),
  repeat,
  read(ReadStream,Fact),
  (
    Fact = '$aleph_good'(_,Label,Clause)
  ->
    Label = [_,_,_,F|_],
    F >= FMin,
    show_stats(Evalfn,Label),
    pp_dclause(Stream, Clause),
    fail
  ;
    close(ReadStream),
    !
  ).
show(Stream, features):-
  aleph5_setting(evalfn,Evalfn),
  (
    '$aleph_feature'(feature,_)
  ->
    true
  ;
    gen_features
  ),
  p_message(Stream, 'features from good clauses'),
  '$aleph_feature'(feature,feature(Id,Label,_,Head,Body)),
  show_stats(Evalfn,Label),
  pp_dclause(Stream, feature(Id,(Head:-Body))),
  fail.
show(Stream, constraints):-
  aleph5_setting(good,true),
  nl(Stream),
  p_message(Stream, constraints),
  aleph5_setting(noise,N),
  FMin is -N,
  '$aleph_good'(_,Label,Clause),
  split_clause(Clause,false,_),
  Label = [_,_,_,F],
  F >= FMin,
  pp_dclause(Stream, Clause),
  show_stats(coverage,Label),
  fail.
show(Stream, constraints):-
  show(Stream, false/0).
show(Stream, Name/Arity):-
  functor(Pred,Name,Arity),
  current_predicate(Name,Pred),
  nl(Stream),
  p1_message(Stream, definition),
  p_message(Stream, Name/Arity),
  clause(Pred,Body),
  \+(in(Body,'$aleph_search'(pclause,pclause(_,_)))),
  pp_dclause(Stream, (Pred:-Body)),
  fail.
show(_Stream, train_pos):-
  aleph5_setting(portray_examples,Pretty),
  aleph_portray(train_pos,Pretty).
show(_Stream, train_neg):-
  aleph5_setting(portray_examples,Pretty),
  aleph_portray(train_neg,Pretty).
show(_Stream, test_pos):-
  aleph5_setting(portray_examples,Pretty),
  aleph_portray(test_pos,Pretty).
show(_Stream, test_neg):-
  aleph5_setting(portray_examples,Pretty),
  aleph_portray(test_neg,Pretty).
show(_Stream, _Category).

settings:-
  current_stream(Stream),
  settings(Stream).

settings(Stream):-
  show(Stream, settings).

% examples(?Type,?List)
% show all examples numbers in List of Type
examples(Type,List):-
  aleph5_setting(portray_literals,Pretty),
  example(Num,Type,Atom),
  memberchk(Num,List),
  aleph_portray(Atom,Pretty),
  current_stream(Stream),
  format(Stream, '.\n', []),
  fail.
examples(_,_).

% bottom(-Clause)
%   returns current bottom clause
bottom(Clause):-
  '$aleph_sat'(lastlit,Last),
  get_clause(1,Last,[],ClauseList),
  list_to_clause(ClauseList,Clause).

% posleft(-List)
%  returns positive examples left to be covered
posleft(PList):-
  '$aleph_global'(atoms_left,atoms_left(pos,PosLeft)),
  intervals_to_list(PosLeft,PList).

% write_rules/0 due to Mark Reid
write_rules:-
  aleph5_setting(rulefile,File),
  write_rules(File), !.
write_rules.

write_features:-
  aleph5_setting(featurefile,File),
  write_features(File), !.
write_features.

write_rules(File):-
  aleph_open(File,write,Stream),
  set_output(Stream),
  '$aleph_global'(rules,rules(L)),
  reverse(L, L1),
  write_rule(L1),
  flush_output(Stream),
  set_output(user_output).

write_rule(Rules):-
  member(RuleId,Rules),
  '$aleph_global'(theory,theory(RuleId,_,Rule,_,_)),
  pp_dclause(Rule),
  fail.
write_rule(_).

write_features(File):-
  aleph_open(File,write,Stream),
  set_output(Stream),
  listing('$aleph_feature'/2),
  close(Stream),
  set_output(user_output).
write_features(_).


best_hypothesis(Head1,Body1,[P,N,L]):-
  '$aleph_search'(selected,selected([P,N,L|_],Clause,_,_)),
  split_clause(Clause,Head2,Body2), !,
  Head1 = Head2, Body1 = Body2.

hypothesis(Head1,Body1,Label):-
  '$aleph_search'(pclause,pclause(Head2,Body2)), !,
  Head1 = Head2, Body1 = Body2,
  get_hyp_label((Head2:-Body2),Label).
hypothesis(Head1,Body1,Label):-
        '$aleph_global'(hypothesis,hypothesis(_,Theory,_,_)),
  (Theory = [_|_] -> member(Clause,Theory);
    Theory = Clause),
  split_clause(Clause,Head2,Body2),
  Head1 = Head2, Body1 = Body2,
  get_hyp_label((Head2:-Body2),Label).

rdhyp(Stream):-
  retractall('$aleph_search'(pclause,_)),
  retractall('$aleph_search'(covers,_)),
  retractall('$aleph_search'(coversn,_)),
  read(Clause),
  add_hyp(Clause),
  nl(Stream),
  show(hypothesis).

addhyp:-
  '$aleph_global'(hypothesis,hypothesis(Label,Theory,PCover,NCover)),
  Theory = [_|_], !,
  add_theory(Label,Theory,PCover,NCover).
addhyp:-
  '$aleph_global'(hypothesis,hypothesis(Label,_,PCover,_)), !,
  rm_seeds,
  worse_coversets(PCover,pos,Label,Worse),
  (Worse = [] -> true;
    '$aleph_global'(last_clause,last_clause(NewClause)),
          update_coversets(Worse,NewClause,pos,Label)), !.
addhyp:-
  '$aleph_search'(selected,selected(Label,RClause,PCover,NCover)), !,
  add_hyp(Label,RClause,PCover,NCover),
  rm_seeds,
  worse_coversets(PCover,pos,Label,Worse),
  (Worse = [] -> true;
    '$aleph_global'(last_clause,last_clause(NewClause)),
          update_coversets(Worse,NewClause,pos,Label)), !.

% add bottom clause as hypothesis
%  provided minacc, noise and search constraints are met
%  otherwise the example saturated is added as hypothesis
add_bottom:-
  retractall('$aleph_search'(selected,selected(_,_,_,_))),
  bottom(Bottom),
  add_hyp(Bottom),
        '$aleph_global'(hypothesis,hypothesis(Label,Clause,_,_)),
  (clause_ok(Clause,Label) -> true;
    '$aleph_sat'(example,example(Num,Type)),
    example(Num,Type,Example),
    retract('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
    aleph5_setting(evalfn,Evalfn),
    complete_label(Evalfn,Example,[1,0,1],Label1),
    asserta('$aleph_global'(hypothesis,hypothesis(Label1,(Example:-true),[Num-Num],[])))).


% specialise a hypothesis by recursive construction of
% abnormality predicates
sphyp:-
  retractall('$aleph_search'(sphyp,hypothesis(_,_,_,_))),
  retractall('$aleph_search'(gcwshyp,hypothesis(_,_,_,_))),
        retract('$aleph_global'(hypothesis,
        hypothesis([P,N,L|T],Clause,PCover,NCover))),
        asserta('$aleph_search'(sphyp,hypothesis([P,N,L|T],Clause,PCover,NCover))),
        store(searchstate),
        gcws,
        retractall('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
        asserta('$aleph_global'(hypothesis,
      hypothesis([P,N,L|T],Clause,PCover,NCover))),
        reinstate(searchstate).

addgcws:-
        retract('$aleph_search'(gcwshyp,hypothesis(Label,C,P,N))), !,
  asserta('$aleph_search'(gcwshyp,hypothesis(Label,C,P,N))),
  addhyp,
  add_gcws.

rmhyp:-
        retract('$aleph_search'(pclause,pclause(Head,Body))),
        asserta('$aleph_local'(pclause,pclause(Head,Body))), !.
rmhyp:-
        retract('$aleph_global'(hypothesis,hypothesis(Label,Clause1,P,N))),
        asserta('$aleph_local'(hypothesis,hypothesis(Label,Clause1,P,N))), !.
rmhyp.


covers:-
        get_hyp(Hypothesis),
        label_create(Hypothesis,Label),
        extract_cover(pos,Label,P),
        examples(pos,P),
  length(P,PC),
  p1_message('examples covered'),
  p_message(PC),
  retractall('$aleph_search'(covers,_)),
  asserta('$aleph_search'(covers,covers(P,PC))).
coversn:-
        get_hyp(Hypothesis),
        label_create(Hypothesis,Label),
        extract_cover(neg,Label,N),
        examples(neg,N),
  length(N,NC),
  p1_message('examples covered'),
  p_message(NC),
  retractall('$aleph_search'(coversn,_)),
  asserta('$aleph_search'(coversn,coversn(N,NC))).

% covers(-Number)
%   as in covers/0, but first checks if being done
%   within a greedy search
covers(P):-
  get_hyp(Hypothesis),
  (aleph5_setting(greedy,true) ->
    '$aleph_global'(atoms,atoms_left(pos,Pos));
    '$aleph_global'(atoms,atoms(pos,Pos))),
  label_create(Hypothesis,pos,Pos,Label),
  retractall('$aleph_search'(covers,_)),
  extract_pos(Label,PCover),
  interval_count(PCover,P),
  asserta('$aleph_search'(covers,covers(PCover,P))).

% coversn(-Number)
%   as in coversn/0, but first checks if being done
%   within a greedy search
coversn(N):-
  get_hyp(Hypothesis),
  (aleph5_setting(greedy,true) ->
    '$aleph_global'(atoms_left,atoms_left(neg,Neg));
    '$aleph_global'(atoms_left,atoms(neg,Neg))),
  label_create(Hypothesis,neg,Neg,Label),
  retractall('$aleph_search'(coversn,_)),
  extract_neg(Label,NCover),
  interval_count(NCover,N),
  asserta('$aleph_search'(coversn,coverns(NCover,N))).

% covers(-List,-Number)
%   as in covers/1, but returns list of examples covered and their count
covers(PList,P):-
  get_hyp(Hypothesis),
  (aleph5_setting(greedy,true) ->
    '$aleph_global'(atoms,atoms_left(pos,Pos));
    '$aleph_global'(atoms,atoms(pos,Pos))),
  label_create(Hypothesis,pos,Pos,Label),
  retractall('$aleph_search'(covers,_)),
  extract_pos(Label,PCover),
  intervals_to_list(PCover,PList),
  length(PList,P),
  asserta('$aleph_search'(covers,covers(PCover,P))).

% coversn(-List,-Number)
%   as in coversn/1, but returns list of examples covered and their count
coversn(NList,N):-
  get_hyp(Hypothesis),
  (aleph5_setting(greedy,true) ->
    '$aleph_global'(atoms_left,atoms_left(neg,Neg));
    '$aleph_global'(atoms_left,atoms(neg,Neg))),
  label_create(Hypothesis,neg,Neg,Label),
  retractall('$aleph_search'(coversn,_)),
  extract_neg(Label,NCover),
  intervals_to_list(NCover,NList),
  length(NList,N),
  asserta('$aleph_search'(coversn,coverns(NCover,N))).

example_saturated(Example):-
  '$aleph_sat'(example,example(Num,Type)),
  example(Num,Type,Example).

reset:-
  clean_up,
  clear_cache,
  aleph_abolish('$aleph_global'/2),
  aleph_abolish(example/3),
  assert(example(0,uspec,false)),
	forall(
		current_setting(Setting),
    restore_setting(Setting)
	),
  !.

%! reset(+Base:atom) is det.
% Reset that also unloads the background knowledge from the last
% background knowledge file.

reset(Base):-
  absolute_file_name(data(Base), File, [file_type(background_knowledge)]),
  unload_file(File),
  % Then reset the rest.
  reset.

% Generic timing routine due to Mark Reid.
% Under cygwin, cputime cannot be trusted
% so walltime is used instead. To use cputime, set the body of this
% predicate to "Time is cputime".
%%statistics(walltime,[Time|_]).

stopwatch(Time):-
  Time is cputime.

wallclock(Time):-
  statistics(real_time,[Time|_]).

time(P,N,[Mean,Sd]):-
        time_loop(N,P,Times),
  mean(Times,Mean),
  sd(Times,Sd).

test(F,Flag,N,T):-
  retractall('$aleph_local'(covered,_)),
  retractall('$aleph_local'(total,_)),
  asserta('$aleph_local'(covered,0)),
  asserta('$aleph_local'(total,0)),
  (F = [_|_] ->
    test_files(F,Flag);
    test_file(F,Flag)
  ),
  retract('$aleph_local'(covered,N)),
  retract('$aleph_local'(total,T)).

test_files([],_).
test_files([File|Files],Flag):-
  test_file(File,Flag),
  test_files(Files,Flag).

test_file('?',_):- !.
test_file(File,Flag):-
  aleph5_setting(portray_examples,Pretty),
  aleph_open(File,read,Stream), !,
  repeat,
  read(Stream,Example),
  (Example = end_of_file -> close(Stream);
    retract('$aleph_local'(total,T0)),
    T1 is T0 + 1,
    asserta('$aleph_local'(total,T1)),
    (once(depth_bound_call(Example)) ->
      (Flag = show ->
        p1_message(covered),
        aleph_portray(Example,Pretty),
        nl;
        true);
      (Flag = show ->
        p1_message('not covered'),
        aleph_portray(Example,Pretty),
        nl;
        true),
      fail),
    retract('$aleph_local'(covered,N0)),
    N1 is N0 + 1,
    asserta('$aleph_local'(covered,N1)),
    fail),
  !.
test_file(File,_):-
  p1_message('cannot open'), p_message(File).

in(false,_):-
  !,
  fail.
in(bottom,Lit):-
  !,
        '$aleph_sat'(lastlit,Last),
        get_clause(1,Last,[],FlatClause),
  member(Lit,FlatClause).
in((Head:-true),Head):- !.
in((Head:-Body),L):-
  !,
  in((Head,Body),L).
in((L1,_),L1).
in((_,R),L):-
  !,
  in(R,L).
in(L,L).

in((L1,L),L1,L).
in((L1,L),L2,(L1,Rest)):-
  !,
  in(L,L2,Rest).
in(L,L,true).

% draw a random number from a distribution
random(X,normal(Mean,Sigma)):-
  var(X), !,
  normal(Mean,Sigma,X).
random(X,normal(_,_)):-
  !,
  number(X).
  % X >= Mean - 3*Sigma,
  % X =< Mean + 3*Sigma.
random(X,Distr):-
  Distr = [_|_],
  var(X), !,
        draw_element(Distr,X1),
  X = X1.
random(X,Distr):-
  Distr = [_|_],
  nonvar(X), !,
        member(Prob-X,Distr),
  Prob > 0.0.

mean(L,M):-
  sum(L,Sum),
  length(L,N),
  M is Sum/N.

sd(L,Sd):-
  length(L,N),
  (N = 1 -> Sd = 0.0;
    sum(L,Sum),
    sumsq(L,SumSq),
    Sd is sqrt(SumSq/(N-1) - (Sum*Sum)/(N*(N-1)))).

sum([],0).
sum([X|T],S):-
  sum(T,S1),
  S is X + S1.

sumsq([],0).
sumsq([X|T],S):-
  sumsq(T,S1),
  S is X*X + S1.

% auxilliary definitions  for some of the above

check_legal(int(L)-int(U),X):-
  !,
  number(L,IL),
  number(U,IU),
  number(X,IX),
  IX >= IL,
  IX =< IU.
check_legal(float(L)-float(U),X):-
  !,
  number(L,FL),
  number(U,FU),
  number(X,FX),
  FX >= FL,
  FX =< FU.
check_legal([H|T],X):-
  !,
  memberchk(X,[H|T]).
check_legal(read(filename),X):-
  X \= '?',
  !,
  exists_file(X).
check_legal(_,_).

number(+1e10,Inf):-
  Inf is 1e10, !.
number(-1e10,MInf):-
  MInf is -1e10, !.
number(X,Y):-
  Y is X, !.

% the following needed for compatibility with P-Progol
special_consideration(search,ida):-
  aleph5_set_setting(search,bf), aleph5_set_setting(evalfn,coverage), !.
special_consideration(search,compression):-
  aleph5_set_setting(search,heuristic), aleph5_set_setting(evalfn,compression), !.
special_consideration(search,posonly):-
  aleph5_set_setting(search,heuristic), aleph5_set_setting(evalfn,posonly), !.
special_consideration(search,user):-
  aleph5_set_setting(search,heuristic), aleph5_set_setting(evalfn,user), !.

special_consideration(refine,Refine):-
  aleph5_set_setting(refineop,Refine), !.
special_consideration(refineop,auto):-
  gen_auto_refine, !.

special_consideration(portray_literals,true):-
  aleph5_set_setting(print,1), !.

special_consideration(record,true):-
  aleph5_restore_setting(recordfile_stream),
  (
    aleph5_setting(recordfile, File),
    exists_file(File)
  ->
    aleph_open(File, append, Stream),
    aleph5_set_setting(recordfile_stream, Stream)
  ;
    true
  ),
  !.
special_consideration(record,false):-
  aleph5_restore_setting(recordfile_stream), !.
special_consideration(recordfile, File):-
  aleph5_restore_setting(recordfile_stream),
  (
    aleph5_setting(record, true)
  ->
    aleph_open(File, append, Stream),
    aleph5_set_setting(recordfile_stream,Stream)
  ;
    true
  ),
  !.
special_consideration(good,true):-
  aleph5_restore_setting(goodfile_stream),
  (aleph5_setting(goodfile,F) ->
    aleph_open(F,append,Stream),
    aleph5_set_setting(goodfile_stream,Stream);
    true), !.
special_consideration(good,false):-
  aleph5_restore_setting(goodfile_stream), !.
special_consideration(goodfile,File):-
  aleph5_restore_setting(goodfile_stream),
  (aleph5_setting(good,true) ->
    aleph_open(File,append,Stream),
    aleph5_set_setting(goodfile_stream,Stream);
    true), !.
special_consideration(minscore,_):-
  aleph_abolish('$aleph_feature'/2), !.
special_consideration(_,_).

rm_special_consideration(portray_literals,_):-
  restore_setting(print),
  !.
rm_special_consideration(refine,_):-
  restore_setting(refineop),
  !.
rm_special_consideration(record,_):-
  aleph5_restore_setting(recordfile_stream),
  !.
rm_special_consideration(recordfile_stream,_):-
  (
    aleph5_setting(recordfile_stream, Stream),
    is_stream(Stream)
  ->
    close(Stream)
  ;
    true
  ),
  !.
rm_special_consideration(good,_):-
  aleph5_restore_setting(goodfile_stream), !.
rm_special_consideration(goodfile_stream,_):-
  (aleph5_setting(goodfile_stream,S) -> close(S); true), !.
rm_special_consideration(_,_).

get_hyp((Head:-Body)):-
  '$aleph_search'(pclause,pclause(Head,Body)), !.
get_hyp(Hypothesis):-
        '$aleph_global'(hypothesis,hypothesis(_,Hypothesis,_,_)).

add_hyp(end_of_file):- !.
add_hyp(Clause):-
        nlits(Clause,L),
  label_create(Clause,Label),
        extract_count(pos,Label,PCount),
        extract_count(neg,Label,NCount),
        retractall('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
        extract_pos(Label,P),
        extract_neg(Label,N),
  aleph5_setting(evalfn,Evalfn),
  complete_label(Evalfn,Clause,[PCount,NCount,L],Label1),
        asserta('$aleph_global'(hypothesis,hypothesis(Label1,Clause,P,N))).

add_hyp(Label,Clause,P,N):-
        retractall('$aleph_global'(hypothesis,hypothesis(_,_,_,_))),
        asserta('$aleph_global'(hypothesis,hypothesis(Label,Clause,P,N))).

add_theory(Label,Theory,PCover,NCover):-
        member(C,Theory),
  add_hyp(Label,C,PCover,NCover),
  update_theory(_),
        fail.
add_theory(_,_,PCover,NCover):-
  rm_seeds(pos,PCover),
  (aleph5_setting(evalfn,posonly) -> rm_seeds(rand,NCover); true),
  '$aleph_global'(atoms_left,atoms_left(pos,PLeft)),
  interval_count(PLeft,PL),
  p1_message('atoms left'), p_message(PL), !.

add_gcws:-
  retract('$aleph_search'(gcwshyp,hypothesis(L,C,P,N))),
  asserta('$aleph_global'(hypothesis,hypothesis(L,C,P,N))),
  update_theory(_),
  fail.
add_gcws.

restorehyp:-
  retract('$aleph_local'(pclause,pclause(Head,Body))),
  assertz('$aleph_search'(pclause,pclause(Head,Body))), !.
restorehyp:-
  retract('$aleph_local'(hypothesis,hypothesis(Label,Clause1,P,N))),
        asserta('$aleph_global'(hypothesis,hypothesis(Label,Clause1,P,N))), !.
restorehyp.

get_hyp_label(_,Label):- var(Label), !.
get_hyp_label((_:-Body),[P,N,L]):-
        nlits(Body,L1),
        L is L1 + 1,
        ('$aleph_search'(covers,covers(_,P))-> true;
                        covers(_),
                        '$aleph_search'(covers,covers(_,P))),
        ('$aleph_search'(coversn,coverns(_,N))-> true;
                        coversn(_),
                        '$aleph_search'(coversn,coversn(_,N))).

show_global(Key,Pred):-
  '$aleph_global'(Key,Pred),
  copy_term(Pred,Pred1),
  numbervars(Pred1,0,_),
  aleph_writeq(Pred1),
  current_stream(Stream),
  format(Stream, '.\n', []),
  fail.
show_global(_,_).

aleph_portray(hypothesis,true):-
  aleph_portray(hypothesis), !.
aleph_portray(hypothesis,false):-
  p_message('hypothesis'),
  hypothesis(Head,Body,_),
  pp_dclause((Head:-Body)), !.
aleph_portray(_,hypothesis):-  !.

aleph_portray(search,true):-
  aleph_portray(search), !.
aleph_portray(search,_):- !.

aleph_portray(train_pos,true):-
  aleph_portray(train_pos), !.
aleph_portray(train_pos,_):-
  !,
  aleph5_setting(train_pos,File),
  show_file(File).

aleph_portray(train_neg,true):-
  aleph_portray(train_neg), !.
aleph_portray(train_neg,_):-
  !,
  aleph5_setting(train_neg,File),
  show_file(File).

aleph_portray(test_pos,true):-
  aleph_portray(test_pos), !.
aleph_portray(test_pos,_):-
  !,
  aleph5_setting(test_pos,File),
  show_file(File).

aleph_portray(test_neg,true):-
  aleph_portray(test_neg), !.
aleph_portray(test_neg,_):-
  !,
  aleph5_setting(test_neg,File),
  show_file(File).

aleph_portray(Lit,true):-
  aleph_portray(Lit), !.
aleph_portray(Lit,_):-
  aleph_writeq(Lit).

aleph_writeq(Lit):-
  current_stream(Stream),
  write_term(Stream, Lit, [numbervars(true), quoted(true)]).

show_file(File):-
  aleph_open(File,read,ReadStream),
  repeat,
  read(Stream,Clause),
  (
    Clause = end_of_file
  ->
    close(ReadStream)
  ;
    writeq(Stream, Clause),
    format(Stream, '.\n', []),
    fail
  ).

time_loop(0,_,[]):- !.
time_loop(N,P,[T|Times]):-
  wallclock(S),
        P,
  wallclock(F),
  T is F - S,
        N1 is N - 1,
        time_loop(N1,P,Times).

list_profile:-
  % get number of calls for each profiled procedure
  findall(D-P,profile_data(P,calls,D),LP),
  % sort them
  sort(LP,SLP),
  % and output (note the most often called predicates will come last
  current_stream(Stream),
  write_profile_data(Stream, SLP).

write_profile_data(_Stream, []).
write_profile_data(Stream, [D-P|SLP]):-
  % just swap the two calls to get most often called predicates first.
  format(Stream, '~w: ~w\n', [P,D]),
  write_profile_data(Stream, SLP).

set_current_stream(Stream):-
  retractall(current_stream(_Stream)),
  asserta(current_stream(Stream)).

%! aleph5_set_setting(+Name:atom, +Value) is semidet.
% Sets the new value for the setting with the given name.

aleph5_set_setting(Name,Value):-
  set_setting(Name, Value),
  % @tbd This broadcast is never listened to.
  broadcast(set(Name, Value)),
  special_consideration(Name, Value).

