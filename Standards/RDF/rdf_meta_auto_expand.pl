:- module(
  rdf_meta_auto_expand,
  [
    (rdf_meta_expand)/1,
    op(1150, fx, (rdf_meta_expand))
  ]
).

/** <module> RDF meta auto expand
RDF_META_AUTO_EXPAND
Module created to automate the insertion
of rdf_global_id statements in predicates
that need these conversions.

Usage:
The directive 'rdf_meta_expand' can be used
to register a predicate to be auto-expanded.
Each argument of this predicate is replaced
by either:
- e : Expand this term
- i : Don't expand this term

Example:
:- rdf_meta_expand pred(e,e,i).

pred(Converted, AlsoConverted, NotConvertedLabel):-
  rdf(Converted, rdf:type, rdfs:Class),
  rdf(Converted, rdf:type, rdfs:Class),
  writeln(NotConvertedLabel).

:- X = rdfs:domains, pred(X, rdfs:range, label).

Result:
The resulted listing of pred will be:
pred(Converted2, AlsoConverted2, NotConvertedLabel):-
  rdf_global_id(Converted2, Converted),
  rdf_global_id(AlsoConverted2, AlsoConverted),
  rdf(Converted, rdf:type, rdfs:Class),
  rdf(Converted, rdf:type, rdfs:Class),
  writeln(NotConvertedLabel).
*/

:- dynamic rdf_meta_expand_db/1.



rdf_meta_expand(Term):-
  ( 
    \+(rdf_meta_expand_db(Term))
  ->
    assert(rdf_meta_expand_db(Term))
  ;
    true
  ).

%% expand_body(+Expansions, +Body, -ExpandedBody)
% Expand the original Body with each element in
% the list of Expansions by combining them in 
% a conjunction using the ',' operator.

expand_body([], Body, Body).
expand_body([Expansion], Body, (Expansion, Body)).
expand_body([Expansion|Expansions], Body, (Expansion, ExpandedBody)):-
  expand_body(Expansions, Body, ExpandedBody).

%% rdf_meta_expand_all(
%%   +ArgumentTypes,
%%   +OriginalArguments
%%   -NewArguments
%%   -Expansions
%% )
% For each argument in OriginalArgument that 
% has the corresponding argument type 'e':
%   1. Add a new variable to the new arguments.
%   2. Add a rdf_global_id statement to the expansions.

rdf_meta_expand_all([], [], [], []).
rdf_meta_expand_all([i|ArgTypes], [Arg|Args], [Arg|NewArgs], Expansions):-
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).
rdf_meta_expand_all([e|ArgTypes], [Arg|Args], [Arg|NewArgs], Expansions):-
  \+ var(Arg),!,
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).
rdf_meta_expand_all(
  [e|ArgTypes], 
  [Arg|Args], 
  [NewArg|NewArgs],
  [rdf_global_id(NewArg,Arg) | Expansions]
):-
  rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions).

system:term_expansion(TermIn, TermOut):-
  (
    % If TermIn is a predicate
    TermIn =.. [:-, Head,  Body],
    % Separate Functor from Args
    Head =.. [Functor | Args],
    % Create a list of unbounded variables 
    % of length ArgLen.
    length(Args, ArgLen),
    length(ArgTypes, ArgLen),
    % Create a query term that can be used
    % to match TermIn on the list of
    % predicates set to auto-expand
    MatchTerm =.. [Functor|ArgTypes],
    % Match query to auto-expand fact base
    rdf_meta_expand_db(MatchTerm),
    % Create lists of renamed arguments and 
    % expansions to the body.
    rdf_meta_expand_all(ArgTypes, Args, NewArgs, Expansions),
    % Expand the original Body with Expansions
    expand_body(Expansions, Body, ExpandedBody),
    % Create the (altered) TermOut
    NewHead =.. [Functor | NewArgs],
    TermOut =.. [:-, NewHead, ExpandedBody],
    !
  ;
    % Either something went wrong
    % or TermIn was not a predicate
    TermOut = TermIn
  ).

